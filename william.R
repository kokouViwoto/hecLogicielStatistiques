# Projet - 6-613-11 Logiciels statistiques pour analyse de donnees (S02) 
# Nom: William Tankou - Matricule 11118098

# MAKE SURE YOU SET THE CURRENT DIRECTORY AS THE WORKING DIRECTORY
# In Rstudio, click on 'Session' -> 'Set Working Directory' -> 'To Source File Location'

#install.packages(ggplot2)
#install.packages("data.table")

#import packages
library(plyr)
library(data.table)


#library(countrycode)

# Read data - We assume the data are located in a folder named data placed in the same location as the code
crashes = read.csv("data/Airplane_Crashes_and_Fatalities_Since_1908.csv", header=TRUE, sep=",", dec = ",", 
                   fill = TRUE, stringsAsFactors=TRUE)

# Replace column name 'Flight..' by 'Flight' to keep our sanity
names(crashes)[names(crashes) == 'Flight..'] <- 'Flight'

canada = c('Nunavut', 'Quebec', 'Northwest Territories', 'Ontario',
           'British Columbia', 'Alberta', 'Saskatchewan', 'Manitoba',
           'Yukon', 'Newfoundland and Labrador', 'New Brunswick',
           'Nova Scotia', 'Prince Edward Island')

errors = list( "(Bolivia"="Bolivia", "(Russia"="Russia", 
               "Afghanstan"='Afghanistan', 'Airzona'='Arizona',
               "Alaksa"="Alaska", "Alakska"="Alaska", "Arazona"="Arizona",
               "Aregntina"="Argentina", "Atlantic"="Atlantic Ocean", 
               "AtlantiOcean"="Atlantic Ocean", "BaltiSea"="Baltic Sea", 
               "Boliva"="Bolivia", "Bosnia-Herzegovina"="Bosnia Herzegovina",
               "British Columbia Canada"="British Columbia", 
               "Belgium Congo"="Zaire", "Belgian Congo (Zaire)"="Zaire",
               "Belgian Congo"="Zaire", "Bulgeria"="Bulgaria",
               "Cailifornia"="California", "Calilfornia"="California",
               "Cameroons"="Cameroon", "Canada2"="Canada", "Cape Verde Islands"="Cape Verde",
               "Chili"="Chili", "Coloado"="Colorado", "Comoro Islands" = "Comoros",
               "Comoros Islands" = "Comoros", "D.C."="United States", "Deleware"="Delaware",
               "Algiers"="Algeria", "Aires"="Argentina", "PacifiOcean"="Pacific Ocean",
               "Mediterranean"="Mediterranean Sea", "Wisconson"="Wisconsin",
               "DemocratiRepubliCogo"="Zaire", "DemocratiRepubliof Congo"="Zaire",
               "DemoctratiRepubliCongo"="Zaire","Djbouti"="Djibouti","Domincan Republic"="Dominican Republic",
               "Dominica"="Dominican Republic", "Hunary"="Hungary","Virginia."="Virginia", "Vienna"="Austria",
               "the Mediterranean"="Mediterranean Sea", "Thiland"="Thailand", "Moscow"="Russia",
               "Massachutes"="Massachusetts","Louisana"="Louisiana","Jamacia"="Jamaica",
               "Inodnesia"="Indonesia", "Amsterdam"="Netherlands", "Zaïre"="Zaire",
               "Yugosalvia"="Yugoslavia", "Washingon"="Washington", "U.S. Virgin Islands"="US Virgin Islands",
               "UK"="United Kingdom", "UAE"="United Arab Emirates", "Tennesee"="Tennessee",
               "South Africa (Namibia)"="Namibia", "Russian"="Russia", "Rico"="Porto Rico",
               "Qld. Australia"="Australia", "Queensland  Australia"="Australia", 
               "Phillipines"="Philippines", "Philipines"="Philippines", "New York (Idlewild)"="New York",
               "Morroco"="Morrocco", "Minnisota"="Minnesota", "Yukon Territory"="Yukon",
               "Surinam"="Suriname", "South Dekota"="South Dakota", "Republiof Djibouti"="Djibouti",
               "Republiof Djibouti"="Djibouti", "Georgia"="Georgia_", "Okinawa"="Japan", "Oklohoma"="Oklahoma"
)
sea_words = c('Sea', 'Ocean', 'Channel', 'Mediterranean', 'miles', 'Gulf', 'Strait', 'off', "NE of")
colors = sample(colors())
training_words = c('Test', 'Training', 'Demonstration')

fix_country_name <- function(input){  # return corrected country if possible
  if(input %in% names(errors))
    return(errors[[input]])
  else
    return(input)
}

get_surface <- function(input){ # return either "Land" or "Sea"
  if(is.na(input))
    return(NA)
  
  for(wrd in sea_words){
    if(grepl(wrd, input, ignore.case = TRUE))
      return('Sea')
  }
  return('Land')
}

get_activity <- function(input){
  if(is.na(input))
    return(NA)
  
  for(wrd in training_words){
    if(grepl(wrd, input, ignore.case = TRUE))
      return('Training')
  }
  return('NO-Training')
}

get_survivor_rate <- function(aboard, fatalities){
  aboard = as.numeric(aboard)
  fatalities = as.numeric(fatalities)
  return( (aboard-fatalities)*100/aboard )
}

# Prepping the data -> Build dataframe
crashes = as.data.frame( t(
  apply(crashes, 1, function(row) {  # use apply instead of loop
    if(grepl("Military", row['Operator']) || grepl("Air Force", row['Operator']) 
       || grepl("Army", row['Operator']) || grepl("Navy", row['Operator'])) 
      cat = "Military"
    else 
      cat = "Commercial"
    
    loc = unlist(strsplit(row['Location'],','), use.names=FALSE)
    country = trimws(loc[3])
    count = trimws(loc[2])
    count2 = trimws(loc[1])
    
    if(is.na(country))
      country = count
    
    if( is.na(country)){
      loc2 = unlist(strsplit(count2,' '), use.names=FALSE)
      loc3 = tail(loc2, n=1)    # last element in the array
      loc4 = tail(loc2, n=2)[1] # one before the last element
      
      if(!is.na(loc3) && (loc3 %in% sea_words) )
        loc3 = country = paste(loc4, loc3, sep=" ")
      
      country = loc3 
    }
    country = fix_country_name(country)
    
    if( !is.na(match(country, state.abb)) ||  !is.na(match(country, state.name)) )
      country = "United States"
    else if(country %in% canada)
      country ='Canada'

    surface = get_surface(country)
    activity = get_activity(row['Route'])
    
    known_destination = toString(trimws(tail(unlist(strsplit(row['Route'],'-'), use.names=FALSE), n=1)))
    destination_city = trimws(unlist(strsplit(known_destination, ','), use.names=FALSE))[1]
    destination_country = fix_country_name(toString(tail(
      trimws(unlist(strsplit(known_destination, ','), use.names=FALSE)), n=1)))
    
    survival_rate = get_survivor_rate(row['Aboard'], row['Fatalities'])
    
    return(c(row, Category=cat, CrashCountry=country, Surface=surface, 
             Destination=known_destination, DestinationCity=destination_city,
             DestinationCountry=destination_country, Activity=activity,
           Survival_Rate=survival_rate))
   }
  )
 )
)

crashes$Survival_Rate <- as.numeric(as.character(crashes$Survival_Rate))

#*************  ANSWERS TO QUESTIONS: **************#
### QUESTION 1 - DANGEROUS PLACES ###  

# top15 - accidents / country where crash happened
crash_freq = count(crashes, 'CrashCountry')
crash_freq = as.data.frame(crash_freq[order(crash_freq$freq, decreasing = TRUE),])
names(crash_freq)[names(crash_freq) == 'freq'] <- 'Plane_Crashes_by_Country'
top15_crash_freq = head(crash_freq, n = 10)

# top15 - accidents / destinations
crash_dest_freq = count(crashes, 'DestinationCity')
crash_dest_freq = as.data.frame(crash_dest_freq[order(crash_dest_freq$freq, decreasing = TRUE),])
names(crash_dest_freq)[names(crash_dest_freq) == 'freq'] <- 'Crashes_by_Destination'
crash_dest_freq = crash_dest_freq[! crash_dest_freq$DestinationCity %in% c('Training', NA, 'Test flight', 'Sightseeing'),]
top15_crash_dest_freq = head(crash_dest_freq, n = 10)

# accidents / countries / flight categories
cr_cat_freq = count(crashes, c('CrashCountry','Category'))
cr_cat_freq = as.data.frame(cr_cat_freq[order(cr_cat_freq$freq, decreasing = TRUE),])
names(cr_cat_freq)[names(cr_cat_freq) == 'freq'] <- 'Number_of_Planes_Crashes'

top15_cr_cat_mil_freq = head(cr_cat_freq[cr_cat_freq$Category=='Military',], n = 10)
top15_cr_cat_com_freq = head(cr_cat_freq[!cr_cat_freq$Category=='Military',], n = 10)

# Which COuntry has the most military plane crashes - Same as top15_cr_cat_mil_freq
#mil_loc_freq = count( crashes[crashes$Category=='Military', ] , 'CrashCountry' )
#mil_loc_freq = as.data.frame(mil_loc_freq[order(mil_loc_freq$freq, decreasing = TRUE),])
#names(mil_loc_freq)[names(mil_loc_freq) == 'freq'] <- 'Military_Plane_Crashes_per_Country'

#--- PLOTS QUESTION 1
colors = sample(colors)
par(mfrow=c(3,1))
barplot( top15_crash_freq$Plane_Crashes_by_Country, names.arg = top15_crash_freq$CrashCountry,
         ylim = c(0, 1800), sub = "TOP10 - COUNTRIES WHERE CRASHES OCCURED",
         col=colors, ylab = "Number of Plane Crashes")

colors = sample(colors)
barplot( top15_cr_cat_mil_freq$Number_of_Planes_Crashes, 
         names.arg = top15_cr_cat_mil_freq$CrashCountry,
         ylim = c(0, 150), sub = "TOP10 - MILITARY CRASH SITES",
         col=colors, ylab = "Number of Plane Crashes")

colors = sample(colors)
barplot( top15_cr_cat_com_freq$Number_of_Planes_Crashes, 
         names.arg = top15_cr_cat_com_freq$CrashCountry,
         ylim = c(0, 1400), sub = "TOP10 - COMMERCIAL CRASH SITES",
         col=colors, ylab = "Number of Plane Crashes")

par(mfrow=c(1,1))
colors = sample(colors)
barplot( top15_crash_dest_freq$Crashes_by_Destination, names.arg = top15_crash_dest_freq$DestinationCity,
         ylim = c(0, 40), sub = "TOP10 - DESTINATIONS OF PLANE", col=colors,
         ylab = "Number of Plane Crashes")

#barplot( mil_loc_freq$Military_Plane_Crashes_per_Country, 
#         names.arg = mil_loc_freq$CrashCountry,
#         ylim = c(0, 150), sub = "TOP10 - DESTINATIONS OF PLANE", col=colors)

### Question 2- RISKIER PLANES CATEGORY ###
cat_freq = count(crashes, 'Category')
cat_freq = as.data.frame(cat_freq[order(cat_freq$freq, decreasing = TRUE),])
names(cat_freq)[names(cat_freq) == 'freq'] <- 'Plane_Crashes_by_Category'

#TODO: Chart + Explanation

# Do planes have the most accidents at Sea or in Land
sur_freq = count(crashes, c('Surface','Category'))
sur_freq = as.data.frame(sur_freq[order(sur_freq$freq, decreasing = TRUE),])
names(sur_freq)[names(sur_freq) == 'freq'] <- 'Plane_Crashes_Per_Surface'
sur_freq[is.na(sur_freq)] = 'Unknown'

sur_mil_freq = sur_freq[sur_freq$Category=='Military',]
sur_com_freq = sur_freq[!sur_freq$Category=='Military',]

#--- PLOTS QUESTION 2
colors = sample(colors)
pie( cat_freq$Plane_Crashes_by_Category, labels = cat_freq$Category,
     sub="Categories of plane crashes", col=colors)

par(mfrow=c(1,2))
colors = sample(colors)
pie( sur_mil_freq$Plane_Crashes_Per_Surface, labels = sur_mil_freq$Surface,
     sub="SURFACE - MILITARY PLANES", col=colors)

colors = sample(colors)
pie( sur_com_freq$Plane_Crashes_Per_Surface, labels = sur_com_freq$Surface,
     sub="SURFACE - COMMERCIAL PLANES", col=colors)

#### 3 - POURCENTAGE DE SURVIE ####
chance_of_survival = aggregate( crashes$Survival_Rate, 
             by=list(crashes$Category, crashes$Activity), 
             FUN=mean, na.rm=TRUE, simplify=TRUE)

names(chance_of_survival) = c('Category','During_Trainig','Survival_Rate')
chance_mil_freq = chance_of_survival[chance_of_survival$Category=='Military',]
chance_com_freq = chance_of_survival[!chance_of_survival$Category=='Military',]
chance_mil_freq$During_Trainig = paste("Military", chance_mil_freq$During_Trainig, sep='-')
chance_com_freq$During_Trainig = paste("Commercial", chance_com_freq$During_Trainig, sep='-')

chance_freq = rbind( chance_mil_freq[,2-3], chance_com_freq[,2-3] )

par(mfrow=c(1,1))
colors = sample(colors)
barplot( chance_freq$Survival_Rate, 
         names.arg = chance_freq$During_Trainig,
         ylim = c(0, 25), sub = "CHANCE OF SURVIVAL",
         col=colors, ylab = "Probability of Survival")