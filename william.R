# Projet - 6-613-11 Logiciels statistiques pour analyse de donnees (S02) 
# Nom: William Tankou - Matricule 11118098

# MAKE SURE YOU SET THE CURRENT DIRECTORY AS THE WORKING DIRECTORY
# In Rstudio, click on 'Session' -> 'Set Working Directory' -> 'To Source File Location'

#import packages
library(plyr)
library(ggplot2)
#library(countrycode)

# Read data - We assume the data are located in a folder named data placed in the same location as the code
crashes = read.csv("data/Airplane_Crashes_and_Fatalities_Since_1908.csv", header=TRUE, sep=",", dec = ",", 
                   fill = TRUE, stringsAsFactors=TRUE)

# Replace column name 'Flight..' by 'Flight' to keep our sanity
names(crashes)[names(crashes) == 'Flight..'] <- 'Flight'

# Preview the data
#head(crashes)

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
               "Republiof Djibouti"="Georgia_", "Georgia"="Georgia_", "Okinawa"="Japan", "Oklohoma"="Oklahoma"
               )
sea_words = c('Sea', 'Ocean', 'Channel', 'Mediterranean', 'miles', 'Gulf', 'Strait', 'off', "NE of")

colors = c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan") 

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

# Prepping the data -> adding Category-Country-
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
      #stop()
      if( !is.na(match(country, state.abb)) ||  !is.na(match(country, state.name)) )
        country = "United States"
      else if(country %in% canada)
        country ='Canada'
      #else if(!is.na(country) && (country == 'Sea' || country == 'Channel'))
      #  country = paste(loc4, loc3, sep=" ")
      
      surface = get_surface(country)
      
      known_destination = toString(trimws(tail(unlist(strsplit(row['Route'],'-'), use.names=FALSE), n=1)))
      destination_city = trimws(unlist(strsplit(known_destination, ','), use.names=FALSE))[1]
      destination_country = fix_country_name(toString(tail(
                              trimws(unlist(strsplit(known_destination, ','), use.names=FALSE)), n=1)))
      
      
      # ADD OPERATIONS COLUMNS (TEST, PRACTICE, COMMERCIAL...)
      return(c(row, Category=cat, CrashCountry=country, Surface=surface, 
               Destination=known_destination, DestinationCity=destination_city,
               DestinationCountry=destination_country))
    }
  )
 )
)

# Questions:
# 1. Quelles sont les Top 10 des destinations dangereuses (souligner la plus dangereuse)?  Bar chart (destination / nombre d’accidents) (william)

# accidents / pays du crash
crash_freq = count(crashes, 'CrashCountry')
crash_freq = as.data.frame(crash_freq[order(crash_freq$freq, decreasing = TRUE),])
names(crash_freq)[names(crash_freq) == 'freq'] <- 'Plane_Crashes_by_Country'
top10_crash_freq = head(crash_freq, n = 10)
#TODO: Chart + Explanation

# accidents / destinations

# accidents / countries / flight categories
cr_cat_freq = count(crashes, c('CrashCountry','Category'))
cr_cat_freq = as.data.frame(cr_cat_freq[order(cr_cat_freq$freq, decreasing = TRUE),])
names(cr_cat_freq)[names(cr_cat_freq) == 'freq'] <- 'Number_of_Planes_Crashes'
top10_cr_cat_freq = head(cr_cat_freq, n = 10)
#TODO: Chart + Explanation

#accident 
# where does military planes crash the most - USA because they have more planes?
# commercial plane


# 2. Quel type d’avion a connu le plus de crash? Creer des categories nous memes. (william)
# nombre de crash ou l'avion est de type commercial VERSUS nombre de crash ou l'avion est de type militaire
cat_freq = count(crashes, 'Category')
cat_freq = as.data.frame(cat_freq[order(cat_freq$freq, decreasing = TRUE),])
names(cat_freq)[names(cat_freq) == 'freq'] <- 'Plane_Crashes_by_Category'
#TODO: Chart + Explanation

# Do planes have the most accidents at Sea or in Land
sur_freq = count(crashes, c('Surface','Category'))
sur_freq = as.data.frame(sur_freq[order(sur_freq$freq, decreasing = TRUE),])
names(sur_freq)[names(sur_freq) == 'freq'] <- 'Plane_Crashes_Per_Surface'
#TODO: Chart + Explanation

# Which COuntry has the most military plane crashes
mil_loc_freq = count( crashes[crashes$Category=='Military', ] , 'CrashCountry' )
mil_loc_freq = as.data.frame(mil_loc_freq[order(mil_loc_freq$freq, decreasing = TRUE),])
names(mil_loc_freq)[names(mil_loc_freq) == 'freq'] <- 'Military_Plane_Crashes_per_Country'
#TODO: Chart + Explanation

# 3. Quel est le pourcentage de chance de survie lors d’un crash. (Celui ci dépendra du type d’avions. Combien de passagers, destinations )? (william)
# survivants = aboard - fatalities
#TODO Aggregate on means + Chart + Explanation




#counts <- table(crashes$CrashCountry)
#barplot(counts, main="Car Distribution", 
#        xlab="Number of Gears")


#q = table(head(arrange(crash_freq,desc(Plane_Crashes_by_Country)), n = 10))
#barplot(q, main="Car Distribution", 
#        xlab="Number of Gears")

#p = head(crash_freq, n = 10)

#ggplot(crashes, aes(x = factor(CrashCountry))) + geom_bar(stat = "count")

#p2<-ggplot(mtc,aes(x=factor(gear),y=wt,fill=factor(vs)), color=factor(vs)) +  
#  stat_summary(fun.y=mean,position="stack",geom="bar")