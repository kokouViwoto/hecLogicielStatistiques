# Projet - 6-613-11 Logiciels statistiques pour analyse de donnees (S02) 
# Nom: William Tankou - Matricule 11118098

# MAKE SURE TO SET THE CURRENT DIRECTORY AS THE WORKING DIRECTORY
# In Rstudio, click on 'Session' -> 'Set Working Directory' -> 'To Source File Location'

#import packages
library(plyr)
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

errors = list( "(Bolivia" = "Bolivia", "(Russia" = "Russia", 
               "Afghanstan" = 'Afghanistan', 'Airzona' = 'Arizona',
               "Alaksa"="Alaska", "Alakska"="Alaska", "Arazona"="Arizona",
               "Aregntina" = "Argentina", "Atlantic"="Atlantic Ocean", 
               "AtlantiOcean"="Atlantic Ocean", "BaltiSea"="Baltic Sea", 
               "Boliva" = "Bolivia", "Bosnia-Herzegovina"= "Bosnia Herzegovina",
               "British Columbia Canada" = "British Columbia", 
               "Belgium Congo"="Belgian Congo (Zaire)",
               "Belgian Congo" = "Belgian Congo (Zaire)", "Bulgeria"="Bulgaria",
               "Cailifornia"="California", "Calilfornia"="California",
               "Cameroons"="Cameroon", "Canada2"="Canada", "Cape Verde Islands"="Cape Verde",
               "Chili"="Chili", "Coloado"="Colorado", "Comoro Islands" = "Comoros",
               "Comoros Islands" = "Comoros", "D.C."="United States", "Deleware"="Delaware"
               )

fix_country_name <- function(input){
  if(input %in% names(errors))
    return(errors[[input]])
  else
    return(input)
}


# Add a new column for the Category and the country
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
        loc3 = tail(loc2, n=1)

        if(!is.na(loc3) && loc3 == 'Channel')  # Ebglish channel is considered as sea
          loc3 = 'Sea'
        country = loc3 
      }
      country = fix_country_name(country)
      #stop()
      
      
      if( !is.na(match(country, state.abb)) ||  !is.na(match(country, state.name)) )
        country = "United States"
      else if(country %in% canada)
        country ='Canada'
      
      return(c(row, Category=cat, Country=country))
    }
  )
 )
)

# crash frequencies
crash_freq = count(crashes, 'Country')
crash_freq = crash_freq[order(crash_freq$freq, decreasing = TRUE),]
names(crash_freq)[names(crash_freq) == 'freq'] <- 'Number_Plane_Crashes'

cat_freq = count(crashes, 'Category')
cat_freq = cat_freq[order(cat_freq$freq, decreasing = TRUE),]
names(cat_freq)[names(cat_freq) == 'freq'] <- 'Number_Plane_Crashes'

# Questions:
# 1. Quelles sont les Top 10 des destinations dangereuses (souligner la plus dangereuse)?  Bar chart (destination / nombre d’accidents) (william)
# accidents / pays du crash
# accidents / destinations
# accidents / countries by flight categories

#accident 
# where does military planes crash the most - USA because they have more planes?
# commercial plane

# 2. Quel type d’avion a connu le plus de crash? Creer des categories nous memes. (william)
# nombre de crash ou l'avion est de type commercial VERSUS nombre de crash ou l'avion est de type militaire

# 3. Quel est le pourcentage de chance de survie lors d’un crash. (Celui ci dépendra du type d’avions. Combien de passagers, destinations )? (william)
# survivants = aboard - fatalities