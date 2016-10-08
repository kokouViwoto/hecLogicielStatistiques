
DATA_PATH="/Users/kviwoto/Documents/CoursHEC/LogicielsStatistiques/Projet"
DATA_FILE_NAME="Airplane_Crashes_and_Fatalities_Since_1908.csv"


setwd(DATA_PATH)

airPlanesCrashesData = read.csv(DATA_FILE_NAME, 
                               header=TRUE,
                               blank.lines.skip = TRUE,
                               stringsAsFactors = FALSE)  # read csv file

###### Get Crash location 

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
               "Inodnesia"="Indonesia", "Amsterdam"="Netherlands"
)

sea_words = c('Sea', 'Ocean', 'Channel', 'Mediterranean', 'miles', 'Gulf', 'Strait', 'off')

EUROPE_COUNTRIES <- c("Albania","Andorra","Armenia","Austria","Azerbaijan","Belarus","Belgium","Bosnia",
                      "Herzegovina","Bulgaria","Croatia","Cyprus","Czech","Denmark","Estonia","Finland",
                      "France","Georgia","Germany","Greece","Hungary","Iceland","Ireland","Italy","Kazakhstan",
                      "Kosovo","Latvia","Liechtenstein","Lithuania","Luxembourg","Macedonia","Malta","Moldova",
                      "Monaco","Montenegro","Netherlands","Norway","Poland","Portugal","Romania","Russia",
                      "Marino","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Turkey","Ukraine",
                      "Kingdom","Vatican","Holy")

isValidEntry <- function(entry) {
  if(is.na(entry) || is.null(entry) || length(entry) == 0){
    return(FALSE)
  }
  return(TRUE)
}

getCrashLocation <- function(location){
  
  if(!isValidEntry(location)){
    return("INVALID")
  }
  
  if(location %in% names(errors)){
    return(errors[[location]])
  }
  
  if(location %in% canada){
    return("CANADA")
  }
  
  if(location %in% sea_words){
    return("SEA")
  }
 
  seaWithRegex <- "\\bSEA\\b"
  
  if(grepl(seaWithRegex, location,ignore.case = TRUE)){
    return("SEA")
  }
  
  locationWithRegex <- "\\bVIRGINIA\\b"
  
  if(grepl(locationWithRegex, location,ignore.case = TRUE)){
    return("VIRGINIA")
  }
  
  canadaWithRegex <- "\\bCANADA\\b"
  
  if(grepl(canadaWithRegex, location,ignore.case = TRUE)){
    return("CANADA")
  }
  
  
  for(europeCountry in EUROPE_COUNTRIES){
    europWithRegex <- paste("\\b",europeCountry,"\\b", sep="")
    if(grepl(europWithRegex, location,ignore.case = TRUE)){
      return("EUROPE")
    }
  }
  return(location)
}

## Enlever les blancs en debut en fin de chaines de caractères
trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)
}

################Question 7 #####################

getMonthFromDate <- function (currentDate){
  if(!isValidEntry(currentDate)){
    return("INVALID")
  }
  return(returntheMonth <- as.numeric(unlist(strsplit(currentDate, "[/]"))[1]))
}

createMonthAndLocationObjectFromDataSet <- function(dataFrameToTest) {
  
    monthsList <- c()
    locationsList <- c()
    
    for(i in 1:nrow(dataFrameToTest)){
      originalMonth <- trim(dataFrameToTest$Date[i])
      originalLocation <- trim(dataFrameToTest$Location[i])
      
      theMonth <- getMonthFromDate(originalMonth)
      location <- getCrashLocation(originalLocation)
      
      if(theMonth != "INVALID" && location != "INVALID"){
        monthsList <- c(monthsList, theMonth)
        locationsList <- c(locationsList, location) 
      }
    }
    dataFramFilterOutput = data.frame("Month"=monthsList, "Locations"=locationsList) 
}

monthAndLocationObjectFromDataSet <- createMonthAndLocationObjectFromDataSet(airPlanesCrashesData)



library(plyr)
head(count(monthsList, c("Month", "Locations")))
#crashesCountByMonthAndByDestination = aggregate(monthsList$Month, list(month=monthsList$Month,location=monthsList$Locations), FUN=sum,na.rm=TRUE)

