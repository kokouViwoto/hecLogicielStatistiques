
DATA_PATH_FOLDER="Change Me. This should be the path to the folder that contains the dataset"
DATA_FILE_NAME="Change Me. This should be the dataset file name"

setwd(DATA_PATH_FOLDER)

airPlanesCrashesData = read.csv(DATA_FILE_NAME, 
                               header=TRUE,
                               blank.lines.skip = TRUE,
                               stringsAsFactors = FALSE)  # read csv file

###### Get Crash location 

canada = c('Nunavut', 'Quebec', 'Northwest Territories', 'Ontario',
           'British Columbia', 'Alberta', 'Saskatchewan', 'Manitoba',
           'Yukon', 'Newfoundland and Labrador', 'New Brunswick',
           'Nova Scotia', 'Prince Edward Island',"Canada")

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

AFRICAN_COUNTRIES <- c("Abidjan","Algeria","Angola","Benin","Botswana","Burkina","Faso","Burundi","Cabo","Verde",
                      "Cameroon","African","Chad","Comoros","Congo","Cogo","Ivoiry","Djibouti","Egypt","Guinea",
                      "Eritrea","Ethiopia","Gabon","Gambia","Ghana","Guinea","Bissau","Kenya","Lesotho",
                      "Liberia","Libya","Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco",
                      "Mozambique","Namibia","Niger","Nigeria","Rwanda","Sao","Tome","Principe","Senegal",
                      "Seychelles","Sierra","Leone","Somalia","Africa","Sudan","Swaziland","Tanzania",
                      "Togo","Tunisia","Uganda","Zambia","Zimbabwe","Zaire","kinshasa","Morrocco")

WESTERN_HEMISPHERE <- c("Argentina","Bolivia","Brazil","Chile","Colombia","Costa","Rica","Cuba","Dominican","Ecuador",
                        "Salvador","Guatemala","Haiti","Honduras","Mexico","Nicaragua","Panama","Paraguay","Peru",
                        "Uruguay","Venezuela")

sea_words = c('Sea', 'Ocean', 'Channel', 'Mediterranean', 'miles', 'Gulf', 'Strait', 'off')

EUROPE_COUNTRIES <- c("Albania","Andorra","Armenia","Austria","Azerbaijan","Belarus","Belgium","Bosnia",
                      "Herzegovina","Bulgaria","Croatia","Cyprus","Czech","Denmark","Estonia","Finland",
                      "France","Georgia","Germany","Greece","Hungary","Iceland","Ireland","Italy","Kazakhstan",
                      "Kosovo","Latvia","Liechtenstein","Lithuania","Luxembourg","Macedonia","Malta","Moldova",
                      "Monaco","Montenegro","Netherlands","Norway","Poland","Portugal","Romania","Russia",
                      "Marino","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Turkey","Ukraine",
                      "Kingdom","Vatican","Holy","London","England","Roumania","Romainia","Amsterdam","holland")

SOUTHEAST_ASIA_COUNTRIES <- c("Australia","Brunei","Darussalam","Burma","Cambodia","Indonesia","Laos","Malaysia",
                              "Zealand","Islands","Island","Philippines","Philipines","Singapore","Thailand","Vietnam","Asian",
                              "Bangladesh","Asia","India","Iraq","Kazakhstan","Maldives","Nepal","Pakistan","Sri",
                              "Lanka","Uzbekistan","Hong","Kong","Macau","Mongolia","Taiwan","China","Japan","Korea")


US_CITIES <- c("Alaska","Alabama","Arkansas","Arizona","California","Colorado","Connecticut","Washington","Delaware",
               "Florida","Georgia","Hawaii","Iowa","Idaho","Illinois","Indiana","Kansas","Kentucky","Louisiana","Massachusetts",
               "Maryland","Maine","Michigan","Minnesota","Missouri","Mississippi","Montana","Carolina","Dakota","Nebraska",
               "Hampshire","Jersey","Mexico","Nevada","York","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode","Island",
               "Carolina","Dakota","Tennessee","Tennesee","Texas","Utah","Virginia","Vermont","Washington","Wisconsin","Wyoming",
               "United","States","Arazona","Ilinois","Oklohoma","Angeles","boston","Airzona")

MIDDLE_EAST <- c("Bahrain","Cyprus","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman",
                 "Qatar","Saudi","Arabia","Syria","Turkey","Arab","Emirates","Yemen")
  
isValidEntry <- function(entry) {
  if(entry == "" || is.na(entry) || is.null(entry) || length(entry) == 0){
    return(FALSE)
  }
  return(TRUE)
}

getCrashLocation <- function(locationArg){
  
  if(!isValidEntry(locationArg)){
    return("INVALID")
  }
  
  location = locationArg
  if(locationArg %in% names(errors)){
    location = errors[[location]]
  }
  
  if(location %in% canada){
    return("CANADA")
  }
  
  for(seaPlace in sea_words){
    
    lowerCaseLocation = tolower(location)
    lowerSeaPlace = tolower(seaPlace)
    if(grepl(lowerSeaPlace, lowerCaseLocation,ignore.case = TRUE)){
      return("SEA")
    }
  }
  
  for(middleEastCountry in MIDDLE_EAST){
    
    lowerCaseLocation = tolower(location)
    lowerMiddleEastCountry = tolower(middleEastCountry)
    if(grepl(lowerMiddleEastCountry, lowerCaseLocation,ignore.case = TRUE)){
      return("MIDDLE_EAST")
    }
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
  
  for(africanCountry in AFRICAN_COUNTRIES){
    
    lowerCaseLocation = tolower(location)
    lowerAfricanCountry = tolower(africanCountry)
    if(grepl(lowerAfricanCountry, lowerCaseLocation,ignore.case = TRUE)){
      return("AFRICA")
    }
  }
  
  for(westernCountry in WESTERN_HEMISPHERE){
    
    lowerCaseLocation = tolower(location)
    lowerWesternCountry = tolower(westernCountry)
    if(grepl(lowerWesternCountry, lowerCaseLocation,ignore.case = TRUE)){
      return("WESTERN_HEMISPHERE")
    }
  }
  
  for(southEastCountry in SOUTHEAST_ASIA_COUNTRIES){
    
    lowerCaseLocation = tolower(location)
    lowerSouthEastCountry = tolower(southEastCountry)
    if(grepl(lowerSouthEastCountry, lowerCaseLocation,ignore.case = TRUE)){
      return("SOUTHEAST_ASIA_COUNTRIES")
    }
  }
  
  for(usCity in US_CITIES){
    
    lowerCaseLocation = tolower(location)
    lowerUsCity = tolower(usCity)
    if(grepl(lowerUsCity, lowerCaseLocation,ignore.case = TRUE)){
      return("UNITED_STATES")
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
monthAndLocationObjectFromDataSet <- count(monthAndLocationObjectFromDataSet, c("Month", "Locations"))
monthAndLocationObjectFromDataSet <- monthAndLocationObjectFromDataSet[order(monthAndLocationObjectFromDataSet$Month, monthAndLocationObjectFromDataSet$freq),]

monthAndLocationObjectFromDataSet <- monthAndLocationObjectFromDataSet[monthAndLocationObjectFromDataSet$freq > 20,]

legendLabels <- c("AFRICA","MIDDLE_EAST","SEA","ASIA","HEMISPHERE","EUROPE","UNITED_STATES")
legendColours <- c("red", "orange", "blue", "yellow", "green", "grey", "orange")
par(mfrow=c(1,2))
for(i in 1:6){
  monthCalculation <- monthAndLocationObjectFromDataSet[monthAndLocationObjectFromDataSet$Month == i, ]
  plotName <- paste("Number of crashes for ",month.name[i], sep="")
  barplot(monthCalculation$freq, main=plotName, 
          xlab="Countries", ylab="Crash Places Count",
          cex.lab = 1.0, cex.main = 1.4, beside=TRUE, col=legendColours,
          legend.text = legendLabels,
          args.legend = list(x = "topleft"))
}
