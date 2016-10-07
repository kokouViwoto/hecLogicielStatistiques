
DATA_PATH="/Users/kviwoto/Documents/CoursHEC/LogicielsStatistiques/Projet"
DATA_FILE_NAME="Airplane_Crashes_and_Fatalities_Since_1908.csv"


setwd(DATA_PATH)

airPlanesCrashesData = read.csv(DATA_FILE_NAME, 
                               header=TRUE,
                               blank.lines.skip = TRUE,
                               stringsAsFactors = FALSE)  # read csv file


## Enlever les blancs en debut en fin de chaines de caractères
trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)
}

################Question 7 #####################

getMonthFromDate <- function (currentDate){
   theMonth <- as.numeric(unlist(strsplit(currentDate, "[/]"))[1])
}

createMonthColumnFromDataSet <- function(dataFrameToTest) {
  
    monthsList <- c()
    for(i in 1:nrow(dataFrameToTest)){
      originalMonth <- trim(dataFrameToTest$Date[i])
      
      if(!is.null(originalMonth) && !is.na(originalMonth) && length(originalMonth) > 0){
        theMonth <- getMonthFromDate(originalMonth)
        monthsList <- c(monthsList, theMonth)
      }
      else{
        monthsList <- c(monthsList, "INVALID")
      }
    }
    monthsList
}

monthsList <- createMonthColumnFromDataSet(airPlanesCrashesData)
