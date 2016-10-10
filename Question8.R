
DATA_PATH="/Users/kviwoto/Documents/CoursHEC/LogicielsStatistiques/Projet"
DATA_FILE_NAME="Airplane_Crashes_and_Fatalities_Since_1908.csv"


setwd(DATA_PATH)

airPlanesCrashesData = read.csv(DATA_FILE_NAME, 
                               header=TRUE,
                               blank.lines.skip = TRUE,
                               stringsAsFactors = FALSE)  # read csv file

AIR_FRANCE <- list(name="AIR_FRANCE", keywords=c("Air","France"))
AIR_CANADA <- list(name="AIR_CANADA", keywords=c("Air", "Canada"))
TRANS_CANADA <- list(name="TRANS_CANADA", keywords=c("Trans","Canada"))
AIR_ONTARIO <- list(name="AIR_ONTARIO", keywords=c("Air","Ontario"))
AIR_INDIA <- list(name="AIR_INDIA", keywords=c("Air","India"))
AIR_CARAIBES <- list(name="AIR_CARAIBES", keywords=c("Air", "Caraibes"))
AIR_MADAGASCAR <- list(name="AIR_MADAGASCAR", keywords=c("Air","Madagascar"))
AIR_NIAGARA <- list(name="AIR_NIAGARA", keywords=c("Air", "Niagara"))
AIR_GUADELOUPE <- list(name="AIR_GUADELOUPE", keywords=c("Air", "guadeloupe"))
AIR_AMERICA <- list(name="AIR_AMERICA", keywords=c("Air", "America"))
AIR_MALI <- list(name="AIR_MALI", keywords=c("Air", "Mali"))
DEUTSCHE_LUFTHANSA <- list(name="DEUTSCHE_LUFTHANSA", keywords=c("deutsche", "lufthansa"))
CHINA_AIRLINES <- list(name="CHINA_AIRLINES", keywords=c("China", "Airlines"))

OPERATORS <- c(AIR_FRANCE,AIR_CANADA,TRANS_CANADA,AIR_ONTARIO,AIR_INDIA,AIR_CARAIBES,
               AIR_MADAGASCAR,AIR_NIAGARA,AIR_GUADELOUPE,AIR_AMERICA,AIR_MALI,DEUTSCHE_LUFTHANSA,CHINA_AIRLINES)


## Enlever les blancs en debut en fin de chaines de caractères
trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)
}

isValidEntry <- function(entryArg) {
  entry = trim(entryArg)
  if(entry == "" || is.na(entry) || is.null(entry) || length(entry) == 0){
    return(FALSE)
  }
  return(TRUE)
}

wordContainsAllSelectedWords <- function(aWord, selectedWords){
  
  if(!isValidEntry(aWord)){
    return(FALSE)
  }
  MATCHES_COUNT = 0
  lowerCaseWord <- tolower(aWord)
  
  for(aSelectedWord in as.vector(selectedWords)){
    lowerCaseSelectedWord = tolower(aSelectedWord)
    print(selectedWords)
    print(lowerCaseSelectedWord)
    if(grepl(lowerCaseWord,lowerCaseSelectedWord,ignore.case = TRUE)){
      MATCHES_COUNT = MATCHES_COUNT + 1
    }
  }
  
  return (MATCHES_COUNT == length(selectedWords))
}

getOperatorName <- function(operatorArg){
  for(i in 1:length(OPERATORS)){
    if (i %% 2 == 0){
      if (wordContainsAllSelectedWords(operatorArg,OPERATORS[i])){
        return(OPERATORS[i-1])
      }
    }
  }
  
  return("INVALID")
}

getFatality <- function(fatalityArg){
 
  if(isValidEntry(fatalityArg)){
    return(fatalityArg)
  }
  
  return("INVALID")
}


createOperatorWithFatalitiesFrame <- function(dataFrameToTest) {
  
    operatorNamesList <- c()
    fatalitiesList <- c()
    
    for(i in 1:40){
      operatorName <- getOperatorName(dataFrameToTest$Operator[i])
      fatality <- getFatality(dataFrameToTest$Fatalities[i])
      if(operatorName != "INVALID" && fatality != "INVALID"){
        operatorNamesList <- c(operatorNamesList, operatorName)
        fatalitiesList <- c(fatalitiesList, fatality)
      }
    }
    
    operatorWithFatalitiesFrame = data.frame("Operator"=operatorNamesList, "Fatality"=fatalitiesList) 
}

operatorWithFatalitiesFrame <- createOperatorWithFatalitiesFrame(airPlanesCrashesData)

head(operatorWithFatalitiesFrame)
