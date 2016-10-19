
DATA_PATH="/Users/kviwoto/Documents/CoursHEC/LogicielsStatistiques/Projet"
DATA_FILE_NAME="Airplane_Crashes_and_Fatalities_Since_1908.csv"


setwd(DATA_PATH)

airPlanesCrashesData = read.csv(DATA_FILE_NAME, 
                               header=TRUE,
                               blank.lines.skip = TRUE,
                               stringsAsFactors = FALSE)  # read csv file

AIR_FRANCE <- c("Air","France")
AIR_CANADA <- c("Air", "Canada")
TRANS_CANADA <- c("Trans","Canada")
AIR_ONTARIO <- c("Air","Ontario")
AIR_INDIA <- c("Air","India")
AIR_CARAIBES <- c("Air", "Caraibes")
AIR_MADAGASCAR <- c("Air","Madagascar")
AIR_NIAGARA <- c("Air", "Niagara")
AIR_GUADELOUPE <- c("Air", "guadeloupe")
AIR_AMERICA <- c("Air", "America")
AIR_MALI <- c("Air", "Mali")
DEUTSCHE_LUFTHANSA <- c("deutsche", "lufthansa")
CHINA_AIRLINES <- c("China", "Airlines")

OPERATORS <- data.frame("AIR_FRANCE"=AIR_FRANCE, "AIR_CANADA"=AIR_CANADA,"TRANS_CANADA"=TRANS_CANADA,"AIR_ONTARIO"=AIR_ONTARIO,
                        "AIR_INDIA"=AIR_INDIA,"AIR_CARAIBES"=AIR_CARAIBES,"AIR_MADAGASCAR"=AIR_MADAGASCAR,"AIR_NIAGARA"=AIR_NIAGARA,
                        "AIR_GUADELOUPE"=AIR_GUADELOUPE,"AIR_AMERICA"=AIR_AMERICA,"AIR_MALI"=AIR_MALI,"DEUTSCHE_LUFTHANSA"=DEUTSCHE_LUFTHANSA,
                        "CHINA_AIRLINES"=CHINA_AIRLINES)


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
  
  for(i in 1:length(selectedWords)){
    
    lowerCaseSelectedWord = tolower(selectedWords[i])
    if(grepl(lowerCaseSelectedWord,lowerCaseWord,ignore.case = TRUE)){
      MATCHES_COUNT = MATCHES_COUNT + 1
    }
  }
  
  return (MATCHES_COUNT == length(selectedWords))
}

getOperatorName <- function(operatorArg){
  
  for (i in names(OPERATORS)) {
    keywords <- OPERATORS[[i]]
    if (wordContainsAllSelectedWords(operatorArg,keywords)){
      return(names(OPERATORS[i]))
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
    
    for(i in 1:nrow(dataFrameToTest)){
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

fatalitiesSumGroupByOperator = aggregate(operatorWithFatalitiesFrame$Fatality, list(Operator=operatorWithFatalitiesFrame$Operator), FUN=sum,na.rm=TRUE)

legendLabels <- c("AIR_FRANCE","TRANS_CANADA","AIR_ONTARIO","AIR_INDIA","AIR_CARAIBES","AIR_MADAGASCAR",
                  "AIR_NIAGARA","AIR_GUADELOUPE","AIR_AMERICA","AIR_MALI","DEUTSCHE_LUFTHANSA","CHINA_AIRLINES")

legendColours <- c("red", "orange", "blue", "yellow", "green", "grey", "orange", "black")

plotName <- "Comparaison de morts par operateur aérien"
barplot(fatalitiesSumGroupByOperator$x, main=plotName, 
          xlab="Operateur", ylab="Nombre total de morts",
          cex.lab = 1.0, cex.main = 1.4, beside=TRUE, col=legendColours,
          legend.text = legendLabels,
          args.legend = list(x = "topright"))
