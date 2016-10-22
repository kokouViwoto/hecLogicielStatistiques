
DATA_PATH_FOLDER="/Users/kviwoto/Documents/CoursHEC/LogicielsStatistiques/Projet"
DATA_FILE_NAME="Airplane_Crashes_and_Fatalities_Since_1908.csv"

if (length(DATA_PATH_FOLDER) == 0 || length(DATA_FILE_NAME) == 0 ){ 
  stop("THE DATA_FILE_NAME AND DATA_PATH_FOLDER variables should be set")
}

setwd(DATA_PATH_FOLDER)

airPlanesCrashesData = read.csv(DATA_FILE_NAME, 
                               header=TRUE,
                               blank.lines.skip = TRUE,
                               stringsAsFactors = FALSE)  # read csv file



### Liste des catégories a charcher dans le data frame
BOMB_ATTACK_CATEGORIES <- c("DYNAMITE","BOMB","EXPLOSIVE")
ERROR_CATEGORIES <- c("ERROR")
SHOT_DOWN_CATEGORIES <- c("SHOT")
SUICIDE_CATEGORIES <- c("SUICIDE")
HEART_ATTACK_CATEGORIES <- c("HEART")
DRUNK_ATTACK_CATEGORIES <- c("ALCOHOL","DRINK", "DRUNK")

## Enlever les blancs en debut en fin de chaines de caractères
trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)
}

##################################################################################
# Cette fonction sert a sélectionner les descriptions contenant les mots choisis:
# Elle recoit en parametre: 
#      dataFrameToTest : le Dataframe a verifier
#      wordsToSearchFor: Liste de mots a verifier dans le dataframe.
# Elle retourne: 
#      dataFramFilterOutput: l'objet de type data frame contenant les resultats
#      filtré.
#
##################################################################################

selectSummaryContainingWords <- function (dataFrameToTest, wordsToSearchFor){

  selectedDates <- c()
  selectedTimes <- c()
  selectedLocations <- c()
  selectedOperators <- c()
  selectedFlights <- c()
  selectedRoutes <- c()
  selectedAboard <- c()
  selectedFatalities <- c()
  selectedSummaries <- c()
  
  for(i in 1:nrow(dataFrameToTest)){
    aSummary = trim(dataFrameToTest$Summary[i])
    alreadyAdded = FALSE;
    for (word in wordsToSearchFor) {
      wordWithRegex <- paste("\\b",word,"\\b", sep="")
      
      if(!alreadyAdded && nchar(aSummary) > 0 && grepl(wordWithRegex, aSummary,ignore.case = TRUE)){
        alreadyAdded = TRUE;
        selectedDates <- c(selectedDates, dataFrameToTest$Date[i])
        selectedTimes <- c(selectedTimes, dataFrameToTest$Time[i])
        selectedLocations <- c(selectedLocations, dataFrameToTest$Location[i])
        selectedOperators <- c(selectedOperators, dataFrameToTest$Operator[i])
        selectedFlights <- c(selectedFlights, dataFrameToTest$Flight[i])
        selectedRoutes <- c(selectedRoutes, dataFrameToTest$Route[i])
        selectedAboards <- c(selectedAboard, dataFrameToTest$Aboard[i])
        selectedFatalities <- c(selectedFatalities, dataFrameToTest$Fatalities[i])
        selectedSummaries <- c(selectedSummaries, aSummary)
      }
    }
  }
  
  dataFramFilterOutput = data.frame("Date"=selectedDates, "Time"=selectedTimes,
                                    "Location"=selectedLocations,"Operator"=selectedOperators,
                                    "Flight"=selectedFlights,"Routes"=selectedRoutes,
                                    "Aboard"=selectedAboards,"Fatalities"=selectedFatalities,"Summary"=selectedSummaries) 
  
}

### Appelle de la fonction avec les categories recherchées.
bombAttackCrashes <- selectSummaryContainingWords(airPlanesCrashesData,BOMB_ATTACK_CATEGORIES)
errorCrashes <- selectSummaryContainingWords(airPlanesCrashesData,ERROR_CATEGORIES)
shotDownCrashes <- selectSummaryContainingWords(airPlanesCrashesData,SHOT_DOWN_CATEGORIES)
suicideCrashes <- selectSummaryContainingWords(airPlanesCrashesData,SUICIDE_CATEGORIES)
heartAttackCrashes <- selectSummaryContainingWords(airPlanesCrashesData,HEART_ATTACK_CATEGORIES)
crazyPeopleCrashes <- selectSummaryContainingWords(airPlanesCrashesData,CRAZY_PEOPLE_CATEGORIES)
drunkPeopleCrashes <- selectSummaryContainingWords(airPlanesCrashesData,DRUNK_ATTACK_CATEGORIES)

#### Creation de Pie Chart pour les résultats trouvés.
slices <- c(length(bombAttackCrashes$Summary), length(errorCrashes$Summary), length(shotDownCrashes$Summary),
            length(suicideCrashes$Summary), length(heartAttackCrashes$Summary), length(drunkPeopleCrashes$Summary)) 
lbls <- c("bomb Attack", "human error", "shot down", "suicide", "heart attack", "drunk people")

pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Crashes")

