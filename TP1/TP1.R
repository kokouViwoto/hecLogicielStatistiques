DATA_PATH_FOLDER="Change Me. This should be the path to the folder that contains the dataset"
DATA_FILE_NAME="Change Me. This should be the dataset file name"

setwd(DATA_PATH_FOLDER)

airplane_table = read.csv(DATA_FILE_NAME) 

summary(airplane_table)

names(airplane_table)

#####################################################################################

######################################## Question 1 à 5  #############################################################

#telechargement du package gggmap afin de pouvoir utiliser la fonction afficher_map 

install.packages("ggmap")
library(ggmap)
gpclibPermit()
library(maptools)
library(maps)

#####################################################################################

#Fonctions utilisées 

trouver_frequence= function(firstVector, secondVector){
  #cette fonction a pour but de trouver la frequence des variables specifiées
  #inclus dans le vecteur1 (firstVector) dans le deuxieme vecteur (secondVector)

  occurence_total = 0 #initialisation de la variable occurence_total
  for(i in firstVector){
    occurence = 0
    for(j in secondVector){
      if (i==j){
        occurence = occurence + 1
      }
    }
    occurence_total = c(occurence_total, occurence)
  }
  occurence_total[-1] #retourne le vecteur avec la frequence de chaque variable. la premiere valeure est omise
}

afficher_map = function(visited){
  #cette fonction affiche sur une map une series de ville basee sur leurs coordonees geo 
  #un vecteur doit avec des noms de villes et pays doit etre en input
  
  ll.visited = geocode(visited)
  visit.x = ll.visited$lon
  visit.y = ll.visited$lat
  
  mp = NULL
  mapWorld = borders("world", colour="gray50", fill="gray50")
  mp = ggplot() + mapWorld
  
  mp = mp+ geom_point(aes(x=visit.x, y=visit.y), shape = 21, colour = "red", fill = "white", size = 2, stroke = 3)
  mp 
}





#####################################################################################
# Question 1: Nombre total d'ecrasement à travers le temps
#####################################################################################

#préparation des données

#1
Annee = substr(airplane_table$Date, 7, 10) #extrait les annees de toutes les dates et les sauvegardent dans Annee

#2
table_temporaire = cbind(airplane_table[, 1:3], Annee) #combine une partie du jeu de donnees initiales et la variable Annee ensemble

#3
x = trouver_frequence(unique(Annee), table_temporaire$Annee) #appelle de la fonction qui retourne la frequence du nombre d'annees

#4
table_occurence_annee = as.data.frame(cbind(as.numeric(unique(Annee)), as.numeric(x)))#combination des annees et des frequences pour creer un data.frame a 2 colonnes

colnames(table_occurence_annee) = c("Annee", "Occurence") #definition du nom de chaque colonne dans table_occurence_annee

#5
table_occurence_annee = table_occurence_annee[order(as.numeric(as.character(table_occurence_annee$Annee))),] #ordonne mes donnees en ordre croissant en fonction de l'annee

table_occurence_annee

#Affiche le graphique du nombre de crashs qui a eu lieu a travers le temps
plot(x = table_occurence_annee$Annee, y = table_occurence_annee$Occurence, type="l", main= "Crashs à travers le Temps", xlab="Années", ylab="Nombre de crashs")



#####################################################################################
#Question 2: Nombre de fatalites par Année à travers le temps
#####################################################################################

#preparation des donnees

table_de_fatalites_par_annee = data.frame(as.numeric(as.character(airplane_table[,11])), as.numeric(as.character(Annee)))

colnames(table_de_fatalites_par_annee) = c("Fatalites", "Annees") #definition du nom de chaque colonnes du data.frame

sum_des_fatalites_par_annee = NULL #initialisation de la variable
list_des_sommes_des_fatalites = NULL #initialisation de la variable

#boucle for pour trouver la somme du nombres de fatalites pour chaque annee
for(i in unique(table_de_fatalites_par_annee$Annees)){
  sum_des_fatalites_par_annee = sum(table_de_fatalites_par_annee$Fatalites[table_de_fatalites_par_annee$Annees==i])
  list_des_sommes_des_fatalites = c(list_des_sommes_des_fatalites, sum_des_fatalites_par_annee)
}

table_des_sommes_des_fatalites = data.frame(list_des_sommes_des_fatalites, as.numeric(as.character(unique(Annee))))

colnames(table_des_sommes_des_fatalites) = c("Sommes_des_Fatalites", "Annees")

table_des_sommes_des_fatalites = na.omit(table_des_sommes_des_fatalites) #omet les valeurs manquantes

table_des_sommes_des_fatalites = table_des_sommes_des_fatalites[order(as.numeric(as.character(table_des_sommes_des_fatalites$Annees))),] #ordonne mes donnees en ordre croissant en fonction de l'annee

table_des_sommes_des_fatalites = na.omit(table_des_sommes_des_fatalites) #omet les valeurs manquantes

#Affiche le graphique du nombre de fatalités à travers le temps

plot(x = table_des_sommes_des_fatalites$Annees, y = table_des_sommes_des_fatalites$Sommes_des_Fatalites, type = "h", main= "Fatalités à travers le Temps", xlab="Années", ylab="Nombre de fatalités")



#####################################################################################
#Question 3: Nombres de fatalités par localisation
#####################################################################################


sum_des_fatalites_par_location = NULL #initialisation de la variable
list_des_sommes_des_fatalites_par_location = NULL #initialisation de la variable

#boucle for pour trouver la somme du nombres de fatalites pour chaque location
for(i in unique(airplane_table$Location)){
  sum_des_fatalites_par_location = sum(airplane_table$Fatalities[airplane_table$Location==i])
  list_des_sommes_des_fatalites_par_location = c(list_des_sommes_des_fatalites_par_location, sum_des_fatalites_par_location)
}

table_des_sommes_des_fatalites_par_location = data.frame(list_des_sommes_des_fatalites_par_location, unique(airplane_table$Location))

colnames(table_des_sommes_des_fatalites_par_location) = c("Sommes_des_Fatalites", "Location") #definition du noms des colonnes

#organisation de la table en ordre decroissant par nombres de fatalites par location
ordered_table_des_sommes_des_fatalites_par_location = table_des_sommes_des_fatalites_par_location[rev(order(as.numeric(as.character(table_des_sommes_des_fatalites_par_location$Sommes_des_Fatalites)))),]

#trouver une facon d'omettre les valeurs NA
top_50_location_par_fatalites = na.omit(ordered_table_des_sommes_des_fatalites_par_location)

top_50_location_par_fatalites = top_50_location_par_fatalites[1:50,] 

top_50_location_par_fatalites #affiche le nombre fatalites par location

#utilisation d'une for loop pour transformer chaque nom de locations en "string" 
#ceci est une etape necessaire pour l'utilisation de la fonction afficher_map
top_50_location_par_fatalites_location_seulement = top_50_location_par_fatalites$Location
top_50_villes_par_fatalites_en_string = NULL

for(i in 1:50){
  top_50_villes_par_fatalites_en_string = c(top_50_villes_par_fatalites_en_string, toString(top_50_location_par_fatalites_location_seulement[i]))
}

afficher_map(top_50_villes_par_fatalites_en_string) #appel de la fonction afficher_map 



#####################################################################################
#Question 4: le nombre de crashs par localisation
#####################################################################################


#preparation des données
unique_location = unique(airplane_table$Location) #retourne les lieux d'accident sans repetition

frequence_location = trouver_frequence(unique_location, airplane_table$Location) #la fonction retourne le nombre de crash pour chaque location

table_de_frequence_par_location = as.data.frame(cbind(as.character(unique_location), as.numeric(frequence_location)))

colnames(table_de_frequence_par_location) = c("Location", "Frequence") #definit le nom de chaque colone 

table_de_frequence_par_location$Location = as.character(table_de_frequence_par_location$Location) #transforme chaque location en caractere

MostCrashLocation = table_de_frequence_par_location[rev(order(as.numeric(as.character(table_de_frequence_par_location$Frequence)))),]

MostCrashLocation = MostCrashLocation[2:51,] #la premiere valeur est vide, pour cette raison elle est omise

MostCrashLocation #retourne une table de type data.frame avec le top 50 des locations et leurs nombres de crash

#utilisation d'une for loop pour transformer chaque nom de locations en "string" 
#ceci est une etape necessaire pour l'utilisation de la fonction afficher_map

a = MostCrashLocation$Location
vecteur_de_ville_frequence_de_crashs = NULL

for(i in 1:50){
  vecteur_de_ville_frequence_de_crashs = c(vecteur_de_ville_frequence_de_crashs, toString(a[i]))
}

afficher_map(vecteur_de_ville_frequence_de_crashs)


######################################## Préparation des données pourquestions 5,6 et 7 ###############################################

#import packages
library(plyr)
library(data.table)

#######################

# Lecture des donnees
crashes = read.csv(DATA_FILE_NAME, header=TRUE, sep=",", dec = ",", 
                   fill = TRUE, stringsAsFactors=TRUE)

# Remplacement du nom de colonne 'Flight..' par 'Flight' 
names(crashes)[names(crashes) == 'Flight..'] <- 'Flight'

# Liste des provinces canadiennes - Servira a determiner si le pays est canada 
# la library states existe pour les Etats-Unis
canada = c('Nunavut', 'Quebec', 'Northwest Territories', 'Ontario',
           'British Columbia', 'Alberta', 'Saskatchewan', 'Manitoba',
           'Yukon', 'Newfoundland and Labrador', 'New Brunswick',
           'Nova Scotia', 'Prince Edward Island')

# Erreurs dans les pays - cle=nom incorrect - valeur = nom corrige
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

fix_country_name <- function(input){  # Cette fonction retourne le nom corrige du pays si possible
  if(input %in% names(errors))
    return(errors[[input]])
  else
    return(input)
}

get_surface <- function(input){ # retourne "Land" or "Sea" - utile pour savoir si le lieu est sur la terre ferme ou pas
  if(is.na(input))
    return(NA)
  
  for(wrd in sea_words){
    if(grepl(wrd, input, ignore.case = TRUE))
      return('Sea')
  }
  return('Land')
}

get_activity <- function(input){ # cette fonction determine si l'activite est un entrainement ou pas
  if(is.na(input))
    return(NA)
  
  for(wrd in training_words){
    if(grepl(wrd, input, ignore.case = TRUE))
      return('Training')
  }
  return('NO-Training')
}

get_survivor_rate <- function(aboard, fatalities){  # Quelle sont les chances de survie
  aboard = as.numeric(aboard)
  fatalities = as.numeric(fatalities)
  return( (aboard-fatalities)*100/aboard )
}

# Preparation des donnes - La fonction apply ci dessous joue le meme role qu'une bouble for
# cependant, elle est plus rapide. Des nouvelles colonnes sont ajoutees pour
# augmenter les donness brutes
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
      loc4 = tail(loc2, n=2)[1]
      
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



#########################################################################################
#Question 5: Quels sont les endroits les plus dangereux
#########################################################################################


# top15 - accidents / pays
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

# accidents / pays / categories de vols
cr_cat_freq = count(crashes, c('CrashCountry','Category'))
cr_cat_freq = as.data.frame(cr_cat_freq[order(cr_cat_freq$freq, decreasing = TRUE),])
names(cr_cat_freq)[names(cr_cat_freq) == 'freq'] <- 'Number_of_Planes_Crashes'

top15_cr_cat_mil_freq = head(cr_cat_freq[cr_cat_freq$Category=='Military',], n = 10)
top15_cr_cat_com_freq = head(cr_cat_freq[!cr_cat_freq$Category=='Military',], n = 10)

# pays / accidents d'avions militaires - top15_cr_cat_mil_freq
#mil_loc_freq = count( crashes[crashes$Category=='Military', ] , 'CrashCountry' )
#mil_loc_freq = as.data.frame(mil_loc_freq[order(mil_loc_freq$freq, decreasing = TRUE),])
#names(mil_loc_freq)[names(mil_loc_freq) == 'freq'] <- 'Military_Plane_Crashes_per_Country'

#--- Graphiques QUESTION 1
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





#########################################################################################
#Question 6: Quel est le risque selon le type d'avion?
#########################################################################################



cat_freq = count(crashes, 'Category')
cat_freq = as.data.frame(cat_freq[order(cat_freq$freq, decreasing = TRUE),])
names(cat_freq)[names(cat_freq) == 'freq'] <- 'Plane_Crashes_by_Category'

# Est ce que les avions s'ecrasent plus souvent sur la terre ou la mer
sur_freq = count(crashes, c('Surface','Category'))
sur_freq = as.data.frame(sur_freq[order(sur_freq$freq, decreasing = TRUE),])
names(sur_freq)[names(sur_freq) == 'freq'] <- 'Plane_Crashes_Per_Surface'
sur_freq[is.na(sur_freq)] = 'Unknown'

sur_mil_freq = sur_freq[sur_freq$Category=='Military',]
sur_com_freq = sur_freq[!sur_freq$Category=='Military',]

#--- Graphiques
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



#########################################################################################
# Question 7: Quels sont les Pourcentage de survie selon le type d'avions
#########################################################################################



chance_of_survival = aggregate( crashes$Survival_Rate, 
                                by=list(crashes$Category, crashes$Activity), 
                                FUN=mean, na.rm=TRUE, simplify=TRUE)

names(chance_of_survival) = c('Category','During_Trainig','Survival_Rate')
chance_mil_freq = chance_of_survival[chance_of_survival$Category=='Military',]
chance_com_freq = chance_of_survival[!chance_of_survival$Category=='Military',]
chance_mil_freq$During_Trainig = paste("Military", chance_mil_freq$During_Trainig, sep='-')
chance_com_freq$During_Trainig = paste("Commercial", chance_com_freq$During_Trainig, sep='-')

chance_freq = rbind( chance_mil_freq[,2-3], chance_com_freq[,2-3] )

# graphique
par(mfrow=c(1,1))
colors = sample(colors)
barplot( chance_freq$Survival_Rate, 
         names.arg = chance_freq$During_Trainig,
         ylim = c(0, 25), sub = "CHANCE OF SURVIVAL",
         col=colors, ylab = "Probability of Survival")



######################################## Question 8,9,10  #############################################################

#########################################################################################
# Question 8: Quelles sont les causes principales des écrasements d'avions?
#########################################################################################

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

### Appel de la fonction avec les categories recherchées.
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

##################################################################################
#Question 9: Quelle est la période de l'année la plus dangereuse et quelles sont les
#destinations les plus menaçantes ?
##################################################################################

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

##################################################################################
#Question 10: Quels sont les opérateurs qui ont fait le plus de victimes
##################################################################################

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
par(mfrow=c(2,1))
barplot(fatalitiesSumGroupByOperator$x, names=legendLabels, main=plotName, ylim = c(0, 4000), ylab="Nombre total de morts",
        cex.lab = 1.0, cex.main = 1.4, beside=TRUE, col=legendColours, las=2)

