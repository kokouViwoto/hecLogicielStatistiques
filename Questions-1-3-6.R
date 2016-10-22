DATA_PATH_FOLDER="Change Me. This should be the path to the folder that contains the dataset"
DATA_FILE_NAME="Change Me. This should be the dataset file name"

setwd(DATA_PATH_FOLDER)

airplane_table = read.csv(DATA_FILE_NAME) 

summary(airplane_table)

names(airplane_table)

#####################################################################################

#loader les packages avant de pouvoir utiliser la fonction afficher_map 

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
  #il faut passer un vecteur 
  
  
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
#Question 1: Le nombre de crash a travers le temps
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
#Question 3: Nombre de fatalites par Annees travers le temps
#####################################################################################



#preparation des donnees

table_de_fatalites_par_annee = data.frame(as.numeric(as.character(airplane_table[,11])), as.numeric(as.character(Annee)))

colnames(table_de_fatalites_par_annee) = c("Fatalites", "Annees") #definition du nom de chaque colonnes du data.frame

sum_des_fatalites_par_annee = NULL #initialisation de la variable
list_des_sommes_des_fatalites = NULL #initialisation de la variable
  
#for loop pour trouver la somme du nombres de fatalites pour chaque annee
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
#Question 6: Plus grand nombres de fatalites par location 
#####################################################################################




sum_des_fatalites_par_location = NULL #initialisation de la variable
list_des_sommes_des_fatalites_par_location = NULL #initialisation de la variable

#for loop pour trouver la somme du nombres de fatalites pour chaque location
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

#Question: le nombre de crashs par endroit(location) 
#question repondu en affichant un tableau de type data.frame et en representant sur un map 
#le top 50 des locations ou il y a eu des crash
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

