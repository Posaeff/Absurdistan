#####einlesen der Daten BoardGames

#library("tidytuesdayR")
#tuesdata <- tidytuesdayR::tt_load('2022-01-25')
#save(tuesdata,file = "board games")

load("board games")

ratings<- as.data.frame(tuesdata$ratings)
head(ratings)

details <- as.data.frame(tuesdata$details)
head(details)


#########################################

#Aufgabenstellung:

# -Erstellung einer Punktewolke basierend auf der Variable "yearpublished"; x = yearpublished; y=Anzahl der Spiele in dem Jahr; 
# Zusatz: umso größer die Anzahl der Spiele in dem Jahr umso größer ist der geom_point.

#Äras:

# 1 = -3000 -> 1900 # Josep

# 2 = 1901 -> 1945 # Josep

# 3 = 1946 -> 1990 # Tim

# 4 = 1991 -> Rest # Tim


# Dann diese Zusammenklatschen mit Patchwork

########################################
