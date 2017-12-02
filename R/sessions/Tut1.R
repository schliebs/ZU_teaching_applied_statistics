
# Hier laden wir alle nötigen Pakete ---------------------------------------
library(plyr) 
library(tidyverse) #Viele nützliche Funktionen
library(haven) #Paket zum Lesen von SPSS-Daten
library(magrittr) #Alle möglichen zusätzlichen Pipes das kommt noch alles :-)
#----------------------------------------------------------------------------
source("make_dictionary.R") # Hier holen wir noch eine Funktion rein, die es
# uns ermöglicht das meta_file unten zu erstellen.

ess <- read_spss("data/ESS7e02_1/ESS7e02_1.sav") # Datensatz einlesen
meta_file <- make_dictionary(ess,format = "wide") # Meta file erstellen

# Häufigkeitsverteilung der Variable, die das Erhebungsland angiebt.
table(ess$cntry)
# Wir wählen nur die Fälle aus, die in Deutschland befragt wurden
ger <- ess[ess$cntry=="DE",]
fra <- ess[ess$cntry=="FR",]


#Exkurs indizieren ------------------------------------------------------------
x <- c(33,20.8,9,10,9,13,4)
x
# Wir fügen noch namen für jedes einzelne Element zu
names(x) <- c("CDU","SPD","GRUENE","FDP","LINKE","AFD","Sonstige")
x

# Indizieren den Wert für die AfD
x["AFD"] 

# Indizieren mit Positionen: Wert der 1., 2. und 6. partei (CDU, SPD, AfD)
x[c(1,2,6)]

# Logische Abfrage. Welche Werte von x sind größer 20?
x>20 

#Jetz wählen wir alle Elemente aus die größer 20 sind
x[x>20] 

# Summe der Elemente 1,3,4 Jaimaikakoalition 
x[c(1,3,4)] %>% sum() 


#FDP zu NA erklären
x["Sonstige"] <- NA 
x


# Summe aller Vektorelemente
sum(x) # => funktioniert nicht
#Summe über x, funktioniert nur dann, wenn wir na.rm = TRUE
# setzen
sum(x,na.rm = TRUE)


# Jetzt ESS, für Frankreich

# Häufigkeitsverteilung der Variable imsmetn
table(fra$imsmetn)

# Relative Häufigkeitsverteilung 
prop.table(table(fra$imsmetn)) # Die klassische Variante
table(fra$imsmetn) %>% prop.table() %>% round(2)

# Relative Häufigkeitsverteilung der Variable imdfetn
table(fra$imdfetn) %>% prop.table() %>% round(2)


# Jetzt schauen wir uns noch die Variabl yrbrn an.
# Häufigkeitsverteilung
table(fra$yrbrn)

# Wir berechnen das Alter
fra$age <- 2014-fra$yrbrn
# Häufkigkeitsverteilung des Alters
table(fra$age)

# Durchschnittsalter achtung wieder na.rm = T
mean(fra$age,na.rm=TRUE)

# Und zum Schluss noch das Histogramm
ggplot(fra,
       aes(age))+geom_histogram()

# Hausaufgaben:
# Versuchen Sie rauszufinden wie die Einstellungen der Menschen zu Immigration
# in anderen Ländern sind.
# - Welche Hypothesen haben Sie?




