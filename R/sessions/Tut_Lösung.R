
#------------------------------------------------------------------------------

# Hier laden wir Pakete, die wir später verwenden wollen.
library(plyr)
library(tidyverse)
library(haven) #Bitte verwendet die github version.
library(magrittr) # Alle Pipes

# Wir laden den Datensatz: Zum Einlesen von R verwenden wir Funktionen.
# Funktionen haben immer einen Namen und Argumente die innerhalb von Klammern
# übergeben werden. Die meisten Funktionen geben einen Wert zurück. Mit 
# Pfeil Operator <- können wir diesen Rückgabewert einer Variable zuweisen.
#
# Hier holen wir uns noch eine Funktion, die wir später brauchen
source("make_dictionary.R")

ess <- read_spss(file = "data/ESS7e02_1/ESS7e02_1.sav",user_na = TRUE)
dim(ess)

meta_file <- make_dictionary(ess,format = "wide")


## Wiederholung Vektoren/Indizieren: 

x <- c(33,20.8,9,10,9,13)
# Jedes Element erhält einen Namen
names(x) <- c("CDU","SPD","GRUENE","FDP","LINKE","AFD")

# Indizieren durch Name
x["AFD"]

# Indzizieren durch Position
x[2]
x[1] - x[2]

# Mehrere Elemente wählen
jamaika <- x[c(1,3,4)]
sum(jamaika)

# Indizieren durch logischen Vektor
sel <- c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE)
x[sel]

# Logischen vektor selbst berechnen
sel <- x > 20

# Und zum auswählen verändern
x[sel]

# Elemente überschreiben
x[1] <- x[1]-10
x[2] <- x[2]+10
x

# FPD ? Not available any more...
x[4] <- NA
x


# Ende Exkurs-----------------------------------------------------------------

table(ess$cntry)

# Die Variable cntry enhält das Erhebungsland.
# Wir wollen nur die Zeilen (Befragte) auswählen, die in Deutschland befragt 
# wurden.
# In R selektiert man dataframes und matritzen so: [zeile,spalte] lässt man
# eine position weg [zeile,] dann wählt man alle elemente (hier Spalten) aus.

sel <- ess$cntry == "DE"
table(sel)
table(ess$essround)
ger <- ess[sel,]
# So geht es auch mit dem tidyverse
ger <- ess %>% filter(cntry == "DE")

# Wer macht welches Land ???

# Mit Variablen rechnen ##
# Alter
#' Geburtsjahr in Alter umrechnen

# Exkurs #-------------------------------------------------------------------

x <- 1990:1995 # ist das gleich wie c(1990,1991,...)
# Wie alt sind die Personen im Jahr 2014?
2014 - x 

# ---------------------------------------------------------------------------

table(ger$yrbrn)

ger$yrbrn[ger$yrbrn %in% c(7777,8888)] <- NA
ger$age <- 2014-ger$yrbrn

# Doof hier muss man immer ger$ tippen
# So geht es besser. Jetzt passiert alles "within" ger. 

table(ess$yrbrn)
ess %<>% within({
  age <- yrbrn
  age[yrbrn %in% c(7777,8888,9999)] <- NA
  age <- 2014-age
  
  # Außerdem können wir gleich noch eine gruppierte Altersvariable erstellen.
  age_grp <- age
  age_grp[age <= 35] <- 1
  age_grp[age > 35 & age <= 60] <- 2
  age_grp[age > 60] <- 3
  
  age_grp <- factor(age_grp,1:3,c("[16-35]","[36-60]","[>60]"))

  })

table(ess$age_grp)
table(ess$age)

# Exkurs (Unser 1.Plot)

ggplot(ess,aes(x = age))+geom_histogram()
ggplot(ess,aes(x = age))+geom_histogram(binwidth = 5,
                                        fill = "red",
                                        col = "black")


# Geschlecht #---------------------------------------------------------------

ess %<>% within({
  gndr[gndr %in% 9] <- NA
})
table(ess$gndr)



# Zu den interessanten Fragen -----------------------------------------------
# Welche Einstellung haben die xxx zu Immigration 

table(ess$imsmetn)

ess %<>% within({

  culture_same <- imsmetn
  culture_same[culture_same %in% 7:9] <- NA
  
  culture_other <- imdfetn
  culture_other[culture_same %in% 7:9] <- NA

})

table(ess$culture_same,ess$imsmetn,exclude = NULL)
# oder so

# Häufigkeitsverteilung
table(ess$culture_same) 
# Relative Häufigkeitsverteilung
table(ess$culture_same) %>% prop.table() %>% round(2)

# Schöner als Faktor

attributes(ess$imsmetn)

ess %<>% within({
  culture_same_factor <- factor(culture_same,1:4,c("many","some","few","none")) 
  culture_other_factor <- factor(culture_other,1:4,c("many","some","few","none")) 
})

table(ess$culture_same_factor) %>% prop.table() %>% round(2)
table(ess$culture_other_factor) %>% prop.table() %>% round(2)



# Ein 2-faktorielles Experiment im ESS -----------------------------------------
# Treatment
table(ess$admaimg)
# Outcome
table(ess$alpfpe,ess$alpfpne)
table(ess$admaimg,ess$alpfpe)

#' Outcomevariable und Treatment
ess %<>% within({
  treat <- admaimg
  outcome <- alpfpe
  outcome[treat == 2] <- alpfpne[treat == 2]
  outcome[treat == 3] <- allbpe[treat == 3]
  outcome[treat == 4] <- allbpne[treat == 4]
  outcome[outcome %in% 7:8] <- NA
  outcome <- abs(outcome-5)
  
  treat[treat == 9] <- NA
})
table(ess$outcome,ess$treat) %>% prop.table(2) %>% round(2)

table(ess$treat)

#' Treatment in 2 Faktoren zerlegen
ess %<>% within({
  cult <- treat
  cult[treat %in% c(1,3)] <- 0
  cult[treat %in% c(2,4)] <- 1
  
  qual <- treat
  qual[treat %in% c(1,2)] <- 0
  qual[treat %in% c(3,4)] <- 1
  
})

table(ess$outcome,ess$qual) %>% prop.table(2) %>% round(2)
table(ess$outcome,ess$cult) %>% prop.table(2) %>% round(2)


