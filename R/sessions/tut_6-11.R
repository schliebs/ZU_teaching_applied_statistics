## Verständnisfragen

"Erklären Sie die 'Logik' der Kovarianz.
Warum ist die Kovarianz abhängig von den Einheiten der Variablen?
Wie hängen Kovarianz und Pearson's R zusammen?
Welches Ziel verfolgen wir mit der Regressionsanalyse?
Welche Parameter hat die Regressionsgleichung?
Wie interpretiert man den Steigungskoeffizienten?
Wie interpretiert man den Intercept?
Wie interpretiert man R^2?
Welche Annahmen stecken in der Regressionsanalyse? 
Was bedeutet Bias?
Was bedeutet Effizienz?
Welche der 5 Annahmen hat welche Auswirkungen?
Was steckt hinter dem Ommited Variable Bias?
Welche Schritte müssen Sie in R zur Berrechnung einer Regressionsanalyse durchführen?"


#mit dem GLES - Datensatz: (Kontrollieren Sie in allen Berechnungen für das Alter der Befragten.)[Zusatz Marcel: UND GESCHLECHT!]

## Allgemeine Datenvorbereitung zur Analyse (Packages und Datensatz)

# 1. Libraries laden ---------------------------------------------------------

# Hier laden wir Pakete, die wir später verwenden wollen.
library(haven) 
library(magrittr) # Alle Pipes
library(foreign)
library(sjPlot)
library(dplyr)
library(ggplot2)

# 2. Daten laden ---------------------------------------------------------
gles <- read_spss(file = "data/ZA6816_v1-0-0_fb.sav",
                  user_na = T) 


#Welchen Einfluss hat das Einkommen auf die Einschätzung der eigenen wirtschaftlichen Lage?

# Variablen: 
# Allg. Wirtschaftliche Lage Aktuell: t29 (2 sehr gut | -2 sehr schlecht | NAs: -99,-98)

"(1) sehr gut
(2) gut
(3) teils gut, teils schlecht
(4) schlecht
(5) sehr schlecht
------------------------------------------
(-99) keine Angabe"

# Nettohaushaltseinkommen: t70
"(1) unter 500 Euro
(2) 500 bis unter 750 Euro
(3) 750 bis unter 1000 Euro
(4) 1000 bis unter 1250 Euro
(5) 1250 bis unter 1500 Euro
(6) 1500 bis unter 2000 Euro
(7) 2000 bis unter 2500 Euro
(8) 2500 bis unter 3000 Euro
(9) 3000 bis unter 4000 Euro
(10) 4000 bis unter 5000 Euro
(11) 5000 bis unter 7500 Euro
(12) 7500 Euro bis unter 10000 Euro
(13) 10000 Euro und mehr
----------------------------------------
(-99) keine Angabe"

# Geburtsjahr: t2

# Geschlecht: t1 (1:maennlich, 2:weiblich)


table(gles$t29)
table(gles$t70)
table(gles$t2)
table(gles$t1)



# 3. Rekodieren der Variablen
gles %<>% within({
  
  # Allg. Wirtschaftliche Lage Aktuell (2 sehr gut | -2 sehr schlecht | NAs: -99,-98)
  wi_now <- t29
  wi_now [t29 %in% c(-99,-98)] <- NA
  wi_now <- (wi_now-3) * -1
  
  # Einkommen: t70
  
  einkommen <- t70
  einkommen [t70 %in% c(-99,-98)] <- NA
  einkommen_z <- einkommen - mean(einkommen,na.rm = TRUE) # zentrieren auf den Mittelwert 
  
  # Alter
  alter <- t2
  alter <- 2017-alter
  alter_z <- alter-mean(alter,na.rm=T)
  
  #Geschlecht
  sex <- NA
  sex [t1 == 1] <- "Mann"
  sex [t1 == 2] <- "Frau"
  

})

table(gles$wi_now)
table(gles$einkommen)
table(gles$alter)
table(gles$sex)


# 4. Daten selektieren (nur relevante variablen als subset)

gles_sub <- gles %>% select (wi_now,einkommen_z,alter_z,sex)

#5. Daten plotten

p <- ggplot(gles_sub,aes(einkommen_z,wi_now)) + 
  geom_jitter() +
  geom_smooth(method = "lm", col = "black")
p 



# 6. Regressionsanalyse

# Basic Modell
mod1 <- lm(wi_now ~ einkommen_z,data = gles_sub)
summary(mod1)
sjt.lm(mod1)

# Multivariat mit Kontrollvariablen
mod2 <- lm(wi_now ~ einkommen_z + sex + alter_z,data = gles_sub)
summary(mod2)
sjt.lm(mod2)

# Geschlechterunterschied?
p2 <- ggplot(gles_sub,aes(einkommen_z,wi_now,col = sex)) + 
  geom_jitter() +
  geom_smooth(method = "lm")
p2


###############################################################################################

  
#Welchen Einfluss hat das Vertrauen in den Bundestag und die Bundesregierung auf die Einstellung zur AfD?


# Codierung: Vertrauen in Bundestag (t124j) und Bundesregierung (t124k)
"(1) 0 überhaupt nicht
(2) 1
(3) 2
(4) 3
(5) 4
(6) 5
(7) 6
(8) 7
(9) 8
(10) 9
(11) 10 voll und ganz
(-71) kenne ich nicht [im Datensatz gelabelt als nicht bekannt]
-------------------------------------------
(-99) keine Angabe"

# AFD-Einstellung: t14g
"(1) -5 halte überhaupt nichts von der Partei
(2) -4
(3) -3
(4) -2
(5) -1
(6) 0
(7) +1
(8) +2
(9) +3
(10) +4
(11) +5 halte sehr viel von der Partei
(-72) kann ich nicht einschätzen [im Datensatz gelabelt als nicht einzuschaetzen]
-----------------------------------------------------
(-99) keine Angabe"


table(gles$t124j)
table(gles$t124k)
table(gles$t14g)



# 3. Rekodieren der Variablen
gles %<>% within({
  
  # Vertrauen Bundestag
  trust_btag <- t124j
  trust_btag [t124j %in% c(-99,-71)] <- NA
  trust_btag_z <- trust_btag - mean(trust_btag,na.rm = T)
  
  # Vertrauen Bundesregierung
  trust_breg <- t124k
  trust_breg [t124k %in% c(-99,-71)] <- NA
  trust_breg_z <- trust_breg - mean(trust_breg,na.rm = T)
  
  # Einstellung zur AfD
  feel_afd <- t14g
  feel_afd[t14g %in% c(-99,-72)] <- NA 
  feel_afd_z <- feel_afd - mean(feel_afd,na.rm = T)
  
  # Alter
  alter <- t2
  alter <- 2017-alter
  alter_z <- alter-mean(alter,na.rm=T)
  
  #Geschlecht
  sex <- NA
  sex [t1 == 1] <- "Mann"
  sex [t1 == 2] <- "Frau"
  
  
})

table(gles$trust_btag_z)
table(gles$trust_breg_z)
table(gles$feel_afd_z)


# 4. Daten selektieren (nur relevante variablen als subset)

gles_sub2 <- gles %>% select (alter_z,sex,trust_breg_z,trust_btag_z,feel_afd_z)

#5. Daten plotten

p <- ggplot(gles_sub2,aes(trust_breg_z,feel_afd_z)) + 
  geom_jitter() +
  geom_smooth(method = "lm", col = "black")
p 



# 6. Regressionsanalyse

mod3a <- lm(feel_afd_z ~ trust_breg_z + trust_btag_z,data = gles_sub2)
summary(mod3a)
sjt.lm(mod3a)

mod3b <- lm(feel_afd_z ~ trust_btag_z,data = gles_sub2)
summary(mod3b)
sjt.lm(mod3b)

mod3c <- lm(feel_afd_z ~ trust_breg_z ,data = gles_sub2)
summary(mod3c)
sjt.lm(mod3c)

# Mit KV: 
mod4 <- lm(feel_afd_z ~ trust_breg_z + trust_btag_z + alter_z + sex,data = gles_sub2)
summary(mod4)
sjt.lm(mod4)


###############################################################################################

#Welchen Einfluss hat das Vertrauen in den Bundestag auf die Wahrscheinlichkeit zur Wahl zu gehen?

# Y:  Turnout 

# Fragetext: Wenn Wahlen stattfinden, geben viele Leute ihre Stimme ab.
# Andere kommen nicht dazu, ihre Stimme abzugeben, oder nehmen aus anderen
# Gründen nicht an der Wahl teil. Wenn am nächsten Sonntag eine Bundestagswahl
# wäre, würden Sie dann zur Wahl gehen? Ich würde …
#Codierung:
#(1) bestimmt zur Wahl gehen
#(2) wahrscheinlich zur Wahl gehen
#(3) vielleicht zur Wahl gehen
#(4) wahrscheinlich nicht zur Wahl gehen
#(5) bestimmt nicht zur Wahl gehen
#(-98) weiß ich nicht [im Datensatz gelabelt als "weiss nicht"]
----------------------------------------------------------------------
  #(-99) keine Angabe
  
  table(gles$t7)



gles %<>% within({
  
  wahl_abs <- t7
  wahl_abs [t7 %in% c(-99,-98)] <- NA 
  wahl_abs [t7 == -98] <- 3 # Weißt nicht zu 3=vielleicht codiert

# Variante 1
wahl_abs2 <- NA
wahl_abs2 [wahl_abs == 1] <- 1
wahl_abs2 [wahl_abs == 2] <- 0.75
wahl_abs2 [wahl_abs == 3] <- 0.5
wahl_abs2 [wahl_abs == 4] <- 0.25
wahl_abs2 [wahl_abs == 5] <- 0

# Variante 2

wahl_abs3 <- NA
wahl_abs3 <- (wahl_abs - 5) / -4

# Alter
alter <- t2
alter <- 2017-alter
alter_z <- alter-mean(alter,na.rm=T)

#Geschlecht
sex <- NA
sex [t1 == 1] <- "Mann"
sex [t1 == 2] <- "Frau"

})

table(gles$wahl_abs3)
summary(gles$wahl_abs3)


# X:

# Nun werden verschiedene politische Institutionen aufgeführt.
# Bitte geben Sie an, wie sehr Sie persönlich jeder einzelnen Institution
# vertrauen.
#:
#(1) 0 überhaupt nicht
#(2) 1
#(3) 2
#(4) 3
#(5) 4
#(6) 5
#(7) 6
#(8) 7
#(9) 8
#(10) 9
#(11) 10 voll und ganz
#(-71) kenne ich nicht [im Datensatz gelabelt als "nicht bekannt"]
#-------------------------------------------
#  (-99) keine Angabe


table(gles$t124j)
# Ich rekodiere weiß nicht zu vielleicht
gles %<>% within({  
  # Vertrauen Bundestag
  trust_btag <- t124j
 trust_btag [t124j %in% c(-99,-71)] <- NA
  trust_btag_z <- trust_btag - mean(trust_btag,na.rm = T)
})

table(gles$trust_btag_z,gles$t124j)

table(gles$trust_btag_z,gles$t124j,useNA="always")#zeigt NAs mit an

summary(gles$trust_parl)






#4. Daten selektiren -------------------------------------------------------

ana_dat <- select(gles,trust_btag_z,wahl_abs3,sex,alter_z)



#5. Daten plotten

p <- ggplot(data = ana_dat,
            aes(x = trust_btag_z,
                y = wahl_abs3,
                col = sex)) +
  geom_jitter() + 
  geom_smooth(method = "lm") #lineare Regressionsgerade drin
  
p

mod <- lm(wahl_abs3 ~ trust_btag_z + sex + alter_z,data = ana_dat)
summary(mod)
sjt.lm(mod)


#5. Regressionsanalyse

