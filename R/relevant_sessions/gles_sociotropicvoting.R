# Hausaufgabe

'Welchen Einfluss hat das Haushaltsnettoeinkommen 
auf die Einstellungen der Menschen zu Umverteilung und Steuern (t114) ? 
Untersuchen Sie die Fragestellung mit unserem gles Datensatz und einem linearen Regressionsmodell. 
Kontrollieren Sie für mind. zwei relevante Kontrollvariablen. 
Versuchen Sie die Informationen über die Ausprägungen des Haushaltsnettoeinkommens 
im Fragebogen für die Rekodierung zu nutzen.'


library(haven) 
library(magrittr) # Alle Pipes
library(sjPlot)
library(dplyr)
library(ggplot2)

# Daten einlesen

gles <- read_spss(file = "data/ZA6816_v1-0-0_fb.sav",
                  user_na = T)



# Steuern (t114)

'(1) 1 weniger Steuern und Abgaben, auch wenn das weniger sozialstaatliche Leistungen bedeutet
(2) 2
(3) 3
(4) 4
(5) 5
(6) 6
(7) 7
(8) 8
(9) 9
(10) 10
(11) 11 mehr sozialstaatliche Leistungen, auch wenn das mehr Steuern und Abgaben bedeutet
(-98) weiß ich nicht [im Datensatz gelabelt als "weiss nicht"]
-----------------------------
(-99) keine Angabe'


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


table(gles$t114)
table(gles$t70)
table(gles$t2)
table(gles$t1)


##################


# 3. Rekodieren der Variablen
gles %<>% within({
  
  # t114: zentrieren auf den Skalenmittelpunkt: 
  
  sozio <- t114
  sozio [t114 %in% c(-98,-99)] <- NA
  sozio <- sozio - 6

  # Einkommen: t70
  
  einkommen <- t70
  einkommen [t70 %in% c(-99)] <- NA
  einkommen [t70 %in% c(1)] <- 250 # ?
  einkommen [t70 %in% c(2)] <- 625
  einkommen [t70 %in% c(3)] <- 875
  einkommen [t70 %in% c(4)] <- 1125
  einkommen [t70 %in% c(5)] <- 1375
  einkommen [t70 %in% c(6)] <- 1750
  einkommen [t70 %in% c(7)] <- 2250
  einkommen [t70 %in% c(8)] <- 2750
  einkommen [t70 %in% c(9)] <- 3500
  einkommen [t70 %in% c(10)] <- 4500
  einkommen [t70 %in% c(11)] <- 6250
  einkommen [t70 %in% c(12)] <- 8750
  einkommen [t70 %in% c(13)] <- 10000 # kann man drüber diskutieren
  
  # Zentrierung Mittelwert
  einkommen_z <- einkommen - mean(einkommen,na.rm = TRUE) # zentrieren auf den Mittelwert 
  
  # Alter
  alter <- t2
  alter <- 2017-alter
  alter_z <- alter-mean(alter,na.rm=T)
  
  #Geschlecht
  gender <- factor(x = t1,levels = 1:2,labels = c("male","female"))
  #gender <- factor(x = t1,levels = 2:1,labels = c("female","male"))
  
})


table(gles$sozio)
table(gles$einkommen_z)
hist(gles$einkommen_z)
table(gles$einkommen)
table(gles$alter_z)
table(gles$alter)
table(gles$gender)


# 1: Analysedatensatz
ana_dat <- select(gles,sozio,einkommen_z,einkommen,alter_z,alter,gender)

#2: Zeileweise entfernen der Missing values: Vor allem wichtig später bei
# multivariaten modellen die hierarchisch aufgebaut werden.
ana_dat %<>% na.omit()
dim(ana_dat)
str(ana_dat)

# 3 Graphischer Überblick:

# install.packages("hrbrthemes")
# install.packages("extrafont")

library(hrbrthemes)
library(extrafont)

gg1 <- 
  ggplot(data = ana_dat,aes(x = einkommen_z,y = sozio)) + # Grundstock: Datenspezifizierung
  geom_jitter() +                                # Jittered Points
  geom_smooth(method = "lm", col = "red") +      # Lineare Regressionsgerade mit Konfidenzinterval
  labs(x = "Einkommen",
       y = "Sozialstaatsposition",
       title = "Zusammenhang Einkommen ~ Sozialstaatliche Positionen",
       subtitle = "Untertitel",
       caption = "Soz.position: Positiver Wert = Mehr Umverteilung, höhere Steuern") +
  theme_ipsum(grid = "Y")                        # Schönes Design
gg1

# Entweder
ggsave(filename = "plot1.pdf",
       plot = gg1,
       device = "pdf",
       width = 10,
       height = 6,
       dpi = 2000)

# Oder
ggsave(filename = "plot1.png",
       plot = gg1,
       device = "png",
       width = 10,
       height = 6,
       dpi = 2000)



#4: Modell

mod_base <- lm(sozio ~ einkommen_z,data = ana_dat)
mod_base_nonzentriert <- lm(sozio ~ einkommen,data = ana_dat)

mod_control <- lm(sozio ~ einkommen_z + alter_z + gender,data = ana_dat)

summary(mod_base)
summary(mod_base_nonzentriert)

summary(mod_control)

# Kleine Zahlen ausschreiben
options(scipen = 999)

#5 Modelltest
anova(mod_base,mod_control)
sjt.lm(mod_base,mod_control,p.numeric = F,show.ci = F,show.se = T)


# 6: Interaktionen: Gendereffekt?

gg2 <- 
  ggplot(ana_dat,aes(x = einkommen_z,y = sozio,color = gender)) + # Grundstock: Datenspezifizierung
  geom_jitter() +                                # Jittered Points
  geom_smooth(method = "lm") +      # Lineare Regressionsgerade mit Konfidenzinterval
  labs(x = "Einkommen",
       y = "Sozialstaatsposition",
       title = "Zusammenhang Einkommen ~ Sozialstaatliche Positionen",
       subtitle = "Untertitel",
       caption = "Soz.position: Positiver Wert = Mehr Umverteilung, höhere Steuern") +
  theme_ipsum(grid = "Y")                        # Schönes Design
gg2

ggsave(filename = "plot2.pdf",
       plot = gg2,
       device = "pdf",
       width = 10,
       height = 6,
       dpi = 2000)


# Interaktions-Modell

mod_interaktionen <- lm(sozio ~ einkommen_z * gender,data = ana_dat)
mod_interaktion_control <- lm(sozio ~ einkommen_z * gender + alter_z ,data = ana_dat)

summary(mod_interaktionen)
summary(mod_interaktion_control)

sjt.lm(mod_base,mod_interaktionen,mod_interaktion_control,p.numeric = F,show.ci = F,show.se = T)


# Übung 2: Einstellungen zu EU

'Erklärt in separaten Regressionsmodellen den Einfluss von Angst vor I) der Flüchtlingskrise, 
II) der Präsident der USA Donald Trump und III) der Außenpolitik Russlands auf die Einstellung zur EU. 
Schätzt hierzu jeweils ein bivariates Baseline-Modell und anschließend ein weiteres, in dem Ihr für Alter
kontrolliert und eine Interaktion Geschlecht mit der UV einbaut.
Schätzt anschließend ein Gesamtregressionsmodell mit I, II und III als UV und Alter/Geschlecht als KV. 
Interpretiert die Ergebnisse. '


'Codierung:
(1) 1 europäische Einigung so vorantreiben, dass es bald eine gemeinsame Regierung gibt
(2) 2
(3) 3
(4) 4
(5) 5
(6) 6
(7) 7
(8) 8
(9) 9
(10) 10
(11) 11 europäische Einigung geht jetzt schon viel zu weit
(-98) weiß ich nicht [im Datensatz gelabelt als "weiss nicht"]
-----------------------------
(-99) keine Angabe'



# Angst: t225a-l

'Fragetext:
Wie viel Angst macht Ihnen …
(F) die Flüchtlingskrise?
(A) die derzeitige Wirtschaftslage?
(C) die globale Klimaerwärmung?
(D) der internationale Terrorismus?
(G) der Zustand der Europäischen Union?
(H) die politische Entwicklung in der Türkei?
(I) der Präsident der USA Donald Trump?
(J) die Globalisierung?
(K) Opfer eines Verbrechens zu werden?
(L) die Außenpolitik Russlands?

Codierung:
(1) 1 überhaupt keine Angst
(2) 2
(3) 3
(4) 4
(5) 5
(6) 6
(7) 7 sehr große Angst
----------------------------------------
(-99) keine Angabe'



