#------------------------------------------------------------------------------
# Loading of Packages

library(plyr)
library(tidyverse)
library(haven) #Bitte verwendet die github version.
library(magrittr) # Alle Pipes
#library(memisc)
library(foreign)

#----------------------------------------------------------------------------
source("make_dictionary.R") # Hier holen wir noch eine Funktion rein, die es

#------------------------------------------------------------------------------

### Rekapitulation der ersten Tutoriumssitzung: 

## Wie richtet man ein Projekt ein und wozu verwenden wir dies?

## Was ist eine Working Directory und wie könnnen wir diese einsehen und verändern?
getwd()
#setwd("C:/Users/Schliebs/OneDrive/lehre/applied_statistics_spe/02.01_sitzung")
dir()
dir("data/")


## Eindimensionales Indizieren: 
v <- c(33,20.8,9,10,9,13,4)
names(v) <- c("Union","SPD","GRUENE","FDP","LINKE","AFD","Sonstige")
v
## Wählt nacheinander aus dem Vektor v 
## a) das 1., 2. und 5. Element
## b) alle Elemente größer gleich 10 aus
## c) die Elemente "Union" und "AFD" aus
v[c(1,2,5)]
v[v >= 10]
v[c("Union","AFD")]


## Zweidimensionales Indizieren: 
tit <- as.data.frame(datasets::Titanic)
head(tit,n = 10)
str(tit)

# Selektiert alle Rows für die erste Klasse
tit [tit$Class == "1st" ,]
tit [tit [,1] == "1st" ,]
tit [tit [,"Class"] == "1st" ,]


# Der | - Oderstrich

x <- c(10,20,10,20)
y <- c(5,10,10,15)
df_test <- data.frame(x,y)
df_test [df_test$y %in% c(1,2,3,4,5,6,7,8,9,10),]

## Selektiert alle Zeilen/Rows für die erste und Crew
tit [tit$Class == "1st" | tit$Class == "Crew" ,]
tit[tit$Class %in% c("1st","Crew") ,]
## Was macht %in% ? 
names(v) %in% c("Union","GRUENE")

## Selektiert die Variable Freq für alle Survived "Yes" und weist sie einem Vektor "lebt" zu
lebt <- tit[tit$Survived == "Yes" , c("Freq")] ; lebt
## Wie viele haben überlebt?
sum(lebt)
# Wie viele sind gestorben
tot <- tit[tit$Survived == "No" , c("Freq")]
sum(tot)


#------------------------------------------------------------------------------
### Die Pipe: 
v1 <- c("A","A","B","C","C","C")
v2 <- c(1,1,3,1,3,3)
df <- data.frame(v1,v2)
df
table(df$v1,df$v2)
prop.table(x = table(df$v1,df$v2),margin = 2) # 1: zeilenprozente; 2 = Spaltenprozente
round(x = prop.table(x = table(df$v1,df$v2),margin = 2),digits = 2)

inner <- table(df$v1,df$v2)
middle <- prop.table(inner,2)
round(middle,4)

prop.table(inner,2)
inner %>% prop.table(2) 


# Viele Klammern werden schnell unübersichtlich: 
df$v1 %>% table(df$v2)
df$v1 %>% table(df$v2)%>% prop.table(margin = 2) %>% round(digits = 3) 
df$v1 %>% table(df$v2)%>% prop.table(2) %>% round(2)


#------------------------------------------------------------------------------

## Laden der Pakete
library(plyr)
library(tidyverse)
library(haven) 
library(magrittr)
#library(memisc)
library(foreign)

# Laden des Datensatzes
gles <- read_spss(file = "data/ZA6816_v1-0-0_fb.sav") 

# Sonntagsfrage: Für Details siehe Skript letzte Stunde mit F.Bader------------------------
# Variable rekodieren: Wdh: %<>% - Pipe und within-Befehl


gles <- 
  within(data = gles,{
  # Sonntagsfrage
  sofra_1 <- t8aa
  sofra_1[t8aa < 0] <- NA
  sofra_1 <- factor(sofra_1,c(1,4,5,6,7,322,801),
                       c("CDU/CSU","SPD","FDP","GRUENE","LINKE","AfD","Andere"))
  
  sofra_2 <- t8ba
  sofra_2[t8ba < 0] <- NA
  sofra_2 <- factor(sofra_2,c(1,4,5,6,7,322,801),
                       c("CDU/CSU","SPD","FDP","GRUENE","LINKE","AfD","Andere"))
  
  })

table(gles$sofra_1) %>% prop.table()
table(gles$sofra_2) %>% prop.table()

# Splittingverhalten

table(erst = gles$sofra_1,zweit = gles$sofra_2) %>% prop.table(2) %>% round(2)
table(gles$sofra_2) %>% prop.table()

gles <- within(data = gles,{....})

table(gles$t29)

gles %<>% within({
  # Allg. Wirtschaftliche Lage Aktuell (2 sehr gut | -2 sehr schlecht)
  wi_now <- t29
  wi_now [t29 %in% c(-99,-98)] <- NA
  wi_now <- (wi_now-3) * -1
  
  # Allg. Wirtschaftliche Lage prospektiv
  wi_prog <- t30
  wi_prog[t30 %in% c(-99,-98)] <- NA
  wi_prog <- (wi_prog -3 ) * -1
  
  # Angst vor Stellenverlust (4 stark, 1 nein)
  angst_entlassung <- t53
  angst_entlassung[t53 %in% c(-99,-97)] <- NA
  
  # Alter
  alter <- t2
  alter <- 2017-alter

})

table(gles$wi_now)
table(gles$wi_prog)
table(gles$angst_entlassung)
table(gles$alter)

# Zusammenfassung der Verteilung

summary(gles$wi_now)
summary(gles$wi_prog)
summary(gles$angst_entlassung)
summary(gles$alter)

# Grafisch

#install.packages("ggplot2")
library(ggplot2)

#

data <- datasets::beaver2

str(data)
head(data, n = 25)

# Basic R
hist(data$temp)


# GGPLOT
ggplot(data = data) + 
  geom_histogram(mapping = aes(x = temp),
                 binwidth = 0.2)

# Bivariat

# in base R
plot(data$time,data$temp)

# in ggplot 
ggplot(data = data) + 
  geom_point(mapping = aes(x = time,
                           y = temp))

# look at activity
bib <- data[data$time > 500,]

install.packages("hrbrthemes")
install.packages("extrafont")
library(hrbrthemes)
library(extrafont)

gg_bib <- 
  ggplot(data = bib) + 
  geom_point(mapping = aes(x = time,
                           y = temp,
                           color = activ),
             size = 2) + 
  geom_line(mapping = aes(x = time,
                           y = temp,
                           color = activ))+ 
  labs(x = "Zeit", 
       y = "Temperatur",
       title = "Biebern oder Bibbern?",
       subtitle = "Eine vergleichende Temperaturstudie einer Bieberpopulation") + 
  theme_ipsum(grid = "Y")
  
gg_bib

ggsave(filename = "bieber.pdf",
       plot = gg_bib,
       device = "pdf",
       dpi = 1000)

library(extrafont) # neue Schriftarten



# Beispiele
ggplot(gles,aes(x = wi_now)) + 
  geom_histogram(binwidth = 1) + 
  labs(x = NULL, 
       y = NULL,
       title = "Aktuelle wirtschaftliche Lage")

ggplot(gles,aes(x = wi_prog)) + 
  geom_histogram(binwidth = 1) + 
  labs(x = NULL, 
       y = NULL,
       title = "Erwartete Entwicklung der Wirtschaftlichen Lage") + 
  theme_bw()

# Schönes Design: 

# Einmal ausführen: 
#install.packages("hrbrthemes")
#install.packages("extrafont")

library(extrafont) # neue Schriftarten
library(hrbrthemes) # schöne Designs

gg <- 
  ggplot(gles,aes(x = angst_entlassung)) + 
  geom_histogram(binwidth = 1) + 
  labs(x = NULL, 
       y = NULL,
       title = "Angst, den Arbeitsplatz zu verlieren",
       subtitle = "1 = Keine Angst | 4 = sehr hohe Angst",
       caption = "@copyright Vorname Nachname") + 
  theme_ipsum(grid = "Y")
gg

# Speichern der Grafik: 

ggsave(filename = "output.pdf",
       plot = gg,
       device = "pdf",
       dpi = 1000)



table(gles$alter,useNA = "always")

# Konfidenzintervall und t-test: Beispiel Alter
t.test(gles$alter)

# händisch -------------------------------------------------------------------

# Mittelwert des Alters
mean_alter <- mean(gles$alter,na.rm = TRUE)

# Standardabweichung des Alters
sd_alter <- sd(gles$alter,na.rm = TRUE)

# Anzahl der Fälle, für die eine Altersvorgabe vorliegt
n_alter <- na.omit(gles$alter) %>% length()

# Berechnung des Standardfehlers
se_alter <- sd_alter/sqrt(n_alter) 

# Konfidenzintervall
mean_alter + c(-1.96,+1.96)*se_alter #Eigentlich Z-test

# Die ganz korrekten quantile mit der T-Verteilung
qt(c(.025,.975),df = n_alter-1)
mean_alter + c(-1.962157,+1.962157)*se_alter 


# Kreuztabelle _ Chi-quadrat-Test--------------------------------------------------------------

t <- table(gles$angst_entlassung,gles$sofra_2);t
t %>% prop.table(1) %>% round(2)

csq <- chisq.test(t); csq
csq$expected
csq$observed

install.packages("sjPlot")
library(sjPlot)
sjt.xtab(gles$sofra_2,gles$angst_entlassung,show.obs = T,show.col.prc = T)
