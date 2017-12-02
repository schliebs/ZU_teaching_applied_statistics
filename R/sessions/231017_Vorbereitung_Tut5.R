# 1) Themenfindung fÃ¼r die Hausarbeit: 

# Ueberlegt Euch bitte schonmal grob, welche Fragestellungen im Bereich der
# politischen/oekonomischen/soziologischen Forschung Euch interessieren. 
# Ich wuerde empfehlen, dass Ihr mit einem Survey(Umfrage)-Datensatz arbeitet,
# sprich mit Individualdaten. 
# Anbei Links zu einigen Datensätzen, die ich in der kommenden Stunde noch ein
# wenig genauer erlaeutern werde. 

# ESS: http://www.europeansocialsurvey.org/ | https://de.wikipedia.org/wiki/European_Social_Survey 
# GSS: http://gss.norc.org/ | https://en.wikipedia.org/wiki/General_Social_Survey 
# GLES (persÃ¶nicher Favorit!): https://www.gesis.org/wahlen/gles/ | https://de.wikipedia.org/wiki/German_Longitudinal_Election_Study 
# Eurobarometer: http://ec.europa.eu/commfrontoffice/publicopinion/index.cfm | https://de.wikipedia.org/wiki/Eurobarometer
# Arab Barometer: http://www.arabbarometer.org/ | https://en.wikipedia.org/wiki/Arab_Barometer
# Terrorism National Security Survey: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/NDOWAD 
# Socio Economic Panel SOEP: https://www.diw.de/en/soep 
# ALLBUS: https://de.wikipedia.org/wiki/Allgemeine_Bev%C3%B6lkerungsumfrage_der_Sozialwissenschaften 
# mehr zu weiteren Themen gerne auf Nachfrage

#------------------------------------------------------------------------------

# Hier laden wir Pakete, die wir später verwenden wollen.
library(plyr)
library(tidyverse)
library(haven) 
library(magrittr) # Alle Pipes

gles <- read_spss(file = "data/ZA6816_v1-0-0_fb.sav",
                  user_na = T)

# Variablen definieren -------------------------------------------------------
gles %<>% within({  
  #Zweitstimme:
  sofra_2 <- t8ba
  sofra_2[t8ba<0] <- NA
  sofra_2 <- factor(sofra_2,c(1,4,5,6,7,322,801),
                    c("CDU/CSU","SPD","FDP","Gruene","Linke","AFD","Andere"))
})

table(gles$t154)
gles %<>% within({ 
  anti_immi <- t154
  anti_immi[t154 %in% c(-99,-98)] <- NA #Ausprägungen (Suchabfrage ist %in%) mit -99 und -72 werden mit NA ersetzt
  anti_immi <- anti_immi-6  #Mittelpunkt verschieben da die Skala von 1-11 geht
})


gles %<>% within({ #Einstellung zur AFD
  feel_afd <- t14g
  feel_afd[t14g %in% c(-99,-72)] <- NA 
  feel_afd <- feel_afd -6
  
  # Alter
  alter <- t2
  alter <- 2017-alter
  
})


### Bivariate Statistik

# Aufgabe 1a: Erstellt eine Kreuztabelle mit den Variablen 
# Sonntagsfrage (sofra_2) und der Einstellung zu Migration (anti_immi)
tabelle <- table(gles$sofra_2,gles$anti_immi) ; tabelle 

# Eine große Tabelle. Wir kategorisieren ein wenig. 

gles$anti_immi_cat <- NA
gles$anti_immi_cat [gles$anti_immi %in% c(-5,-4,-3)] <- "Mig. erleichtern"
gles$anti_immi_cat [gles$anti_immi %in% c(-2:2)] <- "neutral"
gles$anti_immi_cat [gles$anti_immi %in% c(3:5)] <- "Mig. einschraenken"

hist(gles$anti_immi)
tabelle2 <- table(gles$sofra_2,gles$anti_immi_cat) ; tabelle2
table(gles$sofra_2,gles$anti_immi_cat) %>% 
  prop.table(margin = 2) %>% 
  round(2)
prop.table(tabelle2,margin = 1) %>% round(2)


# 1b: Berechnet einen Chi-Quadrat-Test (chisq.test) für die beiden Variablen und 
# interpretiert das Ergebnis (alpha = 0.05)

chisq.test(tabelle2)

# 1c: Versucht, die Tabelle mit den erwarteten Werten bei Unanbhängigkeit
# darzustellen (Tipp: ...$ecpected)

tabelle2
chisq.test(tabelle2)$expected 
chisq.test(tabelle2)$expected %>% prop.table(2) %>% round(2)



## Aufgabe 2: Graphische Darstellung: 

install.packages("ggplot2")
install.packages("hrbrthemes")
install.packages("extrafont")

library(ggplot2)
library(hrbrthemes)
library(extrafont)

# 2a : Erstellt einen ggplot-point- scatterplot (geom_point) mit den beiden Variablen anti_immi (X) und feel_afd (Y)

# 2a-2: Was ist hieran noch nicht perfekt? Ersetzt geom_point durch geom_jitter (!). Was ist jetzt anders?

# 2b: Codiert zudem die Geschlechtsvariable (sex) selbst und differenziert bei der Farbe nach Geschlecht

# 2c: Legt jeweils eine Regressionsgerade in den Plot (+geom_smooth(method = "lm"))

# 2d: Fügt ein schönes Design aus dem hrbrthemes-Paket hinzu (z.B. +theme_ipsum (grid = "Y"))

# 2e: Speichert den Plot mit ggsave als pdf (ACHTUNG: zuvor: library(extrafont))



## BONUS: Für Nerds: Machen wir den Plot Interaktiv: 

# install.packages("ggiraph")

library(ggiraph)

gg_interactive <- 
  ggplot(data = gles,
         aes(x = alter,
             y = feel_afd)) + 
  geom_point_interactive(aes(tooltip = paste0("Wahlentscheidung: ",sofra_2))) + 
  labs(title = "AfD und Einstellungen zu Fluechtlingen",
       subtitle = "Interactive non-jittered plot") + 
  theme_ipsum(grid = "Y") 
  
ggiraph(ggobj = gg_interactive, # statt gg muss hier der Name des objekts stehen, den Ihr oben dem ggplot gegeben habt
        pointsize = 12)
