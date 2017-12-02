# Anbei ein paar Uebungsaufgaben gerade auch mit Blick auf das Mid-Term.

# Zeile 5-65 sind zum verstehen/nachvollziehen da, danach sollt und könnt Ihr selbst codieren. 

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

x = table(gles$sofra_1) %>% prop.table() 
paste(round(x*100,2),"%")

table(gles$sofra_2) %>% prop.table()

# Splittingverhalten

table(erst = gles$sofra_1,zweit = gles$sofra_2) %>% prop.table(2) %>% round(2)
table(gles$sofra_2) %>% prop.table()

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
  
  # Einstellung zur AfD
  feel_afd <- t14g
  feel_afd[t14g %in% c(-99,-72)] <- NA 
  feel_afd <- feel_afd -6
  
  # Geschlecht
  gender <- factor(t1, 1:2, c("Mann", "Frau")) 
  
})

table(gles$wi_now)
table(gles$wi_prog)
table(gles$angst_entlassung)
table(gles$alter)
table(gles$feel_afd)
table(gles$gender)

gles$angst_entlassung [!is.na(gles$angst_entlassung) & gles$feel_afd %in% c(-5,-4,-3)] 

c(1) %in% c(2,3,4)
2 == 2 | 2 == 3 | 2 == 4

#####################

# Bevor Ihr anfangen zu codet, noch einige Verständnisfragen: 
# ich werde nicht die Zeit habe, alle durchzugehen, aber wir können einen kleinen Exkurs machen, wenn noch etwas unklar ist

data$v1 [data$v1 == -99] <- NA

'Welche Buchstaben werden zur Beschreibung von Variablen in der Statistik verwendet?
Was versteht man unter Messen?
Wie rekodiert man eine Variable in R?
Was unterscheidet die Intervall von der Ratioskala?
Welche Annahme treffen Sie, wenn Sie eine Antwortskala von 1 bis 11 als intervallskalliert interpretieren?
Welche Vor- und Nachteile haben das arithmethische Mittel und der Median?
Wie lauten die dazugehörigen R Befehle und warum müssen Sie na.rm = T in der Funktion setzen?
Was ist ein p-Quantil?
Wie berechnet man das 75%-Quantil in R?
Wie muss die Verteilung einer metrischen Variable aussehen, damit sie die maximale Varianz aufweist?
Wie unterescheiden sich Standardfehler und Standardabweichung?
Wie lauten die 3 Komponenten eines statistischen Tests?'

# data
bb <- datasets::beaver1
quantile(bb$temp,probs = c(0,0.2,0.4,0.6,0.8,1))
?qnorm

qnorm(c(0.01,0.25,0.5,0.75,0.99),mean = 0,sd = 1)

se <- sd/sqrt(n)

# Ab hier müsst ihr selbst coden: 

##A  T-Test univariat: Beantwortet die folgenden Fragen

# 1) Was für eine zukünftige wirtschaftliche Entwicklung erwarten die Befragten im Durchschnitt? (variable: wi_prog)

mean(gles$wi_prog,na.rm = TRUE)

# 2) Unterscheidet sich der Wert signifikant von Null (5%-Level). Berechnet einen T-Test. 

# manuell

sd <- sd(gles$wi_prog,na.rm = TRUE)
n <- length(gles$wi_prog[!is.na(gles$wi_prog)]);n

se <- sd/n ;se

t.test(gles$wi_prog,conf.level = 0.95)

# 3) Berechnet ein Konfidenzintervall für den Mittelwert. Wie interpretieren wir ein solches Intervall?

gles$bil_kat <- NA
gles$bil_kat [gles$bild %in% c("Abi","Real")] <- "hoch"
gles$bil_kat [gles$bild %in% c("Haupt","Grund")] <- "niedrig"

##B  T-Test bivariat: 

# 1) Wie sind die Deutschen im Durchschnitt gegenüber der AfD Eingestellt? Signifikant verschieden von Null? Konfidenzintervall?

# 2) Wir wollen nun schauen, ob es bei der Einstellung zur AfD einen Geschlechterunterschied gibt. 
#    Berechnet einen T-Test für Gruppenunterschiede. 
#    Was bedeutet das Konfidenzintervall? P-Value? Wir können wir diesen aus dem T-Wert ableiten?

t.test(gles$feel_afd ~ gles$gender,conf.level = 0.95)


## C GGplot: Wir wollen nun die letzte Stunde am Bieberbeispiel gelernten GGplots üben: 
## Ich gebe Euch hier den Code, versucht aber bitte, genau zu verstehen, was genau passiert und spielt gerne selbst rum.

# install.packages("ggplot2")
# install.packages("hrbrthemes")
# install.packages("extrafont")

library(ggplot2)
library(hrbrthemes)
library(extrafont)

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
  theme_ipsum(grid = "Y")


# Angst den Arbeitsplatz zu verlieren: 
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

ggsave(filename = "entlassung.pdf",
       plot = gg,
       width = 10,
       height = 5.5,
       device = "pdf",
       dpi = 1000)

Was zeigt ein Scatterplott an?
Warum macht es Sinn bei Likert-Skalen Items einen Jitterplot (geom_jitter()) zu verewenden)'


## Gibt es einen Zusammenhang zwischen der Erwartung der wirtschaftlichen Entwicklung und der Sympathie für die AfD?

# Einfacher plot
plot(gles$feel_afd,gles$wi_prog) # was ist hieran nicht gut?

# Mit ggplot: 
ggplot(data = gles,
       aes(x = wi_prog,
           y = feel_afd)) + 
  geom_point() 
# was können wir tun? Jitterplot!

ggplot(data = gles,
       aes(x = wi_prog,
           y = feel_afd)) + 
  geom_jitter() 

# Noch ein wenig schöner und mit Regressionsgerade und nach Geschlechtern aufgetrennt

gg_afd <- 
  ggplot(data = gles,
       aes(x = wi_prog,
           y = feel_afd,
           col = gender)) + 
  geom_jitter() + 
  geom_smooth(method = "lm") + # Das erkläre ich Euch im Tut
  labs(x = "Erwartete wirt. Entwicklung", 
       y = "Sympathie AfD",
       title = "AfD - Sozialpessimisten?",
       subtitle = "Untertitel",
       caption = "@copyright Vorname Nachname") + 
  theme_ipsum(grid = "Y"); gg_afd


# Speichern der Grafik: 

ggsave(filename = "afd_plot.pdf",
       width = 10,
       height = 5.5,
       plot = gg_afd,
       device = "pdf",
       dpi = 1000)

