


#------------------------------------------------------------------------------

# Hier laden wir Pakete, die wir später verwenden wollen.
library(plyr)
library(tidyverse)
library(haven) #Bitte verwendet die github version.
library(magrittr) # Alle Pipes
library(memisc)
library(foreign)

gles <- read_spss(file = "data/ZA6816_v1-0-0_fb.sav",user_na = T)

# Sonntagsfrage ------------------------------------------------------------

# Methode: Skalenniveau und Datentypen (Nominalskala,factor)
# Häufigkeitsverteilung, Kreuztabelle
#Fragetext:
#Bei der Bundestagswahl können Sie ja zwei Stimmen vergeben. Die Erststimme
#für einen Kandidaten aus Ihrem Wahlkreis und die Zweitstimme für eine Partei.
#Hier ist ein Musterstimmzettel, ähnlich wie Sie ihn bei der Bundestagswahl
#erhalten.Was würden Sie auf Ihrem Stimmzettel ankreuzen, wenn am nächsten 
#Sonntag eine Bundestagswahl wäre?
#(A) Erststimme
#(B) Zweitstimme

# Erststimme
table(gles$t8aa)
table(gles$t8ba)
table(gles$t33aa)
table(gles$t33ba)

# Variable rekodieren

gles %<>% within({
  # Sonntagsfrage
  sofra_1 <- t8aa
  sofra_1[t8aa < 0] <- NA
  sofra_1 <- factor(sofra_1,c(1,4,5,6,7,322,801),
                       c("CDU/CSU","SPD","FDP","GRUENE","LINKE","AfD","Andere"))
  
  sofra_2 <- t8ba
  sofra_2[t8ba < 0] <- NA
  sofra_2 <- factor(sofra_2,c(1,4,5,6,7,322,801),
                       c("CDU/CSU","SPD","FDP","GRUENE","LINKE","AfD","Andere"))
  
  # Recall
  sofra13_2 <- t33ba
  sofra13_2[t33ba < 0] <- NA
  sofra13_2 <- factor(sofra13_2,c(1,4,5,6,7,322,801),
                    c("CDU/CSU","SPD","FDP","GRUENE","LINKE","AfD","Andere"))

  })

table(gles$sofra_1) %>% prop.table()

table(gles$sofra_2) %>% prop.table()

# Splittingverhalten

table(gles$sofra_1,gles$sofra_2) %>% prop.table(2) %>% round(2)
table(gles$sofra_2) %>% prop.table()

# Wählerwanderung
table(gles$sofra13_2) %>% prop.table()
table(gles$sofra_2,gles$sofra13_2) %>% prop.table(2) %>% round(2)

# -------------------------------------------------------------------------
# Statistikblock Whiteboard: Notation, Messtheori, Verteilungen,
# -------------------------------------------------------------------------
# Sachfragen

# Methode: Skalenniveau und Datentypen (intervallskala,factor),
# Verteilungsparameter, Konfidenzintervall und T-Test

# Nun geht es um die Zuzugsmöglichkeiten für Ausländer.
# Sollten die Zuzugsmöglichkeiten für Ausländer erleichtert 
# oder eingeschränkt werden?
# Welche Politik vertreten Ihrer Meinung nach die Parteien dazu?

#1 Zuzugsmöglichkeiten für Ausländer sollten erleichtert werden
# 11 Zuzugsmöglichkeiten für Ausländer sollten eingeschränkt werden


table(gles$t8ba) %>% prop.table()

gles %<>% within({
  # Einstellung zu Immigration
  anti_immi <- t154
  anti_immi[t154 %in% c(-99,-98)] <- NA
  anti_immi <- anti_immi-6
  
  # Sympathieskalometer
  feel_afd <- t14g
  feel_afd[t14g %in% c(-99,-72)] <- NA
  feel_afd <- feel_afd -6
  
})

table(gles$anti_immi)
table(gles$feel_afd)
# Zusammenfassung der Verteilung

summary(gles$anti_immi)
# Grafisch
ggplot(gles,aes(x = anti_immi)) + geom_histogram(binwidth = 1)
# Mittelwert

mx <- mean(gles$anti_immi,na.rm = T)
mx
quantile(gles$anti_immi,c(.25,.5,.75),na.rm = T)
median(gles$anti_immi,na.rm = T)

var(gles$anti_immi,na.rm = T)
sdx <- sd(gles$anti_immi,na.rm = T)



nx <- na.omit(gles$anti_immi) %>% length()

# -------------------------------------------------------------------------
# Statistikblock Whiteboard: Inferenzstatistik (Konfidenzintervall und T-Test)
# -------------------------------------------------------------------------



# Konfidenzintervall und t-test
t.test(gles$anti_immi)
# zu Fuß #-------------------------------------------------------------------
sex <- sdx/sqrt(nx) #Standardfehler von x :-)
# Konfidenzintervall
qnorm(c(.025,.975),mx,sex)
mx + c(-1.96,+1.96)*sex #Eigentlich Z-test
# Die ganz korrekten quantile mit der T-Verteilung
qt(c(.025,.975),df = nx-1)


# -------------------------------------------------------------------------
# Statistikblock Whiteboard: Kausalität, 2-Stichproben T-Test
# -------------------------------------------------------------------------
# Männer und die AfD Mittelwertvergleich
table(gles$t1)
gles %<>% within({
  # Sonntagsfrage
  gender <- factor(t1,
                   1:2,c("Mann","Frau"))
  
  
  
})

table(gles$gender)
t.test(feel_afd~gender,gles)

# Kreuztabelle--------------------------------------------------------------
# -------------------------------------------------------------------------
# Statistikblock Whiteboard: chi^2-Test
# -------------------------------------------------------------------------

# Beispiel im Kurs

tab <- matrix(c(50,50,100,100),byrow = T,ncol = 2)
chisq.test(tab)

tab <- matrix(c(70,30,80,120),byrow = T,ncol = 2)
test <- chisq.test(tab)
plot(1:50,dchisq(1:50,df = 1), type = "l")
test$expected
# Cramers V ist ein standardisiertes Maß
q <- min(dim(tab))
N <- sum(tab)
V <- sqrt(test$statistic/((q-1)*N))
V

# Einstellung zu Immigration und Parteiwahl ----------------------------------

gles %<>% within({
  # Sonntagsfrage
  immi <- recode(anti_immi,
                          "pro" <- -5:-3,
                          "neutral" <- -2:2,
                          "contra" <- 3:5)

  })

is.factor(gles$immi)
table(gles$sofra_1,gles$immi) %>% prop.table(2) %>% round(2)
# Test
tab <- table(gles$sofra_1,gles$immi)
tab
chisq.test(tab)
chisq.test(tab,simulate.p.value = T)
plot(1:50,dchisq(1:50,df = 12), type = "l")

# Schönere Darstellung
library(sjPlot)
sjt.xtab(gles$sofra_1,gles$immi,show.obs = T,show.col.prc = T)

# Misstrauen in die politischen Eliten und Parteiwahl -------------------------

table(gles$t124h)
gles %<>% within({
  # Sonntagsfrage
  trust_parl <- t124h
  trust_parl[trust_parl %in% c(-71,-99)] <- NA
  trust_parl <- trust_parl-6
  
  trust_parl <- recode(trust_parl,
                 "nein" <- -5:-3,
                 "etwas" <- -2:2,
                 "sehr" <- 3:5)
  
  
})

is.factor(gles$trust_parl)

table(gles$sofra_1,gles$trust_parl) %>% prop.table(2) %>% round(2)
# Test
tab <- table(gles$sofra_1,gles$trust_parl)
tab
chisq.test(tab)
chisq.test(tab,simulate.p.value = T)

plot(1:50,dchisq(1:50,df = 12), type = "l")


