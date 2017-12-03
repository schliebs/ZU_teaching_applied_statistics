library(haven) 
library(magrittr) 
library(sjPlot)
library(tidyverse)
library(ggplot2)

# Ein neues Paket: psych
library(psych)

gles <- read_spss(file = "data/offline/gles_summerpanel/ZA6816_v1-0-0_fb.sav",
                  user_na = T) #Einmal das ganze makieren und
#Strg und Enter drücken :)

# Variablen definieren -------------------------------------------------------

gles %<>% within ({
  schutz <- e0117a
  schutz[e0117a %in% c(-98,-99)] <- NA
  schutz_r <- (schutz - 6)*-1
  
  scham <- e0117c
  scham[e0117c %in% c(-98,-99)] <- NA
  
  verkraften <- e0117e
  verkraften[e0117e %in% c(-98,-99)] <- NA
  
  vorteile <- e0117g
  vorteile[e0117g %in% c(-98,-99)] <- NA
  
  zusammenhalt <- e0117k
  zusammenhalt[e0117k %in% c(-98,-99)] <- NA
  zusammenhalt_r <- (zusammenhalt - 6)*-1
  
  kosten <- e0117m
  kosten[e0117m %in% c(-98,-99)] <- NA
  kosten_r <- (kosten - 6)*-1
  
  kritik <- e0117n
  kritik[e0117n %in% c(-98,-99)] <- NA
  
})

anadat1 <- select(gles, scham, verkraften,
                   vorteile, zusammenhalt_r, kosten_r) %>% 
  na.omit()

mat <- cor(anadat1) %>% round(2)
anadat1 %>% as.matrix() %>% alpha(check.keys = T)


## Use Index for continued analysis of the sociotropy analysis: 

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



# Analysedatensatz: 

ana_dat2 <- select(gles,sozio,einkommen_z,einkommen,alter_z,alter,gender,
                   scham, verkraften,vorteile, zusammenhalt_r, kosten_r)

ana_dat2 %<>% na.omit()

ana_dat2 %<>% within({index <- scham + 
                               verkraften +
                               vorteile +
                               zusammenhalt_r +
                               kosten_r})
library(ggplot2)
library(ggthemes)
gg1 <- 
  ggplot(data = ana_dat2,aes(x = index,y = sozio)) + # Grundstock: Datenspezifizierung
  geom_jitter() +                                # Jittered Points
  geom_smooth(method = "lm", col = "red") +      # Lineare Regressionsgerade mit Konfidenzinterval
  labs(x = "Index",
       y = "Sozialstaatsposition",
       title = "Zusammenhang Index ~ Sozialstaatliche Positionen",
       subtitle = "Untertitel",
       caption = "Soz.position: Positiver Wert = Mehr Umverteilung, höhere Steuern") +
  theme_economist()                        # Schönes Design
gg1

ggsave(filename = "results/figures/plot1.pdf",
       plot = gg1,
       device = "pdf",
       width = 10,
       height = 6,
       dpi = 2000)


# Regressionsmodell
mod1 <- lm(sozio ~ index,data = ana_dat2)
summary(mod1)
 