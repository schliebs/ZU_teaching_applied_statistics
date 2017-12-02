# 1: Packages Laden 
# (falls einige noch nicht installiert sind, jeweils install.packages("....."))

# Datamanagement/Pipes
library(plyr)
library(tidyverse)
library(magrittr)

# Visualisierung:
library(ggplot2)
library(hrbrthemes) # install.packages("hrbrthemes")
library(extrafont)


# Einfache Scatterplots

# Zusammenhang Sympathie Jamaika ~ Migrationseinstellung

ggplot(data = gles_data,            # GGplot starten und Datensatz angeben
       aes(x = migration_z,        # Specify mapping: aestethics (variable stuff)
           y = jamaika_z))  + 
  geom_jitter()                              # jittered scatterplot
  

# Hinzufügen einer Regressionsgerade
ggplot(data = gles_data,                          
       mapping = aes(x = migration_z,        
                     y = jamaika_z)) +  
  geom_jitter(color = "purple") + 
  geom_smooth(method = "lm") # lineare Regressionsgerade


# Unterscheiden nach Parteiidentifikation (farblich)
ggplot(data = gles_data,                          
       mapping = aes(x = migration_z,        
                     y = jamaika_z,
                     col = pid)) +  # groupieren nach Color !
  geom_jitter() + 
  geom_smooth(method = "lm",se = FALSE) # wir schalten die Konfidenzintervalle aus 


# Die Farben sind hässlich: 
ggplot(data = gles_data,                          
       mapping = aes(x = migration_z,        
                     y = jamaika_z,
                     col = pid)) +  
  geom_jitter(alpha = 0.4) +  # wir machen die Punkte ein bisschen durchsichtig
  geom_smooth(size = 2,method = "lm",se = FALSE)+ 
  scale_color_manual(values = c("afd" = "blue",  # wir weisen den Parteien farben zu
                                "fdp" = "yellow",
                                "gruene" = "green",
                                "keine" = "grey",
                                "linke" = "purple",
                                "spd" = "red",
                                "union" = "black"),
                     name = "PID",
                     labels = c("Die Blauen","Liberale","Ökos","Keine","Kommunisten","Sozis","Union"))

# Wir belabeln die Achsen
ggplot(data = gles_data,                          
       mapping = aes(x = migration_z,        
                     y = jamaika_z,
                     col = pid)) +  
  geom_jitter(alpha = 0.7) +  
  geom_smooth(size = 2,method = "lm",se = FALSE)+ 
  scale_color_manual(values = c("afd" = "blue",
                                "fdp" = "yellow",
                                "gruene" = "green",
                                "keine" = "grey",
                                "linke" = "purple",
                                "spd" = "red",
                                "union" = "black"),
                     name = "PID",
                     labels = c("AfD","FDP","Grüne","Keine","Linke","SPD","Union")) + 
  labs(x = "Migrationsfeindlichkeit", # Labels
       y = "Sympathie Jamaika",
       title = "Titel",
       subtitle = "Untertitel",
       caption = "Dieser Plot wird ihnen präsentiert von ZF")


# Jetzt noch ein schönes Theme und sichern unter dem Objekt gg1
gglala <-  # zuweisen
  ggplot(data = gles_data,                          
       mapping = aes(x = migration_z,        
                     y = jamaika_z,
                     col = pid)) +  
  geom_jitter(alpha = 0.7) +  
  geom_smooth(size = 2,method = "lm",se = FALSE)+ 
  scale_color_manual(values = c("afd" = "blue",
                                "fdp" = "yellow",
                                "gruene" = "green",
                                "keine" = "grey",
                                "linke" = "purple",
                                "spd" = "red",
                                "union" = "black"),
                     name = "PID",
                     labels = c("AfD","FDP","Grüne","Keine","Linke","SPD","Union")) + 
  labs(x = "Migrationsfeindlichkeit", 
       y = "Sympathie Jamaika",
       title = "Titel",
       subtitle = "Untertitel",
       caption = "whatever") #+ 
 # theme_ipsum(grid = "Y") #Theme mit schöner Schrit und mit horizontalen Grid-Linien

gglala #print


# Schriftarten
loadfonts(device="pdf")

# Entweder pdf
ggsave(filename = "results/gles_jitterplot.pdf",
       plot = gglala,
       device = "pdf",
       width = 10,
       height = 6,
       dpi = 2000)

# oder png
ggsave(filename = "results/gles_jitterplot.png",
       plot = gg1,
       device = "png",
       width = 10,
       height = 6,
       dpi = 2000)




#######################################################################

# Barplots Wahlentscheidung

# Eindimensional
table(gles_data$zweitstimme)

# Häufigkeitsverteilung der Zweitstimme
ggplot(data = gles_data) + 
  geom_bar(aes(x = zweitstimme))
               

# In Prozent
ggplot(data = gles_data) + 
  geom_bar(aes(x = zweitstimme,
               y = (..count..)/sum(..count..), # relative häufigkeit 
               fill = zweitstimme))+ # Farblich
  scale_fill_manual(values = c("afd" = "blue",
                                "fdp" = "yellow",
                                "gruene" = "green",
                                "andere" = "grey",
                                "linke" = "purple",
                                "spd" = "red",
                                "union" = "black"),
                     name = "Zweitstimme",
                     labels = c("AfD","Andere","FDP","Grüne","Linke","SPD","Union"))

# Labels + Theme/Labels

ggplot(data = gles_data,
       aes(x = zweitstimme,
           y = (..count..)/sum(..count..), # relative häufigkeit 
           fill = zweitstimme)) + 
  geom_bar(stat = "count")+ # Farblich 
  
  geom_text(aes(x = zweitstimme,
                y=..count../sum(..count..)+0.02,
                label=paste0(round(..count../sum(..count..)*100,1)," %"),
                color = zweitstimme), 
            stat="count")+
  scale_fill_manual(values = c("afd" = "blue",
                               "fdp" = "yellow",
                               "gruene" = "green",
                               "andere" = "grey",
                               "linke" = "purple",
                               "spd" = "red",
                               "union" = "black"),
                    name = "Zweitstimme",
                    labels = c("AfD","Andere","FDP","Grüne","Linke","SPD","Union"))+
  scale_color_manual(values = c("afd" = "blue",
                                "fdp" = "yellow",
                                "gruene" = "green",
                                "andere" = "grey",
                                "linke" = "purple",
                                "spd" = "red",
                                "union" = "black"),
                     name = "Zweitstimme",
                     labels = c("AfD","Andere","FDP","Grüne","Linke","SPD","Union"))+
  labs(x = "Partei", 
       y = "Anteil Zweitstimmen",
       title = "Titel",
       subtitle = "Untertitel",
       caption = "whatever") #+ 
  #theme_ipsum(grid = "Y") #Theme mit schöner Schrit und mit horizontalen Grid-Linien



round(
  prop.table(
  table(zweit = gles_data$zweitstimme,
        erst = gles_data$erststimme),margin = 1),2)

# Twodimensional Barplots  
gg1 <- #objekt zuweisen
  ggplot(data = gles_data,
         aes(x = zweitstimme,
             y = (..count..)/sum(..count..), 
             fill = erststimme)) + 
  geom_bar(stat = "count")+ 
  scale_fill_manual(values = c("afd" = "blue",
                               "fdp" = "yellow",
                               "gruene" = "green",
                               "andere" = "grey",
                               "linke" = "purple",
                               "spd" = "red",
                               "union" = "black"),
                    name = "Davon Erststimme",
                    labels = c("AfD","Andere","FDP","Grüne","Linke","SPD","Union"))+
  scale_color_manual(values = c("afd" = "blue",
                                "fdp" = "yellow",
                                "gruene" = "green",
                                "andere" = "grey",
                                "linke" = "purple",
                                "spd" = "red",
                                "union" = "black"),
                     name = "Davon Erststimme",
                     labels = c("AfD","Andere","FDP","Grüne","Linke","SPD","Union"))+
  labs(x = "Zweitstimme", 
       y = "Anteil Zweitstimmen",
       title = "Titel",
       subtitle = "Untertitel",
       caption = "whatever") #+ 
  #theme_ipsum(grid = "Y")
gg1 #print

# Entweder pdf
ggsave(filename = "results/gles_barplot__stacked.pdf",
       plot = gg1,
       device = "pdf",
       width = 10,
       height = 6,
       dpi = 1000)
