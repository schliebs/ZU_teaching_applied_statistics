# 1: Packages Laden 
# (falls einige noch nicht installiert sind, jeweils install.packages("....."))

# Visualisierung:
library(ggplot2)
library(hrbrthemes) # install.packages("hrbrthemes")
library(extrafont)


# Einfache Scatterplots

# Zusammenhang Sympathie Jamaika ~ Migrationseinstellung

ggplot(data = SOEP_data,                          # GGplot starten und Datensatz angeben
       mapping = aes(x = alter,        # Specify mapping: aestethics (variable stuff)
                     y = netinc)) +  
  geom_jitter()                              # jittered scatterplot
  

# Hinzufügen einer Regressionsgerade
ggplot(data = SOEP_data,                          
       mapping = aes(x = alter,        
                     y = netinc)) +  
  geom_jitter() + 
  geom_smooth(method = "lm") # lineare Regressionsgerade


# Unterscheiden nach Geschlecht (farblich)
ggplot(data = SOEP_data,                          
       mapping = aes(x = alter,        
                     y = netinc,
                     col = sex)) +  # groupieren nach Color !
  geom_jitter() + 
  geom_smooth(method = "lm",se = FALSE) # wir schalten die Konfidenzintervalle aus 

# Die Farben sind optimierbar: 
ggplot(data = SOEP_data,                          
       mapping = aes(x = alter,        
                     y = netinc,
                     col = sex)) +  
  geom_jitter(alpha = 0.7) +  # wir machen die Punkte ein bisschen durchsichtig
  geom_smooth(size = 2,method = "lm",se = FALSE)+ 
  scale_color_manual(values = c("männlich" = "pink",  # wir weisen den Parteien farben zu
                                "weiblich" = "blue"),
                     name = "Geschlecht",
                     labels = c("Mann","Frau"))

# Wir belabeln die Achsen
ggplot(data = SOEP_data,                          
       mapping = aes(x = alter,        
                     y = netinc,
                     col = sex)) +  
  geom_jitter(alpha = 0.7) +  
  geom_smooth(size = 2,method = "lm",se = FALSE)+ 
  scale_color_manual(values = c("männlich" = "pink",  
                                "weiblich" = "blue"),
                     name = "Geschlecht",
                     labels = c("Mann","Frau")) + 
  labs(x = "Alter", 
       y = "Nettoeinkommen",
       title = "Titel",
       subtitle = "Untertitel",
       caption = "whatever")

# Jetzt noch ein schönes Theme und sichern unter dem Objekt gg1
gg1 <-  # zuweisen
  ggplot(data = SOEP_data,                          
       mapping = aes(x = alter,        
                     y = netinc,
                     col = sex)) +  
  geom_jitter(alpha = 0.7) +  
  geom_smooth(size = 2,method = "lm",se = FALSE)+ 
  scale_color_manual(values = c("männlich" = "pink",  
                                "weiblich" = "blue"),
                     name = "Geschlecht",
                     labels = c("Mann","Frau")) + 
  labs(x = "Alter", 
       y = "Nettoeinkommen",
       title = "Titel",
       subtitle = "Untertitel",
       caption = "whatever")#+
 # theme_ipsum(grid = "Y") #Theme mit schöner Schrit und mit horizontalen Grid-Linien

gg1 #print

# Entweder pdf
ggsave(filename = "results/soep_jitterplot.pdf",
       plot = gg1,
       device = "pdf",
       width = 10,
       height = 6,
       dpi = 2000)

# oder png
ggsave(filename = "results/soep_jitterplot.png",
       plot = gg1,
       device = "png",
       width = 10,
       height = 6,
       dpi = 2000)




#######################################################################

# Barplots 

# Eindimensional
table(SOEP_data$netinc)

# Häufigkeitsverteilung der Zweitstimme
ggplot(data = SOEP_data) + 
  geom_bar(aes(x = inc_kat))
               
# In Prozent
ggplot(data = SOEP_data) + 
  geom_bar(aes(x = inc_kat,
               y = (..count..)/sum(..count..))) # relative häufigkeit 

# Labels + Theme/Labels

  prop.table(
  table(inc = SOEP_data$inc_kat,
        sex = SOEP_data$sex),
  margin = 1)

# Twodimensional Barplots  
gg1 <- #objekt zuweisen
  ggplot(data = SOEP_data) + 
  geom_bar(aes(x = inc_kat,
               y = (..count..),
               fill = sex))+ # relative häufigkeit + 
  scale_fill_manual(values = c("männlich" = "pink",  
                                "weiblich" = "blue"),
                     name = "Geschlecht",
                     labels = c("Mann","Frau")) + 
  labs(x = "Einkommensquartil", 
       y = "Häufigkeit",
       title = "Titel",
       subtitle = "Untertitel",
       caption = "whatever") + 
  theme_ipsum(grid = "Y")
gg1 #print

# Entweder pdf
ggsave(filename = "results/soep_barplot.pdf",
       plot = gg1,
       device = "pdf",
       width = 10,
       height = 6,
       dpi = 1000)
