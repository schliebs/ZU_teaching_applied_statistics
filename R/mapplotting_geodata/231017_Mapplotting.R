# Package installieren
getwd()

# Hier laden wir alle nötigen Pakete ---------------------------------------
library(plyr) 
library(tidyverse) #Viele nützliche Funktionen
library(haven) #Paket zum Lesen von SPSS-Daten
library(magrittr) #Alle möglichen zusätzlichen Pipes das kommt noch alles :-)
#----------------------------------------------------------------------------
source("make_dictionary.R") # Hier holen wir noch eine Funktion rein, die es
# uns ermöglicht das meta_file unten zu erstellen.

ess <- read_spss(file = "data/ESS7e02_1/ESS7e02_1.sav") # Datensatz einlesen

meta_file <- make_dictionary(ess,format = "wide") # Meta file erstellen

#############################################################
install.packages("...")
install.packages("...")

install.packages("maps")
library(ggplot2)
library(ggiraph)
library(hrbrthemes)
library(extrafont)
library(htmlwidgets)

map_world <- map_data("world")

# Vergleich der Länderlisten

unique(map_world$region) # unique: jede observation nur einmal
unique(ess$cntry)

# Fixing of countries
ess$country_full <- NA
ess$country_full [ess$cntry == "AT"]  <- "Austria"
ess$country_full [ess$cntry == "BE"]  <- "Belgium"
ess$country_full [ess$cntry == "CH"]  <- "Switzerland"
ess$country_full [ess$cntry == "CZ"]  <- "Czech Republic"
ess$country_full [ess$cntry == "DE"]  <- "Germany"
ess$country_full [ess$cntry == "DK"]  <- "Denmark"
ess$country_full [ess$cntry == "EE"]  <- "Estonia"
ess$country_full [ess$cntry == "ES"]  <- "Spain"
ess$country_full [ess$cntry == "FR"]  <- "France"
ess$country_full [ess$cntry == "GB"]  <- "UK"
ess$country_full [ess$cntry == "HU"]  <- "Hungary"
ess$country_full [ess$cntry == "IE"]  <- "Ireland"
ess$country_full [ess$cntry == "IL"]  <- "Italy"
ess$country_full [ess$cntry == "LT"]  <- "Lithuania"
ess$country_full [ess$cntry == "NL"]  <- "Netherlands"
ess$country_full [ess$cntry == "NO"]  <- "Norway"
ess$country_full [ess$cntry == "PL"]  <- "Poland"
ess$country_full [ess$cntry == "PT"]  <- "Portugal"
ess$country_full [ess$cntry == "SE"]  <- "Sweden"
ess$country_full [ess$cntry == "SI"]  <- "Slovenia"

####################

# Berechnen einer Variable (z.B. Mean) nach Land

# Immigration/Kultur
ess %<>% within({
  
  # Aufnahme gleiche Race/Ethnicity
  culture_same <- imsmetn
  culture_same[culture_same %in% 7:9] <- NA
  
  # Aufnahme andere Race/Ethnicity
  culture_other <- imdfetn
  culture_other[culture_same %in% 7:9] <- NA
  
})

# Dplyr-summarise Befehl
df <- 
  ess %>% 
  group_by(country_full) %>% 
  summarize(same = mean(culture_same,na.rm = TRUE),
            other = mean(culture_other,na.rm = TRUE))
df

# Verbinden der Datensätze (Kartenmaterial/Geodaten und ESS)

data <- left_join(x = map_world,
                  y = df, 
                  by = c("region" = "country_full"))

###################

# Statischer Plot

plot <- 
  ggplot(data = data)+
  geom_polygon(aes(x=long,
                   y=lat,
                   group=group,
                   fill= other),
               color="black",size=0.05)+
  coord_fixed(xlim = c(-10, 35),  ylim = c(35,70), ratio = 1)+ #zoom auf Europa+
  theme_ipsum(grid = "NULL")+
  labs(x=NULL, 
       y=NULL,
       title = "Titel",
       subtitle = "Allow many/few immigrants of \ndifferent race/ethnic group from majority",
       caption= "Grafik: Vorname Nachname")+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())+
  scale_fill_continuous(low = "green",
                        high = "red");plot


ggsave(filename = "Static_Plot.pdf",
       plot = plot,
       device = "pdf",
       width = 16,
       height = 9,
       dpi = 1000,scale = 0.8
       )


# Let's make this interactive

data$tip <- paste0("Country: ",
                   data$region,
                   "\nGleiche Etnhien: ",
                   round(data$same,2),
                   "\nAndere Ethnien: ",
                   round(data$other,2))


# Statischer Plot

interactive_plot <- 
  ggplot(data = data %>% filter(region %in% c(ess$country_full)))+
  geom_polygon_interactive(aes(x=long,
                   y=lat,
                   group=group,
                   fill= other,
                   tooltip = tip),
               color="black",size=0.05) +
  coord_fixed(xlim = c(-10, 35),  ylim = c(35,70), ratio = 1)+
  theme_ipsum(grid = "NULL")+
  labs(x=NULL, 
       y=NULL,
       title = "Titel",
       subtitle = "Allow many/few immigrants of \ndifferent race/ethnic group from majority",
       caption= "Grafik: Vorname Nachname")+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())+
  scale_fill_continuous(low = "green",
                        high = "red")


interactive_plot_out <- ggiraph(code = print(interactive_plot),
                                hover_css = "fill:blue;")
interactive_plot_out

saveWidget(widget = interactive_plot_out,file = "interactive.html",selfcontained = TRUE)


