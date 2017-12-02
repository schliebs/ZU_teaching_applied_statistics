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

# Laden des Datensatzes
library(haven)

# Laden des Datensatzes
gles <- read_spss(file = "data/gles/ZA6800_v1-0-0.sav",
                  user_na = T)

# Fix das Label-Problem
gles[] <- lapply(gles, unclass)  

# Datenrekodieren

gles %<>% within({
  
  # Jamaika (AV)
  jamaika <- q73f
  jamaika [q73f %in% c(-99,-98)] <- NA
  jamaika_z <- jamaika - 6   # Zentrierung Skalenmittelpunkt 

  
  # Zufriedenheit Groko (UV1)
  zufr_groko <- q67
  zufr_groko [q67 %in% c(-99,-98)] <- NA
  zufr_groko_z <- zufr_groko - 6    # Zentrierung Skalenmittelpunkt 

  # Migration UV2
  migration <- q60
  migration [q60 %in% c(-99,-98)] <- NA
  migration_z <- migration - 6    # Zentrierung Skalenmittelpunkt 
  
  # Mögliche Interaktionen: 
  # Parteiidentifikation, Migrationshintergrund, Alter, Einkommen, Ost/West
  
  # Parteiidentifikation
  '(1) CDU/CSU
  (2) CDU
  (3) CSU
  (4) SPD
  (7) DIE LINKE
  (6) GRÜNE
  (5) FDP
  (322) AfD'
  # Codiere nur spezifizierte Partein 
  pid <- NA
  pid [q99a %in% c(1,2,3)] <- "union"
  pid [q99a %in% c(4)] <- "spd"
  pid [q99a %in% c(6)] <- "gruene"
  pid [q99a %in% c(7)] <- "linke"
  pid [q99a %in% c(5)] <- "fdp"
  pid [q99a %in% c(322)] <- "afd"
  pid [q99a %in% c(808)] <- "keine"

  # Wahlentscheidung
  
  erststimme <- q11aa
  erststimme [q11aa %in% c(-99,-98,-97,-83)] <- NA
  erststimme [q11aa %in% c(1)] <- "union"
  erststimme [q11aa %in% c(4)] <- "spd"
  erststimme [q11aa %in% c(5)] <- "fdp"
  erststimme [q11aa %in% c(6)] <- "gruene"
  erststimme [q11aa %in% c(7)] <- "linke"
  erststimme [q11aa %in% c(322)] <- "afd"
  erststimme [q11aa %in% c(801)] <- "andere"
  
  zweitstimme <- q11ba
  zweitstimme [q11ba %in% c(-99,-98,-97,-83)] <- NA
  zweitstimme [q11ba %in% c(1)] <- "union"
  zweitstimme [q11ba %in% c(4)] <- "spd"
  zweitstimme [q11ba %in% c(5)] <- "fdp"
  zweitstimme [q11ba %in% c(6)] <- "gruene"
  zweitstimme [q11ba %in% c(7)] <- "linke"
  zweitstimme [q11ba %in% c(322)] <- "afd"
  zweitstimme [q11ba %in% c(801)] <- "andere"
  
})

table(gles$zweitstimme)
table(gles$jamaika_z)
table(gles$zufr_groko_z)
table(gles$migration_z)
table(gles$pid) %>% prop.table() %>% round(2)

# Analysedatensatz 

gles_data <- select(gles,jamaika_z,zufr_groko_z,migration_z,pid,erststimme,zweitstimme)
gles_data %<>% na.omit()
