# 1: Packages Laden 
# (falls einige noch nicht installiert sind, jeweils install.packages("....."))

# Visualisierung:
library(ggplot2)
library(hrbrthemes) # install.packages("hrbrthemes")
library(extrafont)

# Laden des Datensatzes
library(readstata13)

# Datenrekodieren:
library(car)


# Daten als Objekt importieren
SOEP <- read.dta13(file="data/soep/new8.dta" , convert.factors=F)

# Variable Alter generieren
SOEP$alter = 2003 - SOEP$gebjahr
SOEP$alter [SOEP$alter == 2004] <- NA # entfernen der fehlerhaften

#Sex 

SOEP$sex <- factor(SOEP$sex,levels = c(1,2),labels = c("männlich","weiblich"))

# Ueberstunden recodieren 0=nein, 1=ja
SOEP$over = recode(SOEP$tp72,"2=0;-2=NA;-1=NA; 3=NA")

# Vertragliche und tatsaechliche Wochenarbeitszeit
# Missings bereinigen
SOEP$contract = recode(SOEP$tp7001,"-3=NA;-2=NA;-1=NA")
SOEP$actual = recode(SOEP$tp7003,"-3=NA;-2=NA;-1=NA")
SOEP$contract = SOEP$contract/10
SOEP$actual = SOEP$actual/10

# Vertrauen 
# "Trust in people" und "Can't rely on anybody" recodieren
SOEP$trust = recode(SOEP$tp0301,"-1=NA")
SOEP$rely = recode(SOEP$tp0302,"-1=NA")

SOEP$netinc = recode(SOEP$tp7602,"-3=NA;-2=NA;-1=NA")



# Nur relevante Variablen werden übernommen
SOEP_data <- SOEP[,c("netinc","alter","sex","contract","actual","trust","rely")]
head(SOEP_data)

# Delete NAs
SOEP_data <- na.omit(SOEP_data)

# Einkommen Kategorisiert in Quartile

hist(SOEP$netinc)
quantile(SOEP_data$netinc)

SOEP_data$inc_kat <- NA
SOEP_data$inc_kat [SOEP_data$netinc < quantile(SOEP_data$netinc)[2]] <- "Q1"
SOEP_data$inc_kat [SOEP_data$netinc >= quantile(SOEP_data$netinc)[2] & SOEP_data$netinc < quantile(SOEP_data$netinc)[3]] <- "Q2"
SOEP_data$inc_kat [SOEP_data$netinc >= quantile(SOEP_data$netinc)[3] & SOEP_data$netinc < quantile(SOEP_data$netinc)[4]] <- "Q3"
SOEP_data$inc_kat [SOEP_data$netinc >= quantile(SOEP_data$netinc)[4] ] <- "Q4"

table(SOEP_data$inc_kat)
