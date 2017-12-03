# Zum Wiederholen: GGplot 

# Nur beim allerststen Mal ausführen, danach zum Kommentar machen

#install.packages("plyr")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("hrbrthemes")
#install.packages("extrafont")
#install.packages("datasets")

# Immer ausführen
library(plyr)
library(tidyverse)
library(hrbrthemes)
library(extrafont)
library(ggplot2)
library(datasets)

# Laden des Bieber-Datensatzes
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

# Time Series Analysis
#install.packages("xts")
library(xts)

# Add fictional date for time series
bib$date <- seq(as.Date("2000/1/1"), by = "days", length.out = nrow(bib))

rownames(bib) <- bib$date
bib_xts <- as.xts(bib)
storage.mode(bib_xts) <- "numeric"

# 
bib_logdif <- diff(log(bib_xts$temp))[-1,]

plot.zoo(bib_logdif)

#install.packages("dygraphs")
library(dygraphs)
dygraph(bib_logdif)

bib_logdif %>% as.data.frame() %>% summarise_all(funs(mean = mean(.),
                                                     sum = sum(.)))
#install.packages("fpp")
#install.packages("forecast")

library(fpp)
library(forecast)

Acf(bib_logdif)
Pacf(bib_logdif)

Box.test(bib_logdif,type = "Ljung-Box",lag = 3) # non-stationary!
tseries::adf.test(bib_logdif,alternative = "stationary") #nope

# test for autocorrelation of log differences
df <- data.frame(bib_logdif,lag(bib_logdif,1),lag(bib_logdif,2),lag(bib_logdif,3))

mod <- lm(temp ~ .,data = df)
summary(mod) # autocorrelation too week or to few observations to be significant



temp <- (bib_xts$temp)
# test for autocorrelation of absolute values
df2 <- data.frame(temp,lag(temp,1),lag(temp,2),lag(temp,3))

mod2 <- lm(temp ~ .,data = df2)
summary(mod2) 


# GARCH (funktioniert nicht da zu wenig observations )
n.obs = length(bib_logdif)
w.e = 50
w.t = n.obs - w.e

#install.packages("rugarch")
library(rugarch)

lalala = ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                          mean.model = list(armaOrder=c(0,0)))

bibibibi = ugarchroll(lalala, bib_logdif, fit.control=list(scale=1), n.ahead=1,
                               forecast.length = w.t, refit.every=20, refit.window="moving", calculate.VaR=TRUE, VaR.alpha=0.01)


bibibibi
#########