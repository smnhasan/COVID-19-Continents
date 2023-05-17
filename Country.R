library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(betareg)
library(car)
library(gapminder)
library(dplyr)
library(ggplot2)
library(dplyr)
library(patchwork)
library(hrbrthemes)
library(psych)
library(forecast)
library(lmtest)
library(tseries)
library(plotrix)
library(tidyverse) 
library(glmmTMB)
library(DHARMa)
library(performance)

#Set directory
setwd('E:\\Continent')


#Line graph
#figure 1

#Reading data
COVID <- read.csv("owid-covid-data.csv")

#Consider only upto 2022 data
COVID <- subset(COVID, COVID$date < "2023-03-20")


#Remove World and International information
COVID <-COVID[!(COVID$iso_code=="OWID_AFR" | COVID$iso_code=="OWID_ASI" | 
                  COVID$iso_code == "OWID_EUN" | COVID$iso_code=="OWID_EUR" | 
                  COVID$iso_code=="OWID_INT" | COVID$iso_code=="OWID_HIC" | 
                  COVID$iso_code=="OWID_KOS" | COVID$iso_code=="OWID_LIC" | 
                  COVID$iso_code=="OWID_LMC" | COVID$iso_code=="OWID_NAM" | 
                  COVID$iso_code == "OWID_OCE" | COVID$iso_code=="OWID_SAM"| 
                  COVID$iso_code=="OWID_UMC" | COVID$iso_code=="OWID_WRL" | 
                  COVID$iso_code=="PRK" | COVID$iso_code=="OWID_ENG" |
                  COVID$iso_code=="OWID_NIR" | COVID$iso_code=="OWID_WLS"),]

#removing low population data
COVID <- COVID[ which(COVID$population >= 1000000), ]



options(scipen = 999)

#Week transformation (daily to weekly)
library(lubridate)
COVID$date2 <- as.Date(as.character(COVID$date),format="%Y-%m-%d")
COVID$date2
COVID$Week <- week(as.Date(as.character(COVID$date2),format="%Y-%m-%d"))
COVID$Week

x <- nrow(COVIDCon)
COVIDCon$Week2 <- COVIDCon$Week
for (i in 1:x) {
  if (COVIDCon$date[i] >= "2021-01-01")
    COVIDCon$Week2[i] = COVIDCon$Week[i]+55
}

for (i in 1:x) {
  if (COVIDCon$date[i] >= "2022-01-01")
    COVIDCon$Week2[i] = COVIDCon$Week[i]+108
  
}

print(COVIDCon$Week2)
summary(COVIDCon$Week2)


#Aggregate by weekly (CFR)

ConCFR <- aggregate(COVIDCon$CFR, by= list(COVIDCon$Week2, COVIDCon$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
ConCFR

colnames(ConCFR) <- c("Weeks", "Continent", "CFR")
ConCFR


#weekly line graph
a <- ggplot(ConCFR, aes(x=Weeks, CFR, color=Continent)) + xlab("Weeks") + ylab("Reported CFR (%)") + #+ggtitle("ARIMA Model")+
  
  # geom_line function is used to plot line plot
  geom_line(lwd = 1)+ theme(legend.position="bottom",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8))

a
#Aggregate by weekly (Vaccination)

ConVac <- aggregate(COVIDCon$total_vaccinations_per_hundred, by= list(COVIDCon$Week2, COVIDCon$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
ConVac

colnames(ConVac) <- c("Weeks", "Continent", "Vac")
ConVac


b <- ggplot(ConVac, aes(x=Weeks, Vac, color=Continent)) + xlab("Weeks") + ylab("Vaccination \n(doses/100 people)") + #+ggtitle("ARIMA Model")+
  
  # geom_line function is used to plot line plot
  geom_line(lwd = 1)+ theme(legend.position="bottom",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8))


b
#Save plot save pdf and tiff format
tiff("LineGraph.tiff", units="in", width=6, height=6, res=300)
g <- gridExtra::grid.arrange(b,a)
ggsave(file="LineGraph.pdf", g,width = 6, height = 6, dpi = 300, units = "in") #saves g
dev.off()

