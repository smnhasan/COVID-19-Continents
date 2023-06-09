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

##Descriptive
#Table 1

#Set directory
setwd('E:\\Najmul Bhai\\Continent')

#Reading data
COVID <- read.csv("owid-covid-data.csv")
COVID$iso_code <- COVID$�..iso_code
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

#Creating CFR
COVID$CFR <- (COVID$total_deaths/COVID$total_cases)*100

####2022

#select date
COVID2022 <- subset(COVID, COVID$date == "2023-04-30") 

##Select Asia
COVID2022asia <- subset(COVID2022, COVID2022$continent == "Asia") 

#mean of CFR
mean(COVID2022asia$CFR, na.rm = T)

#standard deviation of CFR
op <- std.error(COVID2022asia$CFR)
op

#mean of vaccination
mean(COVID2022asia$total_vaccinations_per_hundred, na.rm = T)

#standard deviation of vaccination
op <- std.error(COVID2022asia$total_vaccinations_per_hundred)
op


##Africa
COVID2022africa <- subset(COVID2022, COVID2022$continent == "Africa") #5/29/2021 2/24/2020

mean(COVID2022africa$CFR, na.rm = T)
op <- std.error(COVID2022africa$CFR)
op

mean(COVID2022africa$total_vaccinations_per_hundred, na.rm = T)
op <- std.error(COVID2022africa$total_vaccinations_per_hundred)
op

##Europe
COVID2022europe <- subset(COVID2022, COVID2022$continent == "Europe") #5/29/2021 2/24/2020

mean(COVID2022europe$CFR, na.rm = T)
op <- std.error(COVID2022europe$CFR)
op

mean(COVID2022europe$total_vaccinations_per_hundred, na.rm = T)
op <- std.error(COVID2022europe$total_vaccinations_per_hundred)
op

##North America
COVID2022namerica <- subset(COVID2022, COVID2022$continent == "North America") #5/29/2021 2/24/2020

mean(COVID2022namerica$CFR, na.rm = T)
op <- std.error(COVID2022namerica$CFR)
op

mean(COVID2022namerica$total_vaccinations_per_hundred, na.rm = T)
op <- std.error(COVID2022namerica$total_vaccinations_per_hundred)
op

##South America
COVID2022samerica <- subset(COVID2022, COVID2022$continent == "South America") #5/29/2021 2/24/2020

mean(COVID2022samerica$CFR)
op <- std.error(COVID2022samerica$CFR)
op

mean(COVID2022samerica$total_vaccinations_per_hundred)
op <- std.error(COVID2022samerica$total_vaccinations_per_hundred)
op

##Oceania
COVID2022oceania <- subset(COVID2022, COVID2022$continent == "Oceania") #5/29/2021 2/24/2020

mean(COVID2022oceania$CFR, na.rm = T)
op <- std.error(COVID2022oceania$CFR)
op

mean(COVID2022oceania$total_vaccinations_per_hundred, na.rm = T)
op <- std.error(COVID2022oceania$total_vaccinations_per_hundred)
op

####2021

COVID2021 <- subset(COVID, COVID$date == "2021-01-05") #5/29/2021 2/24/2020

##Asia
COVID2021asia <- subset(COVID2021, COVID2021$continent == "Asia") #5/29/2021 2/24/2020

mean(COVID2021asia$CFR, na.rm = T)
op <- std.error(COVID2021asia$CFR)
op

##Africa
COVID2021africa <- subset(COVID2021, COVID2021$continent == "Africa") #5/29/2021 2/24/2020
mean(COVID2021africa$CFR, na.rm = T)
op <- std.error(COVID2021africa$CFR)
op

##Europe
COVID2021europe <- subset(COVID2021, COVID2021$continent == "Europe") #5/29/2021 2/24/2020
mean(COVID2021europe$CFR, na.rm = T)
op <- std.error(COVID2021europe$CFR)
op

##North America
COVID2021namerica <- subset(COVID2021, COVID2021$continent == "North America") #5/29/2021 2/24/2020

mean(COVID2021namerica$CFR, na.rm = T)
op <- std.error(COVID2021namerica$CFR)
op

##South America
COVID2021samerica <- subset(COVID2021, COVID2021$continent == "South America") #5/29/2021 2/24/2020

mean(COVID2021samerica$CFR, na.rm = T)
op <- std.error(COVID2021samerica$CFR)
op

##Oceania
COVID2021oceania <- subset(COVID2021, COVID2021$continent == "Oceania") #5/29/2021 2/24/2020

mean(COVID2021oceania$CFR, na.rm = T)
op <- std.error(COVID2021oceania$CFR)
op

#Mean difference between 2021 information and 2022 information
t.test(COVID2021asia$CFR[1:44], COVID2022asia$CFR , paired = TRUE, alternative = "two.sided")

t.test(COVID2021africa$CFR, COVID2022africa$CFR , paired = TRUE, alternative = "two.sided")

t.test(COVID2021europe$CFR, COVID2022europe$CFR , paired = TRUE, alternative = "two.sided")

t.test(COVID2021namerica$CFR, COVID2022namerica$CFR , paired = TRUE, alternative = "two.sided")

t.test(COVID2021samerica$CFR, COVID2022samerica$CFR , paired = TRUE, alternative = "two.sided")

t.test(COVID2021oceania$CFR, COVID2022oceania$CFR , paired = TRUE, alternative = "two.sided")


#Line graph
#figure 1

#Reading data
COVID <- read.csv("owid-covid-data.csv")

COVID$iso_code <- COVID$�..iso_code

#Consider only upto 2022 data
COVID <- subset(COVID, COVID$date < "2023-05-01") 


#Select only continental data
COVIDCon <-COVID[(COVID$location=="Africa" | COVID$location=="Asia" | 
                   COVID$location=="Europe" | COVID$location=="Oceania" | 
                    COVID$location=="South America" | COVID$location == "North America"),]


#Creating CFR
COVIDCon$CFR <- (COVIDCon$total_deaths/COVIDCon$total_cases)*100
COVIDCon$CFR


options(scipen = 999)

#Week transformation (daily to weekly)
library(lubridate)
COVIDCon$date2 <- as.Date(as.character(COVIDCon$date),format="%Y-%m-%d")
COVIDCon$date2
COVIDCon$Week <- week(as.Date(as.character(COVIDCon$date2),format="%Y-%m-%d"))
COVIDCon$Week

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

for (i in 1:x) {
  if (COVIDCon$date[i] >= "2023-01-01")
    COVIDCon$Week2[i] = COVIDCon$Week[i]+161
  
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

View(ConVac)

#Save plot save pdf and tiff format
tiff("LineGraph.tiff", units="in", width=6, height=6, res=300)
g <- gridExtra::grid.arrange(b,a)
ggsave(file="LineGraph.pdf", g,width = 6, height = 6, dpi = 300, units = "in") #saves g
dev.off()


#Map continentwise 
#Figure 2

#Mean of CFR by continents for map
COVID2022$COVID2022CFRcon <- NA
COVID2022$COVID2022CFRcon[COVID2022$continent=="Asia"] <- mean(COVID2022$CFR[COVID2022$continent=="Asia"], na.rm = T)
COVID2022$COVID2022CFRcon[COVID2022$continent=="Africa"] <- mean(COVID2022$CFR[COVID2022$continent=="Africa"], na.rm = T)
COVID2022$COVID2022CFRcon[COVID2022$continent=="Europe"] <- mean(COVID2022$CFR[COVID2022$continent=="Europe"], na.rm = T)
COVID2022$COVID2022CFRcon[COVID2022$continent=="North America"] <- mean(COVID2022$CFR[COVID2022$continent=="North America"], na.rm = T)
COVID2022$COVID2022CFRcon[COVID2022$continent=="South America"] <- mean(COVID2022$CFR[COVID2022$continent=="South America"], na.rm = T)
COVID2022$COVID2022CFRcon[COVID2022$continent=="Oceania"] <- mean(COVID2022$CFR[COVID2022$continent=="Oceania"], na.rm = T)
COVID2022$COVID2022CFRcon


#Mean of vaccination by continents for map
COVID2022$COVID2022VACcon <- NA
COVID2022$COVID2022VACcon[COVID2022$continent=="Asia"] <- mean(COVID2022$total_vaccinations_per_hundred[COVID2022$continent=="Asia"], na.rm = T)
COVID2022$COVID2022VACcon[COVID2022$continent=="Africa"] <- mean(COVID2022$total_vaccinations_per_hundred[COVID2022$continent=="Africa"], na.rm = T)
COVID2022$COVID2022VACcon[COVID2022$continent=="Europe"] <- mean(COVID2022$total_vaccinations_per_hundred[COVID2022$continent=="Europe"], na.rm = T)
COVID2022$COVID2022VACcon[COVID2022$continent=="North America"] <- mean(COVID2022$total_vaccinations_per_hundred[COVID2022$continent=="North America"], na.rm = T)
COVID2022$COVID2022VACcon[COVID2022$continent=="South America"] <- mean(COVID2022$total_vaccinations_per_hundred[COVID2022$continent=="South America"], na.rm = T)
COVID2022$COVID2022VACcon[COVID2022$continent=="Oceania"] <- mean(COVID2022$total_vaccinations_per_hundred[COVID2022$continent=="Oceania"], na.rm = T)
COVID2022$COVID2022VACcon


####MAP Vaccination

library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log
COVID2022$vaclog <- log10(COVID2022$COVID2022VACcon)+1

worldgovt <- dplyr::select(COVID2022, region = location, vaclog = vaclog, "CC" =  iso_code)
head(worldgovt)


## Make the Vaccination numeric
worldgovt$VAC <- as.numeric(as.character(worldgovt$vaclog))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldVAC <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = VAC)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("") + labs(fill = "Vaccination \n(doses/100 people)") +
  plain
y <- plot(worldVAC)
y

####MAP CFR

library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")

worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log

COVID2022$cfrlog <- log10(COVID2022$COVID2022CFRcon ) +1

worldgovt <- dplyr::select(COVID2022, region = location, cfrlog = cfrlog, "CC" =  iso_code)
head(worldgovt)

## Make the CFR numeric
worldgovt$CFR <- as.numeric(as.character(worldgovt$cfrlog))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldCFR <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = CFR)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("") + labs(fill = "Reported CFR (%)") +
  plain
z <- plot(worldCFR)
z


tiff("Map.tiff", units="in", width=6, height=6, res=300)
g <- gridExtra::grid.arrange(y,z)
ggsave(file="Map.pdf", g,width = 6, height = 6, dpi = 300, units = "in") #saves g

dev.off()


############Associated Factors selection by GLMM 
setwd('E:\\Najmul Bhai\\Continent')
COVID <- read.csv("owid-covid-dataNew.csv")
COVID <- subset(COVID, COVID$date < "2023-05-01") 


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

COVID <- COVID[ which(COVID$population >= 1000000), ]
NROW(COVID)

#Reading more information and merge with main files
GHSI <- read.csv("GHSI.csv")
GHSI[GHSI == 0] <- NA

COVID <- merge(COVID, GHSI, by=c("location"))

WGI <- read.csv("WGI.csv")
WGI[WGI == 0] <- NA

COVID <- merge(COVID, WGI, by=c("location"))

Obesity <- read.csv("Obesity.csv")
Obesity[Obesity == 0] <- NA

COVID <- merge(COVID, Obesity, by=c("location"))


#Creating CFR
COVID$CFR <- (COVID$total_deaths/COVID$total_cases)
COVID$CFR


#Week transformation (daily to weekly)
library(lubridate)
COVID$date2 <- as.Date(as.character(COVID$date),format="%Y-%m-%d")
COVID$date2
COVID$Week <- week(as.Date(as.character(COVID$date2),format="%Y-%m-%d"))
COVID$Week

x <- nrow(COVID)
COVID$Week2 <- COVID$Week
for (i in 1:x) {
  if (COVID$date[i] >= "2021-01-01")
    COVID$Week2[i] = COVID$Week[i]+55
}

for (i in 1:x) {
  if (COVID$date[i] >= "2022-01-01")
    COVID$Week2[i] = COVID$Week[i]+108
  
}

for (i in 1:x) {
  if (COVID$date[i] >= "2023-01-01")
    COVID$Week2[i] = COVID$Week[i]+161
  
}

print(COVID$Week2)
summary(COVID$Week2)

#Aggregate by weekly all selected variables for model

Con_CFR <- aggregate(COVID$CFR, by= list(COVID$Week2, COVID$continent), FUN=mean, na.rm=TRUE)
Con_CFR

colnames(Con_CFR) <- c("Weeks", "Continents", "CFR")
Con_CFR

Con_Vac <- aggregate(COVID$total_vaccinations_per_hundred, by= list(COVID$Week2, COVID$continent), FUN=mean, na.rm=TRUE) 
Con_Vac

colnames(Con_Vac) <- c("Weeks", "Continents", "Vaccinationph")
Con_Vac

Con_aged_65_older <- aggregate(COVID$aged_65_older, by= list(COVID$Week2, COVID$continent), FUN=mean, na.rm=TRUE) 
Con_aged_65_older

colnames(Con_aged_65_older) <- c("Weeks", "Continents", "Age65Older")
Con_aged_65_older

Con_population_density <- aggregate(COVID$population_density, by= list(COVID$Week2, COVID$continent), FUN=mean, na.rm=TRUE) 
Con_population_density

colnames(Con_population_density) <- c("Weeks", "Continents", "PopulationDensity")
Con_population_density

Con_total_tests_per_thousand <- aggregate(COVID$total_tests_per_thousand, by= list(COVID$Week2, COVID$continent), FUN=mean, na.rm=TRUE) 
Con_total_tests_per_thousand

colnames(Con_total_tests_per_thousand) <- c("Weeks", "Continents", "TotalTestpt")
Con_total_tests_per_thousand


Con_gdp_per_capita <- aggregate(COVID$gdp_per_capita, by= list(COVID$Week2, COVID$continent), FUN=mean, na.rm=TRUE) 
Con_gdp_per_capita

colnames(Con_gdp_per_capita) <- c("Weeks", "Continents", "GDP")
Con_gdp_per_capita

Con_SI <- aggregate(COVID$stringency_index, by= list(COVID$Week2, COVID$continent), FUN=mean, na.rm=TRUE) 
Con_SI

colnames(Con_SI) <- c("Weeks", "Continents", "SI")
Con_SI

Con_GHSI <- aggregate(COVID$GHSI, by= list(COVID$Week2, COVID$continent), FUN=mean, na.rm=TRUE) 
Con_GHSI

colnames(Con_GHSI) <- c("Weeks", "Continents", "GHSI")
Con_GHSI

Con_WGI <- aggregate(COVID$WGI, by= list(COVID$Week2, COVID$continent), FUN=mean, na.rm=TRUE) 
Con_WGI

colnames(Con_WGI) <- c("Weeks", "Continents", "WGI")
Con_WGI

Con_ob <- aggregate(COVID$Obesity_rate, by= list(COVID$Week2, COVID$continent), FUN=mean, na.rm=TRUE)
Con_ob

colnames(Con_ob) <- c("Weeks", "Continents", "Obesity")
Con_ob


#Merge all information

ModelData1 <- merge(Con_CFR, Con_Vac, by=c("Continents", "Weeks"))

ModelData2 <- merge(ModelData1, Con_aged_65_older, by=c("Continents", "Weeks"))

ModelData3 <- merge(ModelData2, Con_population_density, by=c("Continents", "Weeks"))

ModelData4 <- merge(ModelData3, Con_total_tests_per_thousand, by=c("Continents", "Weeks"))

ModelData5 <- merge(ModelData4, Con_gdp_per_capita, by=c("Continents", "Weeks"))

ModelData6 <- merge(ModelData5, Con_SI, by=c("Continents", "Weeks"))

ModelData7 <- merge(ModelData6, Con_GHSI, by=c("Continents", "Weeks"))

ModelData8 <- merge(ModelData7, Con_WGI, by=c("Continents", "Weeks"))

ModelData <- merge(ModelData8, Con_ob, by=c("Continents", "Weeks"))

ModelData$Continents <- factor(ModelData$Continents)

summary(ModelData$CFR)

ModelData$PopulationDensity <-ModelData$PopulationDensity 

#GLM model
fit <- glmmTMB(CFR ~ relevel(Continents, ref = "Asia") + Vaccinationph +  TotalTestpt + SI + Weeks, 
               na.action=na.omit,  data = ModelData)

# ModelData$CFR[ModelData$CFR >=1] <- NA
# + GDP+ GHSI  +  WGI  + Obesity +  PopulationDensity
 
    

library(car)
check_collinearity(fit)

#check estimates and P-values
summary(fit)

#check Odds ratio

round(exp(confint(fit)),6)





