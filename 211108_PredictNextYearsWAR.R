# Code from meeting on November 8, 2021
# We used data from FanGraphs to explore what factors might help us predict
#   a player's WAR in the following season.

# Load packages
#   Use install.packages() if you haven't previously installed these

library(readr)
library(dplyr)
library(ggplot2)


# Read data from GitHub for 2021 and 2020 and add year column

Data21 <- read.csv("https://raw.githubusercontent.com/vank-stats/Baseball-Analytics/master/Data/FG_Statcast_Batting2021.csv")
Data21$Year <- 2021

Data20 <- read.csv("https://raw.githubusercontent.com/vank-stats/Baseball-Analytics/master/Data/FG_Statcast_Batting2020.csv")
Data20$Year <- 2020


# Create variable called WAR21 and set to missing
# Then go through each playerid in the 2020 data. If that ID also shows up in 
#   2021 data, put WAR from 2021 in WAR21 variable

Data20$WAR21 <- NA

for(i in Data20$playerid) {
  if(i %in% Data21$playerid) {
    Data20[Data20$playerid == i,]$WAR21 <- Data21[Data21$playerid == i,]$WAR
  }
}


# Percentage variables (e.g. HardHit%) are being read as characters
#   Create new numeric versions of each of these then remove the old version

Data20 <- Data20 %>%
  mutate(BarrelPct = as.numeric(gsub("%", "", `Barrel%`)),
         HardHitPct = as.numeric(gsub("%", "", `HardHit%`)),
         KPct = as.numeric(gsub("%", "", `K%`)),
         BBPct = as.numeric(gsub("%", "", `BB%`))) %>%
  select(-`Barrel%`, -`HardHit%`, -`K%`, -`BB%`)


# Remove players with missing values for WAR21 (players only in 2020 data)

Data20 <- filter(Data20, WAR21 > -100)


# Explore correlations between variables

cor(Data20)


# Create a linear regression model using Barrels and 2020 WAR to predict
#   WAR in 2021

model1 <- lm(WAR21 ~ Barrels + WAR, data = Data20)
summary(model1)


# Add predicted WAR in 2021 as a column in our data

Data20$xWAR21 <- predict(model1, Data20)


# Graph each players predicted WAR in 2021 against their actual WAR in 2021

ggplot(Data20, aes(x = xWAR21, y = WAR21)) + 
  geom_point() + 
  geom_smooth(method = 'lm')