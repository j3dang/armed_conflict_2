library(tidyverse)
library(dplyr)
library(here)
library(usethis) 
library(gitcreds)

rawdat <- read.csv("Raw Data Area\\disaster.csv", header = TRUE)

colnames(rawdat)
str(rawdat)

sum(rawdat$Year >= 2000 & rawdat$Year <= 2019)

filtered <- rawdat %>%
  filter(Year >= 2000 & Year <= 2019 & Disaster.Type %in% c('Earthquake','Drought'))

subsetted <- filtered[,c("Year", "ISO", "Disaster.Type")]

subsetted$drought <- ifelse(subsetted$Disaster.Type == 'Drought', 1, 0)
subsetted$earthquake <- ifelse(subsetted$Disaster.Type == 'Earthquake', 1, 0)

disasters <- subsetted %>%
  group_by(Year, ISO) %>%         
  summarize(
    earthquake = max(earthquake),        
    drought = max(drought),     
  )

#Github legacy code for training and templating
#usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)
#usethis::use_github()


