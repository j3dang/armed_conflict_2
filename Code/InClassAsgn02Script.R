library(tidyverse)
library(dplyr)
library(here)

rawdat <- read.csv("Raw Data Area\\maternalmortality.csv", header = TRUE)
write.csv(rawdat, file = "data\\maternalmortality.csv", row.names = FALSE)

