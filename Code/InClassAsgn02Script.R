library(tidyverse)
library(dplyr)
library(here)
library(usethis) 
library(gitcreds)

rawdat <- read.csv("Raw Data Area\\maternalmortality.csv", header = TRUE)
write.csv(rawdat, file = "data\\maternalmortality.csv", row.names = FALSE)

install.packages("usethis")

selectedCols <- select(rawdat, Country.Name, X2000, X2001, X2002, X2003, X2004, X2005, X2006, X2007, X2008, X2009, X2010, X2011, X2012, X2013, X2014, X2015, X2016, X2017, X2018, X2019)

pivoted <- pivot_longer(selectedCols, 
             cols = c(X2000, X2001, X2002, X2003, X2004, X2005, X2006, X2007, X2008, X2009, X2010, X2011, X2012, X2013, X2014, X2015, X2016, X2017, X2018, X2019),
             names_to = "Year",
             values_to = "MatMor",
             names_prefix = "X")

saveRDS(pivoted, "data/pivotedMatData.rds")

