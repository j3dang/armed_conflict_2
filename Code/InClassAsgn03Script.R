#CONFLICT DATA IS LAGGED BACK 1 YEAR RELATIVE TO MORTALITY
library(glue)
library(dplyr)

maternalMortality <- read.csv("Raw Data Area\\maternalmortality.csv", header = TRUE)
infantMortality <- read.csv("Raw Data Area\\infantmortality.csv", header = TRUE)
neonatalMortality <- read.csv("Raw Data Area\\neonatalmortality.csv", header = TRUE)
under5Mortality <- read.csv("Raw Data Area\\under5mortality.csv", header = TRUE)

conflict <- read.csv("Raw Data Area\\conflictdata.csv", header = TRUE)
covariates <- read.csv("Raw Data Area\\covariates.csv", header = TRUE)

readData <- function(x) {
  selectedCols <<- select(x, Country.Name, X2000:X2019)
  #Use <<- to save intermediate datasets into work environment for viewing
  pivoted <<- pivot_longer(selectedCols, 
                           cols = c(X2000:X2019),
                           names_to = "Year",
                           values_to = "X",
                           names_prefix = "X")
}

readData(maternalMortality)
maternalmortality_processed <- pivoted

readData(infantMortality)
infantmortality_processed <- pivoted

readData(neonatalMortality)
neonatalmortality_processed <- pivoted

readData(under5Mortality)
under5mortality_processed <- pivoted

#This will error out - ignore the warning, the function will give missing ISO codes, those regions will be removed in any case

# Rename the 'X' variable to indicate the type of mortality
maternalmortality_processed <- maternalmortality_processed %>%
  rename(MaternalMortality = X)

infantmortality_processed <- infantmortality_processed %>%
  rename(InfantMortality = X)

neonatalmortality_processed <- neonatalmortality_processed %>%
  rename(NeonatalMortality = X)

under5mortality_processed <- under5mortality_processed %>%
  rename(Under5Mortality = X)


merged_mortality <- maternalmortality_processed %>%
  full_join(infantmortality_processed, by = c("Country.Name", "Year")) %>%
  full_join(neonatalmortality_processed, by = c("Country.Name", "Year")) %>%
  full_join(under5mortality_processed, by = c("Country.Name", "Year"))

# Convert Year to integer
merged_mortality <- merged_mortality %>%
  mutate(Year = as.integer(Year))


library(countrycode)
merged_mortality$ISO <- countrycode(merged_mortality$Country.Name,
                            origin = "country.name",
                            destination = "iso3c")

#Use Disaster code
consolidated_conflict <- conflict %>%
  group_by(year, ISO) %>%
  summarise(deaths = sum(best, na.rm = TRUE))

consolidated_conflict <- consolidated_conflict %>%
  mutate(Year = year - 1) %>%
  select(-year) # Remove the original year column

covariates <- covariates %>%
  mutate(Year = as.integer(year))

analyticalset <- merged_mortality %>%
  full_join(disasters, by = c("ISO", "Year")) %>%
  full_join(consolidated_conflict, by = c("ISO","Year")) %>%
  full_join(covariates, by = c("ISO", "Year"))

usethis::use_github()
usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)