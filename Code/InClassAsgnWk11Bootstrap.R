#fix pct density, continue at home

library(dplyr)

#Using instructor's finaldata for expediency
finaldata <- read.csv("Raw Data Area\\finaldata.csv", header = TRUE)

#Slide 3
data2017 <- finaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(matmor)) 

#Slide 5
data2017 |>
  group_by(armconf1) |>
  summarise(n = n(),
            median.matmor = median(matmor, na.rm = T))

#####Slide 6

obs.med.diff <- median(data2017[data2017$armconf1 == 1,]$matmor) -
  median(data2017[data2017$armconf1 == 0,]$matmor)
obs.med.diff


matmor.arm1 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(matmor) & armconf1 == 1) |>
  dplyr::select(ISO, matmor)
matmor.arm0 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(matmor) & armconf1 == 0) |>
  dplyr::select(ISO, matmor)

set.seed(2024)
B <- 1000
med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.arm1 <- matmor.arm1[sample(nrow(matmor.arm1), size = nrow(matmor.arm1), replace = TRUE),]
  resamp.arm0 <- matmor.arm0[sample(nrow(matmor.arm0), size = nrow(matmor.arm0), replace = TRUE),]
  med.diff[b] <- median(resamp.arm1$matmor) - median(resamp.arm0$matmor)
}
head(resamp.arm1, 12)

##### Slide 6 ends

#Slide 7
hist(med.diff, main = "Distribution of bootstrap statistic")


#####Slide 8
library(boot)

getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$matmor, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout <- boot(data2017, statistic = getmeddiff, strata = data2017$armconf1, R = 1000)
bootout
#####Slide 8 end

###Slide 9
bootout$t0

sd(bootout$t)
###Slide 9 end

#Slide 11
quantile(bootout$t, probs = c(0.025, 0.975))



#Slide 12
2 * bootout$t0 - quantile(bootout$t, probs = 0.975)

2 * bootout$t0 - quantile(bootout$t, probs = 0.025)


#Slide 15
boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))


###############################################################################
###############################################################################
############################INFMOR#############################################
###############################################################################
###############################################################################


#Slide 3
data2017 <- finaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(infmor)) 

#Slide 5
data2017 |>
  group_by(armconf1) |>
  summarise(n = n(),
            median.infmor = median(infmor, na.rm = T))

#####Slide 6

obs.med.diff <- median(data2017[data2017$armconf1 == 1,]$infmor) -
  median(data2017[data2017$armconf1 == 0,]$infmor)
obs.med.diff


infmor.arm1 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(infmor) & armconf1 == 1) |>
  dplyr::select(ISO, infmor)
infmor.arm0 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(infmor) & armconf1 == 0) |>
  dplyr::select(ISO, infmor)

set.seed(2024)
B <- 1000
med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.arm1 <- infmor.arm1[sample(nrow(infmor.arm1), size = nrow(infmor.arm1), replace = TRUE),]
  resamp.arm0 <- infmor.arm0[sample(nrow(infmor.arm0), size = nrow(infmor.arm0), replace = TRUE),]
  med.diff[b] <- median(resamp.arm1$infmor) - median(resamp.arm0$infmor)
}
head(resamp.arm1, 12)

##### Slide 6 ends

#Slide 7
hist(med.diff, main = "Distribution of bootstrap statistic")


#####Slide 8
library(boot)

getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$infmor, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout <- boot(data2017, statistic = getmeddiff, strata = data2017$armconf1, R = 1000)
bootout
#####Slide 8 end

###Slide 9
bootout$t0

sd(bootout$t)
###Slide 9 end

#Slide 11
quantile(bootout$t, probs = c(0.025, 0.975))



#Slide 12
2 * bootout$t0 - quantile(bootout$t, probs = 0.975)

2 * bootout$t0 - quantile(bootout$t, probs = 0.025)


#Slide 15
boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))


###############################################################################
###############################################################################
############################un5mor#############################################
###############################################################################
###############################################################################


#Slide 3
data2017 <- finaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(un5mor)) 

#Slide 5
data2017 |>
  group_by(armconf1) |>
  summarise(n = n(),
            median.un5mor = median(un5mor, na.rm = T))

#####Slide 6

obs.med.diff <- median(data2017[data2017$armconf1 == 1,]$un5mor) -
  median(data2017[data2017$armconf1 == 0,]$un5mor)
obs.med.diff


un5mor.arm1 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(un5mor) & armconf1 == 1) |>
  dplyr::select(ISO, un5mor)
un5mor.arm0 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(un5mor) & armconf1 == 0) |>
  dplyr::select(ISO, un5mor)

set.seed(2024)
B <- 1000
med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.arm1 <- un5mor.arm1[sample(nrow(un5mor.arm1), size = nrow(un5mor.arm1), replace = TRUE),]
  resamp.arm0 <- un5mor.arm0[sample(nrow(un5mor.arm0), size = nrow(un5mor.arm0), replace = TRUE),]
  med.diff[b] <- median(resamp.arm1$un5mor) - median(resamp.arm0$un5mor)
}
head(resamp.arm1, 12)

##### Slide 6 ends

#Slide 7
hist(med.diff, main = "Distribution of bootstrap statistic")


#####Slide 8
library(boot)

getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$un5mor, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout <- boot(data2017, statistic = getmeddiff, strata = data2017$armconf1, R = 1000)
bootout
#####Slide 8 end

###Slide 9
bootout$t0

sd(bootout$t)
###Slide 9 end

#Slide 11
quantile(bootout$t, probs = c(0.025, 0.975))



#Slide 12
2 * bootout$t0 - quantile(bootout$t, probs = 0.975)

2 * bootout$t0 - quantile(bootout$t, probs = 0.025)


#Slide 15
boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))


###############################################################################
###############################################################################
############################neomor#############################################
###############################################################################
###############################################################################


#Slide 3
data2017 <- finaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(neomor)) 

#Slide 5
data2017 |>
  group_by(armconf1) |>
  summarise(n = n(),
            median.neomor = median(neomor, na.rm = T))

#####Slide 6

obs.med.diff <- median(data2017[data2017$armconf1 == 1,]$neomor) -
  median(data2017[data2017$armconf1 == 0,]$neomor)
obs.med.diff


neomor.arm1 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(neomor) & armconf1 == 1) |>
  dplyr::select(ISO, neomor)
neomor.arm0 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(neomor) & armconf1 == 0) |>
  dplyr::select(ISO, neomor)

set.seed(2024)
B <- 1000
med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.arm1 <- neomor.arm1[sample(nrow(neomor.arm1), size = nrow(neomor.arm1), replace = TRUE),]
  resamp.arm0 <- neomor.arm0[sample(nrow(neomor.arm0), size = nrow(neomor.arm0), replace = TRUE),]
  med.diff[b] <- median(resamp.arm1$neomor) - median(resamp.arm0$neomor)
}
head(resamp.arm1, 12)

##### Slide 6 ends

#Slide 7
hist(med.diff, main = "Distribution of bootstrap statistic")


#####Slide 8
library(boot)

getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$neomor, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout <- boot(data2017, statistic = getmeddiff, strata = data2017$armconf1, R = 1000)
bootout
#####Slide 8 end

###Slide 9
bootout$t0

sd(bootout$t)
###Slide 9 end

#Slide 11
quantile(bootout$t, probs = c(0.025, 0.975))



#Slide 12
2 * bootout$t0 - quantile(bootout$t, probs = 0.975)

2 * bootout$t0 - quantile(bootout$t, probs = 0.025)


#Slide 15
boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))

