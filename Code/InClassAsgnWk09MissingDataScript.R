#not functioning yet, continue at home

library(dplyr)
install.packages("plm")
library(plm)
install.packages("mice")
library(mice)

#Using instructor's finaldata for expediency
finaldata <- read.csv("Raw Data Area\\finaldata.csv", header = TRUE)

library(naniar)
finaldata |>
  arrange(year, ISO) |>
  dplyr::select(-country_name, -ISO, -region, -year) |>
  vis_miss()

preds <- as.formula(" ~ armconf1 + loggdp + OECD + pctpopdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought")

matmormod <- plm(update.formula(preds, matmor ~ .), index = c("ISO", "year"), effect = "twoways",
                 model = "within", data = finaldata)
un5mormod <- plm(update.formula(preds, un5mor ~ .), index = c("ISO", "year"), effect = "twoways",
                 model = "within", data = finaldata)
infmormod <- plm(update.formula(preds, infmor ~ .), index = c("ISO", "year"), effect = "twoways",
                 model = "within", data = finaldata)
neomormod <- plm(update.formula(preds, neomor ~ .), index = c("ISO", "year"), effect = "twoways",
                 model = "within", data = finaldata)

midata <- finaldata |>
  mutate(ISOnum = as.numeric(as.factor(finaldata$ISO))) |>
  select(-country_name, -ISO)

mice0  <- mice(midata, seed = 100, m = 5, maxit = 0, print = F)

meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall1000", "matmor", "infmor", "neomor", "un5mor", "loggdp", "pctpopdens")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall1000", "matmor", "infmor", "neomor", "un5mor", "loggdp", "pctpopdens"), "ISOnum"] <- -2

mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)

plot(mice.multi.out)

