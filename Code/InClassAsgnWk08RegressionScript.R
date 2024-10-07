#fix pct density, continue at home

library(dplyr)

#Using instructor's finaldata for expediency
finaldata <- read.csv("Raw Data Area\\finaldata.csv", header = TRUE)


#removed pctpopdens and replaced with popdens
lmmod <- lm(matmor ~ -1 + armconf1 + gdp1000 + OECD + popdens + urban +
              agedep + male_edu + temp + rainfall1000 + earthquake + drought +
              ISO, 
            data = finaldata)

#removed pctpopdens and replaced with popdens
library(plm)
plmmod <- plm(matmor ~ armconf1 + gdp1000 + OECD + popdens + urban +
                agedep + male_edu + temp + rainfall1000 + earthquake + drought,
              index = c("ISO"), model = "within", data = finaldata)


lmmod <- lm(matmor ~ -1 + armconf1 + gdp1000 + OECD + popdens + urban +
              agedep + male_edu + temp + rainfall1000 + earthquake + drought +
              ISO + as.factor(year), 
            data = finaldata)

plmmod <- plm(matmor ~ armconf1 + gdp1000 + OECD + popdens + urban + 
                agedep + male_edu + temp + rainfall1000 + earthquake + drought,
              index = c("ISO", "year"),
              effect = "twoways",
              model = "within",
              data = finaldata)



preds <- as.formula(" ~ armconf1 + gdp1000 + OECD + popdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                  ISO + as.factor(year)")

matmormod <- lm(update.formula(preds, matmor ~ .), data = finaldata)
un5mormod <- lm(update.formula(preds, un5mor ~ .), data = finaldata)
infmormod <- lm(update.formula(preds, infmor ~ .), data = finaldata)
neomormod <- lm(update.formula(preds, neomor ~ .), data = finaldata)