library(ggplot2)
library(haven)

setwd("D:\\Dropbox\\My projects\\Courses\\ST_R\\website\\meetings\\m11\\data")

ndi <- read_dta("NDI_2018_Mar_16.04.18_Public.dta")

#### პირველი საფეხური

ndi[] <- lapply(ndi, unclass)

recodeResilience <- function (x) {
  x <-na_if(x, -9)
  x <-na_if(x, -7)
  x <-na_if(x, -3)
  x <-na_if(x, -2)
  x <- dplyr::recode(x, `-1`=4, `1`=1, `2` = 2, `3` = 3, `4`=5, `5`=6, `6`=7)
}

varNames <- names(ndi[, 139:151])

ndi[, varNames] <- sapply(ndi[, varNames], recodeResilience)

q32factor <- factanal(na.exclude(ndi[, varNames]), 
                      3, rotation="varimax", scores="regression")

ndi_analysis <- ndi[, na.exclude(ndi[, 139:151])]

print(q32factor, digits=2, cutoff=0.4, sort=TRUE)

scores <- as.data.frame(q32factor$scores)

ndi <- cbind(ndi, scores)

## Party ID

ndi$party <- 0
ndi$party[ndi$PARTYSUPP1 == 8] <- 1 # GD
ndi$party[ndi$PARTYSUPP1 == 6] <- 2 # UNM
ndi$party[ndi$PARTYSUPP1 == 26] <- 3 # No Party
ndi$party[ndi$PARTYSUPP1 == -1 | ndi$PARTYSUPP1 == -2] <- 4 # DK/RA

## regression model predicting social cohesion
  
f1_model <- lm(Factor1~factor(party)+factor(RESPSEX)+factor(SETTYPE)+RESPAGE, data=ndi)
summary(f1_model)

## regression model predicting institutional trust in times of crisis
f2_model <- lm(Factor2~factor(party)+factor(RESPSEX)+factor(SETTYPE)+RESPAGE, data=ndi)
summary(f2_model)

## regression model predicting generalized institutional trust 
f3_model <- lm(Factor3~factor(party)+factor(RESPSEX)+factor(SETTYPE)+RESPAGE, data=ndi)
summary(f3_model)











print(resilienceFactor, digits=2, sort=TRUE)

print(resilienceFactor, digits=2, cutoff=0.4, sort=TRUE)

ndi[complete.cases()]


cb <- read_dta("CB_2017_Georgia_10.11.17.dta")



trucomp <- prcomp(cb[, 96:110], scale=TRUE)
trucomp$sdev
trucomp$center
trucomp$scale
trucomp$sdev^2 / sum(trucomp$sdev^2)





