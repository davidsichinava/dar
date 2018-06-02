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

resilienceFactor <- factanal(na.exclude(ndi[, varNames]), 
                      3, rotation="varimax", scores="regression")

scores <- as.data.frame(resilienceFactor$scores)

print(resilienceFactor, digits=2, sort=TRUE)

print(resilienceFactor, digits=2, cutoff=0.4, sort=TRUE)

cb <- read_dta("CB_2017_Georgia_10.11.17.dta")



trucomp <- prcomp(cb[, 96:110], scale=TRUE)
trucomp$sdev
trucomp$center
trucomp$scale
trucomp$sdev^2 / sum(trucomp$sdev^2)





