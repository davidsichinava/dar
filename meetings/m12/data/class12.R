setwd("D:\\Dropbox\\My Projects\\Courses\\ST_R\\website\\meetings\\m12\\data")
library(haven)
library(ggplot2)
library(margins)

may17 <- read_dta("May17_Only_responses_10072013.dta")

names(may17)

table(may17$q7)

may17$homophobia <- 0
may17$homophobia[may17$q7==5] <- 1
table(may17$homophobia)

may17$hied <- 0
may17$hied[may17$d4>=5] <- 1
table(may17$hied)

homophobia.model <- glm(homophobia~hied, data=may17, family = "binomial")
summary(homophobia.model)
exp(homophobia.model$coefficients)

### უმაღლესი განათლების მქონე ადამიანის ჰომოფობობის ალბათობა:

exp(-0.6809+(-0.391)*1)/(1+exp(-0.6809+(-0.391)*1))

### უმაღლესი განათლების არმქონე ადამიანის ჰომოფობობის ალბათობა:
exp(-0.6809+(-0.391)*0)/(1+exp(-0.6809+(-0.391)*0))

View(cplot(homophobia.model, "hied"))


