setwd("D:\\Dropbox\\My Projects\\Courses\\ST_R\\website\\meetings\\m13\\data")
library(haven)
library(ggplot2)
library(margins)

may17 <- read_dta("May17_Only_responses_10072013.dta")


##### მონაცემთა დამუშავება

names(may17)

table(may17$q7)

###### 

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

may17$homophobia <- factor(may17$homophobia, levels=c(0, 1))
may17$sex <- may17$gender
may17$sex[may17$sex==2] <- 0
may17$sex <- factor(may17$sex, levels=c(0, 1))
may17$hied <- 0
may17$hied[may17$d4>=5] <- 1
may17$hied <-factor(may17$hied, levels=c(0, 1))


homophobia.model2 <- glm(homophobia~sex+hied, data=may17, family="binomial")
summary(homophobia.model2)

sex.margins <- cplot(homophobia.model2, "sex")
View(sex.margins)


hied.margins <- cplot(homophobia.model2, "hied")
View(hied.margins)


homophobia.model2 <- glm(homophobia~sex*hied, data=may17, family="binomial")
summary(homophobia.model2)
sex.margins <- cplot(homophobia.model2, "sex")
education.margins <- cplot(homophobia.model2, "hied")

