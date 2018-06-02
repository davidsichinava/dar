library(haven)
library(ggplot2)
library(lmtest)

setwd("D:\\Dropbox\\My projects\\Courses\\ST_R\\website\\meetings\\m9\\class")

cb2017 <- read_dta("CB_2017_Georgia_10.11.17.dta")

cb2017$n1[cb2017$n1 < 1] <- NA
cb2017$n2[cb2017$n2 < 1] <- NA

cor(cb2017$n1, cb2017$n2, use="complete.obs")

ggplot(cb2017, aes(n1, n2))+
  geom_point()

happy.model <- lm(n2~n1, data=cb2017)
summary(happy.model)

mean(happy.model$residuals)
names(resid) <- c("residual")

par(mfrow=c(2, 2))

plot(happy.model)

bptest(happy.model)
