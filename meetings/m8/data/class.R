setwd("D:\\Dropbox\\My projects\\Courses\\ST_R\\website\\meetings\\m8\\data")

library(ggplot2)

face <- read.csv("face.csv")

face$d.share <- face$d.votes / (face$d.votes + face$r.votes)

face$r.share <- face$r.votes / (face$d.votes + face$r.votes)

face$diff.share <- face$d.share - face$r.share

fit <- lm(diff.share~d.comp, data=face)
fit
summary(fit)

ggplot(face, aes(x=d.comp, y=diff.share))+
  geom_point()+
  geom_smooth(method="lm")

library(haven)
cb <- read_dta("CB_2017_Georgia_17.11.17.dta")

cb$j14[cb$j14 == -3 | cb$j14 == -2 | cb$j14 == -1] <- NA
cb$p4_02[cb$p4_02==-3 ] <- NA
cb$p4_02[cb$p4_02==-2 |cb$p4_02==-1  ] <-3

trubank <- lm(p4_02~j14, data=cb)
summary(trubank)

