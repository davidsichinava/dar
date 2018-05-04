library(ggplot2)

ggplot(mtcars, aes(x=mpg, y=wt))+
    geom_point()
ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point(aes(color=cut)) +
    geom_smooth(method="lm")

ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point(aes(color=cut)) +
  geom_smooth()+
  facet_wrap(~cut)

library(haven)

setwd("D:\\Dropbox\\My projects\\Courses\\ST_R\\website\\meetings\\m6\\pres\\data")

ndi <- read_dta("NDI_2017_Dec_25_12_17_public.dta")

names(ndi)

ggplot(ndi, aes(x=JOINEU))+
  geom_bar(aes(weight=WTIND, fill=factor(JOINEU)))+
  scale_fill_manual(values=c("#1b9e77", "#d95f02", "#7570b3", "#FF0000"))


