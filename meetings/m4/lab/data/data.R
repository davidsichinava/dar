setwd("D:\\Dropbox\\My Projects\\Courses\\ST_R\\website\\meetings\\m4\\lab\\data")
data <- read.csv('data.csv')
library(ggplot2)
ggplot(data, aes(q6))+
  geom_histogram()
