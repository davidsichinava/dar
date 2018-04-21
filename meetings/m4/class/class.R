setwd("D:\\Dropbox\\My projects\\Courses\\ST_R\\website\\meetings\\m4\\class")
resume <- read.csv("resume.csv")
prop.table(table(resume$call, resume$race), 2)
