setwd("D:\\Dropbox\\My projects\\Courses\\ST_R\\website\\meetings\\m4\\class")
resume <- read.csv("resume.csv")
prop.table(table(resume$call, resume$race), 2)
0.06447639 - 0.09650924
