setwd("D:\\Dropbox\\My projects\\Courses\\ST_R\\website\\meetings\\m7\\data")
library(ggplot2)
library(tidyr)
congress <- read.csv("congress.csv")
rep <- subset(congress, subset = (party == "Republican"))
dem <- congress[congress$party == "Democrat", ]
rep80 <- subset(rep, subset = (congress == 80))
dem80 <- subset(dem, subset = (congress == 80))
rep112 <- subset(rep, subset = (congress == 112))
dem112 <- subset(dem, subset = (congress == 112))

congress80112 <- subset(congress, subset = (congress == 80 | congress == 112))

ggplot(congress80112, aes(x=dwnom1, y=dwnom2))+
  geom_point(aes(color=party))+
  scale_color_manual(values=c("blue", "green", "red"))+
  facet_wrap(~congress)+
  labs(title="აშშ კონგრესის პოლარიზაცია",
       x="ეკონომიკური ლიბერალიზმი/კონსერვატიზმი",
       y="რასობრივი ლიბერალიზმი/კონსერვატიზმი")

ggplot(congress80112, aes(x=dwnom1, y=dwnom2))+
  geom_point(aes(color=party))+
  scale_color_manual(values=c("blue", "green", "red"),
                     labels=c("დემოკრატი", "სხვა", "რესპუბლიკელი"))+
  facet_wrap(~congress)+
#   guide_legend(title="პოლიტიკური პარტია")+
  labs(title="აშშ კონგრესის პოლარიზაცია",
       x="ეკონომიკური ლიბერალიზმი/კონსერვატიზმი",
       y="რასობრივი ლიბერალიზმი/კონსერვატიზმი")+
  theme_minimal()


dem.median <- tapply(dem$dwnom1, dem$congress, median)
rep.median <- tapply(rep$dwnom1, rep$congress, median)

### ავაგოთ ხაზობრივი დიაგრამა, რომელიც მედიანური ქულის ცვლილებას გვიჩვენებს

demm <- as.data.frame(t(dem.median))
repm <- as.data.frame(t(rep.median))
med <- t(rbind(demm, repm))
med <- as.data.frame(med)
names(med) <- c("democrat", "republican")
med$congress <- as.numeric(row.names(med))

### Make med tidy again
### ggplot-ის თავისებურებებიდან გამომდინარე, ცხრილი უნდა იყოს tidy,
### ანუ თითო სვეტი უნდა იყოს თითო ცვლადი. შესაბამისად,
### tidyr ბიბლიოთეკიდან ვიყენებთ ფუნქცია gather-ს:

med <- gather(med, party, econ, -congress)

ggplot(med, aes(congress, econ))+
  geom_line(aes(color=party))+
  scale_color_manual(values=c("blue", "red"),
                     labels=c("დემოკრატი", "რესპუბლიკელი"))+
  guides(color=guide_legend(title="პარტია"))+
  labs(title="აშშ კონგრესის რადიკალიზაცია ეკონომიკური პოლიტიკისადმი 
       დამოკიდებულების მიხედვით",
       x="კონგრესის მოწვევა",
       y="ეკონომიკური ლიბერალიზმის ინდექსი")+
  theme_minimal()

gini <- read.csv("USGini.csv")
names(gini)

cor(gini$gini[seq(from = 2, to = nrow(gini), by = 2)],
    rep.median - dem.median)

cor(gini$gini[seq(from = 2, to = nrow(gini), by = 2)],
    rep.median - dem.median, method="spearman")

cor(gini$gini[seq(from = 2, to = nrow(gini), by = 2)],
    rep.median - dem.median, method="kendall")

### კლასტერული ანალიზი იდეოლოგიის მიხედვით

dwnom80 <- cbind(congress$dwnom1[congress$congress == 80],
                 congress$dwnom2[congress$congress == 80])
dwnom112 <- cbind(congress$dwnom1[congress$congress == 112],
                  congress$dwnom2[congress$congress == 112])

k80two.out <- kmeans(dwnom80, centers = 2, nstart = 5)
k112two.out <- kmeans(dwnom112, centers = 2, nstart = 5)

names(k80two.out)
k80two.out$centers
k112two.out$centers

table(party = congress$party[congress$congress == 80],
        cluster = k80two.out$cluster)

table(party = congress$party[congress$congress == 112],
        cluster = k112two.out$cluster)

