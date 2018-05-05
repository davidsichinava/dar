<div class="header" style="margin-top:0 px;font-size:60%;">რმაRგ: მერვე შეხვედრა</div>

რაოდენობრივ მონაცემთა ანალიზი R-გარემოში
========================================================
author: დავით სიჭინავა
date: 4 მაისი, 2018
autosize: true
transition: none
css: css/style.css
font-family: 'BPG_upper'
<span style="font-weight:bold; font-family:BPG_upper;">მერვე შეხვედრა</span>



დღევანდელი შეხვედრის გეგმა
========================================================

- ბივარიაციული დამოკიდებულების აღწერა
- k-საშუალოთა ალგორითმი

პოლიტიკური პოლარიზაციის შეფასება
========================================================

- McCarthy, Poole & Rosenthal (2006): Polarized America: The Dance of Ideology and Unequal Riches. MIT Press
- აშშ-ის კონგრესში ხმების პოლარიზაცია, ხმის მიცემის სივრცით სკალაზე

პოლიტიკური პოლარიზაციის შეფასება
========================================================

| ცვლადის სახელი | ცვლადის აღწერა     |
|----------|-------|
|name | კონგრესმენი|
|state | შტატი |
|district | კონგრესის საარჩევნო ოლქი|
|party | კონგრესმენის პარტია |
|congress | მოწვევის ნომერი |
|dwnom1 | DW-NOMINATE ქულა (პირველი განზომილება)|
|dwnom2 | DW-NOMINATE ქულა (მეორე განზომილება)|

ბივარიაციული დამოკიდებულება: 
========================================================

```r
congress <- read.csv("congress.csv")
rep <- subset(congress, subset = (party == "Republican"))
dem <- congress[congress$party == "Democrat", ]
rep80 <- subset(rep, subset = (congress == 80))
dem80 <- subset(dem, subset = (congress == 80))
rep112 <- subset(rep, subset = (congress == 112))
dem112 <- subset(dem, subset = (congress == 112))
```

ბივარიაციული დამოკიდებულება: 
========================================================

```r
congress80112 <- subset(congress, subset = (congress == 80 | congress == 112))

ggplot(congress80112, aes(x=dwnom1, y=dwnom2))+
  geom_point(aes(color=party))+
  scale_color_manual(values=c("blue", "green", "red"))+
  facet_wrap(~congress)+
  labs(title="აშშ კონგრესის პოლარიზაცია",
       x="ეკონომიკური ლიბერალიზმი/კონსერვატიზმი",
       y="რასობრივი ლიბერალიზმი/კონსერვატიზმი")
```

ბივარიაციული დამოკიდებულება: 
========================================================

```r
dem.median <- tapply(dem$dwnom1, dem$congress, median)
rep.median <- tapply(rep$dwnom1, rep$congress, median)
median <- rbind(dem.median, rep.median)
## tapply() ფუნქცია საშუალებას გვაძლევს, გამოთვლები ჩავატაროთ ჯგუფების მიხედვით
```


კორელაცია: 
========================================================
* z-ქულა
* $z{x}_{i} = \frac{x_{i}-\bar{x}}{S_{x}}$,
სადაც $\bar{x}$ წარმოადგენს განაწილების საშუალოს, $S_{x}$ - სტანდარტულ გადახრას

კორელაცია: 
========================================================
* პირსონის კორელაცია:
* $Corr(x,y) = \frac{1}{n}\Sigma\frac{x_{i}-\bar{x}}{S_{x}}*\frac{y_{i}-\bar{y}}{S_{y}}$

კორელაცია: 
========================================================

```r
gini <- read.csv("USGini.csv")
```

კორელაცია: 
========================================================

```r
cor(gini$gini[seq(from = 2, to = nrow(gini), by = 2)],
rep.median - dem.median)
```

k-საშუალოთა ალგორითმი: 
========================================================
* ახასიათებს ფენომენის, ფუნქციის, მონაცემებს _დაჯგუფებას_ (კლასტერიზაციას)
* იტერაციული ალგორითმი

k-საშუალოთა ალგორითმი: 
========================================================

```r
dwnom80 <- cbind(congress$dwnom1[congress$congress == 80],
congress$dwnom2[congress$congress == 80])
dwnom112 <- cbind(congress$dwnom1[congress$congress == 112],
congress$dwnom2[congress$congress == 112])
```

k-საშუალოთა ალგორითმი: 
========================================================

```r
k80two.out <- kmeans(dwnom80, centers = 2, nstart = 5)
k112two.out <- kmeans(dwnom112, centers = 2, nstart = 5)
names(k80two.out)
k80two.out$centers
k112two.out$centers
```

k-საშუალოთა ალგორითმი: 
========================================================

```r
table(party = congress$party[congress$congress == 80], cluster = k80two.out$cluster)

table(party = congress$party[congress$congress == 112], cluster = k112two.out$cluster)
```

k-საშუალოთა ალგორითმი: 
========================================================

```r
k80four.out <- kmeans(dwnom80, centers = 4, nstart = 5)
k112four.out <- kmeans(dwnom112, centers = 4, nstart = 5)
```
