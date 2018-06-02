<div class="header" style="margin-top:0 px;font-size:60%;">DAR: მეცხრე შეხვედრა</div>

რაოდენობრივ მონაცემთა ანალიზი R-გარემოში
========================================================
author: დავით სიჭინავა
date: 18 მაისი, 2018
autosize: true
transition: none
css: css/style.css
font-family: 'BPG_upper'
<span style="font-weight:bold; font-family:BPG_upper;">მეცხრე შეხვედრა</span>



დღევანდელი შეხვედრის გეგმა
========================================================

- წრფივი რეგრესიის დიაგნოსტირება
- მრავლობითი წრფივი რეგრესია


დაშვება:
========================================================
* წრფივი ურთიერთდამოკიდებულება
* ჰომოსკედასტურობა
* ცდომილებათა დამოუკიდებლობა
* ნორმალური განაწილება


რას ვამოწმებთ:
========================================================
* უკიდურეს წერტილებს
* ,,გავლენიან'' ჩანაწერებს


დეტერმინაციის კოეფიციენტი:
========================================================

```r
florida <- read.csv("florida.csv")
fit2 <- lm(Buchanan00 ~ Perot96, data = florida)
fit2
```
დეტერმინაციის კოეფიციენტი:
========================================================

```r
fit2summary <- summary(fit2)
fit2summary$r.squared
```

გავლენიანი ჩანაწერები:
========================================================

```r
plot(fitted(fit2), resid(fit2), xlim = c(0, 1500), ylim = c(-750, 2500),
xlab = "Fitted values", ylab = "Residuals")
abline(h = 0)
florida$county[resid(fit2) == max(resid(fit2))]
```

გავლენიანი ჩანაწერები:
========================================================

```r
florida.pb <- subset(florida, subset = (county != "PalmBeach"))
fit3 <- lm(Buchanan00 ~ Perot96, data = florida.pb)
fit3
R2(fit3)
```

უკიდურესი წერტილები
========================================================

```r
outlierTest(fit)
qqPlot(fit, main="QQ Plot")
leveragePlots(fit)
```

გავლენიანი ჩანაწერები
========================================================

```r
# Influential Observations
# added variable plots 
av.Plots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(face)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(fit,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
```

ცდომილებათა ნორმალურობა
========================================================
* MASS ბიბლიოთეკის გამოყენებით

```r
# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
   main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
```

არამუდმივი ცდომილებათა დისპერსია
========================================================

```r
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)
```

მულტიკოლინეარულობა
========================================================

```r
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?
```

არაწრფივი დამოკიდებულება
========================================================

```r
# Evaluate Nonlinearity
# component + residual plot 
crPlots(fit)
# Ceres plots 
ceresPlots(fit)
```

ცდომილებათა დამოუკიდებლობა
========================================================

```r
durbinWatsonTest(fit)
```

რეგრესია და რანდომიზებული ექსპერიმენტები:
========================================================

```r
women <- read.csv("women.csv")
mean(women$female[women$reserved == 1])
mean(women$female[women$reserved == 0])
```

საშუალოთა შორის სხვაობა (Difference in means):
========================================================

```r
mean(women$water[women$reserved == 1]) - mean(women$water[women$reserved == 0])
```

საშუალოთა შორის სხვაობა (Difference in means):
========================================================

```r
mean(women$irrigation[women$reserved == 1]) - mean(women$irrigation[women$reserved == 0])
```

ექსპერიმენტული დიზაინის ანალიზისას, საშუალოთა შორის სხვაობა უდრის ექსპერიმენტული ცვლადის ბეტა-კოეფიციენტს:
========================================================

```r
lm(water ~ reserved, data = women)
lm(irrigation ~ reserved, data = women)
```

მრავალწევრი რეგრესია
========================================================

```r
social <- read.csv("social.csv")
levels(social$messages)
```

მრავალწევრი რეგრესია
========================================================

```r
fit <- lm(primary2008 ~ messages, data = social)
fit
```

მრავალწევრი რეგრესია
========================================================

```r
social$Control <- ifelse(social$messages == "Control", 1, 0)
social$Hawthorne <- ifelse(social$messages == "Hawthorne", 1, 0)
social$Neighbors <- ifelse(social$messages == "Neighbors", 1, 0)
lm(primary2008 ~ Control + Hawthorne + Neighbors, data = social)
```

პროგნოზი
========================================================

```r
unique.messages <- data.frame(messages = unique(social$messages))
unique.messages

predict(fit, newdata = unique.messages)
```

პროგნოზი
========================================================

```r
fit.noint <- lm(primary2008 ~ -1 + messages, data = social)
fit.noint
```

ექსპერიმენტის ეფექტი
========================================================

```r
# ექსპერიმენტის საშუალო ეფექტი
coef(fit)["messagesNeighbors"] - coef(fit)["messagesControl"]

# საშუალოთა შორის სხვაობა
mean(social$primary2008[social$messages == "Neighbors"]) - mean(social$primary2008[social$messages == "Control"])
```

შესწორებული დეტერმინაციის კოეფიციენტი
========================================================

```r
fitsummary <- summary(fit)
fitsummary$adj.r.squared
```
