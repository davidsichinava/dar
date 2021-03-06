<div class="header" style="margin-top:0 px;font-size:60%;">რმაRგ: პირველი შეხვედრა</div>

რაოდენობრივ მონაცემთა ანალიზი R-გარემოში
========================================================
author: დავით სიჭინავა
date: 22 მარტი, 2019
autosize: true
transition: none
css: css/style.css
font-family: 'BPG_upper'
<span style="font-weight:bold; font-family:BPG_upper;">პირველი შეხვედრა</span>




დღევანდელი შეხვედრის გეგმა
========================================================
incremental: true

- კურსის გაცნობა და ლოგისტიკა
  - საჭირო კომპიუტერული პროგრამები
  - შეფასება, დავალებები, ლაბორატორიული სამუშაო და ა.შ.
- R გარემო

რას გავიგებ ამ კურსის ფარგლებში?
========================================================
incremental: true

- ვნახავთ, თუ როგორაა შესაძლებელი მონაცემთა ეფექტურად მოგროვება და გაანალიზება;
- შევისწავლით რაოდენობრივ მონაცემთა სტატისტიკური ანალიზის ძირითად პრინციპებს;
- გავერკვევით R-გარემოში მუშაობის მთავარ საკითხებში

რა დამჭირდება ამ კურსისთვის?
========================================================
incremental: true

- R;
- R-studio;
- ტექსტური  რედაქტორი - გემოვნებით;
- დაწვრილებით: ლაბორატორიის მსვლელობისას

კურსის ვებსაიტი:
========================================================
incremental: false

<span style="width: 100%;text-align: center">http://davidsichinava.github.io/dar</span>

კურსის სტრუქტურა: 
========================================================
incremental: false

<img src="ico/teacher.png" alt="Drawing" style="width: 48px; vertical-align:middle;"/>  ლექცია
 

კურსის სტრუქტურა: 
========================================================
incremental: false

<img src="ico/teacher.png" alt="Drawing" style="width: 48px; vertical-align:middle;"/>  ლექცია
 
<img src="ico/flask-outline.png" alt="Drawing" style="width: 48px; vertical-align:middle;"/> ლაბორატორია

- პრაქტიკული სავარჯიშოები R-ის გამოყენებით
- დაეთმობა შეხვედრების მეორე საათი


კურსის სტრუქტურა: 
========================================================
incremental: false

<img src="ico/teacher.png" alt="Drawing" style="width: 48px; vertical-align:middle;"/>  ლექცია
 
<img src="ico/flask-outline.png" alt="Drawing" style="width: 48px; vertical-align:middle;"/> ლაბორატორია

კურსის სტრუქტურა: 
========================================================
<img src="ico/open-book.png" alt="Drawing" style="width: 48px; vertical-align:middle;"/> ლიტერატურა
- ძირითადი ტექსტები:
+ Field, A., Miles, J., Field, Z: [_Discovering Statistics Using R._](https://www.dropbox.com/s/4858b2iw27sywqu/DiscoveringR.epub?dl=0) 4th Edition. Thousand Aoaks, CA: Sage Publications. 2012. (DIS) - _ამ წიგნისთვის დაგჭირდებათ epub-რიდერი_
+ Imai, K.: [_Quantitative Social Science, An Introduction_](https://www.dropbox.com/s/3wb6t5igedhqjih/QSS.pdf?dl=0). Princeton University Press. 2017 (QSS)
+ Grolemund, G. & Wickham, H.: [_R for Data Science_](http://r4ds.had.co.nz/) (R4DS)
+ Dalgaard, Peter. [_Introductory statistics with R_](https://www.dropbox.com/s/vus2di6528t1513/IntroR.pdf?dl=0). Springer Science & Business Media, 2008. (ISR)
+ Kruschke, John. [_Doing Bayesian data analysis: A tutorial with R, JAGS, and Stan_](https://www.dropbox.com/s/gldmg1x690l8qty/Bayesian.pdf?dl=0). Academic Press, 2014 (BAY)

- ჩემს მიერ დარიგებული ლიტერატურა და  კურსის ვებსაიტზე ატვირთული ტექსტები

კურსის სტრუქტურა: 
========================================================
incremental: false

<img src="ico/teacher.png" alt="Drawing" style="width: 48px; vertical-align:middle;"/>  ლექცია
 
<img src="ico/flask-outline.png" alt="Drawing" style="width: 48px; vertical-align:middle;"/> ლაბორატორია

<img src="ico/open-book.png" alt="Drawing" style="width: 48px; vertical-align:middle;"/> ლიტერატურა

<img src="ico/book.png" alt="Drawing" style="width: 48px; vertical-align:middle;"/> დავალებები
- ლაბორატორიისას დაგირიგდებათ დავალებები, რომელიც მომდევნო კვირის მანძილზე უნდა ატვირთოთ დროპბოქსზე


შეფასება: 
========================================================
incremental: false

<img src="img/fu.gif" alt="Drawing" style="width: 500px; display: block; margin-left: auto; margin-right: auto;"/>
 

შეფასების კომპონენტები: 
========================================================
incremental: false
| კომპონენტი             | ქულა |
|------------------------|------|
| დასწრება               | 5    |
| ლაბორატორიული დავალება | 30   |
| შუალედური გამოცდა      | 25   |
| დასკვნითი გამოცდა      | 40   |


დავიწყოთ?
========================================================
incremental: false

<img src="img/go_erthaoz.gif" alt="Drawing" style="width: 500px; display: block; margin-left: auto; margin-right: auto;"/>

R-გარემოს გაცნობა
====================================
type: subsection

- შესავალი R-გარემოში
-  R-ის მომხმარებლის გრაფიკული გარემო: R-Studio
- R-ის მომხმარებლის გრაფიკული გარემოს ძირითადი ელემენტები
- R-მარკირების დოკუმენტის შექმნა
- ,,წიგნიერი პროგრამირება’’ და პროექტების ორგანიზების კარგი პრაქტიკა


რა არის R?: 
========================================================
incremental: false

<img src="img/ihge.png" alt="Drawing" style="width: 1000px; display: block; margin-left: auto; margin-right: auto;"/>


რა არის R?: 
========================================================
incremental: false

- დაპროგრამების ენა

რა არის R?: 
========================================================
incremental: false

- (ერთ-ერთი ყველაზე ხშირად გამოყენებადი) დაპროგრამების ენა

<img src="img/ieee_spectrum_ranking.png" alt="Drawing" style="width: 600px; display: block; margin-left: auto; margin-right: auto;"/>

რა არის R?: 
========================================================
incremental: false

- დაპროგრამების ენა
- ვრცელდება უფასოდ
  - GPL2-GPL3 ლიცენზია
- ხელმისაწვდომია ყველა პოპულარული საოპერაციო პლატფორმისთვის
 
რა არის R?: 
========================================================

<img src="img/r_screenshot.PNG" alt="Drawing" style="width: 600px; display: block; margin-left: auto; margin-right: auto;"/>


რა არის R-Studio?: 
========================================================
incremental: true

R-ის ნაკლად კარგა ხანი ითვლებოდა მოსახერხებელი სამომხმარებლო გარემოს არარსებობა

R-Studio ამ პრობლემის მოსაგვარებლად არსებობს

რა არის R-Studio?: 
========================================================
incremental: true

<img src="img/Rstudio_screen.PNG" alt="Drawing" style="width: 800px; display: block; margin-left: auto; margin-right: auto;"/>



დახმარება: 
========================================================


```r
### ზოგადი დახმარება:
help.start()
```

დახმარება: 
========================================================


```r
### დახმარება რომელიმე ფუნქციასთან დაკავშირებით
help(lm) ## ან
?lm

### მაგალითის ჩვენება
example(lm)


### ფუნქციის / ბიბლიოთეკის ვინიეტის ჩვენება
vignette("ggplot2-specs")
```

სამუშაო დირექტორია: 
========================================================


```r
getwd()
```

სამუშაო დირექტორია: 
========================================================


```r
setwd("D:/Dropbox/R/My awesome research")
```
ან


```r
setwd("D:\\Dropbox\\R\\My awesome research")
```

ან


```r
setwd('D:\\Dropbox\\R\\My awesome research')
```

ბიბლიოთეკები: 
========================================================


```r
install.packages("ბიბლიოთეკის სახელწოდება")
```


```r
library("ბიბლიოთეკის სახელწოდება")
```

კვლევის დოკუმენტირება და განმეორებადობა (რეპლიკაცია) 
========================================================

<img src="img/reproduction.png" alt="Drawing" style="width: 500px; display: block; margin-left: auto; margin-right: auto;"/>

<span style="width: 100%;text-align: center; font-size:14px">წყარო: Baker, M. (2016): Is there a reproducibility crisis? Nature. Vol. 533.</span>



კვლევის დოკუმენტირება და განმეორებადობა (რეპლიკაცია) 
========================================================
მაგრამ...

> The lexicon of reproducibility to date has been multifarious and ill-defined. The causes of and remedies for what is called poor reproducibility, in any scientific field, require a clear specification of the kind of reproducibility being discussed (methods, results, or inferences), a proper understanding of how it affects knowledge claims, scientific investigation of its causes, and an improved understanding of the limitations of statistical significance as a criterion for claims

<span style="width: 100%;text-align: center; font-size:14px">Goodman, S., Fanelli, D., Ioannidis, J. (2016): [What does research reproducibility mean?](http://d3ukwgt0ah4zb1.cloudfront.net/content/scitransmed/8/341/341ps12.full.pdf)</span>

კვლევის დოკუმენტირება და განმეორებადობა (რეპლიკაცია) 
========================================================
incremental: true
* სტანდარტები (King, 1995):
> The replication standard holds that sufficient information exists with which to understand, evaluate, and build upon a prior work if a third party can replicate the results without any additional information from the author
* მონაცემთა საცავები (Dataverse @ Harvard, Figshare)
* ვერსიათა კონტროლი (Git, Github, Bitbucket)

<span style="width: 100%;text-align: center; font-size:14px">წყარო: Marwick, B. (2014): [Reproducible Research: A primer for the social sciences](http://benmarwick.github.io/CSSS-Primer-Reproducible-Research/)</span>



სკრიპტები: 
========================================================


```r
source("MyAwesomeRegression.R")
```


წიგნიერი პროგრამირება: 
========================================================

<img src="img/don_knuth.jpg" alt="Drawing" style="width: 500px; display: block; margin-left: auto; margin-right: auto;"/>

წიგნიერი პროგრამირება: 
========================================================

> Documentation must be regarded as an integral part of the process of design and coding. A good programming language will encourage and assist the programmer to write clear, self-documenting code, and even perhaps to develop and display a pleasant style of writing.

<span style="width: 100%;text-align: center; font-size:14px">Hoare, T. (1973): Hints on Programming Language Design. [in:] Knuth, D. (1983): [Literate Programming](http://roxygen.org/knuth-literate-programming.pdf). _The Computer Journal_</span>


R-ბლოკნოტი: 
========================================================

<img src="img/notebook_screen.PNG" alt="Drawing" style="width: 700px; display: block; margin-left: auto; margin-right: auto;"/>

R-ბლოკნოტი: მარკირება
========================================================

<img src="img/md_cs.PNG" alt="Drawing" style="width: 700px; display: block; margin-left: auto; margin-right: auto;"/>

პროექტის ორგანიზების საუკეთესო პრაქტიკა:
========================================================

* გამჭვრივალეობა
* მხარდაჭერა
* მოდულების ფორმით ორგანიზება
* პორტატულობა

პროექტის ორგანიზების საუკეთესო პრაქტიკა:
========================================================

1. პროგრამის მოკლე აღწერა
2. ყველა საჭირო ბიბლიოთეკის ჩატვირთვა
3. აბსოლუტური მისამართის თავიდანვე მითითება, ხოლო შემდეგ - ფარდობით დირექტორიებში მუშაობა
4. კოდის სექციებად მოწესრიგება
5. ფუნქციები
6. ობიექტთა თანმიმდევრული სახელდება
7. ფოლდერების და ფაილების ლოგიკური მოწესრიგება

პროექტის ორგანიზების საუკეთესო პრაქტიკა:
========================================================


<img src="img/hbling.png" alt="Drawing" style="width: 600px; display: block; margin-left: auto; margin-right: auto; margin-top: 40px;"/>


მონაცემების წაკითხვა ჩაშენებული ფუნქციების მეშვეობით:
========================================================


```r
### ტაბით გამოყოფილი ცხრილი

1   6   a
2   7   b
3   8   c
4   9   d
5   10  e

### მძიმით გამოყოფილი ცხრილი

v1,v2,v3
1,2,3
4,5,6
7,8,9
a,b,c
```

მონაცემების წაკითხვა ჩაშენებული ფუნქციების მეშვეობით:
========================================================


```r
myTable<-read.table("data/myTable.txt")

myTable<-read.table("data/CB_2017_Georgia_public_17.11.17.csv", header = TRUE, sep=",") ### მძიმით გამოყოფილი ცხრილის წაკითხვა

myTable<-read.csv("data/CB_2017_Georgia_public_17.11.17.csv", header=TRUE) ### მძიმით გამოყოფილი ცხრილის წაკითხვა

myTable<-read.table("data/CB_2017_Georgia_public_17.11.17.txt", header = TRUE, sep="\t") ### ტაბით გამოყოფილი ცხრილის წაკითხვა

galton<-read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Galton.csv")
```

მონაცემების წაკითხვა გარე ბიბლიოთეკების მეშვეობით:
========================================================


```r
install.packages("haven")
library(haven)
```

მონაცემების წაკითხვა გარე ბიბლიოთეკების მეშვეობით:
========================================================

```r
### haven ბიბლიოთეკა:

cb2017spss<-read_sav("data/CB_2017_Georgia_public_17.11.17.sav")
cb2017stata<-read_dta("data/CB_2017_Georgia_public_17.11.17.dta")
```

მონაცემების წაკითხვა გარე ბიბლიოთეკების მეშვეობით:
========================================================

```r
install.packages("foreign")
library(foreign)

cb2017foreign<-read.spss("data/CB_2017_Georgia_public_17.11.17.sav")
```

დავალებები:
========================================================

* ლაბორატორია (მომდევნო კვირა)
* ანოტირებული ბიბლიოგრაფია

ანოტირებული ბიბლიოგრაფია:
========================================================
* ციტირება + სტატიის მოკლე (150-200-სიტყვიანი) კრიტიკული აღწერა
* ძირითადი მიგნებები
* (თუ არსებობს), რას თვლით სტატიის უარყოფით მხარედ
* ნიმუში იხილეთ შემდეგ ვებსაიტზე (ასევე - ლაბორატორიული დავალების ფაილში):
	+ Engle, M. (2016): How to Prepare an Annotated Bibliography: The Annotated Bibliography. ნანახია: http://guides.library.cornell.edu/annotatedbibliography
	

========================================================

<img src="img/morchakino.gif" alt="Drawing" style="width: 600px; display: block; margin-left: auto; margin-right: auto; margin-top: 40px;"/>
