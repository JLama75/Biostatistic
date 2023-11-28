setwd("~/Documents/Biostats")
#first let's make a vector of a values of change in elevation
delta.lat <- c(58.9,7.8,108.6,44.8,11.1,19.2,61.9,30.5,12.7,35.8,7.4,39.3,24,62.1,24.3,55.3,32.7,65.3,-19.3,7.6,-5.2,-2.1,31.0,69,88.6,39.5,20.7,89,69,64.9,64.8)

#to calculate 95% CI we'll need estimated variance
var(delta.lat)
length(delta.lat)-1
#to estimate 95%CI of a variance, we need to obtain the 0.025 and 0.975 critical values for the appropriate X2 distribution
#we'll use qchisq()
#usage is qchisq(percentile,df) 
#for example the critical value for the lower 2.5 percentile is 
qchisq(0.025,30)
qchisq(0.975,30)
#the lower bound of our CI is
up <- 30*var(delta.lat)/qchisq(0.975,30)
#upper bound is 
down <- 30*var(delta.lat)/qchisq(0.025,30)
sqrt(up) #24.50326
sqrt(down) #40.98657
#Find F statistic
F <- (1.58^2/1.16^2) 
#find p-value for f statistic
2*pf(1.855232,30,132,lower.tail=FALSE)
# 0.01887129
#variance is diff
up <- 6*var(sloth)/qchisq(0.975,6)
#upper bound is 
down <- 6*var(sloth)/qchisq(0.025,6)
sqrt(up) #24.50326
sqrt(down) #40.98657


eyestock <- c(8.69,8.15,9.25,9.45,8.96,8.65,8.43,8.79,8.63)

var(eyestock)
length(eyestock) -1
qchisq(0.025,8)
qchisq(0.975,8)

UP <- 8*var(eyestock)/qchisq(0.025,8)
DOWN <- 8*var(eyestock)/qchisq(0.975,8)
sqrt(UP) #0.7630553
sqrt(DOWN) #0.2690359

#import the data
cichlids <- read.csv("Cichlids.csv", header=TRUE)
#let's take a look at the data frame
cichlids
#let's plot the data #if the distribution is normal or not.
library(ggplot2)
ggplot(cichlids, aes(preference)) + geom_histogram(bins=10) + facet_grid(Genotype~.)
#to the test the hypothesis that the variance of two normally distributed variables is equal we'll implement and F-test of equal variance
#test statistic is
var(subset(cichlids, Genotype == "F2")$preference)/var(subset(cichlids, Genotype == "F1")$preference)
#or in tidy:
var(filter(cichlids, Genotype == "F2") %>% pull(preference))/var(filter(cichlids, Genotype == "F1") %>% pull(preference))

#we can compare out test statistic to null using pf()
#usage is pf(stat, df1, df2, lower.tail = TRUE/FALSE)
#we want the upper tail i.e. lower.tail =FALSE
#df1
length(subset(cichlids, Genotype == "F2")$preference) -1 
#df2
length(subset(cichlids, Genotype == "F1")$preference) -1 
#the test
2*pf(6.064405,32,19,lower.tail=FALSE)
qf(0.975,30,20)
qt(0.025,30)

#what is the critical t-value that is greater than 97.5% of observations
qt(0.975,30)
qt(0.025,29)
#to make this EVEN easier we'll use var.test(), 
#usage is var.test(values 1, values 2, ratio of variances under null, alternative="greater"/"less"/two-sided",conf.level)
#default is two-sided
#default ratio is 1 (variances are equal)
#default conf level is 0.95
var.test(subset(cichlids, Genotype == "F2")$preference,subset(cichlids, Genotype == "F1")$preference)

#it the distribution is not n0rmal.

#to compare variance of non-normally distributed variables we use levene's test
#in R we can perform leveneTest() which is part of the "car" package
install.packages("car")
library("car")
#usage is leveneTest(values, factor defining groups, center = "median" or "mean" (median is preferred and the default))
leveneTest(cichlids$preference,cichlids$Genotype,center=mean)
leveneTest(cichlids$preference,cichlids$Genotype,center=median)

#leveneTest(pull(cichlids, preference))
levene.test()
#to compare means of two groups with unequal variance we implement welch's t.test
#in R we can perform a welch's t-Test using t.test()
#usage when comparing two means is
#t.test(observed values group 1, observed values group 2, alternative="two.sided"/"less"/"greater", paired = TRUE/FALSE, var.equal = TRUE/FALSE(default),conf.level= your desired conf.)
#this test is two sided (Default)
#this test is not paired so paired = FALSE
#the proposed difference in means under the null is zero
#variances are not equal for var.equal=FALSE(default)
#we'll use conf.level = 0.95 (default)
#when the variance of two groups are not equal we cannot use two sample t-test. So, we have to use Welch two sample t test.
t.test(subset(cichlids, Genotype == "F2")$preference,subset(cichlids, Genotype == "F1")$preference,paired=FALSE)
#for standard two sample t-test we use
t.test(subset(cichlids, Genotype == "F2")$preference,subset(cichlids, Genotype == "F1")$preference,paired=FALSE, var.equal = T)



#PRACTICE
beerr <- read.csv("BeerAndMosquitoes.csv", header=TRUE)
beerr_no <- subset(beerr, drink == "beer")
library(ggplot2)
ggplot(beerr, aes(change)) + geom_histogram(bins=10) + facet_grid(drink~.)

#is a normal distribution so we can use F test or levene test.

leveneTest(beerr$change,beerr$drink,center=median)
leveneTest(beerr$change,beerr$drink,center=mean)

var.test(subset(beerr, drink == "beer")$change,subset(beerr, drink == "water")$change)
#var(subset(beerr, drink == "beer"))
  
#variance is not significantly different so we can use the standard two samplt t-test.
t.test(subset(beerr, drink == "beer")$change,subset(beerr, drink == "water")$change, var.equal = T )
#mean is significantly different from each other. with mean is greater in beer 
s1 <- c(rep(17.5,14))
s2 <- c(rep(19.9,17))
mean(s2)
t.test(s1,s2,var.equal = T)


SP2<-(((3.37^2) *13)+((2.47^2)*16))/29
se<-sqrt(((1/14)+(1/17))*SP2)
t<- 2.4/se
2*pf(((3.37^2)/(2.47^2)),13,16, lower.tail = FALSE)
pf(1.86,13,16,lower.tail = FALSE)
(3.37^2)/(2.47^2)
2*pt(2.286703,29,lower.tail=FALSE)

beerr_no1 <- beerr_no$beforeDrink
beerr_no1 <- beerr_no1 %>% data.frame
colnames(beerr_no1) <- "activation"
beerr_no1$treatment <- "beforeDrink"


beerr_no2 <- beerr_no$afterDrink
beerr_no2 <- beerr_no2 %>% data.frame
colnames(beerr_no2) <- "activation"
beerr_no2$treatment <- "afterDrink"

beerr_noall <- rbind(beerr_no1, beerr_no2)
leveneTest(beerr_noall$activation,beerr_noall$treatment,center=mean)

ggplot(beerr_no, aes(afterDrink)) + geom_histogram(bins=5) 
ggplot(beerr_no, aes(beforeDrink)) + geom_histogram(bins=5)
leveneTest(beerr_no$beforeDrink,beerr_no$afterDrink,center=median)

var.test(beerr_no$afterDrink,beerr_no$beforeDrink)
#var(subset(beerr, drink == "beer"))

#variance is not significantly different so we can use the standard two samplt t-test.
t.test(beerr_no$afterDrink,beerr_no$beforeDrink, var.equal = T, paired = T )
#mean is significantly different from each other. with mean is greater in beer 


RAT <- read.csv("RATreciprocity.csv", header=TRUE)
#one sample t-test paired.
#first calculate the change.
ggplot(RAT, aes(AfterHelp)) + geom_histogram(bins=5)
ggplot(RAT, aes(AfterNoHelp)) + geom_histogram(bins=5)

leveneTest(RAT$AfterHelp,RAT$AfterNoHelp,center=mean)

var.test(RAT$AfterHelp,RAT$AfterNoHelp)
#var(subset(beerr, drink == "beer"))
t.test(RAT$AfterHelp,RAT$AfterNoHelp, var.equal = T, paired = T)
mean(RAT$AfterHelp)
mean(RAT$AfterNoHelp)

Spin <- read.csv("SpinocerebellarAtaxia.csv", header=TRUE)
ggplot(Spin, aes(lifespan)) + geom_histogram(bins=5)  + facet_grid(treatment~.)
leveneTest(Spin$lifespan,Spin$treatment,center=mean)
#two-sample t-test
var.test(subset(Spin, treatment == "NoExcercise")$lifespan,subset(Spin, treatment == "Exercise")$lifespan)

#var(subset(beerr, drink == "beer"))
t.test(subset(Spin, treatment == "Exercise")$lifespan,subset(Spin, treatment == "NoExcercise")$lifespan, var.equal = T)
mean(RAT$AfterHelp)
mean(RAT$AfterNoHelp)