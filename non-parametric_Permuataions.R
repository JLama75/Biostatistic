library(tidyverse)
setwd("~/Documents/Biostats")
#problem 1
ducks <- read_csv("nestparasites.csv")
ducks

#plot the data at a few different bin widths. How would you describe the distribution?
#null= index should be 0 or mean should be 0. so we should do one sample t test.
library("ggplot2")
ggplot(ducks, aes((parasitismFirstIndex))) + geom_histogram(bins=10)
#right skewed. and have some outliers on the left side up
qqnorm(pull(ducks,parasitismFirstIndex)) 
#+ qqline(pull(ducks,parasitismFirstIndex),p=c(0.001,0.999))
#cannot perform log transformation or square transforamtion due to presence of negative value, cannot perform reciprocal due
#to presence of 0 in the data.

#it's skewed and there are several outliers, a good candidate for a non-parametric test
#we'll use the function sign.test in the BDSA package
#install.packages("BSDA")
library("BSDA")

#we can assign the signs ourselves and do a binomial test.
ducks
#true mean null = zero (i.e. equal negative and positive ) so overall probability of success is 0.5 & the predicted mean for binomial distribution will be 6.5 (13/2)
#we have one zero, we cannot assign this zero to negative or postive. #for binomial, we need number of trials
#and overall probability.
binom.test(12,13)
#we can also use the function sign.test
#usage is sign.test(vector of values, hypothesized median, alternative = "two.sided"/"greater"/"less",conf.level)
#our hypothesized median is zero which is the default
#this is a two sided test (default)
#conf.level = 0.95 (default)
SIGN.test(pull(ducks,parasitismFirstIndex))
#note that if you provide two vectors of values, a "dependent-sign" test will be performed that is a non-parametric equvalent to a paired t-test 

#practice problem 2 with your classmates
plants <- read_csv("plantspeciation.csv")

plants

#null - no diff in species number. so mean change = 0 
#paired one sample t-test.
plants$diff <- plants$dioecious - plants$monomorphic
SIGN.test(pull(plants,diff))
SIGN.test(pull(plants,monomorphic),pull(plants,dioecious))
#dioecious plants have more species. monomorphic have more speciation rate.
#problem 3
recycle <- read_csv("recycling.csv")
recycle
ggplot(recycle, aes(paperUsage)) + geom_histogram(bins =10) + facet_grid(recyclingBinPresent~.)
#the graph are not simmilar in shape so we cannot use mann-whitney U test. unequal variance, not normal.
qqnorm(subset(recycle, recyclingBinPresent=="absent")%>% pull(paperUsage)) #skewed left
qqnorm(subset(recycle, recyclingBinPresent=="present")%>% pull(paperUsage))

#distribution are not normal, cannot be transformed
#the non-parametric option is to use the mann-whitney U test
#to determine whether we use the normal approximation or not we need to know the sample size
subset(recycle, recyclingBinPresent=="absent")
subset(recycle, recyclingBinPresent=="present")
#one (both in fact) of our sample sizes are above 15 so we will use the normal approximation
#R function for mann-whitney with normal approximation is wilcox_test in the library "coin"
#install.packages("coin")
library("coin")
#usages is wilcox_test(values~binary factor of groups,distribution="exact",data=your data frame)
#specifing the exact P-value indicates we will calculate the exact P-value for our test statistic from the Z-distribution
wilcox_test(paperUsage~recyclingBinPresent,data=recycle,distribution="exact")
wilcox_test(paperUsage~as.factor(recyclingBinPresent),data=recycle,distribution="exact")
#what if both of our samples sizes are less than 15?
#in this case we can't use the normal approximation.
#we'll use hte function wilcox.test
#for mann-whitney, usage is wilcox.test(values 1, values 2)
#alternatively we can do wilcox.test(values~binary factor of groups)
#BEWARE!!!!!!
#!!!!!!!!!
#The value of W that is reported (which is the same was U) will be for the first group
#first group corresponding to values 1, or first level in your factor of groups
#however to evaluate the test significance the U value for the larger group will be evaluated against the null distribution
#therefore the W value the function reports isn't necessarily the value of your test statistic
#do the test both ways and take the test statistic that is larger for us in reporting your results.
#this will also tell you which group had higher ranks.
wilcox.test(recycle$paperUsage~recycle$recyclingBinPresent)
#reorder the levels
recycle$recyclingBinPresent <- factor(recycle$recyclingBinPresent, levels=c("present","absent"))
#do the test again:
wilcox.test(recycle$paperUsage~recycle$recyclingBinPresent)
#note that the test statistic changes, but the P-values does not. 
#higher U value is for the present which is 291.5

#let's try using a permutation test
#let's make a single permutation or our data using sample
recycle
mean(recycle$paperUsage)
permuted <- sample(pull(recycle,paperUsage),size=41) 
mean(permuted[1:22]) - mean(permuted[23:41])
#execute those lines again
#let's make a function called "permuted null so that we can make lots of replicates
permuted.null <- function(x,y,z) {permuted <- sample(x,size=length(x))
a <- y+1
b <- y+z
mean(permuted[1:y])-mean(permuted[a:b])}
#usage is permuted.null(vector of values, sample size group 1, sample size group 2)

#we'll use replicate to execute the permutation 100000 times
null <- data.frame("means" = replicate(100000, permuted.null(pull(recycle,paperUsage),19,22)))

#let's look at null 
ggplot(null, aes(means)) + geom_histogram()

#How can we calculate a P-value directly from the null? First we need a test statistic
mean(subset(recycle, recyclingBinPresent=="absent")%>% pull(paperUsage))-mean(subset(recycle, recyclingBinPresent=="present")%>% pull(paperUsage))
#how often do we see values this extreme or more extreme in the null?
dim(subset(null, means >= 7.755981))
dim(subset(null, means <= -7.755981))
(476+329)/100000

#to avoid sampling error, even with the very large number of replicates we'll use the z approximation instead
2*pnorm(7.755981,0,sqrt(var(null$means)),lower.tail=FALSE)

#ready for the shortcut? here it is
#use the function oneway_test in the package "coin"
#usage is oneway_test(vector of values ~ factor of groups)
oneway_test(recycle$paperUsage~as.factor(recycle$recyclingBinPresent))
#note the P-value and Z-score are the same as the ones you just calculated.
