
#practice problem 1
#first let's calculate our Z-statistic
Z.score <- (120-123.8)/3.1
Z.score

#then let's evaluate the probability of an observation as far from the mean or further (measured in SD)
#we'll use pnorm which will report the the probability of a value as extreme or more extreme than that we observe. 
?pnorm
#usage is pnorm(observed.value,mean,sd,lower.tail=TRUE/false). 
#defaults are mean = 0, sd = 1, i.e. the standard normal distribution
#is lower.tail = TRUE (default) probabilities are P[X ≤ x] i.e. probability of an observation less than or equal to value observed otherwise, P[X > x].
#in our case we are evaluating the probability of a value less than or equal to that we observed on the  standard normal, so default values are OK.
pnorm(Z.score)
pnorm((120-123.8)/3.1,lower.tail=FALSE)
pnorm(0.24,lower.tail=FALSE)
#if we don't care about the z score we can also let the pnorm function automatically convert our observed value to Z-scale by providing it with the mean and the standard deviation.
pnorm(120, mean = 123.8, sd = 3.1, lower.tail = F)
pnorm(9, mean = 14, sd = 5)
#part 2 with your classmates
pnorm()
#practice problem 2
dolfin <- c(77.7,84.8,79.4,84.0,99.6,93.6,89.4,97.2)
mean(dolfin)
SE <- sd(dolfin)/sqrt(8)
qt(0.025,7)
qt(0.975,7)
qt(0.001,7)
qt(0.99,7)
mean(dolfin) - 4.78529*SE
mean(dolfin) + 2.997952*SE
(mean(dolfin) - 50)/SE
#now let's evaluate the probability of the test statistic over the t distriubtion
#we'll use the function pt()
?pt
#usage is pt(t-value, df, lower.tail = TRUE/FALSE)
pt(13.35531,7,lower.tail=FALSE)


2*1.545735e-06


#first let's make a vector of all the observations
delta.elevation <- c(58.9,7.8,108.6,44.8,11.1,19.2,61.9,30.5,12.7,35.8,7.4,39.3,24,62.1,24.3,55.3,32.7,65.3,-19.3,7.6,-5.2,-2.1,31.0,69,88.6,39.5,20.7,89,69,64.9,64.8)
length(delta.elevation)
#let's look at this histogram
library("ggplot2")
ggplot(data.frame("delta.lat" = delta.elevation),aes(delta.elevation)) + geom_histogram(bins=7)
mean(delta.elevation)
sd(delta.elevation)/sqrt(31)
#we'll be usinng the function qt to obtain the critical value of t. for our desired significance level and df
?qt
#usage is qt(percentile/quantile,df,lower.tail=TRUE/FALSE)
#because t-distribution if symmetric, or a two sided test the Pr[X > x] = -Pr[X < x]
#for a significance level of 0.05 we are interested in a 2.5 and 97.5 percentiles
#our degrees of freedom are n-1=30
#what is the critical t-value that is larger than only 2.5% of observations
qt(0.025,30)

#what is the critical t-value that is greater than 97.5% of observations
qt(0.975,30)
#note that we can also ask what is the critical t-value that is less than only 2.5% of observations
qt(0.025,30,lower.tail=FALSE)
#to calculate a confidence interval we also need the standard error
SE <- sd(delta.elevation)/sqrt(31)
#lower bound of 95% CI = estimate - critical t-value * SE
mean(delta.elevation) - 2.042272*SE
#upper bound of 95% CI = estimate + critical t-value * SE
mean(delta.elevation) + 2.042272*SE
#first let's calulcate out test statistic or t-value
(mean(delta.elevation) - 0)/SE
#now let's evaluate the probability of the test statistic over the t distriubtion
#we'll use the function pt()
?pt
#usage is pt(t-value, df, lower.tail = TRUE/FALSE)
pt(7.141309,30,lower.tail=FALSE)
#note that this is a one-side P-value, for two-sided test:
2*pt(7.141309,30,lower.tail=FALSE)

sloth <- c(1.53,1.06,0.93,1.38,1.47,1.20,1.16)
mean(sloth)
sd(sloth)
#now for the grand shortcut using the function t.test
#for a one sample t-test the usage is t.test(vector of observations,alternative="two.sided"/"less"/"greater", mu = mean under null, conf.level = desired confidence level (default = 0.05))
#for this example, the mean under our null hypothesis is zero, which is the default
?t.test
t.test(delta.elevation)
#note that this function reports EVERYTHING, mean, test statistic, degrees of freedom, P-value, and CI
#to generate a 99% CI
t.test(delta.elevation, conf.level = 0.99)
t.test(dolfin,conf.level =0.95)

