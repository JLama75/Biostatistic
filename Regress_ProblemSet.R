
setwd("~/Documents/Biostats")
library(tidyverse)

brain <- read_csv("brainMetabolism.csv")
brain

# A tibble: 18 × 4
# species                  brainMass lnBrainMass gliaNeuronRatio
# <chr>                        <dbl>       <dbl>           <dbl>
#1 Homo sapiens                1373.         7.22            1.65
#2 Pan troglodytes              336.         5.82            1.2 
#3 Gorilla gorilla              509.         6.23            1.21
#4 Pongo pygmaeus               343.         5.84            0.98
#5 Hylobates muelleri           102.         4.62            1.22

ggplot(brain, aes(lnBrainMass,gliaNeuronRatio)) + geom_point()

#usage is lm(regression model, data)
# y= a+bx linear regression. as variance in y is normally distributed acrross x-values

brain.lm <- lm(gliaNeuronRatio~lnBrainMass,brain)
summary(brain.lm)

(y) =(a) +ln(b)lnx
(0.10107+(0.19744*7.22))
(0.10107+(1.21828*7.22))
#CI<- (1.1177074-1.935424)
#36.05617

ggplot(brain, aes(lnBrainMass,gliaNeuronRatio)) + geom_point() + 
  #let's add a regression line
  stat_smooth(method="lm")
summary(brain.lm)

confint(brain.lm, 'lnBrainMass',level=0.95)
confint(brain.lm,level = 0.95)

brain_nonhuman <- brain[-1,]
brain_nonhuman.lm <- lm(gliaNeuronRatio~lnBrainMass,brain_nonhuman)
summary(brain_nonhuman.lm)
#0.16370 0.18113
predictions <- data.frame("lnBrainMass"=seq(from = 2.2, to = 7.3, by = 0.01))
#now we need to generate the regression model we'll be predicting from
brain.lm <- lm(gliaNeuronRatio~lnBrainMass,brain)
#now we need to generact prediction intervals for individuals Y values for the range of X-values
predict(brain.lm, newdata=predictions,interval="prediction",level=0.95)
predict(brain.lm, newdata=predictions,interval="confidence",level=0.95)
#summary(brain.lm)
predictions <- cbind(predictions,predict(brain.lm, newdata=predictions,interval="prediction",level=0.95))
confidence <- cbind(predictions,predict(brain.lm, newdata=predictions,interval="confidence",level=0.95))
#1.328004-1.727690
#make a scatter plot inlcuding our prediction intervals
#this first line draws only the prediction line

predictions_nonhuman <- data.frame("lnBrainMass"=seq(from = 2.2, to = 7.3, by = 0.01))
predictions_nonhuman <- cbind(predictions_nonhuman,predict(brain_nonhuman.lm, newdata=predictions,interval="prediction",level=0.95))
confidence_nonhuman <- cbind(predictions_nonhuman,predict(brain_nonhuman.lm, newdata=predictions,interval="confidence",level=0.95))

##########################################################################
tree <- read_csv("TreeSeedling.csv")
tree

# A tibble: 21 × 2
#       fleckDuration growth
#              <dbl>  <dbl>
#  1           3.4  0.013
#  2           3.2  0.008
#  3           3    0.007

MS_res<-(0.001024-(0.002529688*0.2535))/(21-2)
SE <- sqrt(MS_res/100.210)
ggplot(tree, aes(fleckDuration,growth)) + geom_point()

#usage is lm(regression model, data)
# y= a+bx linear regression. as variance in y is normally distributed acrross x-values

tree.lm <- lm(growth~fleckDuration,tree)
summary(tree.lm)
#slope = 0.0025295
#intercept = -0.0017087
confint(tree.lm, 'fleckDuration',level=0.99)
#fleckDuration 0.001246901 0.003812023
confint(tree.lm, '(Intercept)',level=0.95)
anova(tree.lm)

names(tree.lm)
#make a new column in our data frame with the residuals
tree$residuals <- tree.lm$residuals
#plot them. I added a red line slope 0, intercept 0.
ggplot(tree, aes(fleckDuration,residuals)) + geom_point() + geom_abline(slope = 0, intercept = 0, colour = "red")


####################################################################################

daphnia <- read_csv("DaphniaParasiteLongevity.csv")
daphnia
daphnia.lm <- lm(sqrtSpores~longevity,daphnia)
summary(daphnia.lm)
ggplot(daphnia, aes((longevity),sqrtSpores)) + geom_point() +  geom_smooth()
ggplot(daphnia, aes(longevity,sqrtSpores)) + geom_point() +  geom_smooth()
ggplot(daphnia, aes(longevity,sqrtSpores)) + geom_point() +  geom_smooth(formula=y~x,method="lm")
ggplot(daphnia, aes(longevity,sqrtSpores)) + geom_point() +  geom_smooth(formula=y~log(x+1),method="lm")
ggplot(daphnia, aes(longevity,sqrtSpores)) + geom_point() +  geom_smooth(formula=y~poly(x,2),method="lm")

daphnia$residuals <- daphnia.poly.lm$residuals
#plot them. I added a red line slope 0, intercept 0.
ggplot(daphnia, aes(longevity,residuals)) + geom_point() + geom_abline(slope = 0, intercept = 0, colour = "red")

daphnia.poly.lm <- lm(sqrtSpores~poly(longevity,2),daphnia)
summary(daphnia.poly.lm)
anova(daphnia.poly.lm)
confint(daphnia.poly.lm, 'poly(longevity, 2)1',level=0.95)
confint(daphnia.poly.lm, 'poly(longevity, 2)2',level=0.95)
#2878.197 4351.83
3615.02

# 0.8152405
2587.58

#0.477783 to 0.6662545

#0.8877097
#-0.5555069 to 0.3512698

DEET <- read_csv("DEETMosquiteBites.csv")
DEET
ggplot(DEET, aes(dose,bites)) + geom_point() +geom_smooth()

DEET
DEET.lm <- lm(bites~dose,DEET)
DEET$residual <- DEET.lm$residuals

ggplot(DEET, aes(dose,residual)) + geom_point() + geom_abline(slope = 0, intercept = 0, colour = "red")


anova(DEET.lm)
#R2<- SSreg/SStotal

SStotal <- 9.9732 + 22.0837
R2 <-9.9732/SStotal
R2


Stork <- read_csv("storkStress.csv")
Stork
ggplot(Stork, aes(corticosterone,survival)) + geom_point() 


#we'll perform the logistic regression with glm
#usage is similar to lm()
#glm(formula,data,family="binomial")
#as the y has binomial outcome.
stork.logr <- glm(survival~corticosterone,Stork,family="binomial")
summary(stork.logr)
# null deviance-Residual deviance = 3.838
45.234-41.396
#calculate drop in deviance --> chi square decrease in predicted variance due to adding a model
3.838
#calculate P-value 
pchisq(3.838,1,lower.tail=FALSE) #0.05010325
#null deviance - estimate : intercept
#residual deviance - calculates two estimates :intercept and slope.
#df = 1

2.70304/0.07980
exp(-0.07980)
