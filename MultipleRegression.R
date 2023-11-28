#two factor anova with an interaction term
setwd("~/Documents/Biostats")
algae <- read.csv("algae.csv")
algae
#recode factors as factors to avoid downstream problems

algae$herbivores <- as.factor(algae$herbivores)
algae$height <- as.factor(algae$height)

library(ggplot2)

ggplot(algae, aes(height,sqrtArea, color=herbivores)) + geom_point()

#to examine both main effects and the interaction term
#multi or uni factorial
algae.aov <- aov(sqrtArea~herbivores + height +herbivores:height, algae)
#look at the type I AOV table
summary(algae.aov)

#we can also use short hand for examining both main effects and the interaction
algae.aov <- aov(sqrtArea~herbivores*height, algae)
#we can also run this as a linear regression
#look at the type I AOV table
#Type I- takes into account the order of the factors. sequential sum of squares usually when having apriori hypothesis where one treatment effects more.
anova(algae.aov)
# to obtain a type III ANOVA table use the "car package"
library("car")
Anova(algae.aov, type = 3)
#specify type = 2 in cases where interaction term is not significant

#compare all group means-- which treatment group has more effect
TukeyHSD(algae.aov,conf.level=0.95)


#contrast plus and minus herbivores
#generalize linear hypothesis function
summary(glht(algae.aov,linfct=mcp(herbivores=c(-1,1)),test=adjusted("none")))

#obtain effect estimates you must specify a regression model in lm
algae.lm<- lm(sqrtArea~herbivores*height, algae)
summary(algae.lm)
confint(algae.lm, level=0.95)



#Two factor anova with a random effect (randomized block design)
#fixed effect and random effect.
zooplankton <- read.csv("zooplankton.csv")
zooplankton
#let's check how things are coded, since numbered blocks will be treated as integers instead of factors.
lapply(zooplankton,class)
#let's recode block
zooplankton$block <- as.factor(zooplankton$block)
#let's fit the null and alternative aov model. 
#we use the function "lmer". Usage is essentially the same as lm.
#install.packages("lme4")
#install.packages("lmerTest")
library("lme4")
library("lmerTest")
#random effects are coded as (1|random effect)
#if you plan to test hypotheses about fixed effects, set REML=FALSE
zooplankton.alternative <- lmer(diversity~(1|block)+treatment,zooplankton,REML=FALSE)
zooplankton.null <- lmer(diversity~(1|block),zooplankton,REML=FALSE)
#to test the hypothesis that treatment explains significant variation in a type I framework we use 
#anova(alternative,null)
anova(zooplankton.alternative,zooplankton.null)
###this is not a good type 3 example but if you want to do type III with random effects
zooplankton.alternative <- lmer(diversity~treatment+(1|block),zooplankton,REML=TRUE)
Anova(zooplankton.alternative,type=3,test=c("F"))


#for fun, let's compare to the fixed effects only model
zooplankton.aovf <- aov(diversity~treatment+ block,zooplankton)
zooplankton.lm <- aov(diversity~treatment+ block,zooplankton)
summary(zooplankton.aovf)
#note the difference in the P-value, we get a more powerful test if we want code block as a random effect.

#what if we want to perform post-hoc comparisons between groups?
#we cannot use TukeyHSD on a mixed effect model so we'll have to use glht() (from multicomp)
library("multcomp")
summary(glht(zooplankton.alternative, linfct = mcp(treatment = "Tukey")), test = adjusted("holm"))
?glht
#obtain confidence intervals if you need them for group means
confint(glht(zooplankton.alternative,level=0.95))
#what if we want to compare only treatment to control
#custom contrasts are a little trickier, you should consult documentation but here is an example:
#first determine order of factor levels
levels(as.factor(zooplankton$treatment))
#to compare control no fish control to fish use contest usage is contest(model, L=c(specific contrasts))
contest(zooplankton.alternative, L = c(-1,.5,.5)) # adding fish effects diversity.
contest(zooplankton.alternative, L = c(0,1,-1))


#mole rats on your own 
molerats <- read.csv("moleRats.csv")
molerats
lapply(molerats,class)



molerats.aov <- aov(lnEnergy~lnMass + caste +lnMass:caste, molerats)
#look at the type I AOV table
summary(molerats.aov)
#we can also use short hand for examining both main effects and the interaction
molerats.aov <- lm(lnEnergy~lnMass*caste, molerats)
molerats2.aov <- lm(lnEnergy~lnMass + caste, molerats)
anova(molerats.aov,molerats2.aov)

molerats.lm <- lm(lnEnergy~lnMass*caste, molerats)
anova(molerats.lm)
anova(molerats2.aov)
summary(molerats.lm)

