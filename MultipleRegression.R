#two factor anova with an interaction term
setwd("~/Documents/Biostats")
library(tidyverse)
algae <- read_csv("algae.csv")
names(algae)
algae
ggplot(algae, aes(x=as.factor(height),y=sqrtArea)) + geom_boxplot() + geom_jitter() 

#to examine both main effects and the interaction term
algae.aov <- aov(sqrtArea~herbivores + height +herbivores:height, algae)
#look at the AOV table
summary(algae.aov)
#we can also use short hand for examining both main effects and the interaction
algae.aov <- aov(sqrtArea~herbivores*height, algae)
#we can also run this as a linear regression'
lapply(algae,class)
algae.lm <- lm(sqrtArea~herbivores + height +herbivores:height, algae)
#look at the AOV table
anova(algae.lm)
#its the same. What are the F tests here?

#Two factor anova with a random effect (randomized block design)
zooplankton <- read.csv("zooplankton.csv",header=TRUE)
zooplankton
names(zooplankton)
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
#type 1- first account for block and then look at the effect of treatment.
zooplankton.alternative <- lmer(diversity~treatment+(1|block),zooplankton,REML=FALSE)
zooplankton.null <- lmer(diversity~(1|block),zooplankton,REML=FALSE)
#let's perform the likelihood ratio test. 
#we can do this with anova()
#anova(alternative,null)
#result for likelihood test.
anova(zooplankton.alternative,zooplankton.null)
#for fun, let's compare to the fixed effects only model
zooplankton.aovf <- aov(diversity~treatment+ block,zooplankton)
summary(zooplankton.aovf)
#note the difference in the P-value, we get a more powerful test if we want code block as a random effect.

#after accounting for the exp blocking to find which of the treatment groups has an effect.
#what if we want to perform contrasts?
#we cannot use TukeyHSD on a mixed effect model so we'll have to use glht() (from multicomp)
#we also need a new package, lsmeans
#install.packages("lsmeans")
library("lsmeans")
library("multcomp")
summary(glht(zooplankton.alternative,lsm(pairwise~treatment)))
#obtain confidence intervals if you need them 
confint(glht(zooplankton.alternative,lsm(pairwise~treatment)))
#what if we want to compare only treatment to control
#the reference class indicates which level of the factor is our reference. 
#look at the order of factor levels for treatment 
levels(zooplankton$treatment)
#our control is first, so we can our reference level is 1 (default)
summary(glht(zooplankton.alter,lsm(trt.vs.ctrl~treatment,ref=c(1))))
#custom contrasts are a little trickier, see lsmeans documentation




