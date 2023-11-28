setwd("~/Documents/Biostats")
library(tidyverse)
#load the data
caffeine <- read_csv("caffeene.csv")
caffeine
#plot the data
library("ggplot2")
ggplot(caffeine, aes(x=as.factor(ppmCaffeine),y=consumptionDifferenceFromControl)) + geom_boxplot() + geom_jitter() 
#unqeual variance, some distributions not normal. let's ignore that for now.
#are the means equal?
#we'll use function aov
#usage is aov(response~explanatory...,data.frame)
caffeine.aov <- aov(consumptionDifferenceFromControl~as.factor(ppmCaffeine),caffeine)
#using the function summary we can look at the ANOVA table
summary(caffeine.aov)
#n-k = df
#we can also perform tukey-HSD to compare the means
#we'll use TukeyHSD() 
TukeyHSD(caffeine.aov,conf.level=0.95)
#Assign the means to groups based on their p-values
#assign letters a/b, a, b, b
#how about a Kruskal Wallis test.
#we'll use the function kruskall.test(), same usage as aov
kruskal.test(consumptionDifferenceFromControl~ppmCaffeine,caffeine)
#seed dispersal with your classmates
dispersal <- read_csv("plants.csv")
dispersal
ggplot(dispersal, aes(x=as.factor(treatment),y=generations)) + geom_boxplot() + geom_jitter() 
#n=16, groups =4 each has 4 samples. Has few sample size so we could ignore normality.
dispersal.aov <- aov(generations~treatment, dispersal)
summary(dispersal.aov)
#Plant species persistance is dependent upon seed dispersal.
TukeyHSD(dispersal.aov,conf.level=0.95)
install.packages("agricolae")
library(agricolae)
HSD.test(plant.av, trt = 'group')
#Difference is between medium and isolated.


#establish contrast levels -- apriori biological hypothesis
#explicitly coding explanatory variables as factors is required for doing contrasts in glht
dispersal$treatment <- as.factor(dispersal$treatment)
aov.dispersal <- aov(generations~treatment,dispersal)
levels(dispersal$treatment)
#weighted groups. 
contrast.matrix <- rbind("dispersal vs none" = c(-1/3,1,-1/3,-1/3))
contrast.matrix
#evaluate contrast using glht
install.packages("multcomp")
library("multcomp")
#usage is summary(glht(aov.object,linfct=mcp(explantory.factor.name=contrast.matrix),test=adjusted("none")) ))
summary(glht(aov.dispersal,linfct=mcp(treatment=contrast.matrix),test=adjusted("none")))

