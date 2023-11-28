setwd("~/Documents/Biostats")
library("tidyverse")
#import the data and take a look
seals <- read.csv("seals.csv", header=TRUE)
seals
#to compare the mean oxygen consumption between the two groups we can use the function t.test again
#usage when comparing two means is
#t.test(observed values group 1, observed values group 2, alternative="two.sided"/"less"/"greater", paired = TRUE/FALSE, var.equal = TRUE/FALSE(default),conf.level= your desired conf.)
#this is test is two sided (Default)
#this test is paired to paired = TRUE
#the proposed difference in means under the null is zero
#variances are assumed equal under paired T
#we'll use conf.level = 0.05 (default)
t.test(seals$Oxygen.use.feeding,seals$Oxygen.use.nonfeeding,paired=TRUE,conf.level = 0.99)
#or using the tidy command pull
t.test(pull(seals,Oxygen.use.feeding),pull(seals,Oxygen.use.nonfeeding),paired=TRUE)



#import the data and take a look
lizards <- read.csv("lizards.csv", header=TRUE)
lizards
#this test is two sided
#the data are not paired
#we will assume that variances are equal
#we'll use conf.level = 0.05
t.test(subset(lizards,Survive == 0)$Squamosal.horn.length,subset(lizards,Survive == 1)$Squamosal.horn.length,paired=FALSE,var.equal=TRUE)
subset(lizards,Survive == 1)$Squamosal.horn.length
#we get an annoying error because a missing value caused R to interpret horn length as a factor 
#a quick way to deal with this is to specify "." as NA when you import the file 
lizards <- read.csv("lizards.csv", header=TRUE, na.string = ".")
t.test(subset(lizards,Survive == 0)$Squamosal.horn.length,subset(lizards,Survive == 1)$Squamosal.horn.length,paired=FALSE,var.equal=TRUE)
# or in tidyR
t.test(filter(lizards, Survive==0) %>% pull(Squamosal.horn.length),filter(lizards, Survive==1) %>% pull(Squamosal.horn.length),paired=FALSE,var.equal=TRUE)
#supports the alternative hypothesis, consistent with biological hypothesis 


df <-data.frame(matingsystem=c(rep("monogamous",4), rep("polyandrous"), 4), tetes.area=c(0.83,0.85,0.82,0.89,0.96,0.94,0.99,0.91))
t.test(df$matingsystem$testes.area,paired=TRUE,conf.level = 0.99)
#or using the tidy command pull
t.test(c(0.83,0.85,0.82,0.89), c(0.96,0.94,0.99,0.91),paired=FALSE,var.equal = TRUE)
#

