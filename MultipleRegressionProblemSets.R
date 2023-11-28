setwd("~/Documents/Biostats")
#Solcing multiple Regression problem sets

GeneExpr <- read.csv("Gene_ExpressionData.csv")

library("lme4")
library("lmerTest")
library("tidyverse")
library("Rmisc")

GeneExpr
#   expression background allele rep
#1   0.8217885          A      1   1
#2   0.8351617          A      1   1
#3   0.8184126          A      1   1
#4   1.0367981          B      2   1
#5   1.0444706          B      2   1
#6   1.0378711          B      2   1
#7   1.0515868          C      2   1

lapply(GeneExpr,class)
GeneExpr$allele <- as.factor(GeneExpr$allele)
GeneExpr$rep <- as.factor(GeneExpr$rep)

#fitting linear mixed-effects models
GenExpr.alternative <- lmer(expression~(1|rep)+background+allele,GeneExpr,REML=FALSE) # with block +treatment
GenExpr.null <- lmer(expression~(1|rep),GeneExpr,REML=FALSE)# only block

#to test the hypothesis that treatment explains significant variation in a type I framework we use 
#anova(alternative,null)

anova(GenExpr.alternative,GenExpr.null) 
#Output: GenExpr.alternative    6 -59.658 -47.724 35.829  -71.658 36.046  3  7.322e-08 ***
############################################
#Plots

#install.packages("RColorBrewer") values = colorRampPalette(brewer.pal(12, "Accent")
library(RColorBrewer)
display.brewer.all() 
library(ggplot2)


GeneExpr$background <- factor(GeneExpr$background, levels = c("A", "B", "C"),
                  labels = c("Background A","Background B", "Background C")
)

GeneExpr$allele <- factor(GeneExpr$allele,  levels = c("1", "2"),
                          labels = c("allele1","allele2"))

ggplot(GeneExpr, aes(allele,expression, color=background)) + geom_jitter(alpha=0.5,size=3) +
  geom_boxplot(alpha=0.7, aes( fill = background)) + 
   theme_bw() + 
  facet_wrap(GeneExpr$background,nrow=1) + 
  scale_color_brewer(palette= "Dark2") + scale_fill_brewer(palette= "Dark2") +
  xlab("") + ylab("Relative Gene expression\n") +
  theme(axis.text=element_text(size=18),axis.title =element_text(size=20),
        strip.text = element_text(size = 18), legend.text = element_blank(),
        legend.title = element_blank(), legend.position = "none")

#deebf7 + scale_x_discrete(breaks=c("0.5","1","2"),labels=c("Dose 0.5", "Dose 1", "Dose 2"))
#9ecae1
#3182bd


#---------------------------------------------------------------------------------------
GenExpr.alternative <- lmer(expression~background +(1|rep)+allele+background:allele,GeneExpr,REML=FALSE) # with block +treatment
GenExpr.null <- lmer(expression~(1|rep)+background+allele,GeneExpr,REML=FALSE)# without interaction
GenExpr.lm <- lm(expression~background+allele,GeneExpr)

#to test the hypothesis that background:allele interaction explains significant variation in a type I framework we use 
#anova(alternative,null)
anova(GenExpr.alternative,GenExpr.null) 

#interaction is not needed.
#We won't include interaction term in our new model.

GenExpr.null

#Does expression differ between alleles? Use type I anova to account for covariate.
anova(GenExpr.null, type = "I")
anova(GenExpr.lm)
#F value 2,1- 39.8973, P-value-6.53e-08 

#to look at the effect estimates:
summary(GenExpr.null)
confint(GenExpr.null, level=0.95)
#0.13384121 0.25763990
#5.1494      2    0.07617 

ranova(GenExpr.null)
#hence block is appropriate to include in the model
ranef(GenExpr.null)

#library("Rmisc")

Marsh <- read.csv("MarshData.csv")
marsh_edit <- Marsh %>% group_by(Year,Zone) %>% summarise(mean = mean(Aboveground))
#m <- summarySE(Marsh, measurevar="len", groupvars=c("supp","dose"))

ggplot(marsh_edit, aes(Year,mean)) + geom_point(alpha=0.5,size=3,aes(color=Zone))   +
  geom_line(aes(color=Zone,linetype=Zone))+
  theme_bw() +scale_color_brewer(palette= "Dark2") 

Marsh.full <- lmer(Aboveground~(1|Year)+Temperature_growing_season+Precipitation+River_discharge+Mean_Seal_Level+Mean_Tide_Range+SPDI+Zone,Marsh,REML=FALSE) # with block +treatment
Marsh.null <- lmer(expression~(1|rep),GeneExpr,REML=FALSE)# only block

#Is block important?
Marsh.full <- lmer(Aboveground~(1|Year)+Zone,Marsh,REML=FALSE) # with block +treatment
Marsh.null <- lmer(Aboveground~Zone,Marsh,REML=FALSE)

anova(Marsh.full,Marsh.null)
#Yes chi-square- 186.4      1  < 2.2e-16 ***

anova(Marsh.full)
anova(Marsh.null)

#Is interaction imp?

Marsh.full <- lmer(Aboveground~(1|Year)+Zone+(1|Year:Zone),Marsh,REML=FALSE) # with block +treatment
Marsh.null <- lmer(Aboveground~(1|Year)+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)

#Yes. 13.056      1  0.0003024 ***

library(car)
anova(Marsh.full, type = "I")
summary(Marsh.full)
#Does temp have effects?

Marsh.full <- lmer(Aboveground~(1|Year)+Temperature_growing_season+Zone+(1|Year:Zone),Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+Zone+(1|Year:Zone),Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)
#No

#Does temp interaction with zone have effects?

Marsh.full <- lmer(Aboveground~(1|Year)+Temperature_growing_season:Zone+Zone+(1|Year:Zone),Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)
#No

#Does precipitation  have effects?

Marsh.full <- lmer(Aboveground~(1|Year)+Precipitation+Zone+(1|Year:Zone),Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)
#No
#without considering block does precipitation have effect
Marsh.full <- lm(Aboveground~Precipitation+Zone,Marsh)
Marsh.null <- lm(Aboveground~Zone,Marsh)
anova(Marsh.full,Marsh.null)

#No

#Does precipitation interaction with zone have effects?

Marsh.full <- lmer(Aboveground~(1|Year)+Precipitation+Precipitation:Zone+Zone+(1|Year:Zone),Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+Precipitation+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)

#No
#Does precipitation interaction with temperature have effects?
Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+Precipitation:Temperature_growing_season+Zone,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)

#NO

#Does river discharge have effects?

Marsh.full <- lmer(Aboveground~(1|Year)+Zone+(1|Year:Zone)+River_discharge,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)
#Yes it does 13.971      1  0.0001856 ***

#Does river discharge interaction with zone have effects?

Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Zone,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+Zone+River_discharge,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)

#Yes, 14.568      1  0.0001352 ***

#Does river discharge interaction with temperature have effects?
Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+River_discharge:Temperature_growing_season+Zone,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)

#No

#Does river discharge interaction with ppt have effects?
Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+River_discharge:Precipitation+Zone,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)
#No

#Does Mean_Seal_Level have effects?

Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Mean_Seal_Level+Zone,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)
#NO

#Does mean sea level interaction with zone have effects?
Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Mean_Seal_Level:Zone+Zone,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)
#NO

#Does mean sea level interaction with ppt have effects?
Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Mean_Seal_Level:Temperature_growing_season+Zone,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)

#NO

#Does mean sea level interaction with temp have effects?
Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Mean_Seal_Level:Precipitation+Zone,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)
#NO


#Does Mean_Tide_Range have effect
Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Mean_Tide_Range+Zone,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)
#NO

#Does Mean_Tide_Range have  interaction effect with zone
Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Mean_Tide_Range:Zone+Zone,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)
#Yes 7.7888      2    0.02036 *


#Does Mean_Tide_Range have interaction effect with others too?
Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Mean_Tide_Range:Zone+Mean_Tide_Range:Temperature_growing_season+Zone,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Mean_Tide_Range:Zone+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)
#NO

#Does SPDI  or its interaction have effect
Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Mean_Tide_Range:Zone+SPDI:Mean_Seal_Level+Zone,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Mean_Tide_Range:Zone+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)
#NO


#Does SPDI  or its interaction have effect
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+Zone+ Mean_Tide_Range:Zone:River_discharge,Marsh,REML=FALSE)
Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Mean_Tide_Range:Zone+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)


anova(Marsh.full)
anova(Marsh.null)


#Does SPDI  or its interaction have effect
Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Mean_Tide_Range:Zone+Zone,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+Zone+ Mean_Tide_Range:Zone:River_discharge,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)


Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+SPDI+SPDI:Zone+Zone,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+SPDI+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)
#8.5939      1   0.003373 **


Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+SPDI+SPDI:Zone+Zone +Temperature_growing_season ,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+SPDI+SPDI:Zone+Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)

Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+SPDI+SPDI:Zone+Zone+River_discharge:Zone+Mean_Tide_Range:Zone,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+SPDI+SPDI:Zone+Zone+River_discharge:Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)


Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+Zone+Precipitation+River_discharge:Zone+Mean_Tide_Range:Zone,Marsh,REML=FALSE)
Marsh.null <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+Zone+River_discharge:Zone+Mean_Tide_Range:Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.null)
anova(Marsh.full)

Marsh.full.n <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Mean_Tide_Range:Zone+Zone,Marsh,REML=FALSE)
anova(Marsh.full.n)

anova(Marsh.full,Marsh.full.n)
lapply(Marsh,class)

Marsh.full.n <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+River_discharge+River_discharge:Zone+Mean_Tide_Range:Zone+Zone,Marsh,REML=FALSE)
Marsh.full <- lmer(Aboveground~(1|Year)+(1|Year:Zone)+Zone+Precipitation+River_discharge:Zone+Mean_Tide_Range:Zone,Marsh,REML=FALSE)
anova(Marsh.full,Marsh.full.n)
summary(Marsh.full.n)
confint(Marsh.full.n$River_discharge, level=0.95)


#River_discharge+Zone+River_discharge:Zone+Mean_Tide_Range:Zone
