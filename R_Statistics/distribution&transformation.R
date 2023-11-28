#let's learn how to evaluate some distributions with normal q-q plots
setwd("~/Documents/Biostats")
#first let's consider a normal distribution
sample.data <- data.frame("normal" = rnorm(100,5,1))
ggplot(sample.data, aes(normal,colour="")) + geom_histogram(bins=10) + scale_colour_manual(values=c("black")) + theme(legend.position="none")
qqnorm(sample.data$normal,datax=TRUE) 
qqnorm(sample.data$normal,datax=TRUE) + qqline(sample.data$normal,datax=TRUE)
qqnorm(sample.data$normal,datax=TRUE) + qqline(sample.data$normal,datax=TRUE,p=c(0.001,0.999))



#how about a right skewed distribution?
#install.packages("fGarch")
library("fGarch")
sample.data$right.skewed <- rsnorm(100,mean = 5,sd= 1,xi=1.5) 
ggplot(sample.data, aes(right.skewed,colour="")) + geom_histogram(bins=10) + scale_colour_manual(values=c("black")) + theme(legend.position="none")
ggplot(sample.data, aes(right.skewed,colour="")) + geom_histogram(bins=6) + scale_colour_manual(values=c("black")) + theme(legend.position="none")
qqnorm(sample.data$right.skewed,datax=TRUE) + qqline(sample.data$right.skewed,datax=TRUE,p=c(0.001,0.999))

#what if its even more skewed?
sample.data$right.skewed.5 <- rsnorm(100,mean = 5,sd= 1,xi=5)
ggplot(sample.data, aes(right.skewed.5,colour="")) + geom_histogram(bins=10) + scale_colour_manual(values=c("black")) + theme(legend.position="none")
ggplot(sample.data, aes(right.skewed.5,colour="")) + geom_histogram(bins=6) + scale_colour_manual(values=c("black")) + theme(legend.position="none")
qqnorm(sample.data$right.skewed.5,datax=TRUE) + qqline(sample.data$right.skewed.5,datax=TRUE,p=c(0.001,0.999))

#what if its left skewed?
sample.data$left.skewed.5 <- rsnorm(100,mean = 5,sd= 1,xi=-5)
ggplot(sample.data, aes(left.skewed.5,colour="")) + geom_histogram(bins=10) + scale_colour_manual(values=c("black")) + theme(legend.position="none")
ggplot(sample.data, aes(left.skewed.5,colour="")) + geom_histogram(bins=6) + scale_colour_manual(values=c("black")) + theme(legend.position="none")
qqnorm(sample.data$left.skewed.5,datax=TRUE) + qqline(sample.data$left.skewed.5,datax=TRUE,p=c(0.001,0.999))

#now let's consider a flat unpeaked distribution 
sample.data$flat <- seq(0.1,10,0.1)
ggplot(sample.data, aes(flat,colour="")) + geom_histogram(bins=10) + scale_colour_manual(values=c("black")) + theme(legend.position="none")
ggplot(sample.data, aes(flat,colour="")) + geom_histogram(bins=100) + scale_colour_manual(values=c("black")) + theme(legend.position="none")
qqnorm(sample.data$flat,datax=TRUE) + qqline(sample.data$flat,datax=TRUE,p=c(0.001,0.999))

#what happens if the distribution is bimodal?
sample.data$bimodal <- c(rnorm(50,5,1),rnorm(50,15,1))
ggplot(sample.data, aes(bimodal,colour="")) + geom_histogram(bins=10) + scale_colour_manual(values=c("black")) + theme(legend.position="none")
ggplot(sample.data, aes(bimodal,colour="")) + geom_histogram(bins=6) + scale_colour_manual(values=c("black")) + theme(legend.position="none")
qqnorm(sample.data$bimodal,datax=TRUE) + qqline(sample.data$bimodal,datax=TRUE)

#let's learn how to apply shapiro-wilks
#we'll use the function shapiro.test()
#usage is shapiro.test(vector of values)
shapiro.test(sample.data$normal)

#on your own, try out some of the other distributions we made. What do you observe?

#let's consider some transformations
shapiro.test(sample.data$left.skewed.5)
qqnorm(sample.data$left.skewed.5,datax=TRUE) + qqline(sample.data$left.skewed.5,datax=TRUE,p=c(0.001,0.999))
#let's try the square transformation, which is thought to be good for left.skewed data.
qqnorm(sample.data$left.skewed.5^2,datax=TRUE) + qqline(sample.data$left.skewed.5^2,datax=TRUE,p=c(0.001,0.999))
shapiro.test(sample.data$left.skewed.5^2)
qqnorm(1/sample.data$left.skewed.5,datax=TRUE) + qqline(1/sample.data$left.skewed.5,datax=TRUE,p=c(0.001,0.999))
shapiro.test(1/sample.data$left.skewed.5)

#try on your own with right skewed and bimodal data

#let's work with the log-transformed right-skewed data
?log()
shapiro.test(sample.data$right.skewed)
qqnorm(sample.data$right.skewed,datax=TRUE) + qqline(sample.data$right.skewed,datax=TRUE,p=c(0.001,0.999))
shapiro.test(log(sample.data$right.skewed))
qqnorm(log(sample.data$right.skewed),datax=TRUE) + qqline(log(sample.data$right.skewed),datax=TRUE,p=c(0.001,0.999))
#let's satisy ourselves that the base does not matter
shapiro.test(log(sample.data$right.skewed,base=2))
#let's generate a 95%CI for the mean
#remember the true population mean is 5
t.test(log(sample.data$right.skewed),mu=log(5))
#now we have to back-transform the values 
exp(1.537251)
exp(1.624554)
#for fun, let's compare that to our untransformed data
t.test(sample.data$right.skewed,mu=5)
