#we'll be using chisq.test() to perform the test of independence.
#one way to perform the test is based on a matrix of the contingency table
divorce <- matrix(c(7,254,53,201),nrow=2)
#take a look
divorce
#we can do chisq.test(matrix,correct=FALSE) 
#correct = FALSE indicates no continuity correction, which can be overly conservative Yates continuity correction-- false negative
chisq.test(divorce)
chisq.test(divorce, correct = FALSE)
#another way to perform the test is based on the full data set
#every row is a sampling unit.
patient <- c(rep("husband",261),rep("wife",254))
outcome <- c(rep("married",254),rep("divorced",7),rep("married",201),rep("divorced",53))
long.divorce <- data.frame(patient,outcome)
#let's take a look
long.divorce
#let's apply chisq.test
chisq.test(long.divorce$patient,long.divorce$outcome,correct=FALSE)
#same answer as above

#relative risk: random samples (probability of getting divorsed/not getting divorsed)

#practice problem #2
#first we make the matrix
spiders <- matrix(c(3,6,22,1),nrow=2)
#let's take a look
spiders
#let's take a look at what happens if we try to do chi-square
chisq.test(spiders,correct=FALSE)
#R tells us this is a bad idea. why?
#let's check the expected counts
#what's the total count
sum(c(3,6,22,1))
#use the short cut row total * column total / grand total
sum(spiders[1,])*sum(spiders[,2])/32
sum(spiders[2,])*sum(spiders[,2])/32
sum(spiders[1,])*sum(spiders[,1])/32
sum(spiders[2,])*sum(spiders[,1])/32
#one of the expected counts is less than 5, which is a violation of assumptions 25%
#so we need to use Fisher's exact test
#we'll use fisher.test()
#usage is same as chisq.test EXCEPT that we can specify a one sided or two sided test
fisher.test(spiders)

#also useful for calculating your odds ratio and confidence interval
fisher.test(spiders,conf.int = TRUE, conf.level = 0.95)
#practice problem #3 & 4 with your classmates epitools

mosq <- matrix(c(20,69,16,(173-16)), nrow = 2)
mosq
chisq.test(mosq,correct = FALSE)
#Feeding behavior is not independent of infection status.
sum(c(20,69,16,(173-16)))
#use the short cut row total * column total / grand total
sum(mosq[1,])*sum(mosq[,2])/262
sum(mosq[2,])*sum(mosq[,2])/262
sum(mosq[1,])*sum(mosq[,1])/262
sum(mosq[2,])*sum(mosq[,1])/262


jub <- matrix(c(11,1,6,4), nrow = 2)
jub
chisq.test(jub,correct = FALSE)
fisher.test(jub)
#is independent
