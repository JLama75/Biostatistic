setwd("~/Documents/Biostats")
#random sampling -20 but as the size of the replicates of random sampling increase it approaches true mean
gene.lengths <- read_csv("04e1HumanGeneLengths.csv")
gene.lengths
ggplot(gene.lengths, aes(gene.length,color="genes")) + coord_cartesian(xlim=c(0,15000)) + geom_histogram(binwidth=500,center=250,aes(fill="")) + scale_fill_manual(values=c("darkred")) +scale_colour_manual(values=c("black")) + theme_bw() + theme(legend.position="none") + xlab("Gene Length (number of nucleotides)") + ylab("population frequency") + ggtitle("Genes in the Human Genome")
mean(gene.lengths$gene.length)
#let's look at a random sammple of 20 genes.
subset20 <- data.frame("gene.length" = sample(gene.lengths$gene.length,20))
ggplot(subset20, aes(gene.length,color="genes")) + coord_cartesian(xlim=c(0,15000)) + geom_histogram(binwidth=500,center=250,aes(fill="")) + scale_fill_manual(values=c("darkred")) +scale_colour_manual(values=c("black")) + theme_bw() + theme(legend.position="none") + xlab("Gene Length (number of nucleotides)") + ylab("population frequency")
#the mean is also going to change:
mean(subset20$gene.length)
#let's generate a sampling distribution of the mean, based on 1000 replicates of sample size 20
means.20 <- data.frame("gene.length" = replicate(2000, {mean(sample(gene.lengths$gene.length,20))}))
ggplot(means.20,aes(gene.length,color="")) + geom_histogram(binwidth=100,center=50,aes(fill="")) + scale_fill_manual(values=c("goldenrod1")) +scale_colour_manual(values=c("black")) + theme_bw() + theme(legend.position="none") + xlab("\nAverage Gene Length") + ylab("frequency among 1000 replicate samples\n")  + ggtitle("Sampling Distribution of Mean Gene Length (n = 20)")
#let's plot the distribution and the sampling distribution of the mean side by side
means.20$n <- "sampling distribution of the mean"
gene.lengths$n <- "population distrution of values"
ggplot(rbind(means.20,gene.lengths),aes(gene.length,fill=n,colour=n)) + geom_histogram(binwidth=200,center=50) + scale_fill_manual(values=c("darkred","goldenrod1")) +scale_colour_manual(values=c("black","black")) + theme_bw() + theme(legend.position="none") + xlab("Gene Length") + ylab("frequency") + coord_cartesian(xlim=c(0,15000)) + facet_grid(n~.,scales="free")
#how do the distributions differ?
#Let's compare the means
#mean gene length
mean(gene.lengths$gene.length)
#mean of the sampling distribution size 20
mean(means.20$gene.length)
ggplot(means.20,aes(gene.length,color="")) + geom_histogram(binwidth=100,center=50,aes(fill="")) + scale_fill_manual(values=c("goldenrod1")) +scale_colour_manual(values=c("black")) + theme_bw() + theme(legend.position="none") + xlab("\nAverage Gene Length") + ylab("frequency among 1000 replicate samples\n") + coord_cartesian(xlim=c(0,8000)) + ggtitle("Sampling Distribution of Mean Gene Length (n = 20)")

#what happens when we increase our sample size?
means.20$n = "20"
means.200 <- data.frame("gene.length" = replicate(1000, {mean(sample(gene.lengths$gene.length,200))}),"n" = 200)
ggplot(rbind(means.20,means.200),aes(gene.length,color="")) + geom_histogram(binwidth=100,center=50,aes(fill="")) + scale_fill_manual(values=c("goldenrod1")) +scale_colour_manual(values=c("black")) + theme_bw() + theme(legend.position="none") + xlab("Mean Gene Length") + ylab("frequency") + coord_cartesian(xlim=c(0,8000)) + facet_grid(n~.)
#shall we go up to sample sizes of 2000
means.2000 <- data.frame("gene.length" = replicate(1000, {mean(sample(gene.lengths$gene.length,2000))}),"n" = 2000)
ggplot(rbind(means.20,means.200,means.2000),aes(gene.length,color="")) + geom_histogram(binwidth=100,center=50,aes(fill="")) + scale_fill_manual(values=c("goldenrod1")) +scale_colour_manual(values=c("black")) + theme_bw() + theme(legend.position="none") + xlab("Mean Gene Length") + ylab("frequency") + coord_cartesian(xlim=c(0,8000)) + facet_grid(n~.)
#what about the means
tapply(rbind(gene.lengths,means.20,means.200,means.2000)$gene.length,rbind(gene.lengths,means.20,means.200,means.2000)$n,mean)
#uncertainty decreases.


