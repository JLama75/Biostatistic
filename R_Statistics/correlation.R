library("tidyverse")
giggles <- read_csv("giggles.csv")
giggles
#we'll use cor.test to estimate the correlation and test the hypothesis that it is significantly different from zero.
#usage is cor.test(values of numeric variable 1, values of numeric variable 2)
cor.test(pull(giggles,Age_years),pull(giggles,Fundamental_frequency_Hz))
#if we are worried about heteroscedasticity, we can caulcate spearman's rho by specifying method="spearman"
install.packages("pspearman")
library("pspearman")
#usage is the same as cor.test except that we can specify an approximation using the "approximation" option
#in this case the sample size is >>100 so we use AS89. If sample size is n >100 use approximation="t-distribution
spearman.test(pull(giggles,Age_years),pull(giggles,Fundamental_frequency_Hz),approximation="AS89")


#practice problem #2 with your classmates
resistance <- read_csv("chap16q03TBResistance.csv")
