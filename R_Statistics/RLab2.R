#call ggplot into your R session
library("tidyverse")
#includes ggplot, dplyr
#let's work with mpg take a look
mpg



#let's make a histogram city mileage
ggplot(mpg, aes(cty)) + geom_histogram()


#R suggests we might want to change the binwidth, how?
#putting a question mark in front of the command will pull up the help document
?geom_histogram
ggplot(mpg, aes(cty)) + geom_histogram(binwidth=10)
ggplot(mpg, aes(cty)) + geom_histogram(bins=30)
#explore different bin numbers and widths? Which do you think is the best? How would you describe this distribution?
ggplot(mpg, aes(cty)) + geom_histogram(binwidth=5)
ggplot(mpg, aes(cty)) + geom_histogram(binwidth=3)
#its pretty peaked and symmetric, maybe a little skewed
#what measure of location would you use?


#### ESTIMATING LOCATION############
#to estimate the position first we need to extract the column of data we are interested in as a vector of values
#pull command works well for this
pull(mpg, cty) 
#alternatively if you prefer you call also use the base R notation separating the name of the tibble or data frame and the name of the desired column by a $
mpg$cty

# to calculate the mean, we can pipe the values to the mean function
pull(mpg, cty)  %>% mean
# or the median
pull(mpg, cty)  %>% median
# what about mode
pull(mpg, cty)  %>% mode
#oh wait that is just telling us what type of variable mpg is. 
# In fact, there is not internal function for calculating the mode in R 
# however the table function will tell us how many times each value is observed in a list
pull(mpg, cty)  %>% table 
#use sort to place them in ascending order:
pull(mpg, cty)  %>% table %>% sort 

#### ESTIMATING WIDTH ############
#variance
pull(mpg, cty) %>% var
#standard deviation
pull(mpg, cty) %>% sd
#can anyone think of another way to calcuate SD?
pull(mpg, cty) %>% var %>% sqrt
#what about sum of squares?
#first we need the sample size
pull(mpg, cty) %>% length -1
#then multiply the sample size by the variance
pull(mpg, cty) %>% var*233

#what about coefficient of variation?
100*(pull(mpg, cty) %>% sd)/(pull(mpg, cty) %>% mean)
#What about IQR
pull(mpg, cty) %>% quantile
pull(mpg, cty) %>% IQR # diff between 75% to 14%


### THE LAZY MAN'S SHORTCUT ####

pull(mpg, cty) %>% summary
#even lazier
mpg %>% summary






#### categorical #####
###### BAR GRAPHS ######
#take a look at your data
mpg
#let's make a bargraph of the frequency of different classes of cars
ggplot(mpg, aes(class)) + geom_bar()

#what if we want to consider also the class of car, now we are relating two categorical variables
ggplot(mpg, aes(class, fill = manufacturer)) + geom_bar()


#what if we want to make a proportional bar chart that makes it easier to compare across classes?
ggplot(mpg, aes(class, fill = manufacturer)) + geom_bar(position="fill") 



#### relating categorical and continuous variables #####
###### BOX, VIOLIN and JITTER PLOTS ######
#what if we want to look at the relationship between city fuel efficiency and class?
ggplot(mpg, aes(class, cty)) + geom_boxplot()
ggplot(mpg, aes(class, cty)) + geom_violin()
#what if we want to color our violins
ggplot(mpg, aes(class, cty, fill = manufacturer)) + geom_violin()
#or maybe we just want colorful borders on them
ggplot(mpg, aes(class, cty, color = class)) + geom_violin()
#how about we just plot the data?
ggplot(mpg, aes(class, cty, color = class)) + geom_jitter()
#or how about we do it all?
ggplot(mpg, aes(class, cty, color = class)) + geom_boxplot() + geom_jitter() 
#what if we want just our dots to be colored?
ggplot(mpg, aes(class, cty)) + geom_boxplot() + geom_jitter(aes(color=class)) 
#what if we want our boxes overtop of our data points?
ggplot(mpg, aes(class, cty)) + geom_jitter(aes(color=class)) + geom_boxplot() 
#make boxes transparent?
ggplot(mpg, aes(class, cty)) + geom_jitter(aes(color=class)) + geom_boxplot(alpha=0) 
#ggplot(mpg, aes(class, cty)) + geom_boxplot() + geom_jitter(aes(color=class, alpha=0))

#### relating two continuous variables #####
##### SCATTERPLOTS, LINE PLOTS #####
# how is city mileage related to highway mileage? 
ggplot(mpg, aes(cty, hwy)) + geom_point()
#what if we want a trendline?
ggplot(mpg, aes(cty, hwy)) + geom_point() + geom_smooth()
ggplot(mpg, aes(cty, hwy)) + geom_point() + geom_smooth(method="lm")
#95 percent confidence interval
# what if we want to see how that breaks down by class of car?
ggplot(mpg, aes(cty, hwy, color = class)) + geom_point()

diamonds

######## YOUR TURN ########
#lets work with the included data diamonds.
diamonds
#how many variables are in the data? take note also of what types of variables they are (ordinal, continuous, factors, etc)
#10 (continous, ordinal, ordinal, ordinal, continous, continous, descrete, continous, continous, continous)
#### make a graph relating size (in carats) and price. what is the trend that you observe?
ggplot(diamonds, aes(carat, price)) + geom_point() + geom_smooth()
#positive
#### make a graph relating clarity and price. what is the trend that you observe?
ggplot(diamonds, aes(clarity, price, fill = clarity)) + geom_boxplot()
#negative
#### make a graph relating depth and price. what is the trend that you observe?
ggplot(diamonds, aes(price, depth)) + geom_point() + geom_smooth()
#no change
#### make a graph relating color and price. what is the trend that you observe?
ggplot(diamonds, aes(color, price, fill = color)) + geom_boxplot()
#price increases with color
#### what is most predictive of diamond price: size, clarity, depth or color?
#carat
#### what is the average cost of large diamond (3 karats or greater)
carat <- diamonds %>% subset(carat >= 3) 
carat$price %>%  mean #15243
#### what is the average cost of an ideal cut diamond?
diamonds
diamonds %>% subset(cut == "Ideal") %>% pull(price) %>% mean
#command up
#3457.542
