##you can install a package using the install.packages command
#install.packages("tidyverse")
## when you start a session in R you will want to load an packages you need for your analysis
library(tidyverse)
## you will also want to specify a directory where any files you write can be written and or, raw data files are located that you need to analyze.
setwd("~/Documents/Biostats")


### data frames are tabular data objects supported by base R
#let's take a look at a data frame called iris. To look at an object in R, we just type the name and it will appear on our console
iris

#tibbles are the tidy version
#we can print our data frame as a tibble
iris %>% as_tibble
#you can read the line above kind of like a sentence "take the data frame iris and print it as a tibble" the %>% part is called a pipe, and it makes is very easy to specify a sequence of things you want to do to your data frame.

#let's make a new object that is a tibble of the iris data, so we can work with it"
iris.tbl <- iris %>% as_tibble
# "<-" is called the "assignment operator". 
# It assigns the output we just made (the tibble) to a new object so we can work with it more.  
# you can think of the assignment operator like save as, saving what you are doing on the right hand side of the expression to an object by the name of the left hand side of the epxression.
#you can call objects whatever you want, but I'll try to end tibbles with .tbl in this class as a signifier
#now let's print the tibble we just made
iris.tbl

#let's explore our tibble more
#summary is a base R funcation that summarizes a data frame or a tibble
iris.tbl %>% summary

#notice that there are three iris species in the data frame.
#what if we only want data from virginica?
iris.tbl %>% filter(Species == "virginica")
iris.tbl %>% filter(Species != "virginica")
#note that the "==" is important to indicate an exact character or value match.
iris.tbl %>% filter(Species = "virginica")
#gives you an error prompting you to use the "==" this is one nice feature of tidyverse, base R won't help you out this way

#what if we only want the observations that exceed some numeric value
iris.tbl %>% filter(Sepal.Length > 5)

#notice that there are three iris species in the data frame.
#what if we want to remove the species name variable from the table
iris.tbl %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#the select command allows you to pick only the columns that you want, specifying them by their names
#if it's easier, you can also just say what columns you don't want by using the minus sign
iris.tbl %>% select(-Species)

#what if we want sort our table by a particular variable?
#arrange allows us to sort
iris.tbl %>% arrange(Sepal.Length)
#by default, we sort ascending if we want to sort descending...
iris.tbl %>% arrange(desc(Sepal.Length))

#what if we want calculate a new variable, such as the length to width ratio?
#we use the mutate function

iris.tbl %>% mutate(LW.ratio = Petal.Length/Petal.Width)
#note that in the expression above we use "=" not "==" because we are writing a mathematical formula for calculating the value of the new variable.


#note that in the pipe expressions above, we are printing the output to our console but not saving the changes we've made to our tibble object. What if we want to save?
# there is a safe and clunky option and a powerful and dangerous option. 
#The safe and clunky option is to assign the output to a new object.
iris.v2.tbl <- iris.tbl %>% mutate(LW.ratio = Petal.Length/Petal.Width)
#take a look
iris.v2.tbl
#the more dangerous option is to overwrite your original tibble
iris.tbl <- iris.tbl %>% mutate(LW.ratio = Petal.Length/Petal.Width)
#take a look
iris.tbl
#the even powerful and dangerous alternative to is to use a combined pipe and assignment operator %<>%. 
# I won't be teaching you that, but if you can figure it out for yourself then you are probably responsible enough to use it.

#to save your work you have two options. First, you can save it as an R object using save
iris.tbl %>% save(file="iris.tbl")
#you can load these files into R with the load command. They will have the same name as the object you saved
load("iris.tbl")
#alternatively, if your fuddy-duddy PI wants to see your data in a spreadsheet then you write it as a csv file that can be easily imported into excel
iris.tbl %>% write_csv("data_for_old_people.csv")



#lastly, let's talk about getting data into R
#find the textbook datasets you downloaded, and set the folder for chapter 2 as your working directory
setwd("~/Documents/Biostats")
#we'll import the hemoglobin data
#to make a data frame
finch.df <- read.csv("xid-43694303_1.csv")
#take a look
finch.df
#to make a tibble
finch.tbl <- read_csv("xid-43694303_1.csv")
#take a look
finch.tbl

#make me a modified table in which
# 1) you've made a new column that is the ratio of beaklength to mass (mm/g) named length.by.mass
# 2) you've selected only the cutthroat sparrow ("CUTTHROA" in the species column)
# 3) sorted by descending length.by.mass
colnames(finch.tbl)

B <- finch.tbl %>% mutate(length.by.mass = beaklength.mm/mass.g) %>% filter(species == "CUTTHROA")  %>% arrange(desc(length.by.mass))

#ctr shift M 
#finch1.tbl <- finch.tbl %>% mutate(length.by.mass = beaklength.mm/mass.g)
#finch1.tbl <- finch1.tbl %>% filter(species == "CUTTHROA")
#finch1.tbl <- finch1.tbl %>% arrange(desc(length.by.mass))
#finch1.tbl


