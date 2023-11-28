
ggplot(iris, aes(Sepal.Length)) + geom_histogram()
#control plot appearance, including axis and gridlines with themes. I like theme_classic()
ggplot(iris, aes(Sepal.Length)) + geom_histogram() + theme_classic()
#alter font sizes
ggplot(iris, aes(Sepal.Length)) + geom_histogram() + theme_classic(base_size=18)
#alter font style
ggplot(iris, aes(Sepal.Length)) + geom_histogram() + theme_classic(base_size=18,base_family="Courier")

#control text features with theme() #this is most useful if you need to rotate your X axis labels. #after doing so you might want to adjust the position of the boxes with vjust and hjust
ggplot(iris, aes(Sepal.Length)) + geom_histogram() + theme_classic(base_size=18) + 
  theme(axis.text.x=element_text(angle=90,vjust=0.5)) #hjust- horizontle justification

#use xlab and ylab to change axis titles
ggplot(iris, aes(Sepal.Length)) + geom_histogram() + theme_classic(base_size=18) +
  xlab("Sepal length (mm)") + ylab("Frequency") +ggtitle("My beautiful graph")
#add new lines (\n) to improve spacing between the titles and your plot
ggplot(iris, aes(Sepal.Length)) + geom_histogram() + theme_classic(base_size=18) +
  xlab("\nSepal length (mm)") + ylab("Frequency\n") + ggtitle("My beautiful graph\n")

#If you'd like to make your title larger, you can alter that using a plot.title command in theme
ggplot(iris, aes(Sepal.Length)) + geom_histogram() + theme_classic(base_size=18) +
  xlab("\nSepal length (mm)") + ylab("Frequency\n") + ggtitle("My beautiful graph\n") +
  theme(plot.title=element_text(size=36))

#practice

ggplot(iris,aes(Sepal.Length)) + geom_histogram() + theme_bw() + 
  theme(axis.text = element_text(size = 18), axis.title = element_text(size =20), 
        plot.title = element_text(size = 26, hjust = 0.5)) + ylab("Frequency")+
  xlab("Sepal length (mm)") + ggtitle("My graph")

#scatter plot comparing two continuous variables
ggplot(iris, aes(x=Sepal.Length,y=Petal.Length)) + geom_point() 
#color dots according to species
ggplot(iris, aes(x=Sepal.Length,y=Petal.Length,color=Species)) + geom_point() 
#shade dots according to species
ggplot(iris, aes(x=Sepal.Length,y=Petal.Length,shape=Species)) + geom_point() 
#do both according to species
ggplot(iris, aes(x=Sepal.Length,y=Petal.Length,shape=Species,color=Species)) + geom_point() 


#violin plot relating a categorical and continuous
ggplot(diamonds, aes(x=cut,y=carat)) + geom_violin()
#outline plots according to variable cut
ggplot(diamonds, aes(x=cut,y=carat,color=cut)) + geom_violin()
#fill plots according to variable cut
ggplot(diamonds, aes(x=cut,y=carat,fill=cut)) + geom_violin()
#fill plots according to variable clarity to represent two categorical explanatory variables and one response
ggplot(diamonds, aes(x=cut,y=carat,fill=clarity)) + geom_violin()

#use facets
ggplot(diamonds, aes(x=clarity,y=carat,fill=clarity)) + geom_violin()

ggplot(diamonds, aes(x=cut,y=carat,fill=cut)) + geom_violin() +facet_wrap(diamonds$clarity,nrow=4)
#make a horizontal row of panels according to the variable cut
ggplot(diamonds, aes(x=clarity,y=carat,fill=clarity)) + geom_violin() +facet_grid(~cut)
#make a vertical column of panels according to the variable cut
ggplot(diamonds, aes(x=clarity,y=carat,fill=clarity)) + geom_violin() +facet_grid(cut~.)
#make a series of panels according to the variable cut
ggplot(diamonds, aes(x=clarity,y=carat,fill=clarity)) + geom_violin() +facet_wrap(diamonds$cut,nrow=3)
#make a grid of panels according to the variable cut and color
ggplot(diamonds, aes(x=clarity,y=carat,fill=clarity)) + geom_violin() +facet_grid(cut~color)


#use custom brewer colors
ggplot(iris, aes(x=Sepal.Length,y=Petal.Length,color=Species)) + geom_point() + 
  scale_colour_brewer(palette="Dark2")
#choose your own colors
ggplot(iris, aes(x=Sepal.Length,y=Petal.Length,color=Species)) + geom_point() + 
  scale_colour_manual(values=c("red","green","blue"))
#use custom brewer colors for fill
ggplot(diamonds, aes(x=clarity,y=carat,fill=clarity)) + geom_violin() + 
  scale_fill_brewer(palette="YlOrRd")
#choose your own colors for fill
ggplot(diamonds, aes(x=clarity,y=carat,fill=clarity)) + geom_violin() + 
  scale_fill_manual(values=c("red","orange","yellow","green","turquoise","blue","purple","violet"))


