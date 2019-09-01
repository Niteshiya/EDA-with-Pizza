#import important libraries(i usually import all the libraries i use to avoid to&fro)
library(ggplot2)
library(readr)
library(tibble)
library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)
library(Amelia)
library(corrplot)
library(jpeg)
library(grid)
library(rworldmap)
library(ggmap)
#---------------------------------------------------------------------
#importing data file 
pizza <- read.csv("pizza.csv")
#Exploration and wrangling of data set
str(pizza)
head(pizza)
missmap(pizza,col=c("red","blue"))
#so we see that there are no missing values in the data set 
#First thing we will filter the varriable that we think are important
pizza %>% names()
pizza %>% summary()
usefull <- c("menus.name","postalCode","latitude","longitude","city","province","priceRangeMin","priceRangeMax")
pizza_imp <- pizza[,usefull]
head(pizza_imp)
#forming a dataframe of pizza types and counts
best_pizzas <- pizza %>% group_by(menus.name) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  as.data.frame()
head(best_pizzas)
str(best_pizzas)
#trimming data frame for better comparasion top 10
to_plot <- best_pizzas[1:10,]
ggplot() + 
  geom_col(aes(x=to_plot$menus.name,y=to_plot$count,fill=to_plot$menus.name),color="black") +
  theme(axis.text.x=element_text(angle=90)) +
  xlab("Pizza Types") + ylab("no. of orders/day") + ggtitle("Pizza Popularity")+labs(fill="Pizza type")
#now we know the most polpular pizza among people
#lets plot lattitude and longitude on usa map for better visualization
map <- getMap(resolution = "low")
plot(map,xlim=c(-170,-55),ylim=c(20,70),asp=1)
points(pizza$longitude,pizza$latitude,col=pizza$menus.name,cex=0.2,size=pizza$menus.amountMax)
#Here we see the diffrent clusters of Pizza places
#Now lets see the relation better some of the varriables
spam <- pizza[,c("priceRangeMax","priceRangeMin", "latitude","longitude", "postalCode")]
colnames(spam) = c('MaxPR', 'MinPR', 'lat', 'long', 'pcode')
str(spam)
pairs(spam,label=colnames(spam),main="Pair Matrix",pch=21,bg=c("red","purple","blue","yellow"),upper.panel = NULL)
#In this special type of map we can find relations and insights about givem varriables