#install and run packages for plots

install.packages('ggplot2')
install.packages('GGally')
library(GGally)
library(ggplot2)

#Read the csv file beer_recipes.csv and declare variable beerdata

beerdata <- read.csv("C:/Users/dcal2/OneDrive/Documents/Data & Web Mining/beer_recipes.csv", header = TRUE)

#View top 6 rows

head(beerdata)

#View summary
summary(beerdata)

#view structure
str(beerdata)

#view column names
names(beerdata)

#convert to a dataframe is not already done
df1 <- data.frame(beerdata)

#Convert blanks to NA
df1[df1==""] <- NA

#Select only the columns that we will use for our model

mybeer <- df1[, c('Style','OG', 'FG', 'ABV', 'IBU', 'Color', 'BoilTime' )]

#Check visualisations to see any obvious clusters


barplot(mybeer$Style)

scale(mybeer, center = TRUE, scale = TRUE)

#Compare Alcohol content and color
mybeer3 <- df1[, c('ABV', 'Color' )]

ggpairs(mybeer3)

#Remove outliers with alcohol content above 20% and colour above 50
#mybeer4 <- subset(mybeer3, ABV <= 20 & Color <= 50)

ggpairs(mybeer5)
ggpairs(mybeer6)

ggpairs(mybeer3)

#mybeer5 <- df1[, c('ABV', 'BoilTime')]

#pairs(head(mybeer, n = 250))

mybeer6 <- df1[, c('ABV', 'IBU' )]

mybeer7 <- df1[, c('ABV', 'IBU', 'Style')]

nrow(mybeer7)

#Summary to check what the 6 most common styles of beer are:

summary(mybeer7)

nrow(mybeer7)

plot(mybeer7$Style)


mybeerstyle2 <- subset(mybeer7, mybeer7$Style == 'American IPA', 
                                mybeer7$Style == 'American Pale Ale',
                                mybeer7$Style == 'Saison',
                                mybeer7$Style == 'American Light Lager', 
                                mybeer7$Style == 'American Amber Ale',
                                mybeer7$Style == 'Blonde Ale', select = ABV:Style)
                       
mybeerstyle2
nrow(mybeerstyle2)

mybeerstyle2

#QUESTION
#Focus on bitterness and alcohol level, can we predict what style the beer is?

pairs(mybeer6)

summary(mybeer6)

nrow(mybeer6) #73861

#Remove outlier beers with alcohol level above 20% and bitterness above 1000

mybeersfinal <- subset(mybeer6, ABV <= 20, IBU <= 1000)

pairs(mybeersfinal)

#Kmeans 

set.seed(12)

beer.km <- kmeans(mybeersfinal, centers = 3, n = 20)

#View results
beer.km

#View Centers
beer.km$centers

pairs(beer.km$cluster)

#Plot clusters in colour
plot(mybeer7,col=beer.km$cluster)






