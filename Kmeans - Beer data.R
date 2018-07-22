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

#Select only the columns that we will use for our clustering model

mybeer <- df1[, c('Style','OG', 'FG', 'ABV', 'IBU', 'Color', 'BrewMethod')]

#Check visualisations to see any obvious clusters

pairs.default(mybeer) # NB This is slow to generate, due to the volume of data

plot(mybeer$Style, mybeer$Color) #Too many data points, we will use the means of the variables instead

#First we will view plots and look for outliers:
plot(mybeer$ABV) # We will remove outliers above 20% ABV, these are unusual, may be data input errors
plot(mybeer$IBU) # We will remove ouliers above 1000, measure of specific chemical created by hop content
plot(mybeer$Color) # We will remove outliers above 50, probably data input errors

mybeerclean <- subset(mybeer, ABV <=20 & IBU <= 1000 & mybeer$Color <= 75)
str(mybeerclean)

#Means of ABV, IBU and Color, plotted against Style:

mean(mybeerclean$ABV)
avgABV <- aggregate(data=mybeerclean, mybeerclean$ABV ~ mybeerclean$Style, mean, na.rm = TRUE)
avgABV
plot(avgABV)

mean(mybeerclean$IBU)
avgIBU <- aggregate(data=mybeerclean, mybeerclean$IBU ~ mybeerclean$Style, mean, na.rm = TRUE)
avgIBU
plot(avgIBU)
barplot(mybeerclean$IBU)


mean(mybeerclean$Color)
avgColor <- aggregate(data=mybeerclean, mybeerclean$Color ~ mybeerclean$Style, mean, na.rm = TRUE)
avgColor
plot(avgColor)



#Means of ABV, IBU and Color plotted against BrewMethod
mean(mybeerclean$ABV)
avgABVbrew <- aggregate(data=mybeerclean, mybeerclean$ABV ~ mybeerclean$BrewMethod, mean, na.rm = TRUE)
avgABVbrew

boxplot(mybeerclean$ABV ~ mybeerclean$BrewMethod, col = rainbow(4))

mean(mybeerclean$IBU)
avgIBUbrew <- aggregate(data=mybeerclean, mybeerclean$IBU ~ mybeerclean$BrewMethod, mean, na.rm = TRUE)
avgIBUbrew
plot(avgIBUbrew)

boxplot(mybeerclean$IBU ~ mybeerclean$BrewMethod)

mean(mybeerclean$Color)
avgColorbrew <- aggregate(data=mybeerclean, mybeerclean$Color ~ mybeerclean$BrewMethod, mean, na.rm = TRUE)
avgColorbrew
plot(avgColorbrew)

boxplot(mybeerclean$Color ~ mybeerclean$BrewMethod)

summary(mybeerclean)

#Summary to check what the 6 most common styles of beer are:

mybeerstyle2
nrow(mybeerstyle2)

mybeerstyle2

#QUESTION
#Focus on bitterness and alcohol level, can we predict what style the beer is?


#Kmeans test

set.seed(12)

beer.km <- kmeans(avgIBU, centers = 6, n = 20)

#View results
beer.km

#View Centers
beer.km$centers


#Plot clusters in colour
plot(mybeerclean$Style,col=beer.km$cluster)


sns$age <- ifelse(sns$age >= 13 & sns$age < 20, sns$age, NA)





