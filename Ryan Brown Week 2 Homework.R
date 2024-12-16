#Week 2 Homework: Ryan Brown

#Part 1.1
fname <- file.choose()
hotdogs <- read.csv(file = fname
                 ,header = TRUE
                 ,stringsAsFactors = FALSE)
barplot(hotdogs$Dogs.eaten)
barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year)
barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year, col="red"
        ,border=NA, xlab="Year", ylab="Hotdogs and Buns (HDB) Eaten")

fill_colors <- c() 

for (i in 1:length(hotdogs$Country)) {
  if (hotdogs$Country[i] == "United States") {
    fill_colors<- c(fill_colors, "#821122")
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}

barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year, col=fill_colors
        ,border=NA, xlab="Year", ylab="Hotdogs and Buns (HDB) Eaten")

fill_colors <- c() 

for (i in 1:length(hotdogs$New.record)) {
  if (hotdogs$New.record[i] == 1) {
    fill_colors<- c(fill_colors, "#821122")
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}

barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year, col=fill_colors
        ,border=NA, xlab="Year", ylab="Hotdogs and Buns (HDB) Eaten")

barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year, col=fill_colors
        ,border=NA, main = "Nathan's Hot Dog Eating Contest Results, 1980-2010",space=0.3, xlab="Year", ylab="Hot dogs and Buns (HDB) Eaten")

#Part 1.2
fname <- file.choose()
hot_dog_places<- read.csv(file = fname
                    ,header = TRUE
                    ,stringsAsFactors = FALSE)
names(hot_dog_places) <- c("2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
hot_dog_matrix <- as.matrix(hot_dog_places)
barplot(hot_dog_matrix, border=NA, space=0.25, ylim=c(0,200),xlab="Year", ylab="Hot Dogs and Buns (HDBs) Eaten"
        ,main="Hot Dog Eating Contest Results, 1980-2010")

#Part 1.3
subscribers <- read.csv("/Users/broiwniemixxx/Desktop/IST719/Week 2/flowingdata_subscribers.csv", sep=",", header=TRUE)
subscribers[1:5,]
plot(subscribers$Subscribers)
plot(subscribers$Subscribers, type="p", ylim=c(0,30000))
plot(subscribers$Subscribers, type="h", ylim=c(0,30000), xlab="Day", ylab="Subscribers")
points(subscribers$Subscribers, pch=19, col="black")

#Part 1.4
population <- read.csv("/Users/broiwniemixxx/Desktop/IST719/Week 2/world-population.csv", sep=",", header=TRUE)
plot(population$Year, population$Population, type="l", ylim=c(0,7000000000), xlab="Year", ylab="Population")

#Part 1.5
postage <- read.csv("/Users/broiwniemixxx/Desktop/IST719/Week 2/us-postage.csv", sep=",", header=TRUE)
plot (postage$Year, postage$Price, type="s")
plot (postage$Year, postage$Price, type="s", main="US Postage Rates for Letters, First Ounce, 1991-2010"
      ,xlab="Year", ylab="Postage Rates (Dollars)")

#Part 2
fname <- file.choose()
art <- read.csv(file = fname
                   ,header = TRUE
                   ,stringsAsFactors = FALSE)
par(mfrow = c(2,2))
hist(art$total.sale, main="Distribution of Total.Sales", xlab="Price", ylab="# of Sales",col="Blue")
d <- density(art$total.sale)
plot(d, main="Distribution of Total.Sales", xlab="Price", ylab="Density of Sales")
polygon(d,col="black")

drawing <- art %>% filter(paper == "drawing")
boxplot(drawing$total.sale, main="Distribution of the Total Sales for Drawing Paper", col="gray")

watercolor <- art %>% filter(paper == "watercolor")
boxplot(watercolor$total.sale, main="Distribution of the Total Sales for Watercolor", col="orange")


plot(art$unit.price, art$units.sold, xlab = "Unit Price", ylab = "Units Sold", main = "Unit Price vs. Units Sold")     

par(mfrow = c(1,2))
hist(drawing$units.sold, xlab="# of Units Sold", main="Sales of Drawing Paper")
hist(watercolor$units.sold, xlab="# of Units Sold", main="Sales of Watercolor")


par(mfrow = c(1,2))
hist(drawing$total.sale, xlab="# of Units Sold", main="Income from Drawing Paper")
hist(watercolor$total.sale, xlab="# of Units Sold", main="Income from Watercolor")
