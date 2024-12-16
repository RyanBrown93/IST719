# Homework 4 part 2 

# 4.1
crime <- read.csv(
  "http://datasets.flowingdata.com/crimeRatesByState2005.csv",
  sep=",", header=TRUE)

crime[1:3,]

crime2 <- crime[crime$state != "District of Columbia",]
crime2 <- crime2[crime$state != "United States of America",]

plot(crime2$murder, crime2$burglary)

scatter.smooth(crime2$murder, crime2$burglary,
               xlim=c(0,10), ylim=c(0,1200))

pairs(crime2[,2:9], panel=panel.smooth)


# 4.2
crime <- read.csv(
  "http://datasets.flowingdata.com/crimeRatesByState2005.tsv",
  sep="\t", header=TRUE)

symbols(crime$murder, crime$burglary, circles=crime$population)

radius <- sqrt(crime$population / pi)
symbols(crime$murder, crime$burglary, circles=radius)
symbols(crime$murder, crime$burglary, circles=radius
        ,inches=0.35, fg="white", bg="red", xlab="Murder Rate"
        ,ylab="Burglary Rate")
text(crime$murder, crime$burglary, crime$state, cex=0.5)


# 4.3
birth <- read.csv(
  "http://datasets.flowingdata.com/birth-rate.csv")
hist(birth$X2008)
hist(birth$X2008, breaks=5, col="purple", main="Global Distribution of Birth Rates"
     ,xlab="Live births per 1,000 population", ylab="Country Count")

# 4.4

birth2008 <- birth$X2008[!is.na(birth$X2008)]
d2008 <- density(birth2008)
density.default(x=birth2008)
d2008frame <- data.frame(d2008$x, d2008$y)
write.table(d2008frame, "birthdensity.txt", sep="\t")
write.table(d2008frame,"birthdensity.txt", sep=",", row.names=FALSE )
plot(d2008, type="n"
     ,main="GLOBAL DISTRIBUTION OF BIRTH RATES IN 2008"
     ,xlab="Live births per 1,000 population"
     ,ylab="Density")
polygon(d2008, col="#821122", border="#cccccc")


# 4.5

tvs <- read.csv(
  "http://datasets.flowingdata.com/tv_sizes.txt"
  ,sep="\t", header=TRUE)

tvs <- tvs[tvs$size <80,]
tvs <- tvs[tvs$size>10,]
breaks = seq(10,80,by=5)
par(mfrow=c(4,2))

hist(tvs[tvs$year == 2009,]$size, breaks=breaks)
hist(tvs[tvs$year == 2008,]$size, breaks=breaks)
hist(tvs[tvs$year == 2007,]$size, breaks=breaks)
hist(tvs[tvs$year == 2006,]$size, breaks=breaks)
hist(tvs[tvs$year == 2005,]$size, breaks=breaks)
hist(tvs[tvs$year == 2004,]$size, breaks=breaks)
hist(tvs[tvs$year == 2003,]$size, breaks=breaks)
hist(tvs[tvs$year == 2002,]$size, breaks=breaks)

install.packages("lattice")
library(lattice)

fname <- file.choose()
art <- read.csv(file = fname
                ,header = TRUE
                ,stringsAsFactors = FALSE)
histogram(~units.sold | rep, data=art, xlab="Units Sold",main="Art Pieces Sold by Rep")


fname <- file.choose()
sales <- read.csv(file = fname
                ,header = TRUE
                ,stringsAsFactors = FALSE)

