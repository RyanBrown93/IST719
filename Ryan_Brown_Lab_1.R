#Author: Ryan Brown
#Purpose: Lab 1, Week 1

pie(c(7,4,2,12))
x <- c(7,6,7.2,12)
pie(x)
pie(x, main="Ryan's Pie")
pie(x, main="Ryan's Pie", col = c("red", "orange", "tan", "yellow"))
pie(x, main="Ryan's Pie", col = c("red", "orange", "tan", "yellow")
    ,labels = c("a", "b", "c", "d"))
plot(c(1,3,6,4))
plot(c(1,3,6,4), pch = 6)
plot(c(1,3,6,4), pch = 16, col = c("red", "orange", "tan", "yellow"), cex = 3)
x <- "Ryan"
x
x <- rnorm(n=10)
x
plot(x)
plot(x, type = "l")
plot(x, type = "h")
plot(x, type = "h", lwd=5, lend = 2, col = "orange")
plot(x, type = "h", lwd=5, lend = 2, col = "orange"
     , main = "change in net worth"
     , xlab = "time in years"
     , ylab = "in millions"
     , bty = "n")
par()
par(bg = "gray")
plot(x, type= "h", lwd=20, col = c("blue", "orange")
     , bty="n",
     lend=2)
my.par <- par()
par(my.par)
n <- 27
my.letters <- sample(letters[1:3], size=n, replace=T)
tab <- table(my.letters)
barplot(tab, col = c("brown", "tan", "orange")
        , names.arg = c("sales", "ops", "it")
        , border = "white"
        , xlab = "departments"
        , ylab = "employees"
        , main = "Company Employees"
        , horiz = TRUE
        , las = 1)


barplot(tab, col = c("brown", "tan", "orange")
        , names.arg = c("sales", "ops", "it")
        , border = "white"
        , xlab = "departments"
        , ylab = "employees"
        , main = "Company Employees"
        , horiz = TRUE
        , las = 1
        , density = 20
        , angle = c(45, 90, 12))

x <- rnorm (n=1000, mean = 10, sd=1)
hist(x, main = "What is the Distribution of X?")
boxplot(x, horizontal = T)
x <- rlnorm (n=1000, meanlog =1, sdlog = 1 )

par(mfrow=c(2,1))
boxplot(x, horizontal = T)
hist(x)
par(mfrow=c(2,1))