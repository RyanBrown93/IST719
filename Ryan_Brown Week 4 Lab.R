# Week 4 Lab: Layouts in R

my.dir <- "/Users/broiwniemixxx/Desktop/"
sales <- read.csv(file=paste0(my.dir,"sales.csv")
                  ,header=TRUE
                  ,stringsAsFactors = FALSE)
dat.1 <- tapply(sales$units.sold, list(sales$wine),sum)
dat.2 <- tapply(sales$income, list(sales$wine),sum)

par(mfrow=c(2,1))
par(mar=c(0.5,5,4,1), cex.lab=.8)
barplot(dat.2, xaxt="n", las=2)
mtext(text="income", side=2, line=4, adj=0)
mtext(text="income on Units Sold", side=3, line=1, cex=1.3, adj=0)

par(mar=c(6,5,0,1), cex.lab=0.8)
bar.out <- barplot(dat.1, xaxt="n", las=2)
axis(side=1, at=bar.out, labels=gsub(" ", "\n", names(dat.2)),las=2)

M<-matrix(
  c(1,1,1
    ,1,1,1
    ,2,2,2)
    ,nrow=3, byrow=T)
layout(M)
layout.show(2)
par(mar=c(.5,5,4,1), cex.lab=.8)
barplot(dat.2, xaxt="n", las=2)
mtext(text="income", side=2, line=4, adj=0)
mtext(text="income on Units Sold", side=3, line=1, cex=1.3, adj=0)
par(mar=c(6,5,0,1), cex.lab=0.8)
bar.out <- barplot(dat.1, xaxt="n", las=2)
axis(side=1, at=bar.out, labels=gsub(" ", "\n", names(dat.2)),las=2)

M<-matrix(
  c(1,1,1,3
    ,1,1,1,4
    ,2,2,2,5)
  ,nrow=3, byrow=T)
layout(M)
layout.show(5)
par(mar=c(.5,5,4,1), cex.lab=.8)
barplot(dat.2, xaxt="n", las=2)
mtext(text="income", side=2, line=4, adj=0)
mtext(text="income on Units Sold", side=3, line=1, cex=1.3, adj=0)
par(mar=c(6,5,0,1), cex.lab=0.8)
bar.out <- barplot(dat.1, xaxt="n", las=2)
axis(side=1, at=bar.out, labels=gsub(" ", "\n", names(dat.2)),las=2)

par(mar=C(1,1,1,1))
pie(dat.3)
pie(dat.4)
pie(dat.5)


dat.1 <- tapply(sales$units.sold, list(sales$wine),sum)
dat.2 <- tapply(sales$income, list(sales$wine),sum)
dat.3 <- tapply(sales$units.sold, list(sales$type),sum)
dat.4 <- tapply(sales$units.sold, list(sales$rep.region),sum)
dat.5 <- tapply(sales$units.sold, list(sales$year),sum)

split.screen(figs=c(2,1))
screen(1)
pie(dat.1)

screen(2)
pie(dat.2)

screen(1, new=F)
mtext("Ryan", side=3, line=1)

screen(2, new=F)
mtext("Here", side=3, line=1)

close.screen(1:2)

split.screen(c(2,2))
screen()
pie(dat.3)

# Week 4 Lab: Fonts

n<- 500
x <- abs(rnorm(n,6,2))
y <- x^2 + rnorm(n,0,2*x)

my.par <- par()
my.par$adj
my.par$family

plot(x,y)
plot(x,y, main="Fiddling with Fonts", xlab="Some X Lab", ylab="Ylab Text", font=2)
plot(x,y, main="Fiddling with Fonts", xlab="Some X Lab", ylab="Ylab Text", font=3)
plot(x,y, main="Fiddling with Fonts", xlab="Some X Lab", ylab="Ylab Text", font=4)
plot(x,y, main="Fiddling with Fonts", xlab="Some X Lab", ylab="Ylab Text", font=5)

my.par$font.axis
my.par$font.lab

plot(x,y, main="Fiddling with Fonts", xlab="Some X Lab", ylab="Ylab Text"
     , font.axis=2, font.lab=3, font.main=1)

plot(x,y, main="Fiddling with Fonts", xlab="Some X Lab", ylab="Ylab Text"
     ,family="HersheyGothicEnglish")

par(family="mono")
plot(x,y, main="Fiddling with Fonts", xlab="Some X Lab", ylab="Ylab Text")

par(family="serif")
plot(x,y, main="Fiddling with Fonts", xlab="Some X Lab", ylab="Ylab Text")

plot(1:10, 1:10, type="n")
# I am using a Mac so this wont work #
windowsFonts(
  A = windowsFont("Arial Black"),
  B = windowsFont("Bookman Old Style"),
  C = windowsFont("Comic Sans MS"),
  D = windowsFont("Symbol")
)

text(2,2,"Hello World Default")
text(3,3,"Hello World Default", family="A")
text(4,4,"Hello World Default", family="B")
text(5,5,"Hello World Default", family="C")
text(6,6,"Hello World Default", family="D")

install.packages("extrafont")
library(extrafont)
loadfonts(device="win")
