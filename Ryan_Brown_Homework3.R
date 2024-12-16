fname <- file.choose()
NBA <- read.csv(file = fname
                 ,header = TRUE
                 ,stringsAsFactors = FALSE)
str(NBA)
options(scipen=999)

col.vec <-rep(rgb(255,155,75, maxColorValue = 255), dim (NBA)[1])
col.vec[NBA$Position == "PG"] <- rgb(255, 0, 0, maxColorValue = 255)        
col.vec[NBA$Position == "SG"] <- rgb(0, 255, 0, maxColorValue = 255)
col.vec[NBA$Position == "SF"] <- rgb(0, 0, 255, maxColorValue = 255)
col.vec[NBA$Position == "PF"] <- rgb(255, 255, 0, maxColorValue = 255)
col.vec[NBA$Position == "C"] <- rgb(255, 165, 0, maxColorValue = 255)

par(bg="lightgray")
plot(NBA$Salary, NBA$Age, pch=20, cex=0.8,col=col.vec, ylab="Player Age", xlab="Player Salary", main="Player Salary & Age")
legend("bottomright", cex=0.8, bty="n",
       title = "Position", c("PG", "SG", "SF", "PF", "C"),
       fill=c("red", "green", "blue", "yellow", "orange" ))

plot(NBA$Salary, NBA$GP, pch=20, cex=0.8,col=col.vec, ylab="Games Played", xlab="Player Salary", main="Player Salary & Games Played")
legend("bottomright", cex=0.8, bty="n",
       title = "Position", c("PG", "SG", "SF", "PF", "C"),
       fill=c("red", "green", "blue", "yellow", "orange" ))

plot(NBA$Salary, NBA$MP, pch=20, cex=0.8,col=col.vec, ylab="Games Played", xlab="Player Salary", main="Player Salary & Avg. Minutes Played (per Game)")
legend("bottomright", cex=0.8, bty="n",
       title = "Position", c("PG", "SG", "SF", "PF", "C"),
       fill=c("red", "green", "blue", "yellow", "orange" ))

install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

NBA %>%
  group_by(Salary, Position) %>%
  summarise(mean=mean(Salary),
            sd=sd(Salary)) %>%
  ggplot(aes(x=Position, y=mean, fill=Position)) +
  geom_bar(position=position_dodge(width=.75), stat = "identity", width=.7) + labs(y= "Average Salary")
