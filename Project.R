library(ggplot2)
library(maps)
library(mapdata)
library(usmapdata)
library(usmap)
library(dplyr)
library(tidyr)
install.packages("ggpubr")
library(ggpubr)
options(scipen=999)
install.packages("ggpmisc")
library(ggpmisc)

my.dir <- "/Users/broiwniemixxx/Desktop/"
crime <- read.csv(file=paste0(my.dir, "crime data 2012-2022.csv")
                      ,head=TRUE
                      ,stringsAsFactors = FALSE)

crime_by_city <- read.csv(file=paste0(my.dir, "crime by city.csv")
                          ,sep=",")

wa <- map_data("county", "washington")
#wa$subregion[wa$subregion == "island"] <- "Whidbey Island"
mid_range <- function(x) mean(range(x))
wa_c <- do.call(rbind, lapply(split(wa, wa$subregion), function(d)
  data.frame(lat = mid_range(d$lat), long = mid_range(d$long), subregion = unique(d$subregion))))
wa$population <- sample(1000000,size = nrow(wa), replace = TRUE)

wa_cc <- merge(x=wa_c, y=crime, by="subregion", all.x=TRUE)
wa_cc <- wa_cc[-c(2,3)]
wa$subregion <- gsub("\\b([a-z])", "\\U\\1", wa$subregion, perl=TRUE)
wa_crime <- left_join(wa,crime, by="subregion") 
wa <- wa_crime


wa$subregion <- gsub("\\b([a-z])", "\\U\\1", wa$subregion, perl=TRUE)
wa_c$subregion <- gsub("\\b([a-z])", "\\U\\1", wa_c$subregion, perl=TRUE)

wa.counties.murders <- 
  ggplot(wa, aes(long, lat)) +
  coord_fixed(1.3) + labs(title="Murders per County, Washington State: 2012 - 2022") +
  geom_polygon(data=wa, aes(x=long, y=lat, group = group, fill=Murder), color="black") +
  geom_text(aes(label = subregion), data = wa_c, size = 2) +
  scale_fill_continuous(name="Murder Rate by County", low = "honeydew4", 
                        high = "darkred",limits = c(0,800), breaks=c(0,200,400,600,800), 
                        na.value = "grey50")
wa.counties.murders + 
  theme(legend.background = element_rect(fill = "transparent"),
                           legend.box.background = element_rect(fill = "transparent"),
                           panel.background = element_rect(fill = "transparent"),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           plot.background = element_rect(fill = "transparent", color = NA))

wa.counties.assault <- 
  ggplot(wa, aes(long, lat)) +
  coord_fixed(1.3) + labs(title="Assault per County, Washington State: 2012 - 2022") +
  geom_polygon(data=wa, aes(x=long, y=lat, group = group), fill="grey", color="black") +
  geom_point(data=crime_by_city, aes(x=Longitude, y=Latitude, size = Assault), 
             color = "darkred") + #, alpha = .5)
  geom_text(aes(label = subregion), data = wa_c, size = 2)
 

wa.counties.assault + 
  theme(legend.background = element_rect(fill = "transparent"),
                            legend.box.background = element_rect(fill = "transparent"),
                            panel.background = element_rect(fill = "transparent"),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            plot.background = element_rect(fill = "transparent", color = NA))

#Barplot: Murders per County (can do any crime)
barplot(crime$Murder, names.arg=crime$subregion, las=2, cex.names=0.7
        ,xlab="Counties", ylab="Murders from 2012 - 2022")

#Stacked Barplot of Crimes per County
crime<- crime[, -c(8,14,15,16,17,18,19)]
df_long_crime <- crime %>% pivot_longer(!subregion, names_to = "Type", values_to = "Count")
ggplot(df_long_crime, aes(x = subregion, y = Count, fill = Type)) + # specify x and y axis, specify fill
  geom_col(position = "stack", width = 0.8) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Types of Crime by County: 2012-2022") +
  xlab("County") + ylab("Number of Crimes") +
  scale_fill_manual(values=c("darkred","darkblue","black","darkgray","yellow",
                             "lightblue","purple","darkgreen","darksalmon","deeppink",
                             "darkgoldenrod")) +
  theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5))


# King County Crimes
cbc <- crime_by_city[, -c(3:4,11,13,17:20)]
king_county <- cbc %>% filter(County == "King County")

#king_county contains crime rate info
king_county <- king_county[, -1]
long_crime_by_city_kk <- king_county[, -13] %>% pivot_longer(!City, names_to = "Type", values_to = "Count")

kk_totals <- aggregate(Count ~ City, data = long_crime_by_city_kk,  sum)

ggplot(long_crime_by_city_kk, aes(x = City, y = Count, fill = Type)) + # specify x and y axis, specify fill
  geom_col(position = "stack", width = 0.8) +
  #geom_text(aes(label=Count, fill=NULL), data=kk_totals, angle=90, hjust=-0.1)  +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("King County Crimes by City: 2012-2022") +
  xlab("City") + ylab("Number of Crimes") +
  scale_fill_manual(values=c("darkred","darkblue","black","darkgray","yellow",
                             "lightblue","purple","darkgreen","darksalmon","deeppink",
                             "darkgoldenrod")) +
  theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5))


total_crime_kk <- aggregate(long_crime_by_city_kk$Count, by=list(Crime.Type=long_crime_by_city_kk$Type), FUN=sum)
names(total_crime_kk)[2] <- "Total"

ggplot(total_crime_kk, aes(x=reorder(Crime.Type, -Total), y=Total)) + 
  geom_text(aes(label=Total), position=position_dodge(width=0.9), vjust=-0.25) +
  xlab("Type of Crime") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("King County Crime: 2012 - 2022") +
  geom_bar(aes(fill=Total),stat="identity", show.legend = FALSE) +
  scale_fill_gradient(low="honeydew4", high="darkred") + # Remove background elements manually
  theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5))


# Crime Time-Series: State Level
crime_ts <- read.csv(file=paste0(my.dir, "crime wa state by county and city.csv")
                  ,head=TRUE
                  ,stringsAsFactors = FALSE)

crime_rate_wa <- subset(crime_ts, County == "Washington State")

ggplot(crime_rate_wa, aes(x=IndexYear, y=Crime.Rate)) + geom_line(color="darkred") +
  geom_point() + ggtitle("Washington State Crime Rate: 2012 - 2022") +
  xlab("Year") + ylab("Average Crime Rate") +
  scale_x_continuous(breaks=2012:2022) + 
  theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5))

# County Crime Rates
crime_rate_c <- crime_ts[,c("IndexYear","County","Crime.Rate")]

# by-year crime rate for counties
crime_rate_c_final <- aggregate(Crime.Rate ~ County + IndexYear, data = crime_rate_c,  mean)

# 2012-2022 avg crime rate for counties
crime_rate_c_total <- aggregate(Crime.Rate ~ County, data = crime_rate_c_final,  mean)

ggplot(crime_rate_c_total, aes(x=County, y=Crime.Rate)) + 
  geom_bar(aes(fill=crime_rate_c_total$Crime.Rate),stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("County") + ylab("Average Crime Rate: 2012-2022") +
  ggtitle("Average Crime Rate by County: 2012-2022") +
  scale_fill_gradient(low="honeydew4", high="darkred") +
  # Remove background elements manually
  theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5))

# Crime Rate State Map
ggplot(wa, aes(long, lat)) +
  coord_fixed(1.3) + labs(title="Crime Rate per County, Washington State: 2012 - 2022") +
  geom_polygon(data=wa, aes(x=long, y=lat, group = group, fill=Crime.Rate), color="black") +
  geom_text(aes(label = subregion), data = wa_c, size = 2) +
  scale_fill_continuous(name="Crime Rate by County", low = "honeydew4", 
                        high = "darkred",limits = c(0,85), breaks=c(0,25,45,65,85),
                        na.value = "grey50") + 
  theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA))

# King County Crime Rate 2012 - 2022
crime_rate_kk_time <- crime_ts[,c("IndexYear","County","Crime.Rate","Murder","Manslaughter",
                                  "Sexual.Assault","Assault","Arson","Burglary",
                                  "Robbery","Theft")]
  
crime_rate_kk_time <- crime_rate_kk_time %>% filter(County == "King County")
crime_rate_kk_time <- crime_rate_kk_time[-c(2)]

avg_cr_kk <- crime_rate_kk_time[,c("IndexYear","Crime.Rate")]
avg_cr_kk <- aggregate(.~IndexYear,avg_cr_kk,mean)

crime_rate_kk_time <- crime_rate_kk_time[-c(2)]
crime_rate_kk_time <- aggregate(.~IndexYear,crime_rate_kk_time,sum)
crime_rate_kk_time <- merge(crime_rate_kk_time,avg_cr_kk, by="IndexYear")


t <- crime_rate_kk_time[, -10] %>% pivot_longer(!IndexYear, names_to = "Type", values_to = "Count")
t <- t[t$IndexYear %in% c('2018', '2019','2020','2021','2022'),]

ggplot(data = t, aes(x = Type, y = Count, color=as.factor(IndexYear), group = IndexYear)) +
  geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_color_manual(values=c("darkred","lightblue","darkgoldenrod","purple","black",
                              "lightblue","purple","darkgreen","darksalmon","deeppink",
                              "black")) + ggtitle("King County Violent Crimes: 2012-2022") +
  xlab("Crime Type") +
  theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA))

# Highest Crime Rate City 2022
crime_rate_city <- crime_ts[,c("IndexYear","City","Crime.Rate")]
crime_rate_city <- aggregate(Crime.Rate ~ City + IndexYear, data = crime_rate_city,  mean)
crime_rate_city <- aggregate(Crime.Rate ~ City, data = crime_rate_city,  mean)

g <- crime_rate_city %>% 
  arrange(desc(Crime.Rate)) %>%
  slice(1:10)

g[,-1] <-round(g[,-1],0)

ggplot(g, aes(x=reorder(City, -Crime.Rate), y=Crime.Rate)) +
  geom_bar(aes(fill=Crime.Rate),stat="identity", show.legend = FALSE) +
  scale_fill_gradient(low="honeydew4", high="darkred") +
  geom_text(aes(label=Crime.Rate), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA))

# Crime Rate by City: King County
crime_rate_city_kk <- crime_ts[,c("IndexYear","County","City","Crime.Rate")]
crime_rate_city_kk <- crime_rate_city_kk %>% filter(County == "King County")
crime_rate_city_kk <- aggregate(Crime.Rate ~ City + IndexYear, data = crime_rate_city_kk,  mean)
crime_rate_city_kk <- aggregate(Crime.Rate ~ City, data = crime_rate_city_kk,  mean)

kc <- crime_rate_city_kk %>% 
  arrange(desc(Crime.Rate)) %>%
  slice(1:10)

kc[,-1] <-round(kc[,-1],0)

ggplot(kc, aes(x=reorder(City, -Crime.Rate), y=Crime.Rate)) +
  geom_bar(aes(fill=Crime.Rate),stat="identity", show.legend = FALSE) +
  scale_fill_gradient(low="honeydew4", high="darkred") +
  geom_text(aes(label=Crime.Rate), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA))

ggplot(total_crime_kk, aes(x="", y=Total, fill=Crime.Type)) +
  geom_bar(aes(fill=Total),stat="identity", width=1) +
  scale_fill_gradient(low="honeydew4", high="darkred") +
  coord_polar("y", start=0)

#Tukwila

tukwila <- long_crime_by_city_kk %>% filter(City == "Tukwila")

ggplot(tukwila, aes(x="", y=Count, fill=Type)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

crime_rate_a <- crime_ts[,c("IndexYear","City","Crime.Rate","Murder","Manslaughter",
                                  "Sexual.Assault","Assault","Arson","Burglary",
                                  "Robbery","Theft")]
crime_rate_a <- crime_rate_a %>% filter(City == "Auburn")

crime_rate_k <- crime_ts[,c("IndexYear","City","Crime.Rate","Murder","Manslaughter",
                            "Sexual.Assault","Assault","Arson","Burglary",
                            "Robbery","Theft")]
crime_rate_k <- crime_rate_k %>% filter(City == "Kent")
