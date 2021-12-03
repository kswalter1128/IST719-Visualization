# Lab5
# Kyle Walter

require(data.table)

setwd("G:/My Drive/Graduate School/IST719 Vizualization")

tweets <- fread("Week 5/climatetweets_useforlecture_25k.csv")

my.media <- tweets$media
table(my.media)
my.media[my.media ==""] <- "text only"
my.media <- gsub("\\|photo","",my.media)

pie(100*round(table(my.media)/sum(table(my.media)),4))

tweets$created_at

#Twitter Date Style
#"Mon Aug 15 13:05:42 +0000 2016"

conversion.string <- "%a %b %d %H:%M:%S +0000 %Y"

tmp <- strptime(tweets$created_at,format = "%a %b %d %H:%M:%S +0000 %Y", 
                tz = "GMT")
tmp[is.na(tmp)]


tweets[,':='(date = strptime(created_at, format = conversion.string))]
rm(tmp)

as.factor(weekdays(tweets$date[1:3], abbreviate = T))

barplot(table(weekdays(tweets$date, abbreviate = T)))
tmp <- tweets$user_utc_offset
tmp
tweets$date[7:10]+tmp[7:10]

known.times <- tweets$date + tweets$user_utc_offset
index <- which(is.na(known.times))
known.times <- known.times[-index]
barplot(table(hour(known.times)))

#5.1.5 ***rewatch so I can understand better***
start.date <- as.POSIXct("2016-06-24 23:59:59")
end.date <- as.POSIXct("2016-06-26 00:00:00")

index <- which((tweets$date > start.date) & (tweets$date < end.date))
tweets.25th <- tweets$date[index]
format.Date(tweets.25th, "%Y%m%d%H%M")

tmp.date <- as.POSIXct(strptime(format.Date(tweets.25th, "%Y%m%d%H%M"),"%Y%m%d%H%M" ))

plot(table(tmp.date))

temp.tab <- table(tmp.date)

plot(as.POSIXct(names(temp.tab)), as.numeric(temp.tab), type = "h")

x <- seq(from = start.date+1, to = end.date -1, by = "min")
length(x)
y <- rep(0, length(x))
y[match(names(temp.tab), as.character(x))] <- as.numeric(temp.tab)
plot(x, y, type = "p", pch = 16, cex = .4)
plot(x, y, type = "p", pch = ".", cex = .4)
plot(x,y, type = "l")

# hashtag Wordcloud

tweets$text[5:10]

require(stringr)
tags <- str_extract_all(tweets$text,"#\\S+", simplify = F)
tags <- tags[lengths(tags)>0]
tags <- unlist(tags)
tags <- tolower(tags)
tags <- gsub("#|[[:punct:]]","",tags)
tags.tab <- sort(table(tags), decreasing = T)
tags.tab[1:10]
zap <- which(tags.tab<3)
tags.tab <- tags.tab[-zap]
boxplot(as.numeric(tags.tab))

df <- data.frame(wrods = names(tags.tab), count=as.numeric(tags.tab),
                 stringsAsFactors = F)
par(mfrow=c(3,3))
plot(df$count, main = "raw")
y <- df$count/max(df$count)
plot(y, main = "0 - 1")
plot(df$count^2, main = "^2")
plot(df$count^(1/2), main = "^(1/2)")
plot(log(df$count), main = "NatLog")


#Word cloud continued

library(wordcloud)
mypal <- colorRampPalette((c('red','orange3','gold')))
gc()

index <- which(df$count > 9)
par(mar = c(0,0,0,0), bg ="black")
my.counts <- (df$count[index])^(1/2)

wordcloud(df$wrods[index], my.counts, scale = c(4,.4)
          , min.freq = 1, max.words = Inf, random.order = F
          , random.color = F, ordered.colors = T, rot.per = 0
          ,colors = mypal(length(df$wrods[index])))

#5.2.1 Sankey Diagrams

sales <- fread("Week 3/sales.csv")
library(alluvial)
alluv.df <- sales[,.(units.sold=sum(units.sold)), .(rep.region, type)]
colnames(alluv.df)[1] <- "reg"

alluvial(alluv.df[,1:2], freq = alluv.df$units.sold)

my.cols <- rep("gold", nrow(alluv.df))
my.cols[alluv.df$type=="red"] <- "red"

alluvial(alluv.df[,1:2], freq = alluv.df$units.sold, col = my.cols)

options(stringsAsFactors = F)
alluv.df <- sales[,.(units.sold=sum(units.sold)), .(rep.region, type, wine)]
colnames(alluv.df)[1] <- "reg"

redwine <- colorRampPalette(c("red","purple"))
whitewine <- colorRampPalette(c("gold","yellow","orange"))
wine=unique(alluv.df[type=="white"]$wine)
color=whitewine(length(alluv.df[type=="white"]))
whitewineColor <- data.table(wine, color)
redWineColor <- data.table(wine=unique(alluv.df[type=="red"]$wine),
                           color=redwine(length(alluv.df[type=="red"])))


alluvial(alluv.df[,1:3], freq = alluv.df$units.sold,
         col = ifelse(alluv.df$type=="red","red","gold"))


#Teemap
require(RColorBrewer)
require(treemap)

treemap(sales, index = c("rep.region"),
        vSize = "income",
        fontsize.labels = 12,
        palette = "Greens")

treemap(sales, index = c("rep.region"),
        vSize = "income",
        vColor = "units.sold",
        type = "dens",
        fontsize.labels = 12,
        palette = "Greens")


treemap(sales, index = c("rep.region"),
        vSize = "income",
        vColor = "units.sold",
        type = "value",
        fontsize.labels = 12,
        palette = "OrRd")


treemap(sales, index = c("rep.region", "sales.rep","type"),
        vSize = "income",
        vColor = "units.sold",
        type = "index",
        fontsize.labels = 12,
        palette = brewer.pal(8, "Set1"))

#River Plot

require(riverplot)
river <- riverplot.example()
par(mfrow = c(2,1), mar = c(1,1,1,1))
plot(river, srt = 90, lty =1)

x <- river

x$edges$Value[2] <- 45
x$edges$Value[1] <- 15
x$nodes$x[5] <- 5

plot(x)

df <- sales[, .(income=sum(income)), 
            .(type, wine)]
df <- df[order(type, income)]
node.name <- c("wine",unique(df$type),df$wine)
node.position <- c(1,2,2,3,3,3,3,3,3,3)
node.color <- rep("gray",length(node.name))
node.color <- c("deepskyblue","red","yellow", "brown4","firebrick3","deeppink4",
                "khaki1","lightgoldenrod1","gold","goldenrod1")
node <- data.frame(ID = node.name, x = node.position, col = node.color,
                   stringsAsFactors = F)
paren.nodes=c("wine","wine",df$type)
child.nodes=c("red","white",df$wine)

value <- c(sum(df$income[df$type=="red"]), sum(df$income[df$type =="white"]), df$income)
edges <- data.frame(N1=paren.nodes, N2 = child.nodes, Value=value)

r <- makeRiver(nodes = node, edges = edges)

par(mar = c(0,0,0,0))
plot(r)

#5.3

dat <- tapply(sales$units.sold, list(sales$type, sales$rep.region), sum)
barplot(dat, beside = T, col=c("brown","gold"),
        main = "Units Sold by Region by Type")
