require(data.table)
require(magrittr)
require(RColorBrewer)

#Barplot 1
nathans <- fread("http://datasets.flowingdata.com/hot-dog-contest-winners.csv")

fill_colors <- c()

for(i in 1:length(nathans$`New record`)){
  if(nathans$`New record`[i]==1){
    fill_colors <- c(fill_colors, "#821122")
  }else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}

barplot(nathans$`Dogs eaten`, names.arg = nathans$Year, col = fill_colors, border = NA,
        xlab = "Year", ylab = "Hot dogs and buns (HDB) eaten", space = 0.3, main = "Nathan's Hot Dog Eating Contest Results, 1980-2010")

#Barplot 2
hot_dog_places <- fread("http://datasets.flowingdata.com/hot-dog-places.csv", header = T)

hot_dog_matrix <- as.matrix(hot_dog_places)

barplot(hot_dog_matrix, border = NA, space = 0.25, ylim = c(0,200),
        xlab = "Year",
        ylab = "Hotdogs and buns (HBDs) eaten",
        main = "Hot Dog Eating Contest Ruesults, 180-2010")

#Scatter Plot
subscribers <- fread("http://datasets.flowingdata.com/flowingdata_subscribers.csv")

plot(subscribers$Subscribers, type = "h", ylim = c(0, 30000), xlab = "Day", ylab = "Subscribers")
points(subscribers$Subscribers, pch=19, col="black")

#Time Series Chart
population <- fread("http://datasets.flowingdata.com/world-population.csv")
plot(population$Year, population$Population, type = "l", ylim = c(0,7000000000), xlab = "Year", ylab = "Population")

#Step chart
postage <- fread("http://datasets.flowingdata.com/us-postage.csv")
plot(postage$Year, postage$Price, type = "s", main = "US Postage Rates for Letters, First Ounce, 1991-2010",
     xlab = "Year", ylab = "Postage Rate (Dollars)")
#Part 2
# art.csv

setwd("G:/My Drive/Graduate School/IST719 Vizualization/Week 3")
art <- fread("art.csv")
str(art) #Check out the data types

#clean up the data set
unique(art$paper.type)
art[paper.type=="pads", ':='(paper.type="pad")]
art[,':='(date=lubridate::mdy(date))]

par(mfrow = c(2,2))

bystore <- art[,.(total.sales=sum(total.sale)),.(store)]
bystore <- bystore[order(total.sales)]
bystore2 <- tapply(art$total.sale, list(art$store), sum)
bytype <- tapply(art$total.sale, list(art$paper.type, art$paper), sum)
byDate<- art[,.(total.sales=sum(total.sale)),.(year,lubridate::month(date))]
bytype <- imputeTS::na_replace(bytype) #NA replaced with zero where no sales exist

hist(art$total.sale, main = "Distribution of total.sales", col = "blue", border = "white")

pie(bystore$total.sales, labels = bystore$store, 
        col = brewer.pal(4, "Oranges")
        , main = "Distribution of total.sale")
#plot(byDate$lubridate, byDate$total.sales, pch = 9, xlim = c(1,12), border = F,
#     main = "Distribution of total.sales")

#year <- unique(byDate$year)

#lines(subset(byDate$total.sales, byDate$year==2012), col = "red")
#lines(subset(byDate$total.sales, byDate$year==2013), col = "orange")
#lines(subset(byDate$total.sales, byDate$year==2014), col = "blue")
#lines(subset(byDate$total.sales, byDate$year==2015), col = "maroon")

#legend(1, 5200, legend = year, col = c("red","orange","blue","maroon"), lty = 1, cex = 0.3)

#Subset

drawing <- art[paper == "drawing"]
drawing <- tapply(drawing$total.sale, list(drawing$paper.type), sum)

barplot(drawing, beside = T, col = brewer.pal(4, "Pastel2"),
        main = "Distribution for total sales of drawing paper"
        , xlab = "Paper"
        , ylab = "Total Sales")
watercolor <- art[paper=="watercolor"]
boxplot(data = watercolor, total.sale ~ paper.type, col = brewer.pal(4, "Set1"), main = "Distribution for total sales for watercolor paper")

# Part 3
par(mfrow = c(3,1))

plot(art$unit.price, art$units.sold,  "h", col="red", xlim = c(0,30)
     , main = "Price vs Units Sold"
     , sub = "Part 3 Question 1"
     , xlab = "Unit Price"
     , ylab = "Units Sold"
     , lwd = 5)
papervunitsold <- art[,.(TotalUnits=sum(units.sold)), .(paper)]
barplot(papervunitsold$TotalUnits, names.arg = papervunitsold$paper
        ,horiz = T
        , col =c("blue","orange")
        , main = "Units of Paper Sold"
        , sub = "Part 3 Question 2"
        , xlab = "Total Units Sold"
        , ylab = "Paper Category Sold")
options(scipen = 999)
papervincome <- art[,.(TotalSales=sum(total.sale)), .(paper)]
barplot(papervincome$TotalSales, names.arg = papervincome$paper
        ,horiz = T
        , col =c("blue","orange")
        , main = "Total Income by Type of Paper"
        , sub = "Part 3 Question 3"
        , xlab = "Total Income"
        , ylab = "Paper Category Sold")
