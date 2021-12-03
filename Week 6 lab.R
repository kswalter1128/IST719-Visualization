#Week 6 lab
#Kyle Walter

setwd("G:/My Drive/Graduate School/IST719 Vizualization")

sales <- read.csv("Week 3/sales.csv", header = T,
                  stringsAsFactors = F)
library(ggplot2)

p <- ggplot(sales)
p
class(p)
p$data
p$layers
p$scales
summary(p)
View(p$data)
class(p$data)


ggplot(sales) + aes(x = expenses)

plot(sales$expenses)

ggplot(sales,aes(x = expenses, y = income)) + geom_point()

#Save plot for turn in
ggplot(sales,aes(x = expenses, y = income, color = type)) + geom_point()

#6.2.3
ggplot(sales,aes(x = expenses, y = income, color = unit.price  > 14)) + geom_point()

ggplot(sales,aes(x = expenses, y = income)) + geom_point(color = ifelse(sales$unit.price<14,'red','green'))

ggplot(sales,aes(x = expenses, y = income, color = unit.price)) + geom_point()

# Save plot for turn in
ggplot(sales,aes(x = expenses, y = income, color = rep.region, shape = type, alpha = unit.price, size = units.sold)) + geom_point()


p1 <- ggplot(sales)
p2 <- ggplot(sales) +  aes(y = income, x = expenses, shape = rep.region)

summary(p2)

#6.2.5 Geometry

ggplot(sales)+aes(y=income, x= expenses)+geom_point()+geom_rug()

incomepred <- predict(lm(sales$income~sales$expenses))

ggplot(sales)+aes(y=income, x= expenses)+geom_point()+geom_line(aes(y = incomepred), color = "red",  lwd = 3)

ggplot(sales)+aes(y=income, x= expenses)+geom_point()+geom_line(aes(y = incomepred), color = "red",  lwd = 3)

ggplot(sales)+aes(y=income, x= expenses)+geom_point(color = 'pink')+geom_rug()+
  geom_line(aes(y=incomepred)) + geom_line(aes(y=incomepred+150)) + geom_vline(xintercept = 10, color = "blue")+
  geom_hline(yintercept = 500, color = "orange") + geom_abline(intercept = 50, slope = 100, color = 'red', lty = 3, lwd =2)

ggplot(sales)+aes(y=income, x= expenses)+geom_point()+geom_smooth(method = 'loess')
ggplot(sales)+aes(y=income, x= expenses)+geom_point()+geom_smooth()

#overplotting
ggplot(sales)+aes(y=income, x= expenses)+geom_bin2d(bins = 50)

price <- ifelse(sales$unit.price > 14, "expensive", "moderate")
price[sales$unit.price<9] <- "cheap"

#save this plot for turn in
ggplot(sales)+aes(y=income, x= expenses, color = price)+geom_bin2d(bins=50)

# 6.3.2 More Depth

sales2 <- as.data.table(sales)

df <- sales2[,.(units.sold=sum(units.sold)), .(year)]
df2 <- sales2[, .(units.sold=sum(units.sold)),.(year, rep.region)]
colnames(df2)[2] <- "region"

ggplot(sales)+aes(x=income) + geom_blank()
ggplot(sales)+aes(x=income) + geom_histogram(binwidth = 10)

ggplot(sales)+aes(x=income) + geom_histogram(binwidth = 10, fill = 'orange')+
  geom_vline(aes(xintercept=mean(income)), color ="blue", linetype = 'dashed', size = 1)

# save this plot for turn in
ggplot(sales)+aes(x=income) + geom_histogram(binwidth = 10, fill = 'orange', alpha = .9)+
  aes(y = ..density..)+geom_density(alpha = .3, fill = 'blue', color = 'blue')

ggplot(sales) + aes(x = rep.region, y = income) + geom_boxplot()


ggplot(df) + aes(x = year, y = units.sold) + geom_line() + ylim(c(0, 40000))

ggplot(df) + aes(x = year, y = units.sold) + geom_step() + ylim(c(0, 40000))

ggplot(df) + aes(x = year, y = units.sold)+ 
  geom_ribbon(aes(ymin = df$units.sold - 1000, ymax = df$units.sold + 1000), fill = 'yellow') +
  geom_line() + ylim(c(0, 40000))

# save this plot for turn in
ggplot(df2) + aes(x = year, y = units.sold, color = region) + geom_line() + ylim(c(0, 10000))


#6.3.3 geom_bar

df <- sales[,.(sales=sum(units.sold)),.(region=rep.region)]

ggplot(sales) + aes(x=rep.region) + geom_bar(fill = 'blue', width = .5) + ggtitle("Number of Sales by Region")

ggplot(sales) + aes(x=rep.region, fill = type) + geom_bar(position = "dodge")
ggplot(sales) + aes(x=rep.region, fill = type) + geom_bar(position = "fill")

ggplot(df) + aes(x= region, y = sales, fill = region) + geom_bar(stat = "identity")

ggplot(df) + aes(x = "", y=sales, fill = region) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 45)


#6.3.5 Stats

p <-  ggplot(sales)+ aes(x = income)
p + geom_histogram() + stat_bin(binwidth = 5)

p + stat_density()

ggplot(sales) + aes (y= income) + geom_boxplot() + stat_boxplot()

ggplot(sales) + aes(x = expenses, y = income) + stat_bin2d() + stat_density_2d(color = 'red')

ggplot(sales) + aes(x = rep.region) + stat_count()

ggplot(df) + aes(x = region, y = sales) + geom_bar(stat = "identity")

ggplot(sales) + aes(x = income) + geom_histogram(aes(fill = ..count..)) + aes (y = ..density..) + geom_density(fill = ' yellow', alpha = .1)



# Generates 1 pdf file with all the plots on seperate Pages
pdf.options(height = 8.5, width = 11)
pdf(file = "Week 6/Lab6_KW.pdf")

#Save plot for turn in
ggplot(sales,aes(x = expenses, y = income, color = type)) + geom_point()

# Save plot for turn in
ggplot(sales,aes(x = expenses, y = income, color = rep.region, shape = type, alpha = unit.price, size = units.sold)) + geom_point()

#save this plot for turn in
ggplot(sales)+aes(y=income, x= expenses, color = price)+geom_bin2d(bins=50)

# save this plot for turn in
ggplot(sales)+aes(x=income) + geom_histogram(binwidth = 10, fill = 'orange', alpha = .9)+
  aes(y = ..density..)+geom_density(alpha = .3, fill = 'blue', color = 'blue')

# save this plot for turn in
ggplot(df2) + aes(x = year, y = units.sold, color = region) + geom_line() + ylim(c(0, 10000))

ggplot(df) + aes(x = "", y=sales, fill = region) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 45)

ggplot(sales) + aes(x = income) + geom_histogram(aes(fill = ..count..)) + 
  aes (y = ..density..) + geom_density(fill = ' yellow', alpha = .1)

dev.off()

#6.3.2 - need to fix
require(data.table)
sales <- as.data.table(sales)
sales$pricecate <- ifelse(sales$unit.price>14, "expensive", ifelse(sales$unit.price<9,"cheap","moderate"))
pricerange <- sales[,.(minexpense=min(expenses), maxexpense=max(expenses), minincome=min(income), maxincome=max(income)),.(pricecate)]


ggplot(sales) + aes(x=sales$expenses, y = sales$income) + geom_rect(data = pricerange, mapping = aes(xmin = minexpense, xmax = maxexpense,
                                                        ymin = minincome, ymax = maxincome,
                                                        fill = pricecate), alpha = .3)
