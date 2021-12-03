#Kyle Walter
#Week 4 Lab - Layouts in R

require(data.table)

setwd("G:/My Drive/Graduate School/IST719 Vizualization/Week 4")
sales <- fread("sales.csv")

dat.1 <- tapply(sales$units.sold, list(sales$wine), sum)
dat.2 <- tapply(sales$income, list(sales$wine), sum)

par(mfrow = c(2, 1))
par(mar = c(.5,5,4,1), cex.lab = .8)

barplot(dat.2, xaxt = "n", las = 2, )
mtext(text = "income", side = 2, line = 4, adj = 0)
mtext(text = "income on Units Sold", side = 3, line = 1, adj = 0, cex = 1.3)
par(mar = c(6,5,0,1), cex.lab = .8)
bar.out <- barplot(dat.1, xaxt = "n", las = 2)
axis(side = 1, at = bar.out, labels = gsub(" ", "\n", names(dat.2)), las = 2)

m <- matrix(
  c( 1,1,1
    ,1,1,1
    ,2,2,2)
    , nrow = 3, byrow = T
)
layout(m)
layout.show(2)
par(mar = c(.5,5,4,1), cex.lab = .8)
barplot(dat.2, xaxt = "n", las=2)
mtext(text = "income", side = 2, line = 4, adj = 0)
mtext(text = "Income on Units Sold", side = 3, line = 1, adj = 0, cex = 1.3)
par(mar = c(6,5,0,1), cex.lab = .8)
bar.out <- barplot(dat.1, xaxt = "n", las = 2)
axis(side = 1, at = bar.out, labels = gsub(" ", "\n", names(dat.2)), las = 2)

m <- matrix(
  c( 1,1,1,3
     ,1,1,1,4
     ,2,2,2,5)
  , nrow = 3, byrow = T
)
layout(m)
layout.show(5)
par(mar = c(.5,5,4,1), cex.lab = .8)
barplot(dat.2, xaxt = "n", las=2)
mtext(text = "income", side = 2, line = 4, adj = 0)
mtext(text = "Income on Units Sold", side = 3, line = 1, adj = 0, cex = 1.3)
par(mar = c(6,5,0,1), cex.lab = .8)
bar.out <- barplot(dat.1, xaxt = "n", las = 2)
axis(side = 1, at = bar.out, labels = gsub(" ", "\n", names(dat.2)), las = 2)

par( mar = c(1,1,1,1))
pie(dat.1)
pie(dat.1)
pie(dat.1)

nf <- layout(matrix(c(2,0,1,3),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)
layout.show(nf)

m <- matrix(
  c(
    1,2,2
    ,4,2,2
    ,3,3,5
  ), nrow = 3, byrow = T
)
layout(m)
layout.show(5)

# Part 2 - Layouts in R

dat.3 <- tapply(sales$units.sold, list(sales$type), sum)
dat.4 <- tapply(sales$units.sold, list(sales$rep.region), sum)
dat.5 <- tapply(sales$units.sold, list(sales$year), sum)

split.screen(figs = c(2,1))
screen(1)
pie(dat.1)
screen(2)
pie(dat.2)

screen(1, new = F)
mtext("Kyle", side = 3, line =1)

screen(2, new = F)
mtext("Here", side = 3, line =1)

close.screen(1:2)

split.screen(figs = c(2,1))
screen(1)
pie(dat.1)
screen(2)
pie(dat.2)

split.screen(c(2,2))
screen()
pie(dat.3)

# Week 4 Lab : Fonts

n <- 500
x <- abs(rnorm(n,6,2))
y <- x^2+(rnorm(n, 0,2*x))

my.par <- par()
my.par$adj
my.par$family

plot(x, y)
my.par$font

plot(x, y, main="Fiddling with fonts", xlab = "some x lable", ylab = "ylab text")

plot(x, y, main="Fiddling with fonts", xlab = "some x lable", ylab = "ylab text", font = 2)
plot(x, y, main="Fiddling with fonts", xlab = "some x lable", ylab = "ylab text", font = 3)
plot(x, y, main="Fiddling with fonts", xlab = "some x lable", ylab = "ylab text", font = 4)
plot(x, y, main="Fiddling with fonts", xlab = "some x lable", ylab = "ylab text", font = 5)
plot(x, y, main="Fiddling with fonts", xlab = "some x lable", ylab = "ylab text", font.axis = 2, font.lab =3, font.main =1)

plot(x, y, main="Fiddling with fonts", xlab = "some x lable", ylab = "ylab text", family =  "HersheyGothicEnglish")

par(family = "sans")
plot(x, y, main="Fiddling with fonts", xlab = "some x lable", ylab = "ylab text")

plot(1:10, 1:10, type = "n")
windowsFonts(
  A = windowsFont("Arial Black"),
  B = windowsFont("Bookman Old Style"),
  C = windowsFont("Comic Sans MS"),
  D = windowsFont("Symbol")
)

text(2,2, "Hello world default")
text(4,4, "Hello world Arial", family = "A")
text(6,6, "Hello world Bookman", family = "B")
text(8,8, "Hello world Comic", family = "C")
text(10,10, "Hello world Symbol", family = "D")

library(extrafont)
loadfonts(device = "win")
fonts()
