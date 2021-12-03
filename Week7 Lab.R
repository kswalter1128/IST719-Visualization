#Lab Weeks 7

# Map Basics and Choropleths

require(data.table)
require(ggplot2)

setwd("G:/My Drive/Graduate School/IST719 Vizualization")
df <- fread("Week 7/maplecturedata.csv")

plot(df$x, df$y)
polygon(df$x, df$y, col = "firebrick1", border = NA)

library(maps)
library(mapproj)

map(database = "world")
map("world", regions = 'India')
map("world", regions = 'China')
map('world', regions = c("India", "Pakistan"), fill = T, col = c('orange','brown'))

m <- map('state')

map('state', fill = T, col = c('orange','red','yellow'))
map('county', region = 'New York', fill = T, col = terrain.colors(20))

require(rnaturalearth)
require(rnaturalearthdata)
india <- ne_states(country='India')
map(india)

attributes(india)
names(india)
india$name
map(india, namefield = 'name',regions = c('Gujarat', 'Rajasthan','Madhya','Pradesh'),
    fill = T, col = c('orangered','white','springgreen3'))

require(raster)
india <- raster::getData("GADM",country = "IND", level =1 )
map(india)

map(india, namefield = "NAME_1", regions = 'Gujarat')

india <- raster::getData("GADM",country = "IND", level =2 )
map(india)

map(india, namefield = "NAME_2", regions = 'North 24 Parganas',
    fill = T, col = 'springgreen4')

Japan <- raster::getData("GADM",country = "JPN", level =1)
map(Japan, fill = T, col = terrain.colors(47))

# 7.1.3

load("Files for Class from 2U/shootings.rda")
shootings$Total.Number.of.Victims

sort(shootings$State)

tmp.vec <- gsub("^\\s+|\\s+$","",shootings$State)
sort(tmp.vec)
shootings$State <- tmp.vec

agg.dat <- aggregate(shootings$Total.Number.of.Victims, list(shootings$State), sum)

colnames(agg.dat) <- c("state","victims")
num.cols <- 10
My.Color.vec <- rev(heat.colors(num.cols))

library(plotrix)

agg.dat$index <- round(rescale(agg.dat$victims, c(1,10)),0)
agg.dat$color <- My.Color.vec[agg.dat$index]

#7.3.5

m <- map('state')

state.order <- match.map('state',regions = agg.dat$state, exact = F, warn = T)

cbind(m$names, agg.dat$state[state.order])

map('state', col = agg.dat$color[state.order],
       fill = T, resolution=0, lty = 1, 
       projection = 'polyconic', border = 'tan')
#7.2.1 Points and Geocoding
map('world')
points(0,0,col='red', cex = 3, pch = 8)
abline(h = 43, col = 'blue', lty = 3) #lat
abline(v = -76, col = 'blue', lty = 3) #long

us.cities
map('state')
my.cols <- rep(rgb(1,.6,.2,.7), length(us.cities$name))
my.cols[us.cities$capital>0] <- rgb(.2,.6,1,.9)

points(us.cities$long, us.cities$lat, col = my.cols
       , pch = 16, cex = rescale(us.cities$pop, c(.5,7)))

require(ggmap)
libs <- fread("Files for Class from 2U/newyorklibraries.csv", quote = "\"")

require(tmaptools)
tmaptools::geocode_OSM("3649 Erie Blvd East, Dewitt, NY")

index <- which(libs$CITY %in% c("SYRACUSE","DEWITT","FAYETTEVILLE"))
addy <- paste(libs$ADDRESS[index],libs$CITY[index],
              libs$STABR[index], sep = " ")

map('county', 'new york', fill = T, col = 'orange')
g.codes <- geocode_OSM(addy)
points(g.codes$lon, g.codes$lat, col = 'blue', cex =1.1, pch = 16)


#7.3 More Maps Stuff

require(rworldmap)
#also using plotrix but already loaded

countries<- fread("Files for Class from 2U/countries.csv")
range(countries$`Life expectancy`)
zap <- which(countries$`Life expectancy`==0.0)
countries <- countries[-zap,]
range(countries$`Life expectancy`)
rm(zap)

num.cat <- 10

iso3.codes <- tapply(countries$`Country (en)`, 
                     1:length(countries$`Country (en)`), 
                     rwmGetISO3)

df <- data.table(country = iso3.codes, labels = countries$`Country (en)`, life = countries$`Life expectancy`)

df.map <- joinCountryData2Map(df, joinCode = "ISO3"
                              , nameJoinColumn = "country")

par(mar=c(0,0,1,0))

mapCountryData(df.map
               , nameColumnToPlot = 'life'
               , numCats = num.cat
               , catMethod = c('pretty','fixedwidth','diverging','quantitles')[2]
               , colourPalette = colorRampPalette(c('orangered','palegoldenrod','forestgreen'))(num.cat)
               , oceanCol = 'royalblue4', borderCol = 'peachpuff', mapTitle = 'Life Expectancy'
               )

#india is still loaded

reported <- fread("Files for Class from 2U/indiareportedrapes.csv")
india <- getData("GADM", country = 'IND', level = 1)

cbind(unique(reported$Area_Name), india$NAME_1)

india$NAME_1[india$NAME_1=='NCT of Delhi'] <- 'Delhi'
india$NAME_1 <- gsub(" and ", " & ", india$NAME_1)

map <- fortify(india, regions = "NAME_1")
index <- data.frame(index = 1:36, location = india$NAME_1)
map$idnum <- as.numeric(map$id)
map <- merge(map, index, by.x = "idnum", by.y='index')
map$id <- map$location
map <- map[,2:8]
map$group2 <- as.character(map$group)
map$group2 <- paste0(map$id,stringr::str_extract(map$group2,"\\.\\d+"))
map$group <- as.factor(map$group2)

head(map)
crimes <- aggregate(reported$Cases, list(reported$Area_Name), sum)
colnames(crimes) <- c("id",'ReportedRapes')
crimes[order(crimes$ReportedRapes),]

my.map <- merge(map, crimes, by = 'id')

ggplot() + geom_map(data = my.map, map = my.map) +
  aes(x = long, y = lat, map_id = id, group = group, fill = ReportedRapes) +
  theme_minimal() + ggtitle('Reported Rapes in India')


#7.3.5

library(stringr)
library(rgdal)
library(raster)
library(TeachingDemos)


bikes <- readRDS("Files for Class from 2U/bikes.rds")
nypp <- readOGR("Files for Class from 2U", "nyct2010", stringsAsFactors = F)

syr.neighborhood <- readOGR("Files for Class from 2U/syracuse-neighborhoods_ny.geojson")

par(mar=c(.5,.5,.5,.5))

plot(nypp, border = 'bisque4', lwd = .5)
zoomplot(c(978000,999800), ylim = c(185000, 225000))

df.bike <- data.frame(lat = bikes$start.station.latitude,
                      lon = bikes$start.station.longitude)

head(df.bike)


point.tab <- sort(table(paste(df.bike$lat, df.bike$lon)), decreasing = T)

df.2 <- data.frame(lat = as.numeric(word(names(point.tab), 1)),
                   lon = as.numeric(word(names(point.tab), 2)))


df.2$size <- as.numeric(point.tab)

coordinates(df.2) <- ~lon + lat
crs(df.2) <- CRS("+proj=longlat +datum=WGS84")

df.2 <- spTransform(df.2, crs(nypp))

tmp.size <- .2+(2*df.2$size/max(df.2$size))
points(df.2$lon, df.2$lat, col = 'red', pch = 19, cex = tmp.size)


# These in for the lab
pdf("Week 7/Lab7_KW.pdf")

map(Japan, fill = T, col = terrain.colors(47))
map('state', col = agg.dat$color[state.order],
    fill = T, resolution=0, lty = 1, 
    projection = 'polyconic', border = 'tan')
map('county', 'new york', fill = T, col = 'orange')
points(g.codes$lon, g.codes$lat, col = 'blue', cex =1.1, pch = 16)
mapCountryData(df.map
               , nameColumnToPlot = 'life'
               , numCats = num.cat
               , catMethod = c('pretty','fixedwidth','diverging','quantitles')[2]
               , colourPalette = colorRampPalette(c('orangered','palegoldenrod','forestgreen'))(num.cat)
               , oceanCol = 'royalblue4', borderCol = 'peachpuff', mapTitle = 'Life Expectancy'
)
ggplot() + geom_map(data = my.map, map = my.map) +
  aes(x = long, y = lat, map_id = id, group = group, fill = ReportedRapes) +
  theme_minimal() + ggtitle('Reported Rapes in India')

plot(nypp, border = 'bisque4', lwd = .5)
zoomplot(c(978000,999800), ylim = c(185000, 225000))
points(df.2$lon, df.2$lat, col = 'red', pch = 19, cex = tmp.size)
dev.off()
