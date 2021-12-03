# Code

#packages used in the code
require(data.table) #call data.table
require(ggplot2)
require(ggmap)
require(raster)
require(stringr)
require(tidyverse)
require(gdata)
require(maps)
require(BBmisc)

setwd("J:/My Drive/Graduate School/IST719 Vizualization/Final Project Data") # Set Working Directory
bbData <- fread("Fixed_Broadband_Deployment_Data__June_2020_V1.csv") #Read in the Data
my.par <- par() #grab main par settings
str(bbData) #Show the Structure of the data

#State Subset

neStates=c("ME",'NH','VT','MA','RI','CT','NY','NJ','PA','DE','MD','DC','VA')
neBBData <- bbData[State %in%neStates & Consumer ==1]

#Reduce Memory Load
rm(bbData)
gc()

neBBData[,.(Records=.N), .(State)]




# Get data for the US and create GGMAP Data Frame
USA <- getData(country = 'USA', level = 2)
map <- fortify(USA, region = 'GID_2') %>% as.data.table()
Index <- as.data.table(USA@data)
Index <- Index[,c(6,4,7,13)]
Index[, ':='(id=GID_2 ,state=tolower(NAME_1), county=tolower(NAME_2), 
             NAME_1=NULL, NAME_2=NULL, GID_2=NULL)]
map <- merge(map, Index, by = 'id')


#Clean up main data set
countyCodes<- county.fips
countyCodes$fips <- stringr::str_pad(countyCodes$fips, '5', pad = '0')
neBBData[,':='(fips=str_sub(str_pad(`Census Block FIPS Code`,'15',pad = '0'),1,5))]
head(neBBData)
countyCodes <- countyCodes %>% separate(polyname, into = c('state','county'), sep = ',')

neBBData <- merge(neBBData, countyCodes, by = 'fips')

#Bring in Technology Transaction from FCC Site
techCode <- fread('TechnologCode.csv')
neBBData <- merge(neBBData, techCode, by.x="Technology Code", by.y = 'Code')

#cleanup Environment
keep(neStates,USA, map, neBBData, sure = T)
gc()

#Aggregated Speeds
speeds <- neBBData[,.(AvgUpMbps=mean(`Max Advertised Upstream Speed (mbps)`),
                     AvgDownMbps=mean(`Max Advertised Downstream Speed (mbps)`)),
                   .(state,county)]
DownMap <- merge(map, speeds, by = c("state","county"), all.x = T)

DownMap$HASC_2 <- str_sub(DownMap$HASC_2,4,5)
DownMap <- DownMap[HASC_2%in%neStates]

#DownMap[,':='(AvgUpMbps=nafill(AvgUpMbps,fill = 0), 
#              AvgDownMbps=nafill(AvgDownMbps, fill = 0))]
#DownMap[,':='(AvgDownMbps=log(AvgDownMbps), AvgUpMbps=log(AvgUpMbps))]
#DownMap[,':='(AvgDownMbps=if_else(AvgDownMbps<0,0,AvgDownMbps),
#              AvgUpMbps=if_else(AvgUpMbps<0,0, AvgUpMbps))]

DownMap$AvgSpeed <- (DownMap$AvgUpMbps+DownMap$AvgDownMbps)/2
colorScale <- colorRampPalette(c("#00bdff","#0080a0","#30c988"))

ggplot()+geom_map(data = DownMap, map = DownMap) + theme_minimal()+
  aes(x=long, y=lat, map_id = id, group = group, fill = AvgSpeed, col = "black")+
  ggtitle("Average Internet Speed in the NE USA")+
  coord_map()+scale_fill_gradientn(colors = colorScale(5), na.value = 'black') +
  guides(colour=guide_legend("No data", override.aes=list(colour="black")))

#cleanup Memory Usage
keep(DownMap, neBBData, colorScale,sure = T)
gc()

#What technology should the customer's use

colnames(neBBData)[14:15] <- c("Downstream","Upstream")

melted <- melt(data = neBBData)
melted <- melted[variable%in%c("Upstream","Downstream")]
gc()
ggplot(data =melted)+
  geom_bar(aes(x = reorder(Translation, -value), y = value, fill=variable),
               stat = 'summary', position = 'dodge', fun='mean')+coord_flip()+
  ylab("Average Max Speeds in Mbps")+xlab("Broadband Type")+theme_minimal() +
  ggtitle("Internet Technology Speed Comparison")+scale_fill_manual(values = colorScale(2), name = "Direction")

#What state has the best speed overall

ggplot(data = melted, aes(x = reorder(state, -value), y = value))+
  geom_bar(stat = 'summary', fill='#00bdff')+facet_grid(~variable)+coord_flip()+
  xlab("State")+ylab("Speed in Mbps")+ggtitle("State Average Max Speeds")

#Speed vs Number of Providers
neBBData[,':='(NmbrProviders=length(unique(`Provider Name`))),
                      .(fips)]
Providers <- neBBData[,.(NmbrProviders=max(NmbrProviders), AvgDown=mean(Downstream)
                         ,AvgUp=mean(Upstream)),.(fips)]
Providers <- melt(data = Providers, id.vars = 1:2, value.name = "SpeedMbps")
ggplot(data = Providers, aes(x=NmbrProviders, y=SpeedMbps))+geom_point()+
  geom_smooth()+facet_grid(~variable)+
  ggtitle("Number of Providers vs. Internet Speeds")+theme_light()


melted <- neBBData[,c(14,15)]
melted <- melt(melted)
melted$value <- log(melted$value)
#boxplot(melted$value~melted$variable, horitzontal = T, col = colorScale(2),
 #       xlab = "Direction of Transmission", ylab="log of Mbps", 
  #      main = "Boxplot of speeds")
colnames(melted) <- c("Direction","LogMbps")
