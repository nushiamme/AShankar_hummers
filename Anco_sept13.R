## Anusha Hornbill nest data
# Updated : December 4th, 2013

## Read in packages
library(ggplot2)
library(reshape)
library(plyr)
library(boot)
library(stats)
library(maps)
library(mapdata)
library(maptools)
library(scales)
library(RgoogleMaps)
library(ggmap)
library(raster)
library(rgdal)
library(jpeg)

## Set working directory
## setwd("D://Dropbox/Hornbills/")
setwd("C://Users//ANUSHA//SkyDrive//Hornbills")


## Read in data
#anco2 <- read.csv("Ancodata_Sept2013.csv")
anco <- read.csv("Ancodata_edit.csv")
# Read in ebird data!
ebird <- read.csv("ebird2_dec13.csv")
# Ebird data used for poster
ebird_old <- read.csv("ebird_old.csv")

readJPEG("")

## Cleaning and aggregating data

## Subset necessary columns
anco <- anco[,c(1,2,5,6,7,8,9,11,12,13,14,17,21,22,23,24)]

#Change direction N-NW to NW
anco$Direction[anco$Direction=="N-NW"] <- "NW"

## Loop to count number of nests in each direction
nest.directions <- 0
direc <- function(dat) {
for (i in levels(factor(dat))) {
  nest.directions[i] <- length(dat[dat==i])
  }
return(nest.directions)
}
nest.directions <- direc(anco$Direction)

##### Sort dbh's into size classes. ###### 
#Note: Sort dataset by increasing dbh before doing this.
anco$dbhcl <- 0
n <- length(anco$dbh)
cl <- (max(anco$dbh) - min(anco$dbh))/5
for(i in 1:n) {
  if ((anco$dbh[i]) <= (anco$dbh[1] + cl)) {
    anco$dbhcl[i] <- 1
  } else if ((((anco$dbh[i])-(anco$dbh[1]+cl))*((anco$dbh[1] + (cl*2))-(anco$dbh[i]))) >0) {
    anco$dbhcl[i] <- 2
  } else if ((((anco$dbh[i])-(anco$dbh[1]+(cl*2)))*((anco$dbh[1] + (cl*3))-(anco$dbh[i]))) >0) {
    anco$dbhcl[i] <- 3
  } else if ((((anco$dbh[i])-(anco$dbh[1]+(cl*3)))*((anco$dbh[1] + (cl*4))-(anco$dbh[i]))) >0) {
    anco$dbhcl[i] <- 4
  } else {
    anco$dbhcl[i] <- 5
  }  
}
# Check if it worked
anco$dbhcl

##### End sort #####

## Calculate mean and s.e. for nest site characters
av.dbh <- mean(anco$dbh)
av.dbh
se.dbh <- sd(anco$dbh)/sqrt(length(anco$dbh))
sd.dbh <- sd(anco$dbh, na.rm=T)
sd.dbh


av.ht <- mean(anco$ht, na.rm=T)
av.ht
se.ht <- sd(anco$ht, na.rm=T)/sqrt(length(anco$ht))
sd.ht <- sd(anco$ht, na.rm=T)
sd.ht

av.nestht <- mean(anco$nestht, na.rm=T)
av.nestht
se.nestht <- sd(anco$nestht, na.rm=T)/sqrt(length(anco$nestht))
sd.nestht <- sd(anco$nestht, na.rm=T)
sd.nestht

## Seeing how to do an Anova with habitat (categorial, factors) as the measure variable
anco$hab_binary[anco$habitat=="Open"] <- 0
anco$hab_binary[anco$habitat=="Forest"] <- 1

## Nest site parameters by forest type
mean(anco$dbh[anco$hab_binary==0]) # open
mean(anco$dbh[anco$hab_binary==1]) # forest
sd(anco$dbh[anco$hab_binary==0]) # open
sd(anco$dbh[anco$hab_binary==1]) # forest

## Classify forest distances

forest_class <- function (forest) {
  for (i in 1:length(forest)) {
    if (forest[i] <500) {
      anco$forestclass[i] <- 1
    } else if (500 <= (forest[i]) & (forest[i]) < 1000) {
      anco$forestclass[i] <- 2
    } else if (1000 <= (forest[i]) & (forest[i]) < 1500) {
      anco$forestclass[i] <- 3
    } else if (1500 <= (forest[i]) & (forest[i]) < 2000) {
      anco$forestclass[i] <-4
    } else if (2000 <= (forest[i])) {
      anco$forestclass[i] <- 5
    }
  }
  return(anco$forestclass)
}

anco$forestclass <- forest_class(anco$Forest)

## Logistic regression



###-------- Plots ---------###

## Nest tree sps vs. dbh
sps_dbh <- ggplot(anco, aes(x=Species,y=dbh)) + xlab("Tree Species") + 
  geom_point(col="darkgreen") + theme_bw() + 
  theme(axis.text.x=element_text(angle=60, vjust=1, hjust=1))
sps_dbh

## Habitat vs. distance from building --------- Significant
habitat_building_box <- ggplot(anco, aes(x=habitat,y=Building)) + xlab("Habitat Type") + 
  geom_boxplot(col="darkgreen", size=0.3) + theme_bw() + ggtitle("Distance to building by Habitat type") +
  theme(plot.title = element_text(size=20)) +
  theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5))) +
  theme(axis.text.x = element_text(size=13)) + theme(axis.text.y = element_text(size=13))
habitat_building_box

## Habitat vs. distance from stream
habitat_stream <- ggplot(anco, aes(x=habitat,y=Stream)) + xlab("Habitat Type") + 
  theme_bw() + geom_boxplot() # geom_boxplot(col="darkgreen", size=0.3)
habitat_stream

## Comparing building and stream distances vs habitat. 
# Need to add legend- red is building, blue is stream
#habitat_building <- ggplot(anco, aes(x=habitat,y=Building)) + xlab("Habitat") +
#  ylab("Distance") + geom_point(col="red", size=3) + theme_bw()
#habitat_building + geom_point(data=anco, aes(x = habitat, y = Stream), col="blue", size=3)

## dbh vs. habitat
habitat_dbh <- ggplot(anco, aes(x=habitat,y=dbh)) + xlab("Habitat Type") + 
  geom_boxplot(col="darkgreen", size=0.3) + theme_bw() + ggtitle("dbh by Habitat Type")
habitat_dbh
## As long as you have big trees in open habitat, good. 

## tree ht vs. habitat
habitat_treeht <- ggplot(anco, aes(x=habitat,y=ht)) + xlab("Habitat Type") +
  geom_boxplot(col="darkgreen", size=0.3) + theme_bw()
habitat_treeht

## habitat vs. nestht
habitat_nestht <- ggplot(anco, aes(x=habitat,y=nestht)) + xlab("Habitat Type") + 
  geom_point(col="darkgreen", size=3) + theme_bw()
habitat_nestht

hab_nesthtbox <- ggplot(anco, aes(x=habitat,y=nestht)) + xlab("Habitat Type") +
  ylab("Nest Height") +  geom_boxplot(col="darkgreen", size=0.3) + theme_bw()
hab_nesthtbox ## Check outlier

### dbh vs. distance to Building
dbh_building <- ggplot(anco, aes(x=Building,y=dbh)) + 
  xlab("Distance to nearest building") +
  geom_point(col="darkgreen", size=3) + theme_bw() + ggtitle("dbh by Distance to building") +
  theme(plot.title = element_text(size=20)) +
  theme(axis.title.x = element_text(size = rel(1.5))) + theme(axis.title.y = element_text(size = rel(1.5))) +
  theme(axis.text.x = element_text(size=13)) + theme(axis.text.y = element_text(size=13))
dbh_building

### dbh vs. distance to Forest
dbh_forest <- ggplot(anco, aes(x=forestclass,y=dbh)) + #stat_smooth(method='lm') +
  geom_line(mapping=) + theme_bw()
dbh_forest

## Nest tree sps vs. height
sps_treeht_box <- ggplot(anco, aes(x=Species,y=ht)) + xlab("Tree Species") + 
  geom_boxplot(col="darkgreen", size=0.3) + theme_bw()
sps_treeht_box

## Nest tree sps vs. nest height ------ Interesting
sps_nestht <- ggplot(anco, aes(x=Species,y=nestht)) + xlab("Tree Species") + 
  geom_boxplot(col="darkgreen", size=.3) + theme_bw()
sps_nestht

## Nest cavity direction
direc <- ggplot(anco, aes(Direction)) + geom_bar() + theme_bw()
direc

## Statistical Analyses
## Nest trees have similar characteristics as in the forest, and they seem to "find" them.
## 

# Test for normality- shapiro.test()
shapiro.test(anco$dbh)
shapiro.test(anco$ht)
shapiro.test(anco$nestht) # not normal
shapiro.test(anco$Stream) # Not normal

## Test for correlations between variables.
cor.test(anco$Building, anco$Road, method="pearson")
## For the fun of it,
t.dbh.ht <- cor.test(anco$dbh, anco$ht)
c(t.dbh.ht$estimate, t.dbh.ht$p.value)
t.nestht.ht <- cor.test(anco$nestht, anco$ht)
c(t.nestht.ht$estimate, t.nestht.ht$p.value)
t.nestht.dbh <- cor.test(anco$nestht, anco$dbh)
c(t.nestht.dbh$estimate, t.nestht.dbh$p.value)

# Two sample t test for distances
t.test(x=anco$Building[anco$habitat=="Open"], y=anco$Building[anco$habitat=="Forest"]) ## Significant
t.test(anco$dbh[anco$Species=="Mangifera indica"], anco$dbh[anco$Species=="Terminalia bellerica"])
t.test(x=anco$Stream[anco$habitat=="Open"], y=anco$Stream[anco$habitat=="Forest"])
t.test(x=anco$Road, y=anco$Building)
cor.test(anco$nestht,anco$Forest)

# Trying linear models to look at what affects nest choice
anco.lm <- lm(dbh ~ Stream + Building + Forest + Elevation_m, anco)
anova(anco.lm)

# PCA. Think more.
pca_anco <- prcomp(~dbh + Stream + Building + Forest + sp_num, data=anco, scale=T)
plot(pca_anco$x[,1], pca_anco$x[,2], cex=0.5, pch=16, xlab="PC1", ylab="PC2", xlim=c(-4,5))
text(pca_anco$x[,1], pca_anco$x[,2], labels=(anco$Number), pos=2, offset=0.3, cex=0.6)

## To Do
prcomp


# Trying poisson for orientation
chisq.test(nest.directions)

### ------------- Mapping points
## Function to convert degree minute seconds to decimal degrees
convert <-function(coord){
  tmp1 <- strsplit(coord,"°")
  tmp2 <- strsplit(tmp1[[1]][2],"'")
  tmp3 <- strsplit(tmp2[[1]][2],"\"")
  dec <- c(as.numeric(tmp1[[1]][1]),as.numeric(tmp2[[1]][1]),as.numeric(tmp3[[1]]))
  c <-abs(dec[1])+dec[2]/60+dec[3]/3600
  c <-ifelse(dec[1]<0,-c,c)
  return(c)
}

## Run loop to use convert() function
n <- length(anco$Longitude)
for(i in 1:n){
  anco$lat[i] <- convert(as.character(anco$Latitude[i]))
  anco$lon[i] <- convert(as.character(anco$Longitude[i]))
}

# Google Map
df <- data.frame(x=anco$lon, y = anco$lat)
names(df) <- c("lon","lat")
map <- get_googlemap(center=c(73.65,17.05), scale = 1, maptype="terrain",
                     zoom = 11, color="bw")
exte<-drawExtent()
drawExtent(show=TRUE, col="red")
dev.off()

# ggmap
nestpts <- ggmap(map, extent='exte') + 
  geom_point(aes(x = lon, y = lat), data = df, size = 3.5, colour = 'black', pch=17)
nestpts

# qmap- Best probably. 1. For colour
ancomap <- qmap("Devrukh, India", zoom = 11, legend = "bottomleft", color="color", maptype="hybrid")
nest_pts <- ancomap +
  geom_point(aes(x = lon, y = lat),
             data = anco, size=4, col='red')
nest_pts

# qmap 2. for bw
ancomap_bw <- qmap("Devrukh, India", zoom = 11, legend = "bottomleft", color="bw", maptype="terrain")
nests_bw <- ancomap_bw +
  geom_point(aes(x = lon, y = lat), data = anco, size=4)
nests_bw

india <- qmap("Solapur, India", zoom=5, maptype="terrain", color="bw")

ebirdmap <- india + geom_point(aes(x=LONGITUDE, y=LATITUDE), col="red", data=ebird) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE), col="blue", data=ebird_old)
ebirdmap