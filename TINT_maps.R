#This code imports site coordinates for the Hummingbird Monitoring Network, converts degrees, minutes,
#seconds to decimal degrees, and plots the data on a map of North America
#Sarah R. Supp and Anusha Shankar

library(maps)
library(mapdata)
library(maptools)
library(scales)
library(RgoogleMaps)
library(ggplot2)
library(ggmap)
library(RColorBrewer)

wd = "E:/Google Drive/Erich_paper_TINT"
wd = "/Users/sarah/Desktop/Dropbox/Hummingbirds/NASA_Anusha/"
setwd(wd)

## I've saved the dms file with the corrected values as a new csv, so we don't have to run the convert()
## code again. Leaving the function in just in case, minimized
tint_pts <- read.csv("All_nests.csv", sep = ",", header = T)

##############
#read in coordinate data (old file)
## hmn = read.csv("dms2dd.csv", sep = ",", header = T)

## Function to convert degree minute seconds to decimal degrees
##convert <-function(coord){
#  tmp1 <- strsplit(coord,"?")
#  tmp2 <- strsplit(tmp1[[1]][2],"'")
#  tmp3 <- strsplit(tmp2[[1]][2],"\"")
#  dec <- c(as.numeric(tmp1[[1]][1]),as.numeric(tmp2[[1]][1]),as.numeric(tmp3[[1]]))
#  c <-abs(dec[1])+dec[2]/60+dec[3]/3600
#  c <-ifelse(dec[1]<0,-c,c)
#  return(c)
##}

## Run loop to use convert() function
# n <- length(hmn$longitude)
# for(i in 1:n){
# hmn$latdd[i] <- convert(as.character(hmn$Latitude_dont[i]))
#  hmn$londd[i] <- (-1)*(convert(as.character(hmn$Longitude_dont[i])))
# }

# add column that shows if sites are currently active
# status = c()
# for (i in 1:length (hmn$Last_active_year)){
#  if (hmn$Last_active_year[i] == 2013) {
#    s = "active"}
#  else { s = "inactive"}
#  status = append(status, s)
#  }
# hmn <- cbind(hmn, status)

## Write to file for future use
# write.csv(hmn, file="dmsdd_corrected.csv")

#################### MAKE SWEET MAPS ########
# tutorial by K. Gilbert: http://www.molecularecologist.com/2012/09/making-maps-with-r/

#HMN study sites on a political map with country outlines
map("worldHires","california", xlim=c(-117,-119), ylim=c(32,34), col="gray90", fill=T)
points(tint_pts$X, tint_pts$Y, pch=19, col="red", cex=0.5)  #plot my sample sites
points(hmn$londd, hmn$latdd, pch=19, col="blue", cex=0.5)  #plot my sample sites

########## Make Even Sweeter R Google Maps

#plots just a terrain map of a specified area, no points
lat <- c(33.0,33.9) #define our map's ylim
lon <- c(-118.0,-118.9) #define our map's xlim
center = c(mean(lat), mean(lon))  #tell what point to center on
zoom <- 5  #zoom: 1 = furthest out (entire globe), larger numbers = closer in
terrmap <- GetMap(center=center, zoom=zoom, maptype= "hybrid", destfile = "terrain.png") #lots of visual options, just like google maps: maptype = c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")

#can plot a subset of the points, but get an error with >34 :(
hmn$size <- "small"  #create a column indicating size of marker
hmn$col <- "red"   #create a column indicating color of marker
hmn$char <- ""   #normal Google Maps pinpoints will be drawn
mymarkers <- cbind.data.frame(hmn$latitude, hmn$longitude, hmn$size, hmn$col, hmn$char)   #create the data frame by binding my data columns of GPS coordinates with the newly created columns
names(mymarkers) <- c("lat", "lon", "size", "col", "char")  #assign column headings
lat <- c(10,64)  #now we are plotting the map
lon <- c(-128,-94)
terrain_close <- GetMap.bbox(lonR=range(lon), latR=range(lat), center=c(37,-111), destfile= "terr_hmn.png", markers= mymarkers, zoom=5, maptype="terrain")

#################### MAKE MAXIMUM SWEETNESS MAPS USING GGPLOT2 AND GGMAPS #######
# tutorial by D. Kahle & H. Wickham http://www.slideshare.net/ajayohri/sexy-maps-comes-to-r-ggplot-google-maps-ggmap-rstats

# next try tutorial for mapping on google
df <- data.frame(x=tint_pts$X, y = tint_pts$Y)
map <- get_googlemap(center=c(-118.4176,33.9702), markers=df, scale = 1, maptype="terrain", zoom = 16)
base <- ggmap(map, extent='device')

#make a map with a legend - BEST ONE SO FAR!!
base <- qmap("Loyola Marymount University, CA", zoom = 16, legend = "bottomleft")
no_years <- base +
  geom_point(tint_pts, aes(X, Y, colour = Year, size = Monitored)) + 
  scale_color_manual(values = c("#FA5882", "black")) # Points are right now pink and black
no_years

active_sites <- hmnmap +
  geom_point(aes(x = londd, y = latdd, colour = status), data = hmn) +
  scale_color_manual(values = c("red", "black"))
active_sites

ggsave(filename="Active_sites.jpeg", plot=active_sites, type="jpeg")