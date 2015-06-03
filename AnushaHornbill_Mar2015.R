#SDM extractions and data manipulations for Anusha

#January 3rd 2013

require(raster)
require(maptools)
require(dismo)
require(SDMTools)
require(jpeg)
require(ggplot2)
require(gridExtra)

#read in files

## Setwd for Anusha's laptop
#wdlaptop <- setwd("E:/Toshiba_Desktop/Hornbill Paper in Stony Brook/Landcoverdata/")
#wdlaptop

## Setwd for Sarah's computer
wdsarahcomp <- setwd("C:/Users/Anusha/Desktop/Anusha_Hornbill_Files/")
wdsarahcomp

#Human Impact Index
## Old file path HII<-raster("D:././././layers/hii_global_geo_grid/hii_v2geo/w001001x.adf")
HII <- raster("layers/HII/hii_asia_geo_grid/hii_asia/w001001x.adf")

#Global Landcover
## Old file path Glob<-raster("D:././layers/EnvLayers/GLOBCOVER_L4_200901_200912_V2_3.bil")
Glob <- raster("layers/GlobCover_2009/GLOBCOVER_L4_200901_200912_V2_3.bil")

#Anthropogenic Biomes
## Old file path Anthr<-raster("D:././layers/anthromes_v1/w001001x.adf")
Anthr<-raster("layers/anthromes_2_ESRI_GRID/a2000/anthro2_a2000/w001001x.adf")

#Landscan human population size
## Old file path Pop<-raster("D:././layers/EnvLayers/LandScan/lspop2011.bil")
Pop <- raster("layers/LandScan/lspop2011.bil")

#Bring in bioclim layers
bio12<-raster("layers/Bioclim/bio_30s_esri/bio/bio_12/w001001x.adf")
bio17<-raster("layers/Bioclim/bio_30s_esri/bio/bio_17/w001001x.adf")
bio15<-raster("layers/Bioclim/bio_30s_esri/bio/bio_15/w001001x.adf")
bio6<-raster("layers/Bioclim/bio_30s_esri/bio/bio_6/w001001x.adf")
bio1<-raster("layers/Bioclim/bio_30s_esri/bio/bio_1/w001001x.adf")

##load in the points. 
#load shapefile
pts <- readShapePoints("Cleaned data/Compiled without nest points.shp")
ptsWnest <- readShapePoints("with nests/Compiled2.shp")

#If you have XY coordinates, just use SpatialPoints

#draw extent
exte <- c(67, 89, 5, 26)
#plot(bio1,ext=extent(pts)*3)
#exte <- drawExtent()
#drawExtent(show=TRUE, col="red")
#dev.off()

#Crop the rasters by the desired spatial extent
HII.crop <- crop(HII,exte)
Glob.crop <- crop(Glob,exte)
Anthr.crop <- crop(Anthr,exte)
Pop.crop <- crop(Pop,exte)
bio15.crop <- crop(bio15,exte)
bio6.crop <- crop(bio6,exte)
bio12.crop <- crop(bio12,exte)
bio17.crop <- crop(bio17,exte)
bio1.crop <- crop(bio1,exte)

#remove the uncropped files to reduce space. 
rm(bio1,bio6,bio12,bio15,bio17,Pop,Anthr,Glob,HII)
gc()

#resample rasters at the correct extent
HII.crop <- resample(HII.crop,bio6.crop)
Glob.crop <- resample(Glob.crop,bio6.crop)
Anthr.crop <- resample(Anthr.crop,bio6.crop)
Pop.crop <- resample(Pop.crop,bio6.crop)
plot(HII.crop)
gc()

#Include which layers you want here:
all.layers <- stack(bio15.crop,bio6.crop,bio12.crop,bio17.crop,bio1.crop,HII.crop,
                    Glob.crop,Anthr.crop,Pop.crop)

#names the rasters
names(all.layers) <- c("bio15","bio6","bio12","bio17","bio1","HII","Glob","Anthro","Pop")

#bioclim only
bio.layers<-stack(bio15.crop,bio1.crop,bio6.crop,bio12.crop,bio17.crop)
names(bio.layers)<-c("bio15","bio6","bio12","bio17","bio1")

#Landuse layers only
land.layers <- stack(HII.crop,Glob.crop,Anthr.crop,Pop.crop)
names(land.layers) <- c("HII", "Glob", "Anthr", "Pop")


#remove the stacked objects
rm(bio15.crop,bio6.crop,bio12.crop,bio17.crop,bio1.crop,HII.crop,Glob.crop,Anthr.crop,Pop.crop)

gc()

## Download maxent.jar file from "http://www.cs.princeton.edu/~schapire/maxent/" to
## C:/Users/Anusha/Documents/R/win-library/3.2/dismo/java/maxent.jar
## install.packages("rJava")

# fit model, set which are categorical is a categorical variable
#the input for maxent, just wants the coordinates, which are in the first two columns
#for all layers
me.all <- maxent(all.layers, coordinates(pts)[,1:2],factors='Glob')
me.allNest <- maxent(all.layers, coordinates(ptsWnest)[,1:2],factors='Glob')

#for bioclim layers only
me.bio <- maxent(bio.layers, coordinates(pts)[,1:2])
me.bioNest <- maxent(bio.layers, coordinates(ptsWnest)[,1:2])

me.land <- maxent(land.layers, coordinates(pts)[,1:2])
me.landNest <- maxent(land.layers, coordinates(ptsWnest)[,1:2])

#predict data
# predict to entire dataset
r.bio <- predict(me.bio, bio.layers, progress='window') 
r.bioNest <- predict(me.bioNest, bio.layers, progress='window')

#predict to all layers
r.all <- predict(me.all, all.layers, progress='window') 
r.allNest <- predict(me.allNest, all.layers, progress='window') 

#predict to landuse layers
r.land <- predict(me.land, land.layers, progress='window')
r.landNest <- predict(me.landNest, land.layers, progress='window')

#write rasters to file"
writeRaster(r.bio,"shp_results\\SuitabilityBioclim.tif",overwrite=TRUE)
writeRaster(r.all,"shp_results\\SuitabilityAlllayers.tif",overwrite=TRUE)
writeRaster(r.land, "shp_results\\SuitabilityLandlayers.tif",overwrite=TRUE)

writeRaster(r.bioNest,"shp_results\\SuitabilityBioclimNest.tif",overwrite=TRUE)
writeRaster(r.allNest,"shp_results\\SuitabilityAlllayersNest.tif",overwrite=TRUE)
writeRaster(r.landNest, "shp_results\\SuitabilityLandlayersNest.tif",overwrite=TRUE)

#view the outputs

#variable contributions
jpeg("shp_results\\variablecontribution_bioclim2.jpeg",res=300)
plot(me.bio)
dev.off()

#view response curves
response(me.bio)

#predicted map
plot(r.bio)

response(me.all)

plot(me.all)
plot(r.all)

points(pts)

# compare model to background data, drawing psuedo-absence points
bg <- randomPoints(bio.layers, 500)

points(bg,"red")

#withold points for testing

fold <- kfold(pts, k=5)
occtest <- pts[fold == 1, ]
occtrain <- pts[fold != 1, ]

### Make greyscale maps

#This will take a second
eval.bio <-  evaluate(me.bio, p=coordinates(occtest)[,1:2], a=bg, x=bio.layers)
eval.all <- evaluate(me.all, p=coordinates(occtest)[,1:2], a=bg, x=all.layers)
eval.land <- evaluate(me.land, p=coordinates(occtest)[,1:2], a=bg, x=land.layers)

#With nest points included
eval.bioNest <-  evaluate(me.bioNest, p=coordinates(occtest)[,1:2], a=bg, x=bio.layers)
eval.allNest <- evaluate(me.allNest, p=coordinates(occtest)[,1:2], a=bg, x=all.layers)
eval.landNest <- evaluate(me.landNest, p=coordinates(occtest)[,1:2], a=bg, x=land.layers)


#Ok for images, not rasters
#tiff(filename = "shp_results\\mapwithoutnests_all.tif", overwrite=TRUE)
#dev.off()

#WriteRaster- correct code
rf <- writeRaster(r.bio, filename="shp_results\\mapwithout_bio.tif", format="GTiff", overwrite=TRUE)
dev.off()

#Reciever operating curve
jpeg("Model evals without nests.jpg", width = 6, height = 6,units = "in", res=500)
par(mfrow=c(2,2))
ebio <- plot(eval.bio,"ROC") + title(main = "Bio",outer=T)
eall <- plot(eval.all,"ROC") + title(main ="All", outer=T)
eland <- plot(eval.land,"ROC") + title(main ="Land", outer=T)
dev.off()

## Get proportion of map suitable

## For Bioclim-only model
plot(r.bio)
bio.suit <- length(r.bio[r.bio >= 0.6])
tot.bio.suit <- length(r.bio[r.bio >= 0.01])
prop.bio <- bio.suit/tot.bio.suit
prop.bio

###### TODO Either justify 0.6 as expert opinion or choose minimum presence value, or choose 95% minimum presence

## For all-model
plot(r.all)
all.suit <- length(r.all[r.all >= 0.6])
tot.all.suit <- length(r.all[r.all >= 0.01])
prop.all <- all.suit/tot.all.suit
prop.all

## For land-model
plot(r.land)
land.suit <- length(r.land[r.land >= 0.6])
tot.land.suit <- length(r.land[r.land >= 0.01])
prop.land <- land.suit/tot.land.suit
prop.land

print(c("Bioclim-only model", "AUC", ebio, "Proportion suitable bioclim", prop.bio,
      "Bio+land model", "AUC", eall, "Proportion suitable all", prop.all,
      "Landuse-only model", "AUC", eland, "Proportion suitable land", prop.land))

