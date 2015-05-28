#SDM extractions and data manipulations for Anusha

#January 3rd 2013

require(raster)
require(maptools)
require(dismo)
require(SDMTools)
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

gc()

#Include which layers you want here:
all.layers <- stack(bio15.crop,bio6.crop,bio12.crop,bio17.crop,bio1.crop,HII.crop,
                    Glob.crop,Anthr.crop,Pop.crop)

#names the rasters
names(all.layers) <- c("bio15","bio6","bio12","bio17","bio1","HII","Glob","Anthro","Pop")

#bioclim only
bio.layers<-stack(bio15.crop,bio1.crop,bio6.crop,bio12.crop,bio17.crop)
names(bio.layers)<-c("bio15","bio6","bio12","bio17","bio1")

#remove the stacked objects
rm(bio15.crop,bio6.crop,bio12.crop,bio17.crop,bio1.crop,HII.crop,Glob.crop,Anthr.crop,Pop.crop)

gc()

# fit model, set which are categorical is a categorical variable
#the input for maxent, just wants the coordinates, which are in the first two columns
#for all layers
me.all <- maxent(all.layers, coordinates(pts)[,1:2],factors='Glob')

#for bioclim layers only
me.bio <- maxent(bio.layers, coordinates(pts)[,1:2])

#predict data
# predict to entire dataset
r.bio <- predict(me.bio, bio.layers, progress='window') 


#predict to all layers
r.all <- predict(me.all, all.layers, progress='window') 

#write rasters to file"
writeRaster(r.bio,"C:\\Users\\Jorge\\Desktop\\shp\\SuitabilityBioclim.tif",overwrite=TRUE)
writeRaster(r.all,"C:\\Users\\Jorge\\Desktop\\shp\\SuitabilityAlllayers.tif",overwrite=TRUE)


#view the outputs

#variable contributions
jpeg("C:\\Users\\Jorge\\Desktop\\shp\\variablecontribution_bioclim2.jpeg",res=300)
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

#evaluate model performance

#withold points for testing

fold <- kfold(pts, k=5)
occtest <- pts[fold == 1, ]
occtrain <- pts[fold != 1, ]

#This will take a second
e1 = evaluate(me.bio, p=coordinates(occtest)[,1:2], a=bg, x=bio.layers)

#Ok for images, not rasters
tiff(filename = "C:\\Users\\Jorge\\Desktop\\shp\\mapwithoutnests_all.tif", overwrite=TRUE)
dev.off()

#WriteRaster- correct code
rf <- writeRaster(r.bio, filename="C:\\Users\\Jorge\\Desktop\\shp\\mapwithout_bio.tif", format="GTiff", overwrite=TRUE)
dev.off()


#see auc score
e1

#Reciever operating curve
plot(e1,"ROC")

#see model evaluation


#save to file
