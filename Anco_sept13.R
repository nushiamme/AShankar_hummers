## Read in packages
require(ggplot2)
require(reshape)
require(plyr)
require(boot)
require(stats)

## Set working directory
setwd("C://Users//Anusha/Desktop/R scripts/hornbills")

## Read in data
#anco2 <- read.csv("Ancodata_Sept2013.csv")
anco <- read.csv("Ancodata_edit.csv")

## Cleaning and aggregating data

## Subset necessary columns
anco <- anco[,c(1,2,5,6,7,8,10,11,12,16,20,21,22)]

### Sort dbh's into size classes. Note: Sort dataset by increasing dbh before doing this.
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

## Seeing how to do an Anova with habitat (categorial, factors) as the measure variable
anco$hab_binary[anco$habitat2=="Open"] <- 0
anco$hab_binary[anco$habitat2=="Forest"] <- 1

###-------- Plots ---------###

## Nest tree sps vs. dbh
sps_dbh <- ggplot(anco, aes(x=Species,y=dbh)) + xlab("Tree Species") + 
  geom_point(col="darkgreen") + theme_bw()+ 
  theme(axis.text.x=element_text(angle=60, vjust=1, hjust=1))
sps_dbh

## Habitat vs. distance from building
habitat_building_box <- ggplot(anco, aes(x=habitat,y=Building)) + xlab("Habitat Type") + 
  geom_boxplot(col="darkgreen", size=0.3) + theme_bw()
habitat_building_box

## Habitat vs. distance from stream
habitat_stream <- ggplot(anco, aes(x=habitat,y=Stream)) + xlab("Habitat Type") + 
  theme_bw() + geom_point() # geom_boxplot(col="darkgreen", size=0.3)
habitat_stream

## Comparing building and stream distances vs habitat. 
# Need to add legend- red is building, blue is stream
habitat_building <- ggplot(anco, aes(x=habitat,y=Building)) + xlab("Habitat") +
  ylab("Distance") + geom_point(col="red", size=3) + theme_bw()
habitat_building + geom_point(data=anco, aes(x = habitat, y = Stream), col="blue", size=3)

## dbh vs. habitat
habitat_dbh <- ggplot(anco, aes(x=habitat,y=dbh)) + xlab("Habitat Type") + 
  geom_boxplot(col="darkgreen", size=0.3) + theme_bw()
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
dbh_building <- ggplot(anco, aes(x=Building,y=dbh)) + xlab("Distance to habitation") +
  geom_point(col="darkgreen", size=2) + theme_bw()
dbh_building

## Nest tree sps vs. height
sps_treeht_box <- ggplot(anco, aes(x=Species,y=ht)) + xlab("Tree Species") + 
  geom_boxplot(col="darkgreen", size=0.3) + theme_bw()
sps_treeht_box

## Nest tree sps vs. nest height
sps_nestht <- ggplot(anco, aes(x=Species,y=nestht)) + xlab("Tree Species") + 
  geom_boxplot(col="darkgreen", size=.3) + theme_bw()
sps_nestht

## Habitat type vs. dbh
vegetation <- ggplot(anco, aes(x=habitat,y=dbh)) + geom_boxplot(col="darkgreen") + theme_bw()
vegetation

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

# Two sample t test for distances
t.test(x=anco$Building[anco$habitat=="Open"], y=anco$Building[anco$habitat=="Forest"])
t.test(anco$dbh[anco$Species=="Mangifera indica"], anco$dbh[anco$Species=="Terminalia bellerica"])
t.test(x=anco$Stream[anco$habitat=="Open"], y=anco$Stream[anco$habitat=="Forest"])
t.test(x=anco$Road, y=anco$Building)
## Useful? What does this mean?
ks.test(x=anco$Stream, y=anco$Building)

# Trying linear models to look at what affects nest choice
anco.lm <- lm( ~ Stream + Building + nestht + dbh, anco)
anova(anco.lm)

pca_anco <- prcomp(~dbh + ht + nestht + Stream + Road + Building, data=anco, scale=T)
plot(pca_anco$x[,1], pca_anco$x[,2], cex=0.5, pch=16, xlab="PC1", ylab="PC2", xlim=c(-4,5))
text(pca_anco$x[,1], pca_anco$x[,2], labels=(anco$Number), pos=2, offset=0.3, cex=0.6)
