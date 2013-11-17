## Read in packages
require(ggplot2)
require(reshape)
require(plyr)

## Set working directory
setwd("C://Users//Anusha/Desktop/R scripts/hornbills")

## Read in data
anco <- read.csv("Ancodata_Sept2013.csv")

## Cleaning and aggregating data

### Sort dbh's into size classes. Note: Order dataset by increasing dbh first
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

# Trying a melt function
m.anco <- melt(data=anco, id.vars=c("Number", "habitat2", "nestht"),
               measure.vars= "Building", na.rm =T)

### Plots ###
## Nest tree sps vs. dbh
sps_dbh <- ggplot(anco, aes(x=Species,y=dbh)) + xlab("Tree Species") + 
  geom_point(col="darkgreen") + theme_bw()
sps_dbh

## Habitat vs. distance from building
habitat_building_box <- ggplot(anco, aes(x=habitat2,y=Building)) + xlab("Habitat Type") + 
  geom_boxplot(col="darkgreen", size=0.3) + theme_bw()
habitat_building_box

habitat_building <- ggplot(anco, aes(x=Building,y=habitat2)) + coord_flip() + 
  geom_point(col="darkgreen", size=3) + theme_bw()
habitat_building

## dbh vs. habitat
habitat_dbh <- ggplot(anco, aes(x=habitat2,y=dbh)) + xlab("Habitat Type") + 
  geom_boxplot(col="darkgreen", size=0.3) + theme_bw()
habitat_dbh

## tree ht vs. habitat
habitat_treeht <- ggplot(anco, aes(x=habitat2,y=ht)) + xlab("Habitat Type") + 
  geom_boxplot(col="darkgreen", size=0.3) + theme_bw()
habitat_treeht

## habitat vs. nestht
habitat_nestht <- ggplot(anco, aes(x=habitat2,y=nestht)) + xlab("Habitat Type") + 
  geom_point(col="darkgreen", size=3) + theme_bw()
habitat_nestht

hab_nesthtbox <- ggplot(anco, aes(x=habitat2,y=nestht)) + xlab("Habitat Type") +
  ylab("Nest Height") +  geom_boxplot(col="darkgreen", size=0.3) + theme_bw()
hab_nesthtbox

### dbh vs. distance to Building
dbh_building <- ggplot(anco, aes(x=Building,y=dbh)) + xlab("Distance to habitation") +
  geom_point(col="darkgreen", size=2) + theme_bw()
dbh_building

## Polygon of dbh vs. habitation
# dbh_habpoly <- ggplot(anco, aes(x=Building,y=dbh)) + xlab("Distance to habitation") +
#  coord_flip() +  geom_polygon(col="darkgreen", size=1) + theme_bw()
# dbh_habpoly

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

####
box_anco<-ggplot(m.anco, aes(x=habitat,y=nestht)) +
    geom_boxplot() + theme_bw()
box_anco

## Statistical Analyses

# Test for normality- shapiro.test()
shapiro.test(anco$dbh)
shapiro.test(anco$ht)
shapiro.test(anco$nestht)
shapiro.test(anco$Stream) # Not normal

# Two sample t test for distances
t.test(x=anco$Building[anco$habitat2=="Open"], y=anco$Building[anco$habitat2=="Forest"])
t.test(anco$dbh[anco$Species=="Mangifera indica"], anco$dbh[anco$Species=="Terminalia bellerica"])
t.test(x=anco$Stream[anco$habitat2=="Open"], y=anco$Stream[anco$habitat2=="Forest"])

# Trying linear models to look at what affects nest choice
anco.lm <- lm(dbh ~ Stream + Building + habitat2 + nestht, anco)
anova(anco.lm)