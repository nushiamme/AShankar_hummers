## Thank you Liliana Davalos!
## Started Nov 23, 2016

library(MCMCglmm)
library(nlme)
library(ape)
library(geiger)
library(caper)

wdMS <- setwd("C:\\Users\\ANUSHA\\Dropbox\\Hummingbird energetics\\Tables_for_paper")
wdMS
torpor <- read.csv("Torpor_table_plot_Mar26.csv") #Torpor data file, each row is an individual
freq_table <- read.csv("Frequency_torpor.csv") #Table with data for rate of occurrence of torpor per species; each row is a species

mass.agg <- aggregate(torpor$Mass,   #Get mean mass per species
                      by=list(torpor$Species), 
                      FUN="mean", na.rm=T)
names(mass.agg) <- c("Species", "Mass")
mass.agg
freq_table$mass <- mass.agg$Mass # Add mass data to freq_table

tree<-read.tree("hum294.tre")
#show tip names
tree$tip.label
#replace tip names with those in torpor database
tree$tip.label[1]<-"WNJ"
tree$tip.label[12]<-"TBH"
tree$tip.label[92]<-"EMB"
tree$tip.label[93]<-"FBB"
tree$tip.label[95]<-"GCB"
tree$tip.label[219]<-"BBLH"
tree$tip.label[163]<-"BLUH"
tree$tip.label[156]<-"MAHU"

tips<-data.frame(levels(torpor$Species))
colnames(tips) <- "tips"
rownames(tips)<-tips$tips

#match tree to data, prune tree, species names should be in rownnames of "data" 
tre1<-treedata(tree, tips)$phy
plot(tre1)

#prepare data for pgls using pruned data, data you want to run and the column that matches the two in this case, "Species"
data<-comparative.data(tre1,freq_table,"Species")
#run the phyogenetic model
#First testing the effect of mass on rate of occurrence of torpor; rate of occurence is the percentage of 
#individuals of the species that used torpor; first using Brownian motion
m0<-pgls(Rate_occurrence ~ mass,data)
#now using Pagel's lambda tree transform
m1<-pgls(Rate_occurrence ~ mass,data, lambda="ML")
#extract the AIC to compare models using these data. m1 is better
AIC(m0)
AIC(m1)
summary(m1)
