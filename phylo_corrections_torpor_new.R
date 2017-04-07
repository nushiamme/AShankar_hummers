## Thank you Liliana Davalos!
## Started Nov 23, 2016

library(MCMCglmm)
library(nlme)
library(ape)
library(geiger)
library(caper)

setwd("C:\\Users\\shankar\\Dropbox\\Hummingbird energetics\\Submission_Oct2016")

torpor <- read.csv("Torpor_individual_summaries.csv", sep=";") #Torpor data file, each row is an individual
torpor$NEE_MassCorrected<- torpor$NEE_kJ/(torpor$Mass^(2/3))
#First converting NA's in Hours_torpid into 0's.
torpor$Hours2 <- torpor$Hours_torpid
torpor$Hours2[is.na(torpor$Hours2==TRUE)] <- 0
#Making a column for energy savings as a proportion of energy expenditure (normothermic-torpor)/torpor
torpor$savings2 <- 100-torpor$Percentage_avg
##### CHANGE THIS - ASK Liliana
torpor$savings2[is.na(torpor$savings2)] <- 0
torpor$savings2 <- scale(torpor$savings2, center=T, scale=F)[,1]
#Make Temptrop into a binary variable
torpor$Temptrop2 <- as.character(torpor$Temptrop)
torpor$Temptrop2[torpor$Temptrop2=="Temperate"] <- 0
torpor$Temptrop2[torpor$Temptrop2=="Tropical"] <- 1
torpor$Temptrop2<- as.numeric(torpor$Temptrop2)

#Combine duration and savings to make a composite measure
torpor$combined <- torpor$Hours2*torpor$savings2

#Table with data for rate of occurrence of torpor per species; each row is a species
freq_table <- read.csv("Frequency_torpor_sp.csv") 

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

#prepare data for pgls using pruned data, data you want to run and the column that matches the two 
#in this case, "Species"
data<-comparative.data(tre1,freq_table,"Species")
#run the phyogenetic model
#Testing the effect of mass on rate of occurrence of torpor; Freq_torpor (frequency of torpor) is the percentage of 
#individuals of the species that used torpor; first using Brownian motion
m0<-pgls(Freq_torpor ~ mass,data)
#now using Pagel's lambda tree transform
m1<-pgls(Freq_torpor ~ mass,data, lambda="ML")
#extract the AIC to compare models using these data. m1 is better
AIC(m0)
AIC(m1)
summary(m1)

## Now, to run Bayesian models with repeated measures per species (i.e. multiple individuals per species), 
#we setup an inverse matrix and set up a prior
#Using a Bayesian rather than a maximum likelihood model because with an ML model we could include 
#repeated measures, OR we could include a phylogenetic structure. 
#But to get a hierarchy, with both a phylogeny and then repeated measures 
#within the phylogeny, we need turn the phylogeny into an inverse matrix
inv.phylo<-inverseA(tre1,nodes="TIPS",scale=TRUE)
#set up a prior for a phylogenetic mixed model
prior<-list(G=list(G1=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))
#run the hierarchical phyogenetic model, the name of the species (repeated across rows of observations) 
## Without mass-corrections - don't use
m2<-MCMCglmm(NEE_kJ~Mass+Hours2+Tc_min_C, random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
             prior=prior, data=torpor, verbose=FALSE)
summary(m2)

## Used this model up to April 2017
m3<-MCMCglmm(NEE_MassCorrected~Mass+Hours2+Tc_min_C, random=~Species, 
             ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, verbose=FALSE)
summary(m3)

## New model including energy savings and temp/trop
m4<-MCMCglmm(NEE_MassCorrected~Mass+Hours2+Tc_min_C+savings, random=~Species, 
             ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor[torpor$Hours2!=0,], verbose=FALSE)
summary(m4)

## Without any phylogenetic corrections- shows that results have an inflated significance when 
#phylo corrections are not done
m5<-MCMCglmm(NEE_MassCorrected~Mass+Hours2+Tc_min_C+savings, data=torpor[torpor$Hours2!=0,])
summary(m5)
