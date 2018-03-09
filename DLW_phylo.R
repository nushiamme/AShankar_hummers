## Hummingbird FMR allometry
## Paper authors: Anusha Shankar, Catherine H Graham, Donald R Powers
## Code by: Anusha Shankar, github/nushiamme; 
# contact: anusha<dot>shankar<at>stonybrook<dot>edu
## MCMCglmm models, accounting for both the phylogenetic structure and 
# the repeated-measures per species

### Contents
## Setup, read files in, format data
## Figures

library(MCMCglmm)
library(nlme)
library(ape)
library(geiger) # for treedata() function
library(caper)
library(coda) # only for autocorr function

#### Setup ####
setwd("C:\\Users\\ANUSHA\\Dropbox\\DLW_paper\\Data\\")

## Read in torpor data file
fmr_data <- read.csv("DLW_data2.csv") #Compiled daata from this paper and literature. Each row is an individual

## Read in McGuire et al. 2014 hummingbird phylogeny
tree<-read.tree("hum294.tre")

#### Phylogenetic components - prune tree ####
#Replace tip names in the tree with those in torpor database
## If using an updated tree check tree names again. Using a hard replace here because there aren't
#too many to be replaced.
#(To show tip names, use: tree$tip.label)
## DO - trim to DLW dataset
tree$tip.label[1]<-"WNJ"
tree$tip.label[92]<-"EMB"
tree$tip.label[93]<-"FBB"
tree$tip.label[95]<-"GCB"
tree$tip.label[219]<-"BBLH"
tree$tip.label[163]<-"BLUH"
tree$tip.label[156]<-"RIHU"

tips<-data.frame(levels(torpor$Species))
colnames(tips) <- "tips"
rownames(tips)<-tips$tips

#match tree to data, prune tree, species names should be in rownnames of "data" 
tre1<-treedata(tree, tips)$phy
#To check that the reationships between species in the trimmed tree look right
plot(tre1) 

#### Models ####
## Now, to run Bayesian models with repeated measures per species (i.e. multiple individuals per species), 
#we setup an inverse matrix and set up a prior
#Using a Bayesian rather than a maximum likelihood model because with an ML model we could include 
#repeated measures, OR we could include a phylogenetic structure. 
#But to get a hierarchy, with both a phylogeny and then repeated measures 
#within the phylogeny, we need turn the phylogeny into an inverse matrix
inv.phylo<-inverseA(tre1,nodes="TIPS",scale=TRUE)
#set up a prior for a phylogenetic mixed model
#Setting priors to be very uninformative
prior<-list(G=list(G1=list(V=1,nu=1)),R=list(V=1,nu=1)) 
#run the hierarchical phyogenetic model, the name of the species 
#(repeated across rows of observations) 

#### Models ####

## NEE ~ 
### Full model including rewarming, Oct 2017
mNEE_full <-MCMCglmm(NEE_MassCorrected~Mass+Hours2+Tc_min_C+savings_quantile+
               kJ_rewarming2, 
             random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
             prior=prior, data=torpor, verbose=FALSE, nitt = 5e6, thin = 1000)
summary(mNEE_full)
plot(mNEE_full) 

## Without any phylogenetic corrections- shows that results have an inflated significance when 
#phylo corrections are not done
mNEE_nophylo <-MCMCglmm(NEE_MassCorrected~Mass+Hours2+Tc_min_C+savings,
                        data=torpor[torpor$Hours2!=0,])
summary(mNEE_nophylo)
