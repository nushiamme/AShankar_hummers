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
library(phytools)

#### Setup ####
setwd("C:\\Users\\ANUSHA\\Dropbox\\DLW_paper\\Data\\")

## Read in torpor data file
fmr_data <- read.csv("DLW_data2.csv") #Compiled daata from this paper and literature. Each row is an individual

## Read in McGuire et al. 2014 hummingbird phylogeny
tree_dlw<-read.tree("hum294.tre")

#### Phylogenetic components - prune tree ####
#Replace tip names in the tree with those in torpor database
## If using an updated tree check tree names again. Using a hard replace here because there aren't
#too many to be replaced.
#(To show just tip names, use: tree_dlw$tip.label)
## To get tip numbers for the species in the DLW dataset, to then prune the phylogeny to match the dataset
head.species<-c("latirostris.3", "fulgens","clemenciae", "rubinoides", "imperatrix", "mellivora", "jacula", "coelestis","tzacatl", "urochrysia", "alexandri",
                "Calypte.anna","colombica.fannyae", "colombica.colombica", "benjamini", "yaruqui", "gigas")
ii<-sapply(head.species,grep,tree_dlw$tip.label)
ii

## Manually replacing because it's a manageable number- trimming tree to DLW dataset
tree_dlw$tip.label[1]<-"FLME"
tree_dlw$tip.label[15]<-"PHYA"
tree_dlw$tip.label[83]<-"URBE"
tree_dlw$tip.label[92]<-"HEIM"
tree_dlw$tip.label[93]<-"HERU"
tree_dlw$tip.label[95]<-"HEJA"
tree_dlw$tip.label[128]<-"AGCO"
tree_dlw$tip.label[154]<-"PAGI"
tree_dlw$tip.label[156]<-"EUFU"
tree_dlw$tip.label[163]<-"LACL"
tree_dlw$tip.label[185]<-"ARAL"
tree_dlw$tip.label[188]<-"CAAN"
tree_dlw$tip.label[219]<-"CYLA"
tree_dlw$tip.label[230]<-"CHUR"
tree_dlw$tip.label[234]<-"THCO"
tree_dlw$tip.label[235]<-"THFA"
tree_dlw$tip.label[269]<-"AMTZ"


tips<-data.frame(levels(fmr_data$Species))
colnames(tips) <- "tips"
rownames(tips)<-tips$tips

#match tree to data, prune tree, species names should be in rownnames of "data" 
tre1<-treedata(tree_dlw, tips)$phy
#To check that the relationships between species in the trimmed tree look right
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
prior<-list(G=list(G1=list(V=0.02,nu=0.02)),R=list(V=0.02,nu=0.02)) 
#run the hierarchical phyogenetic model, the name of the species 
#(repeated across rows of observations) 

#### Models ####

## kJ_dayg ~ Mass_g + Big_site + Site ; random = Species 
## Making site a dummy categorical variable
fmr_data$Site.f <- as.factor(fmr_data$Site)
fmr_data$Big_site.f <- as.factor(fmr_data$Big_site)
levels(fmr_data$Big_site.f)[match("AZ",levels(fmr_data$Big_site.f))] <- 0
levels(fmr_data$Big_site.f)[match("CH",levels(fmr_data$Big_site.f))] <- 1
levels(fmr_data$Big_site.f)[match("CR",levels(fmr_data$Big_site.f))] <- 2
levels(fmr_data$Big_site.f)[match("EC",levels(fmr_data$Big_site.f))] <- 3

fmr_data$Temptrop <- as.factor(fmr_data$Big_site)
levels(fmr_data$Temptrop)[match("AZ",levels(fmr_data$Temptrop))] <- 0
levels(fmr_data$Temptrop)[match("CH",levels(fmr_data$Temptrop))] <- 1
levels(fmr_data$Temptrop)[match("CR",levels(fmr_data$Temptrop))] <- 1
levels(fmr_data$Temptrop)[match("EC",levels(fmr_data$Temptrop))] <- 1

DEE_full <-MCMCglmm(kJ_dayg~Mass_g+Temptrop, 
             random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
             prior=prior, data=fmr_data, verbose=FALSE, nitt = 100000, thin = 1000)
summary(DEE_full)
plot(DEE_full) 

