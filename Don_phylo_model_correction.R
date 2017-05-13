## Phylogenetically corrected model for Don Powers et al.
## Code by Anusha Shankar

## Reading in the packages required to run the code
library(MCMCglmm) ## For running MCMC glmm's
library(nlme)
library(ape)
library(geiger)
library(caper)

setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha Committee\\Don_phylo_analyses") #Change the text within quotes to the pathname for the folder your data file is stored in

hov_dat <- read.csv("Don_phylocorrections_Figure2_data.csv") #Read a .csv version of your file in here
#This .csv is now an "object" which is saved in R as a "dataframe" called hov_dat

head(hov_dat) #Displays column names and first 5 rows so you can understand the structure of the "dataframe" and check spellings of column names

tree<-read.tree("hum294.tre") #Reading in the phylogenetic tree
#show tip names
tree$tip.label #Showing all the current tip labels (species names)
#replace tip names with those in hov_dat database
tree$tip.label[192]<-"CAHU" #This one is called Stellula calliope, and not Selasphorus calliope, in the tree- should check why
tree$tip.label[185]<-"BCHU"
tree$tip.label[219]<-"BBLH"
tree$tip.label[163]<-"BLUH"
tree$tip.label[156]<-"MAHU"

tips<-data.frame(levels(hov_dat$Species)) #Save the species names from the dataframe as an object called 'tips'
colnames(tips) <- "tips" #Convert the column name of the object 'tips' into tips
rownames(tips)<-tips$tips #Convert the row names into species names

## Match tree to data, prune tree, species names should be in rown names of "data" 
tre1<-treedata(tree, tips)$phy ## This is removing all the species that we don't need from the tree
plot(tre1) ## Plotting the pruned tree to make sure the relationships make sense now

#Now to run the Bayesian hierarchical model!
#Setup an inverse matrix and set up a prior
#Using a Bayesian rather than a maximum likelihood model because with an ML model we could include 
#repeated measures, OR we could include a phylogenetic structure. 
#But to get a hierarchy with both, with both a phylogeny and then repeated measures 
#within the phylogeny, we need turn the phylogeny into an inverse matrix
inv.phylo<-inverseA(tre1,nodes="TIPS",scale=TRUE)
#set up a prior for a phylogenetic mixed model
prior<-list(G=list(G1=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))
#run the hierarchical phyogenetic model, the name of the species (repeated across rows of observations) 

## First running surface temperature as a function of ambient temperature
model_1 <-MCMCglmm(T_body~T_a, random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
             prior=prior, data=hov_dat, verbose=FALSE)
summary(model_1)

## Now running HDA as a function of ambient temperature
model_2 <-MCMCglmm(T_HDA~T_a, random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
                  prior=prior, data=hov_dat, verbose=FALSE)
summary(model_2)

