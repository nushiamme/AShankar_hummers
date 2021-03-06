## Code for paper titled:
#"Torpid hummingbirds- energy savings in temperate and tropical sites"
## Paper authors: Anusha Shankar*, Rebecca J Schroeder*, 
# Susan M Wethington, Catherine H Graham, Donald R Powers
## *Equal authors
## Code by: Anusha Shankar, github/nushiamme
## Contact: anusha<dot>shankar<at>stonybrook<dot>edu for questions about code
## Thank you Liliana Davalos for help with this!
## Started Nov 23, 2016

library(MCMCglmm)
library(nlme)
library(ape)
library(geiger) # for treedata() function
library(caper)
library(lattice) # for xyplot function

setwd("C:\\Users\\ANUSHA\\Dropbox\\Hummingbird energetics\\Submission_Jul2017\\Data\\")

torpor <- read.csv("Torpor_individual_summaries.csv") #Torpor data file, each row is an individual
tor_freq <- read.csv("Torpor_freq.csv")

#### Adding columns ####
torpor$NEE_MassCorrected<- torpor$NEE_kJ/(torpor$Mass^(2/3))
#First converting NA's in Hours_torpid into 0's.
torpor$Hours2 <- torpor$Hours_torpid
torpor$Hours2[is.na(torpor$Hours2==TRUE)] <- 0
#Making a column for energy savings as a proportion of energy expenditure (normothermic-torpor)/torpor
torpor$savings <- 100-torpor$Percentage_avg
torpor$savings2 <- 100-torpor$Percentage_avg
torpor$savings2[is.na(torpor$savings2)] <- 0
#torpor$savings2 <- scale(torpor$savings2, center=T, scale=F)[,1] # to center savings on zero

## To make column where savings is a binned, ordinal value.
torpor$savings_quantile <- with(torpor, factor(
  findInterval(savings2, c(-Inf,
                       unique(quantile(savings2, probs=seq(0,1, by=0.25))), Inf)), 
  labels=c("1","2","3","4")
))
torpor$savings_quantile <- as.numeric(torpor$savings_quantile)
torpor$savings_quantile2 <- as.factor(torpor$savings_quantile)

## Rewarming kJ column - remove NA's
torpor$kJ_rewarming2 <- torpor$kJ_rewarming_BeforeOvershoot
torpor$kJ_rewarming2[is.na(torpor$kJ_rewarming2==TRUE)] <- 0

#### Phylogenetic components ####
tree<-read.tree("hum294.tre")
#show tip names
#tree$tip.label
#replace tip names with those in torpor database
## If using an updated tree check tree names again
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
#changed nu from 0.002 to 1 for both G and R on May 22, 2017
prior<-list(G=list(G1=list(V=1,nu=1)),R=list(V=1,nu=1)) 
#run the hierarchical phyogenetic model, the name of the species (repeated across rows of observations) 

## Frequency converted into bernoulli individual-level torpor-not
## This is the model for frequency
mfreq1 <- MCMCglmm(Tornor~Mass, random=~Species, family='categorical',
                   ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, 
                   verbose=FALSE, nitt = 5e6, thin = 1000)
summary(mfreq1)
plot(mfreq1) ## Figure 2

mrewarm_tc <- MCMCglmm(kJ_rewarming_BeforeOvershoot~Mass, 
                    random=~Species, family='gaussian',
                    ginverse=list(Species=inv.phylo$Ainv), prior=prior, 
                    data=torpor[torpor$Torpid_not=="T",],
                    verbose=F,nitt=5e6, thin=1000)
summary(mrewarm)
plot(mrewarm)

mrewarm_tc <- MCMCglmm(kJ_rewarming_BeforeOvershoot~Mass+Rewarming_Tc, 
                       random=~Species, family='gaussian',
                       ginverse=list(Species=inv.phylo$Ainv), prior=prior, 
                       data=torpor[torpor$Torpid_not=="T",],
                       verbose=F,nitt=5e6, thin=1000)
summary(mrewarm_tc)
par(mar = rep(2, 4))
plot(mrewarm_tc)

autocorr(mrewarm_tc)

## Without mass-corrections - don't use - exploratory
m2<-MCMCglmm(NEE_kJ~Mass+Hours2+Tc_min_C, random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
             prior=prior, data=torpor, verbose=FALSE)
summary(m2)

## Mass-corrected NEE as a function of Mass
m3a<-MCMCglmm(NEE_MassCorrected~Mass, random=~Species, 
              ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, 
              verbose=FALSE, nitt = 5e6, thin = 1000)
summary(m3a)

## As a function of duration of torpor
m3b<-MCMCglmm(NEE_MassCorrected~Hours2, random=~Species, 
              ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, 
              verbose=FALSE, nitt = 5e6, thin = 1000)
summary(m3b)
plot(m3b) ## Figure 5

## Of min chamber temperature
m3c<-MCMCglmm(NEE_MassCorrected~Tc_min_C, random=~Species, 
              ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, 
              verbose=FALSE, nitt = 5e6, thin = 1000)
summary(m3c)

## Duration + min chamber temperature
m3d<-MCMCglmm(NEE_MassCorrected~Hours2+Tc_min_C, random=~Species, 
              ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, 
              verbose=FALSE, nitt = 5e6, thin = 1000)
summary(m3d)
par(mar = rep(2, 4))
plot(m3d)

## Used this model up to April 2017
m3<-MCMCglmm(NEE_MassCorrected~Mass+Hours2+Tc_min_C, random=~Species, 
             ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, 
             verbose=FALSE, nitt = 5e6, thin = 1000)
summary(m3)

## As a function of hourly energy savings
m4a <- MCMCglmm(NEE_MassCorrected~savings_quantile, random=~Species, 
                ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, 
                verbose=FALSE, nitt = 5e6, thin = 1000)
summary(m4a)

## Duration + min temp + savings
m4b <- MCMCglmm(NEE_MassCorrected~Hours2+Tc_min_C+savings_quantile, random=~Species, 
                ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, 
                verbose=FALSE, nitt = 5e6, thin = 1000)
summary(m4b)

## This was the full model used until July 2017
m5<-MCMCglmm(NEE_MassCorrected~Mass+Hours2+Tc_min_C+savings_quantile,
             random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
             prior=prior, data=torpor, verbose=FALSE, nitt = 5e6, thin = 1000)
summary(m5)
par(mar = rep(2, 4))
plot(m5)
autocorr(m5) #To check how autocorrelated the variables are

### Full model including rewarming, Oct 2017
m6<-MCMCglmm(NEE_MassCorrected~Mass+Hours2+Tc_min_C+savings_quantile+
               kJ_rewarming2, 
             random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
             prior=prior, data=torpor, verbose=FALSE, nitt = 5e6, thin = 1000)
summary(m6)
par(mar = rep(2, 4))
plot(m6)
autocorr(m5) #To check how autocorrelated the variables are

## Without any phylogenetic corrections- shows that results have an inflated significance when 
#phylo corrections are not done
m6 <-MCMCglmm(NEE_MassCorrected~Mass+Hours2+Tc_min_C+savings, data=torpor[torpor$Hours2!=0,])
summary(m6)

