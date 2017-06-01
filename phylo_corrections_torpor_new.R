## Thank you Liliana Davalos!
## Started Nov 23, 2016

library(MCMCglmm)
library(nlme)
library(ape)
library(geiger) # for treedata() function
library(caper)
library(lattice) # for xyplot function

setwd("C:\\Users\\ANUSHA\\Dropbox\\Hummingbird energetics\\Submission_Oct2016")

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

#Make Temptrop into a binary variable
torpor$Temptrop2 <- as.character(torpor$Temptrop)
torpor$Temptrop2[torpor$Temptrop2=="Temperate"] <- 0
torpor$Temptrop2[torpor$Temptrop2=="Tropical"] <- 1
torpor$Temptrop2<- as.numeric(torpor$Temptrop2)

## Use this depending on the model
torpor$Temptrop3 <- as.factor(torpor$Temptrop2) 

#Combine duration and savings to make a composite measure
#torpor$combined <- torpor$Hours2*torpor$savings2

## NOT using this any more
#Table with data for rate of occurrence of torpor per species; each row is a species
#freq_table <- read.csv("Frequency_torpor_sp.csv") 
#mass.agg <- aggregate(torpor$Mass,   #Get mean mass per species
 #                     by=list(torpor$Species), 
  #                    FUN="mean", na.rm=T)
#names(mass.agg) <- c("Species", "Mass")
#mass.agg
#freq_table$mass <- mass.agg$Mass # Add mass data to freq_table

#### Phylogenetic components ####
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

#### SKIP ####
## NOT USING this model for frequency any more. Go to end of script for current frequency model (mfreq1)
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

#### Models ####
## Now, to run Bayesian models with repeated measures per species (i.e. multiple individuals per species), 
#we setup an inverse matrix and set up a prior
#Using a Bayesian rather than a maximum likelihood model because with an ML model we could include 
#repeated measures, OR we could include a phylogenetic structure. 
#But to get a hierarchy, with both a phylogeny and then repeated measures 
#within the phylogeny, we need turn the phylogeny into an inverse matrix
inv.phylo<-inverseA(tre1,nodes="TIPS",scale=TRUE)
#set up a prior for a phylogenetic mixed model
#changed nu from 0.002 to 1 for both G and R on May 22
prior<-list(G=list(G1=list(V=1,nu=1)),R=list(V=1,nu=1)) 
#run the hierarchical phyogenetic model, the name of the species (repeated across rows of observations) 
## Without mass-corrections - don't use
m2<-MCMCglmm(NEE_kJ~Mass+Hours2+Tc_min_C, random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
             prior=prior, data=torpor, verbose=FALSE)
summary(m2)

## The next few models, from m3a to m4, try out NEE_MassCorrected ~ of each variable and then the best ones together. 
## Running them multiple times and saving into a list
models_list <- list()
model_DIC <- data.frame(matrix(ncol = 3, nrow = 6))
names(model_DIC) <- c("Model", "DIC", "pMCMC")
model_DIC$Model <- c("Mass", "Duration", "Tc_min_C", "Duration_Tc", "Mass_Duration_Tc", "All")

## Make a list with the names of the predictor vairable from the torpor dataframe that are being used in this model
## i.e. Species, Mass, Tc_min_C, Hours2 and savings_quantile
varlist <- names(torpor)[3,9,22,31,34]

models <- lapply(varlist, function(x) {
  lm(substitute(read ~ i, list(i = as.name(x))), data = hsb2)
})
hier_bay_mod <- function(torpor){
  
  model_glmm <- MCMCglmm(NEE_MassCorrected~Mass, random=~Species, 
                         ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, verbose=FALSE)
  
  model_summary <- summary(model_glmm)
  my_model_dat <- data.frame("Predictor"=mod_var, "DIC"=model_summary$DIC,
                             "Intercept"=model_summary$solutions[1,1], "Int_Lower_CI"=model_summary$solutions[1,2],
                             "Int_Upper_CI"=model_summary$solutions[1,3], "Int_Eff_samp"=model_summary$solutions[1,4],
                             "Int_pMCMC"=model_summary$solutions[1,5], "Beta"=model_summary$solutions[2,1],
                             "B_Lower_CI"=model_summary$solutions[2,2], "B_Upper_CI"=model_summary$solutions[2,3],
                             "B_Eff_samp"=model_summary$solutions[2,4], "B_pMCMC"=model_summary$solutions[2,5])
  
  write.table(my_model_dat, file=paste("Models/All_vars", "_", "_modelsummary.txt", sep=""))

  save(model_glmm, file=paste("Models/All_vars", "_", "_mod.rda", sep=""))
  print(paste("Ok, model rda has been saved for ", mod_var, "!", sep=""))
  
}

m3a<-MCMCglmm(NEE_MassCorrected~Mass, random=~Species, 
              ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, verbose=FALSE)
summary(m3a)

m3b<-MCMCglmm(NEE_MassCorrected~Hours2, random=~Species, 
              ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, verbose=FALSE)
summary(m3b)

m3c<-MCMCglmm(NEE_MassCorrected~Tc_min_C, random=~Species, 
              ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, verbose=FALSE)
summary(m3c)

m3d<-MCMCglmm(NEE_MassCorrected~Hours2+Tc_min_C, random=~Species, 
              ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, verbose=FALSE)
summary(m3d)

## Used this model up to April 2017
m3<-MCMCglmm(NEE_MassCorrected~Mass+Hours2+Tc_min_C, random=~Species, 
             ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, verbose=FALSE)
summary(m3)

## New model including energy savings and temp/trop
m4a <- MCMCglmm(NEE_MassCorrected~savings_quantile, random=~Species, 
                ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, verbose=FALSE)
summary(m4a)

m4b <- MCMCglmm(NEE_MassCorrected~Hours2+Tc_min_C+savings_quantile, random=~Species, 
                ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, verbose=FALSE)
summary(m4b)
## To increase number of iterations, can add  (verbose=T, nitt = 100000, burnin=500, thin = 100) within the MCMCglmm command
## And to see how fast they converge, do plot(m4b$Sol)


## Trying a model with savings as percentages,not as ordinal bins
m4c <- MCMCglmm(NEE_MassCorrected~savings, random=~Species, 
                ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor[torpor$Hours2!=0,], verbose=FALSE)
summary(m4c)

## Binned savings
m4<-MCMCglmm(NEE_MassCorrected~Mass+Hours2+Tc_min_C+savings_quantile+Temptrop2, random=~Species, 
             ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, verbose=FALSE)
summary(m4)

## Write results into dataframe
model_DIC$DIC <- c(m3a$DIC, m3b$DIC, m3c$DIC, m3d$DIC, m3$DIC, m4$DIC)

#model_DIC$pMCMC <- c()

models_list[[4]] <- model_DIC
models_list

### TRying a model with temptrop as a binary variable - this is the model I'm using
# savings_quantile2 and Temptrop3 are both saved as factors
m5<-MCMCglmm(NEE_MassCorrected~Mass+Hours2+Tc_min_C+savings_quantile2+Temptrop3, 
             random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
             prior=prior, data=torpor, verbose=FALSE, nitt = 1e6, thin = 500, burnin = 10000)
summary(m5)
par(mar = rep(2, 4))
plot(m5)

m5b<-MCMCglmm(NEE_MassCorrected~Mass+Hours2*Tc_min_C, random=~Species, 
             ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, verbose=FALSE)
summary(m5b)

## Without any phylogenetic corrections- shows that results have an inflated significance when 
#phylo corrections are not done
m6 <-MCMCglmm(NEE_MassCorrected~Mass+Hours2+Tc_min_C+savings, data=torpor[torpor$Hours2!=0,])
summary(m6)

## Frequency converted into bernoulli individual-level torpor-not
## This is the model I am finally using for frequency
mfreq1 <- MCMCglmm(Tornor~Mass, random=~Species, family='categorical',
                          ginverse = list(Species=inv.phylo$Ainv), prior=prior, data=torpor, 
                   verbose=FALSE, nitt = 5e6, thin = 1000)
summary(mfreq1)

#Using this site as a guide (Thanks Marisa): 
#http://www.maths.bath.ac.uk/~jjf23/mixchange/onewayanova.html#mcmcglmm
lattice::xyplot(mfreq1$Sol) # Check for stability in the iterations, for intercept first
autocorr.plot(mfreq1$Sol) # check for autocorrelation in intercept
lattice::xyplot(log(mfreq1$VCV)) # Check for stability for the variance terms
autocorr.plot(mfreq1$VCV) # Check for autocorrelation in variance terms; there isn't much
hist(sqrt(mfreq1$VCV[,1]),100, xlab="Species SD", main= "posterior distribution") # high 
#probability that Species SD is very small; The probability that Species variance is less than
# 10 is
mean(mfreq1$VCV[,1]<1) # This is quite high, so Species variance seems to be small
#especially when compared with error variance:
hist(sqrt(mfreq1$VCV[,2]),100, xlab="Error SD", main="posterior distribution") ## Need to check
#what this means- it's not normally distributed
