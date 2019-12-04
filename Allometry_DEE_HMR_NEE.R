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
library(phytools)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
##For cutting HMR temps into bins and making a new binned temperature column
library(Hmisc)
library(plyr) ## For renaming factor levels

#### Setup ####
setwd("C:\\Users\\nushi\\Dropbox\\DLW_paper\\Data")
fmr_data <- read.csv("DLW_TableS1.csv") #Compiled data from this paper and literature. Each row is an individual
nee <- read.csv("C:\\Users\\nushi\\Dropbox\\Hummingbird energetics\\July2018\\Data\\Torpor_individual_summaries_2.csv")
hmr <- read.csv("Groom_et_al_HMR_realtemps_compiled.csv")
bmr <- read.csv("Londono_BMR.csv")


## Read in McGuire et al. 2014 hummingbird phylogeny
tree_dlw<-read.tree("hum294.tre")
tree_hmr<-read.tree("hum294.tre")
#tre_ou_edited <- read.tree("OU_hummer_tree_FMR_edit.txt")

## General plotting functions
## Generic theme
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA)) 

## To add linear regression equation to plot
lm_eqn <- function(y, x){
  m <- lm(y ~ x);
  eq <- substitute(italic(y) ==
                     a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m))[1], digits = 2), 
                        b = format(unname(coef(m))[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));                 
}


## Overall energy budget component averages to match with DEE
low_aggregate <- c(0.45, 0.67, 0.76, 0.72, 0.45, 1.5)
high_aggregate <- c(0.85, 0.67, 0.96, 0.90, 0.85, 1.5)
med_aggregate <- c(0.85, 0.67, 0.76, 0.90, 0.85, 1.5)

mean(low_aggregate)
sd(low_aggregate)/sqrt(sum(low_aggregate))
mean(high_aggregate)
sd(high_aggregate)/sqrt(sum(high_aggregate))
mean(med_aggregate)
sd(med_aggregate)/sqrt(sum(med_aggregate))


## Aggregating dataset by Species and site, to get species means of mass and daily energy expenditure (DEE)
dlw_mean <- data.frame()
dlw_mean <- aggregate(fmr_data$kJ_day, by=list(fmr_data$Species, fmr_data$Big_site), FUN="mean", na.omit=T)
dlw_mass <- aggregate(fmr_data$Mass_g, by=list(fmr_data$Species, fmr_data$Big_site), FUN="mean", na.omit=T)
dlw_mean <- merge(dlw_mean, dlw_mass, by = c("Group.1", "Group.2"))
names(dlw_mean) <- c("Species", "Region", "kJ_day", "Mass_g")

## NEE data to make slope of NEE
nee$Mass_g <- nee$Mass
nee$kJ_day <- nee$NEE_kJ
nee_mean <- data.frame()
nee_mean <- aggregate(nee$NEE_kJ, by=list(nee$Species2), FUN="mean", na.omit=T)
nee_mass <- aggregate(nee$Mass, by=list(nee$Species2), FUN="mean", na.omit=T)
nee_mean <- merge(nee_mean, nee_mass, by = "Group.1")
names(nee_mean) <- c("Species", "kJ_day", "Mass_g")

## HMR data from Groom et al. 2018 for slope of HMR
hmr$hmr_kJ_min <- (hmr$mL_O2_min*20.1)/1000

hmr$temp_bin_C <- cut(hmr$Te_C, c(10,20,25,30, 35, 40, 45))

hmr$temp_bin_C <- as.factor(hmr$temp_bin_C)
p<- ggplot(hmr[!is.na(hmr$Te_C),], aes(log(Mass), log(hmr_kJ_min*60))) + 
  geom_point(aes(col=Species), size=2) + my_theme + #facet_grid(.~temp_bin_C) +
  geom_smooth(method='lm') + theme(axis.text.x=element_text(angle=90, vjust=0.7)) +
  geom_text(aes(x = 2.25, y = 0.75), label = lm_eqn(log(hmr$hmr_kJ_min*60),
                           log(hmr$Mass)), parse=T, size=8) +
  ylab("log(HMR kJ/h)")
p

dat_text <- data.frame(
  label = c(
    lm_eqn(log(hmr$hmr_kJ_min[hmr$temp_bin_C==levels(hmr$temp_bin_C)[1]]),
           log(hmr$Mass[hmr$temp_bin_C==levels(hmr$temp_bin_C)[1]])),
    lm_eqn(log(hmr$hmr_kJ_min[hmr$temp_bin_C==levels(hmr$temp_bin_C)[2]]),
           log(hmr$Mass[hmr$temp_bin_C==levels(hmr$temp_bin_C)[2]])),
    lm_eqn(log(hmr$hmr_kJ_min[hmr$temp_bin_C==levels(hmr$temp_bin_C)[3]]),
           log(hmr$Mass[hmr$temp_bin_C==levels(hmr$temp_bin_C)[3]])),
    lm_eqn(log(hmr$hmr_kJ_min[hmr$temp_bin_C==levels(hmr$temp_bin_C)[4]]),
           log(hmr$Mass[hmr$temp_bin_C==levels(hmr$temp_bin_C)[4]])),
    lm_eqn(log(hmr$hmr_kJ_min[hmr$temp_bin_C==levels(hmr$temp_bin_C)[5]]),
           log(hmr$Mass[hmr$temp_bin_C==levels(hmr$temp_bin_C)[5]])),
    lm_eqn(log(hmr$hmr_kJ_min[hmr$temp_bin_C==levels(hmr$temp_bin_C)[6]]),
           log(hmr$Mass[hmr$temp_bin_C==levels(hmr$temp_bin_C)[6]]))),
  temp_bin_C= levels(hmr$temp_bin_C))

p + geom_text(
  data    = dat_text,
  mapping = aes(x = -Inf, y = 2, label = label),
  hjust   = -0.08,
  vjust=-1,
  size=5, parse=T
)

### TRYING OUT BMR from Londono et al. 2015 dataset
ggplot(bmr[bmr$Apodiformes=="Y",], aes(log(Mass_g), log(100*BMR_W))) + geom_point() + my_theme +
  geom_smooth(method='lm') +
  geom_text(aes(x = 3, y = 2), label = lm_eqn(log(bmr$BMR_W),
                                                    log(bmr$Mass_g)), parse=T, size=8)
  

## Trimming tree to DLW dataset
## Manually replacing because it's a manageable number
tree_dlw$tip.label[1]<-"FLME"
tree_dlw$tip.label[15]<-"PHYA"
tree_dlw$tip.label[83]<-"URBE"
tree_dlw$tip.label[92]<-"HEIM"
tree_dlw$tip.label[93]<-"HERU"
tree_dlw$tip.label[95]<-"HEJA"
tree_dlw$tip.label[128]<-"AGCO"
#tree_dlw$tip.label[154]<-"PAGI" ## Doing this one separately later
tree_dlw$tip.label[156]<-"EUFU"
tree_dlw$tip.label[163]<-"LACL"
tree_dlw$tip.label[185]<-"ARAL"
tree_dlw$tip.label[188]<-"CAAN"
tree_dlw$tip.label[219]<-"CYLA"
tree_dlw$tip.label[230]<-"CHUR"
tree_dlw$tip.label[234]<-"THCO"
tree_dlw$tip.label[235]<-"THFA"
tree_dlw$tip.label[269]<-"AMTZ"

## Tree without the Giant hummingbird
tree_no_Pgigas <- tree_dlw

tree_dlw$tip.label[154]<-"PAGI"

tips<-data.frame(levels(fmr_data$Species))
colnames(tips) <- "tips"
rownames(tips)<-tips$tips

#match tree to data, prune tree, species names should be in rownnames of "data" 
tre1<-treedata(tree_dlw, tips)$phy
#To check that the relationships between species in the trimmed tree look right
plot(tre1, cex=1.5, edge.width = 3) 

## Matching tree without P. gigas and trimming
tips2<-data.frame(levels(droplevels(fmr_data$Species[fmr_data$Species != "PAGI"])))
colnames(tips2) <- "tips"
rownames(tips2)<-tips2$tips

#match tree to data, prune tree, species names should be in rownnames of "data" 
tre1_noPgigas<-treedata(tree_no_Pgigas, tips2)$phy
#To check that the relationships between species in the trimmed tree look right
plot(tre1_noPgigas, cex=1.5, edge.width = 3) 


## Trimming tree to HMR dataset
## Manually replacing because it's a manageable number
tree_hmr$tip.label[2]<-"FLFU"
tree_hmr$tip.label[7]<-"RANA"
tree_hmr$tip.label[40]<-"COCO"
tree_hmr$tip.label[82]<-"OCUN"
tree_hmr$tip.label[90]<-"CLRU"
tree_hmr$tip.label[109]<-"LOCH"
tree_hmr$tip.label[116]<-"SESE"
tree_hmr$tip.label[134]<-"ORES"
tree_hmr$tip.label[154]<-"PAGI"
tree_hmr$tip.label[156]<-"EUFU"
tree_hmr$tip.label[163]<-"LACL"
tree_hmr$tip.label[184]<-"ARCO"
tree_hmr$tip.label[175]<-"RHVE"
tree_hmr$tip.label[185]<-"ARAL"
tree_hmr$tip.label[188]<-"CAAN"
tree_hmr$tip.label[190]<-"SEPL"
tree_hmr$tip.label[191]<-"SERU"
tree_hmr$tip.label[192]<-"STCA"
tree_hmr$tip.label[193]<-"SESA"
tree_hmr$tip.label[219]<-"CYLA"
tree_hmr$tip.label[231]<-"THGL"
tree_hmr$tip.label[257]<-"AMVI"
tree_hmr$tip.label[269]<-"STLA"
tree_hmr$tip.label[280]<-"LEAL"
tree_hmr$tip.label[282]<-"AMFI"

tips_hmr<-data.frame(levels(hmr$Species))
colnames(tips_hmr) <- "tips"
rownames(tips_hmr)<-tips_hmr$tips

#match tree to data, prune tree, species names should be in rownnames of "data" 
tre_hmr<-treedata(tree_hmr, tips_hmr)$phy

#To check that the relationships between species in the trimmed tree look right
plot(tre_hmr, cex=1.5, edge.width = 3) 




## GLS models with OU vs. Brownian motion
## https://www.r-phylo.org/wiki/HowTo/PGLS
## PGLS can only take one value per species, so aggregate by mean DEE first, and then run the pgls model.
fmr.agg <- aggregate(fmr_data$kJ_day,by = list(fmr_data$Species), FUN='mean', na.rm=T)
names(fmr.agg) <- c('Species', 'kJ_day')
mass.agg <- aggregate(fmr_data$Mass_g,by = list(fmr_data$Species), FUN='mean', na.rm=T)
names(mass.agg) <- c('Species', 'Mass_g')
dee.agg <- merge(fmr.agg, mass.agg,by="Species")

## Making FMR and Mass separate objects
fmr<-dee.agg$kJ_day
mass_g<-dee.agg$Mass_g
DF.fmr<-data.frame(fmr,mass_g,row.names=dee.agg$Species)
DF.fmr <-  DF.fmr[tre1$tip.label,]
DF.fmr

## Running brownian motion tree GLS model
bm.fmr<-corBrownian(phy=tre1)
bm.gls<-gls(log(fmr)~log(mass_g),correlation=bm.fmr,data=DF.fmr)
summary(bm.gls)
plot(bm.gls)

## Running GLS model with Ornstein-Uhlenbeck tree
ou.fmr<-corMartins(1,phy=tre1)
ou.gls<-gls(log(fmr)~log(mass_g),correlation=ou.fmr,data=DF.fmr)
summary(ou.gls)
plot(ou.gls$residuals)
plot(ou.gls)

## FOR HMR
## PGLS can only take one value per species, so aggregate by mean HMR first, and then run the pgls model.
hmr_mean <- data.frame()
hmr_mean <- aggregate(hmr$hmr_kJ_min, by=list(hmr$Species), FUN="mean", na.omit=T)
hmr_mass <- aggregate(hmr$Mass, by=list(hmr$Species), FUN="mean", na.omit=T)
hmr_mean <- merge(hmr_mean, hmr_mass, by = "Group.1")
names(hmr_mean) <- c("Species", "kJ_min", "Mass_g")

## Making HMR and Mass separate objects
hmr_sep<-hmr_mean$kJ_min
mass_hmr_g<-hmr_mean$Mass_g
DF.hmr<-data.frame(hmr_sep,mass_hmr_g,row.names=hmr_mean$Species)
DF.hmr <-  DF.hmr[tre_hmr$tip.label,]
DF.hmr

## Running brownian motion tree GLS model
bm.hmr<-corBrownian(phy=tre_hmr)
bm.gls_hmr<-gls(log(hmr_sep)~log(mass_hmr_g),correlation=bm.hmr,data=DF.hmr)
summary(bm.gls_hmr)
plot(bm.gls_hmr$residuals)
plot(bm.gls_hmr)

## Running GLS model with Ornstein-Uhlenbeck tree
ou.hmr<-corMartins(1,phy=tre_hmr)
ou.gls_hmr<-gls(log(hmr_sep)~log(mass_hmr_g),correlation=ou.hmr,data=DF.hmr)
summary(ou.gls_hmr)
plot(ou.gls_hmr$residuals)
plot(ou.gls_hmr)



#### Models ####
## Now, to run Bayesian models with repeated measures per species (i.e. multiple individuals per species), 
#we setup an inverse matrix and set up a prior
#Using a Bayesian rather than a maximum likelihood model because with an ML model we could include 
#repeated measures, OR we could include a phylogenetic structure. 
#But to get a hierarchy, with both a phylogeny and then repeated measures 
#within the phylogeny, we need turn the phylogeny into an inverse matrix
inv.phylo<-inverseA(tre1, nodes="TIPS", scale=TRUE)
inv.phylo_noPgigas <- inverseA(tre1_noPgigas, nodes="TIPS", scale=TRUE)

## Make OU tree
tre_ou <- rescale(tre1, model = "OU", alpha=48.13674) ## Alpha from running OU gls model above
plot(tre_ou, cex=1.5, edge.width = 3)
## Inverse matrix of the OU tree - doesn't work, edge lengths are zero => star phylogeny.
#inv.phylo_ou <-inverseA(tre_ou_edited,nodes="TIPS",scale=T)

#set up a prior for a phylogenetic mixed model
#Setting priors to be very uninformative
prior<-list(G=list(G1=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))  ## On Nov 21, changing V from 0.02 to 1
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

## Make a variable to compare temperate and tropical measurements 
fmr_data$Temptrop <- as.factor(fmr_data$Big_site)
levels(fmr_data$Temptrop)[match("AZ",levels(fmr_data$Temptrop))] <- 0
levels(fmr_data$Temptrop)[match("CH",levels(fmr_data$Temptrop))] <- 1
levels(fmr_data$Temptrop)[match("CR",levels(fmr_data$Temptrop))] <- 1
levels(fmr_data$Temptrop)[match("EC",levels(fmr_data$Temptrop))] <- 1

## Non-log model, just out of interest. Do not use!
DEE_full_raw <-MCMCglmm(kJ_day~Mass_g+Temptrop, 
             random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
             prior=prior, data=fmr_data, verbose=FALSE, nitt = 5000000, thin = 1000)
summary(DEE_full_raw)
plot(DEE_full_raw) 

## Log-log full model. IGNORE and use interaction model instead
DEE_log <-MCMCglmm(log(kJ_day)~log(Mass_g)+Temptrop, 
                    random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
                    prior=prior, data=fmr_data, verbose=FALSE, nitt = 5000000, thin = 1000)
summary(DEE_log)
plot(DEE_log) 


##  "Brownian motion tree" model DEE vs Mass, log-log, with tree
DEE_log_mass <-MCMCglmm(log(kJ_day)~log(Mass_g), 
                        random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
                        prior=prior, data=fmr_data, verbose=FALSE, nitt = 5000000, thin = 1000)
summary(DEE_log_mass)
plot(DEE_log_mass)

## Now run the MCMCglmm model without the tree, because the OU tree yields a star phylogeny
DEE_log_mass_noTree <-MCMCglmm(log(kJ_day)~log(Mass_g), 
                               random=~Species, 
                               prior=prior, data=fmr_data, verbose=FALSE, nitt = 5000000, thin = 1000)
summary(DEE_log_mass_noTree)
plot(DEE_log_mass_noTree)
confint(DEE_log_mass_noTree)


## Full no tree, with interaction term Allowing two slopes and two intercepts; earlier it was + Temptrop
DEE_full_noTree <-MCMCglmm(log(kJ_day)~log(Mass_g)*Temptrop, 
                   random=~Species, 
                   prior=prior, data=fmr_data, verbose=FALSE, nitt = 5000000, thin = 1000)
summary(DEE_full_noTree)
plot(DEE_full_noTree) 


## No tree, and no P. gigas
DEE_log_mass_noTree_noPgigas <-MCMCglmm(log(kJ_day)~log(Mass_g), 
                                        random=~Species, 
                                        prior=prior, data=fmr_data[fmr_data$Species != "PAGI",], verbose=FALSE, nitt = 5000000, thin = 1000)
summary(DEE_log_mass_noTree_noPgigas)
plot(DEE_log_mass_noTree_noPgigas)


## DEE-mass, log-log, no tree, and using only species means
DEE_log_mass_means <-MCMCglmm(log(kJ_day)~log(Mass_g), 
                        random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
                        prior=prior, data=dlw_mean, verbose=FALSE, nitt = 5000000, thin = 1000)
summary(DEE_log_mass_means)
plot(DEE_log_mass_means)


## Derive R2 from MCMCglmm results
## Source: https://www.int-res.com/articles/suppl/m561p001_supp2.pdf
R2 <- function(mod){
  fixed_eff <- colMeans(mod$Sol)
  fixed_var_comp <- var(as.vector(fixed_eff %*% t(mod$X)))
  all_randoms <- colMeans(mod$VCV)
  residual <- all_randoms[["units"]]
  random_var_comp <- sum(all_randoms) - residual
  R2 <- (fixed_var_comp + random_var_comp)/(sum(all_randoms) + fixed_var_comp)
  round(R2,3)
}

R2(DEE_log_mass)
R2(DEE_log_mass_noTree)
R2(DEE_full_noTree)
R2(DEE_log_mass_noTree_noPgigas)
R2(DEE_log_mass_means)


#### HMR Models ####
inv.phylo_hmr<-inverseA(tre_hmr, nodes="TIPS", scale=TRUE)

## Make OU tree
tre_ou_hmr <- rescale(tre_hmr, model = "OU", alpha=0.2473) ## Alpha from running OU gls model above
plot(tre_ou_hmr, cex=1.5, edge.width = 3)
inv.phylo_ou_hmr<-inverseA(tre_ou_hmr, nodes="TIPS", scale=TRUE)
## Inverse matrix of the OU tree - doesn't work, edge lengths are zero => star phylogeny.
#inv.phylo_ou <-inverseA(tre_ou_edited,nodes="TIPS",scale=T)

#set up a prior for a phylogenetic mixed model
#Setting priors to be very uninformative
prior<-list(G=list(G1=list(V=0.02,nu=0.02)),R=list(V=0.02,nu=0.02)) 
#run the hierarchical phyogenetic model, the name of the species 
#(repeated across rows of observations) 
## DEE vs Mass, log-log, with tree

#DECREASED from 5mil to 1mil for testing, change back for final
HMR_log_ou_mass <-MCMCglmm(log(hmr_kJ_min)~log(Mass), 
                        random=~Species, ginverse = list(Species=inv.phylo_ou_hmr$Ainv), 
                        prior=prior, data=hmr, verbose=FALSE, nitt = 1000000, thin = 1000)
summary(HMR_log_ou_mass)
plot(HMR_log_ou_mass)

#DECREASED from 5mil to 1mil for testing, change back for final
HMR_log_mass <-MCMCglmm(log(hmr_kJ_min)~log(Mass), 
                           random=~Species, ginverse = list(Species=inv.phylo_hmr$Ainv), 
                           prior=prior, data=hmr, verbose=FALSE, nitt = 1000000, thin = 1000)
summary(HMR_log_mass)
plot(HMR_log_mass)

## Trying with HMR mean dataset, to hopefully match to Groom's better
#DECREASED from 5mil to 1mil for testing, change back for final
HMR_log10_mass <-MCMCglmm(log10(kJ_min)~log10(Mass), 
                        random=~Species, ginverse = list(Species=inv.phylo_hmr$Ainv), 
                        prior=prior, data=hmr, verbose=FALSE, nitt = 1000000, thin = 1000)
summary(HMR_log10_mass)
plot(HMR_log10_mass)


## Trying PGLS model, presumably similar to what Groom et al. used
row.names(hmr_mean) <- levels(hmr_mean$Species)
pglsModel_hmr <- gls(log10(kJ_min)~log10(Mass_g), correlation = corBrownian(phy = tre_hmr),
                 data = hmr_mean, method = "ML")
summary(pglsModel_hmr)

## Derrick says he used corPagel instead of Brownian, so trying that instead:
pglsPagel_hmr <- gls(log10(mLO2_min)~log10(Mass_g), correlation = corPagel(0.6,tre_hmr),
    data = hmr_mean, method = "ML")
summary(pglsPagel_hmr)


anova(pglsModel_hmr, pglsPagel_hmr)
## Trying PGLS for DLW
row.names(dlw_mean) <- levels(dlw_mean$Species)
pglsModel_fmr <- gls(log10(kJ_day)~log10(Mass_g), correlation = corBrownian(phy = tre1),
                     data = dlw_mean, method = "ML")
summary(pglsModel_fmr)

## Plot temp and tropical individuals
colourCount <- length(unique(c(unique(levels(nee$Species2)), levels(dlw_mean$Species))))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))


## Melt NEE to be able to plot torpor, normo, and total NEE points
nee$Tot_normo_kJ <- nee$Avg_EE_hourly_normo*nee$Hours_normo
nee$Tot_torpid_kJ <- nee$Avg_EE_hourly_torpid*nee$Hours_torpid
m.nee <- melt(nee, id.vars=c("Species2", "Mass"), 
              measure.vars=c("NEE_kJ", "kJ_rewarming_BeforeOvershoot", "Tot_normo_kJ", "Tot_torpid_kJ"))
m.nee$variable <- revalue(m.nee$variable, c("NEE_kJ"="NEE", "kJ_rewarming_BeforeOvershoot"="Rewarming",
                          "Tot_normo_kJ"="Normothermic", "Tot_torpid_kJ"="Torpid"))
m.nee$variable <- ordered(m.nee$variable, levels =c("NEE", "Normothermic", "Rewarming", "Torpid"))

## Hourly EE rather than whole-night
m.nee2 <- melt(nee, id.vars=c("Species2", "Mass"), 
              measure.vars=c("NEE_kJ", "kJ_rewarming_BeforeOvershoot", "Avg_EE_hourly_normo", "Avg_EE_hourly_torpid"))
m.nee2$variable <- revalue(m.nee2$variable, c("NEE_kJ"="NEE", "kJ_rewarming_BeforeOvershoot"="Rewarming",
                                            "Avg_EE_hourly_normo"="Normothermic", "Avg_EE_hourly_torpid"="Torpid"))
m.nee2$variable <- ordered(m.nee2$variable, levels =c("NEE", "Normothermic", "Rewarming", "Torpid"))


## Torpor allometry, just torpid means per indiv
ggplot(nee, aes(log(Mass), log(Avg_EE_hourly_torpid))) + geom_point() + geom_smooth(method="lm")

## Torpor allometry, just normo means per indiv
ggplot(nee, aes(log(Mass), log(Avg_EE_hourly_normo))) + geom_point() + geom_smooth(method="lm")

colourCount_nee <- length(unique(levels(nee$Species2)))
## Torpor allometry, total NEE
ggplot(m.nee2[!is.na(m.nee2$value),], aes(log(Mass_g), log(value))) +  my_theme +
  geom_smooth(aes(fill=variable), method="lm", col="black", alpha=0.3) +
  scale_fill_manual(values=c("black", "blue", "green", "red")) +
  geom_point(aes(col=Species2),size=3, shape=19) +
  scale_colour_manual(values = getPalette(colourCount_nee)) +
  geom_text(aes(x = 1.5, y = 3), col="black", 
          label = lm_eqn(log(100*m.nee2$value[m.nee2$variable=="NEE"]),
                         log(100*m.nee2$Mass_g[m.nee2$variable=="NEE"])), parse=T, size=7) +
  geom_text(aes(x = 1.5, y = 1), col="blue", 
            label = lm_eqn(log(100*m.nee2$value[m.nee2$variable=="Normothermic"]),
                           log(100*m.nee2$Mass_g[m.nee2$variable=="Normothermic"])), parse=T, size=7) +
  geom_text(aes(x = 1.5, y = -1), col="darkgreen", 
            label = lm_eqn(log(100*m.nee2$value[m.nee2$variable=="Rewarming" &
                                             !is.na(m.nee2$value)]),
                           log(100*m.nee2$Mass_g[m.nee2$variable=="Rewarming" &
                                              !is.na(m.nee2$value)])), parse=T, size=7) +
  geom_text(aes(x = 1.5, y = -3), col="red", 
            label = lm_eqn(log(100*m.nee2$value[m.nee2$variable=="Torpid" &
                                             !is.na(m.nee2$value)]),
                           log(100*m.nee2$Mass_g[m.nee2$variable=="Torpid" &
                                              !is.na(m.nee2$value)])), parse=T, size=7) +
  ylab("log(Energy expenditure kJ)") + xlab("log(Mass g)")

## Torpor allometry, total NEE, and total normo, total torpid, total rewarming
ggplot(m.nee[!is.na(m.nee$value),], aes(log(Mass_g), log(value))) +  my_theme +
  geom_smooth(aes(fill=variable), method="lm", col="black", alpha=0.3) +
  scale_fill_manual(values=c("black", "blue", "green", "red")) +
  #geom_point(aes(col=variable),size=3, shape=19) +
  scale_colour_manual(values = getPalette(colourCount_nee)) +
  geom_text(aes(x = 1.5, y = 3), col="black", 
            label = lm_eqn(log(m.nee$value[m.nee$variable=="NEE"]),
                           log(m.nee$Mass_g[m.nee$variable=="NEE"])), parse=T, size=7) +
  geom_text(aes(x = 1.5, y = 1), col="blue", 
            label = lm_eqn(log(m.nee$value[m.nee$variable=="Normothermic"]),
                           log(m.nee$Mass_g[m.nee$variable=="Normothermic"])), parse=T, size=7) +
  geom_text(aes(x = 1.5, y = -2), col="darkgreen", 
            label = lm_eqn(log(m.nee$value[m.nee$variable=="Rewarming" &
                                                 !is.na(m.nee$value)]),
                           log(m.nee$Mass_g[m.nee$variable=="Rewarming" &
                                                  !is.na(m.nee$value)])), parse=T, size=7) +
  geom_text(aes(x = 1.5, y = 0.5), col="red", 
            label = lm_eqn(log(m.nee$value[m.nee$variable=="Torpid" &
                                                 !is.na(m.nee$value)]),
                           log(m.nee$Mass_g[m.nee$variable=="Torpid" &
                                                  !is.na(m.nee$value)])), parse=T, size=7) +
  ylab("log(Energy expenditure kJ)") + xlab("log(Mass g)")
    
  
## Good graph of species means and individual points, with regression line through them. 
ggplot(NULL, aes(log(Mass_g), log(kJ_day))) + 
  geom_point(data=dlw_mean, aes(col=Species), size=6, shape=19) + 
  geom_smooth(data=fmr_data, method=lm, alpha=0.3) + 
  geom_point(data=nee, aes(col=Species)) +
  geom_point(data=fmr_data, aes(col=Species), shape = 19, size=4, alpha=0.5) + 
  my_theme + xlab("Log(Mass (g))") +
  scale_colour_manual(values = getPalette(colourCount)) +
  ylab("Log(kJ per day)")  + 
  theme(legend.key.height=unit(2,"line"))

## Good graph of species means, with regression line through them.
## For both DEE and also NEE, separately
## Make a color scale of the right number of colors
colourCount <- length(unique(c(unique(levels(nee$Species2)), levels(dlw_mean$Species), levels(hmr$Species))))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

## Assuming hovering for 2 hours a day
hmr$kJ_day <- hmr$hmr_kJ_min*60*2
hmr$Mass_g <- hmr$Mass
hmr_mean$kJ_day <- hmr_mean$kJ_min*60*2

ggplot(NULL, aes(log(Mass_g), log(kJ_day))) + 
  geom_point(data=dlw_mean, aes(col=Species), size=6, shape=19) + 
  geom_smooth(data=fmr_data, method=lm, alpha=0.3) + 
  geom_point(data=nee_mean, aes(col=Species),size=6, shape=19) +
  geom_smooth(data=nee, method=lm, alpha=0.3, col='black') + 
  geom_point(data=hmr_mean, aes(col=Species),size=6, shape=19) +
  geom_smooth(data=hmr, method=lm, alpha=0.3, col='red') + 
  #geom_point(data=fmr_data, aes(col=Species), shape = 19, size=4, alpha=0.5) + 
  geom_text(aes(x = 1.75, y = 4.75), col="blue", 
            label = lm_eqn(log(dlw_mean$kJ_day),
                           log(dlw_mean$Mass_g)), parse=T, size=8) +
  geom_text(aes(x = 2.5, y = 3.25), col="black", 
            label = lm_eqn(log(nee_mean$kJ_day),
                           log(nee_mean$Mass_g)), parse=T, size=8) +
  geom_text(aes(x = 2.25, y = 1.5), col="red", 
            label = lm_eqn(log(hmr_mean$kJ_day),
                           log(hmr_mean$Mass_g)), parse=T, size=8) +
  my_theme + xlab("Log(Mass (g))") +
  scale_colour_manual(values = getPalette(colourCount)) +
  ylab("Log(kJ per day)")  + 
  theme(legend.key.height=unit(2,"line"))

## Good graph of just individual points, with regression line through them. 
ggplot(NULL, aes(log(Mass_g), log(kJ_day))) + 
  geom_smooth(data=fmr_data, method=lm, alpha=0.7) + 
  geom_point(data=fmr_data, aes(col=Species), size=4, alpha=0.7) + my_theme + xlab("Log(Mass (g))") +
  scale_colour_manual(values = getPalette(colourCount)) +
  ylab("Log(kJ per day)")  + 
  theme(legend.key.height=unit(2,"line"))


## t-test testing temperate vs. tropical sites:
## Raw DEE significant, but includes PAGI (t(60) = 5.03, p-value = 4.65e-06)
an.fmr <- aov(log(kJ_day)~Temptrop+log(Mass_g), data=fmr_data)
summary(an.fmr)  
plot(an.fmr,1) # residuals vs. fitted. Shows that residuals don't have clear non-linear pattern
plot(an.fmr,2) # Q-Q plot. Shows that residuals are normally fitted for the most part. Few outliers to watch for in next few plots
plot(an.fmr,3) # Scale-location plots. Line is horizontal, no big slope. Means that residuals are spread evenly among predictors. Shows homoscedasticity
plot(an.fmr,5) # Reisduals vs. leverage. All points are well enough away from the dashed red line; shows that no single point is overly influential.
aov_residuals <- residuals(object = an.fmr)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)
