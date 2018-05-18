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
#setwd("C:\\Users\\ANUSHA\\Dropbox\\DLW_paper\\Data\\")
setwd("C:\\Users\\nushi\\Dropbox\\DLW_paper\\Data")
#GFU
setwd("/Users/anshankar/Dropbox/DLW_paper/Data")
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
#head.species<-c("latirostris.3", "fulgens","clemenciae", "rubinoides", "imperatrix", "mellivora", "jacula", "coelestis","tzacatl", "urochrysia", "alexandri",
#                "Calypte.anna","colombica.fannyae", "colombica.colombica", "benjamini", "yaruqui", "gigas")
#ii<-sapply(head.species,grep,tree_dlw$tip.label)
#ii

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

## May 2018 - Trying out GLS models with OU vs. Brownian motion
## https://www.r-phylo.org/wiki/HowTo/PGLS
fmr<-fmr_data$kJ_day
mass_g<-fmr_data$Mass_g
DF.fmr<-data.frame(fmr,mass_g,row.names=row.names(fmr_data))
DF.fmr <-  DF.fmr[tre1$tip.label, ]
DF.fmr
bm.fmr<-corBrownian(phy=tre1)
bm.gls<-gls(log(kJ_day)~log(Mass_g),correlation=bm.fmr,data=DF.geospiza)
summary(bm.gls)


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

DEE_full_raw <-MCMCglmm(kJ_day~Mass_g+Temptrop, 
             random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
             prior=prior, data=fmr_data, verbose=FALSE, nitt = 5000000, thin = 1000)
summary(DEE_full_raw)
plot(DEE_full_raw) 

DEE_log <-MCMCglmm(log(kJ_day)~log(Mass_g)+Temptrop, 
                    random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
                    prior=prior, data=fmr_data, verbose=FALSE, nitt = 5000000, thin = 1000)
summary(DEE_log)
plot(DEE_log) 

DEE_log_mass <-MCMCglmm(log(kJ_day)~log(Mass_g), 
                   random=~Species, ginverse = list(Species=inv.phylo$Ainv), 
                   prior=prior, data=fmr_data, verbose=FALSE, nitt = 5000000, thin = 1000)
summary(DEE_log_mass)
plot(DEE_log_mass)


## Plot temp and tropical individuals
my_theme <- theme_classic(base_size = 15) + 
  theme(panel.border = element_rect(colour = "black", fill=NA)) 
colourCount <- length(unique(dlw$Species))
getPalette <- colorRampPalette(brewer.pal(9, "Set3"))

lm_eqn <- function(y, x){
  m <- lm(y ~ x);
  eq <- substitute(italic(y) == 
                     a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

## Plotting temperate vs tropical hummingbird DEE
ggplot(fmr_data, aes(Mass_g, kJ_day, col=Temptrop)) + my_theme +
  geom_smooth(method='lm',se = F, size=2) + 
  geom_point(size=2, alpha=0.7) +
  geom_text(x = 5, y = 100, col="light blue",
            label = lm_eqn(fmr_data$kJ_day[fmr_data$Temptrop==0],
                           fmr_data$Mass_g[fmr_data$Temptrop==0]), parse=T) +
  geom_text(x = 6, y = 150, col="black", 
            label = lm_eqn(fmr_data$kJ_day[fmr_data$Temptrop==1],
                           fmr_data$Mass_g[fmr_data$Temptrop==1]), parse=T) +
  scale_color_manual(values= c("light blue", "black"), 
                      name="Temperate / Tropical", labels=c("Temperate", "Tropical")) +
  ylab("Daily Energy Expenditure (kJ/day)") + xlab("Initial mass (g)")

## Plotting log-log temperate vs tropical hummingbird DEE
ggplot(fmr_data, aes(log(Mass_g), log(kJ_day), col=Temptrop)) + my_theme +
  geom_smooth(method='lm',se = F, size=2) + 
  geom_point(size=2, alpha=0.7) +
  geom_text(aes(label=Species)) +
  geom_text(x = 1.5, y = 4, col="light blue",
            label = lm_eqn(log(fmr_data$kJ_day[fmr_data$Temptrop==0]),
                           log(fmr_data$Mass_g[fmr_data$Temptrop==0])), parse=T) +
  geom_text(x = 1.75, y = 5, col="black", 
            label = lm_eqn(log(fmr_data$kJ_day[fmr_data$Temptrop==1]),
                           log(fmr_data$Mass_g[fmr_data$Temptrop==1])), parse=T) +
  scale_color_manual(values= c("light blue", "black"), 
                     name="Temperate / Tropical", labels=c("Temperate", "Tropical")) +
  ylab("log(Daily Energy Expenditure (kJ/day))") + xlab("log(Initial mass (g))")

dlw_mean$Temptrop <- as.factor(dlw_mean$Region)
levels(dlw_mean$Temptrop)[match("AZ",levels(dlw_mean$Temptrop))] <- 0
levels(dlw_mean$Temptrop)[match("CH",levels(dlw_mean$Temptrop))] <- 1
levels(dlw_mean$Temptrop)[match("CR",levels(dlw_mean$Temptrop))] <- 1
levels(dlw_mean$Temptrop)[match("EC",levels(dlw_mean$Temptrop))] <- 1

## Species means. Regressions based on spp means vs individual values. What do they tell you? Benefits of having individual-level data. 
ggplot(dlw_mean, aes(log(Mass_g), log(kJ_day))) + 
  geom_point(aes(col=Species), size=5) + theme_bw() + xlab("Log(Mass (g))") +
  #scale_shape_manual("Region\n", values=c(16, 3, 1, 2), labels=c("Arizona", "Chile", "Costa Rica", "Ecuador")) +
  #scale_colour_manual(values = getPalette(colourCount)) +
  geom_text(x = 1.5, y = 4, col="light blue",
            label = lm_eqn(log(dlw_mean$kJ_day[dlw_mean$Temptrop==0]),
                           log(dlw_mean$Mass_g[dlw_mean$Temptrop==0])), parse=T, size=8) +
  geom_text(x = 1.75, y = 5, col="black", 
            label = lm_eqn(log(dlw_mean$kJ_day[dlw_mean$Temptrop==1]),
                           log(dlw_mean$Mass_g[dlw_mean$Temptrop==1])), parse=T, size=8) +
  ylab("Log(kJ per day)")  + geom_smooth(aes(group=Temptrop), method=lm) + 
  theme(strip.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA), legend.key.height=unit(2,"line"))

## Individual values, logged. Regressions based on spp means vs individual values. What do they tell you? Benefits of having individual-level data. 
ggplot(fmr_data, aes(log(Mass_g), log(kJ_day))) + 
  geom_point(aes(col=Species), size=5) + my_theme + xlab("Log(Mass (g))") +
  #scale_shape_manual("Region\n", values=c(16, 3, 1, 2), labels=c("Arizona", "Chile", "Costa Rica", "Ecuador")) +
  #scale_colour_manual(values = getPalette(colourCount)) +
  #geom_text(x = 1.5, y = 4, col="light blue",
   #         label = lm_eqn(log(dlw_mean$kJ_day[dlw_mean$Temptrop==0]),
    #                       log(dlw_mean$Mass_g[dlw_mean$Temptrop==0])), parse=T, size=8) +
  geom_text(x = 1.75, y = 5, col="black", 
            label = lm_eqn(log(fmr_data$kJ_day),
                           log(fmr_data$Mass_g)), parse=T, size=8) +
  ylab("Log(kJ per day)")  + geom_smooth(method=lm) + 
  theme(legend.key.height=unit(2,"line"))

#Relationship without PAGI:
lm(log(fmr_data$kJ_day[fmr_data$Temptrop==1 & fmr_data$Species != "PAGI"]) ~ 
     log(fmr_data$Mass_g[fmr_data$Temptrop==1 & fmr_data$Species != "PAGI"]))

## t-test testing temperate vs. tropical sites:
## Raw DEE signdificant, but includes PAGI (t(60) = 5.03, p-value = 4.65e-06)
an.fmr <- aov(log(kJ_day)~Temptrop+log(Mass_g), data=fmr_data)
summary(an.fmr)  
plot(an.fmr,1) # residuals vs. fitted. Shows that residuals don't have clear non-linear pattern
plot(an.fmr,2) # Q-Q plot. Shows that residuals are normally fitted for the most part. Few outliers to watch for in next few plots
plot(an.fmr,3) # Scale-location plots. Line is horizontal, no big slope. Means that residuals are spread evenly among predictors. Shows homoscedasticity
plot(an.fmr,5) # Reisduals vs. leverage. All points are well enough away from the dashed red line; shows that no single point is overly influential.
aov_residuals <- residuals(object = an.fmr)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)