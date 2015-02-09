## Jan 31, 2015. Anusha Shankar.

library(ape)
library(picante)
library(stringr)
library(ggplot2)

## This script uses the pic() function from ape. pic = phylogenetically independent contrasts

setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha_personal\\Thesis proposal\\R_csv")

## Read in tree and data files
humtree <- read.nexus("humtree.nex")
humdata <- read.csv("Grahamtraits.csv")

## Remove white spaces
humdata$Species_Name <- str_trim(humdata$Species_Name)
humdata$mass_meangr <- str_trim(humdata$mass_meangr)

## Replace row names with species names and then remove species name column
rownames(humdata) <- make.names(humdata$Species_Name, unique = T)
drops <- "Species_Name"
humdata <- humdata[,!(names(humdata) %in% drops)]

#humdata <- humdata[complete.cases(humdata),]

## Replace "." with _ and then Remove all the unnecessary stuff from the tip labels, to try to match to trait data
humtree$tip.label <- gsub("\\.","_", humtree$tip.label)
humtree$tip.label <- gsub("'","", humtree$tip.label)
humtree$tip.label <- sub("(.*?_.*?)_.*", "\\1", humtree$tip.label)
head(humtree$tip.label) # check

## Use only traits which have matching tips on the phylogeny, and vice versa
matchhum <- match.phylo.data(humtree, humdata)

## Replace old data with matched data
htree <- matchhum$phy
hdata <- matchhum$data

## Make the tree and the trait data have the same order of species
hdata <- hdata[htree$tip.label, ]

## Save the traits you want as separate vectors
hmass <- as.numeric(hdata$mass_meangr)
hwchord <- as.numeric(hdata$wchord_meanmm)

#### Make the species names the column names for each of these vectors
names(hmass) <- row.names(hdata)
names(hwchord) <- row.names(hdata)

#### Calculate PIC for the traits
hcontrastmass.var <- pic(hmass, htree, var.contrasts=T)
hcontrastwchord.var <- pic(hwchord, htree, var.contrasts=T)

## If you want the contrasts unscaled by expected variances
hcontrastmass <- pic(hmass, htree, scaled=F)
hcontrastwchord <- pic(hwchord, htree, scaled=F)

## To calculate the contrasts for several variables, create a matrix with the variables in columns, and then use apply
contrasts.htip <- apply(hdata, 2, pic, htree)

## To see the contrasts of wingL, both with and without variances. 
hcontrastmass.var
hcontrastmass

## See contrasts plotted at the appropriate nodes
## plotting just the contrasts, i.e. the first column of the ContrastwingL.var, to 3 decimal places
## adj specifies the position on the plot, frame = "y" would put the numbers in boxes. I added colors to separate the traits
plot(htree)
nodelabels(round(hcontrastmass.var[,1], 3), adj = c(0, -0.5), frame="n", col="blue")
nodelabels(round(hcontrastwchord.var[,1], 3), adj = c(0, 1), frame="n", col="red")

## Do a linear regression on the wing contrasts without extracting the variances.
## -1 constrains the regression to go through the origin
regresswchordmass <- lm(hcontrastwchord~hcontrastmass -1)

## Look at regression stats
summary.lm(regresswchordmass)

## Plot to check the assumption that there is a linear relationship between the two variables
plot(hcontrastmass, hcontrastwchord)

## Add regression line
abline(regresswchordmass)

## Plot phylogeny with traits as points scaled on the tips
op <- par(oma=c(5,7,1,1)) ## Save default par to reset to later
par(mar = rep(2, 4)) ## To make plot bigger
plot(htree, direction = "up", show.tip.label = T, show.node.label = TRUE, 
     cex = 0.7)
par(op) ## Reset to default par
# Plot leaf area on the phylogeny. cex argument scales symbol size by trait
# value.
#tiplabels(pch = 19, col = "black", cex = 3 * (hwchord/max(hwchord)))
tiplabels(pch = 18, col = "red", cex = 3 * (hmass/max(hmass)))

# GLS of wing chord as a function of mass - non-phylogenetic model
masswchord.gls <- gls(hwchord~hmass)
anova(masswchord.gls)

##################### has to be fixed ##################
# Phylogenetic GLS - adds effect of phylogeny to the model
root.pgls <- gls(hwchord ~ hmass, correlation = corBrownian(value = 1, htree))
anova(root.pgls)

# plot relationship
ggplot(aes(hwchord,hmass)) + ggpoint() + xlab("Mass") + ylab("Wing chord")
# add model fit lines - coef is the model fit coefficients, lwd increases
# line width
abline(coef(root.gls), lwd = 2, col = "black")
abline(coef(root.pgls), lwd = 2, col = "red")
legend("bottomleft", legend = c("GLS fit", "Phylogenetic GLS fit"), lwd = 2, 
       col = c("black", "red"))