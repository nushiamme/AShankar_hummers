## Jan 31, 2015. Anusha Shankar.

library(ape)
library(picante)
library(stringr)
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

## Use only traits which have matching tips on the phylogeny, and remove NA's

##########****************** THIS IS GIVING PROBLEMS STILL ************************###############
newhum <- match.phylo.data(humtree, humdata)

#humdata <- na.omit(humdata[match(humtree$tip.label,humdata$Species_Name),])
#head(humdata) # check

## Prune the tree to include only species which we have traits for
#humtree <- na.omit(humtree[match(humdata$Species_Name, humtree$tip.label),])

## Save the traits you want as separate vectors
hmass <- humdata$mass_meangr
hwchord <- humdata$wchord_meanmm

#### Make the species names the column names for each of these vectors
names(hmass) <- row.names(humdata)
names(hwchord) <- row.names(humdata)

#### Calculate PIC for the traits
hcontrastmass.var <- pic(hmass, humtree, var.contrasts=T)

row.names(humdata)
row.names(humdata) <- humdata$Species_Name
