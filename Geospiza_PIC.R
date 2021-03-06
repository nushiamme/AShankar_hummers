## Jan 31, 2015. Anusha Shankar. Script adapted from http://www.r-phylo.org/wiki/HowTo/Phylogenetic_Independent_Contrasts

library(ape)
## This script uses the pic() function from ape. pic = phylogenetically independent contrasts

setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha_personal\\Thesis proposal\\R_csv")
gtree <- read.nexus("Geospiza.nex")
gtip <- read.table("Geospiza.txt")

## Drop one tip in the Geospiza tree, because it is not used later
gtree <- drop.tip(gtree, "olivacea")

## Separate numeric vectors for two of the traits- wing and tarsus
wingL <- gtip$wingL
tarsusL <- gtip$tarsusL

## Rename these numeric vectors with species names from the same database
names(wingL) <- row.names(gtip)
names(tarsusL) <- row.names(gtip)

## Calculate contrasts- the tree (.nex file) contains expected variances which you can scale the contrasts with.
## var.contrasts = T gives you the expected variances along with the contrasts
ContrastwingL.var <- pic(wingL, gtree, var.contrasts = T)
ContrasttarsusL.var <- pic(tarsusL, gtree, var.contrasts = T)

## If you want the contrasts unscaled by expected variances, enter scaled=F
ContrastwingL <- pic(wingL, gtree, scaled=F)
ContrasttarsusL <- pic(tarsusL, gtree, scaled=F)

## To calculate the contrasts for several variables, create a matrix with the variables in columns, and then use apply
contrasts.gtip <- apply(gtip, 2, pic, gtree)

## To see the contrasts of wingL, both with and without variances. 
## ???????? I don't understand- the contrasts are the same in both cases
ContrastwingL.var
ContrastwingL

## See contrasts plotted at the appropriate nodes
## plotting just the contrasts, i.e. the first column of the ContrastwingL.var, to 3 decimal places
## adj specifies the position on the plot, frame = "y" would put the numbers in boxes. I added colors to separate the traits
plot(gtree)
nodelabels(round(ContrastwingL.var[,1], 3), adj = c(0, -0.5), frame="n", col="blue")
nodelabels(round(ContrasttarsusL.var[,1], 3), adj = c(0, 1), frame="n", col="red")

## Do a linear regression on the wing contrasts without extracting the variances.
## -1 constrains the regression to go through the origin
regressTarsusWing <- lm(ContrastwingL~ContrasttarsusL -1)

## Look at regression stats
summary.lm(regressTarsusWing)

## Plot to check the assumption that there is a linear relationship between the two variables
plot(ContrastwingL, ContrasttarsusL)

## Add regression line
abline(regressTarsusWing)
