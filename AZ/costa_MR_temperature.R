library(reshape)
library(ggplot2)

setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha_personal\\Thesis proposal\\R_csv\\AZ")

costa <- read.csv("Costa1986_Don.csv")
costaVO2 <- read.csv("Costa1986_DonVO2.csv")
bblh <- read.csv("BroadBill.csv")
hmr <- read.csv("HMR_AZ_11_20.csv")
names(hmr) <- c("Rufous", "Broad-billed", "Black-chinned")

m.hmr <- melt(hmr,na.rm = T)
names(m.hmr) <- c("Species", "HMR")

## Below LCT for broadbills
lm.below <- lm(bblh$Normothermic~bblh$TempC)

## Below LCT and above UCT for Costas
below <- costa[costa$Temperature<=32,]
lm.belowcosta <- lm(below$BelowLCT~below$Temperature)
lm.abovecosta <- lm(costaVO2$AboveVO2~costaVO2$Temperature)

lm.aboveCosta_pergram <- lm(costa$AboveUCT~costa$Temperature)

## BMR calculated from extrapolating BBLH summer 2012 data down to presumed LCT of 32&deg;C
bmrBBLH <- 0.23846
yinterceptBBLH <- (lm.abovecosta$coefficients[2]*35)-bmrBBLH

yinterceptBBLH <- (0.0144*35)-bmrBBLH


## Unscaled by body mass
hmr_comparison <- ggplot(m.hmr, aes(x=Species, y=HMR)) + geom_point() + geom_boxplot(outlier.colour="red") + theme_bw()
hmr_comparison

## Plot temperatures
ggplot(costa, aes(Temperature, MR)) + geom_point() + theme_bw()
plot(costa$BelowLCT~costa$Temperature)
plot(costa$AboveUCT~costa$Temperature)
