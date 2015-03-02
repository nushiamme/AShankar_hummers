library(reshape)
library(ggplot2)

setwd("C://Users//ANUSHA//Dropbox//Anusha_personal//Thesis_proposal//R_csv//AZ")

costa <- read.csv("Costa1986_Don.csv")
costaVO2 <- read.csv("Costa1986_DonVO2.csv")
bblh <- read.csv("BroadBill.csv")
hmr <- read.csv("HMR_AZ_11_20.csv")
names(hmr) <- c("Rufous", "Broad-billed", "Black-chinned")
## Read data from Pearson's 1954 paper on "The daily energy requirements of a wild Anna hummingbird"
budget <- read.csv("TimeEnergyBudget_pearson1954.csv")

m.hmr <- melt(hmr, na.rm = T)
names(m.hmr) <- c("Species", "HMR")

names(budget) <- c("Activity", "Subactivity", "Time", "Energy_Torpor", "Energy_noTorpor")
m.budget <- melt(budget, id.vars = c("Activity", "Subactivity"),
                 measure.vars = c("Time", "Energy_Torpor", "Energy_noTorpor"))

## HMR
hmr.bblh <- m.hmr$HMR[m.hmr$Species=="Broad-billed"]
q.hmr <- quantile(hmr.bblh,probs=seq(0.33,1))
mean(hmr.bblh[hmr.bblh>q.hmr])

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

## hmr unscaled by body mass
hmr_comparison <- ggplot(m.hmr, aes(x=Species, y=HMR)) + geom_point() + 
  geom_boxplot(outlier.colour="red") + theme_bw()
hmr_comparison

## Plot temperatures
ggplot(costa, aes(Temperature, MR)) + geom_point() + theme_bw()
plot(costa$BelowLCT~costa$Temperature)
plot(costa$AboveUCT~costa$Temperature)

## Fitting a glm
below.glm <- glm(below$BelowLCT~below$Temperature)
hist(below.glm$residuals)


m.budget$value <- as.numeric(m.budget$value)
ggplot(m.budget, aes(variable, value, fill=Activity)) + xlab("Type of budget") + ylab("Percentage") + 
    geom_bar(stat="identity") + theme_bw() + scale_fill_brewer(type = "qual",palette = 6) +
  theme(axis.title.x = element_text(size=16), axis.text.x = element_text(angle=10, family = "Times",
                                                                         vjust=0.5, hjust = 0.5, size=13),
        axis.title.y = element_text(size=16), axis.text.y = element_text(size=13),
        legend.title = element_text(size=16), legend.text = element_text(size = 16))

