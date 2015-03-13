library(reshape)
library(ggplot2)
library(stringr)
library(extrafont)
library(RColorBrewer)

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

names(budget) <- c("Act", "Subactivity", "Activity", "Energy with Torpor", "Energy without Torpor")
m.budget <- melt(budget, id.vars = c("Act", "Subactivity"),
                 measure.vars = c("Activity", "Energy with Torpor", "Energy without Torpor"))

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


## Plot Time/Energy budgets from Pearson 1954 data
m.budget$value <- as.numeric(m.budget$value)
##my.cols <- brewer.pal(3, "Paired")
my.cols <- c("#91cf60","#1c9099", "#e34a33")


budget_plot <- ggplot(m.budget, aes(variable, value, fill=Act)) + xlab("Type of budget") + ylab("Percentage") + 
    geom_bar(stat="identity") + theme_bw() +
  theme(text=element_text(family="sans"), axis.title.x = element_text(size=16, vjust=0.2, face="bold"), 
        axis.text.x = element_text(size=13, face="bold"),
       axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=13),
        legend.title = element_text(size=16), legend.text = element_text(size = 12, face="bold")) +
  guides(fill=guide_legend(title="Activity")) +
  scale_x_discrete(labels = function(variable) str_wrap(variable, width = 16)) + 
  scale_fill_manual(values = my.cols, breaks=c("Torpor/Sleep","Flying","Perching"))
budget_plot


##### Energy budget model ########
sc_temp <- read.csv("SonoitaCreek_Temperatures_S1.csv")

## Melt
m.sc <- melt(sc_temp, id.vars = c("Time", "Mean_Ta"), measure.vars = "MR_ml.h")
m.sc <- m.sc[,c(1,2,4)]
names(m.sc) <- c("Time", "Mean_Ta", "MR_ml_h" )

## DEE = BMR + (TRE_L + TRE_H) + ACT + NEE

## Basal metabolic rate for time spent within the TNZ in 14 daytime hours (5am-7pm)
t_bmr <- sum(m.sc$MR_ml_h[32 < m.sc$Mean_Ta & 
                            m.sc$Mean_Ta < 35 & 4 < m.sc$Time & m.sc$Time < 20])

## TRE_H from SC 14-hour temperature data and broad-bill equation
tre_h <- sum(m.sc$MR_ml_h[m.sc$Mean_Ta > 35 & 4 < m.sc$Time & m.sc$Time < 20])

## TRE_L from SC 14-hour temperature data and broad-bill equation
tre_l <- sum(m.sc$MR_ml_h[m.sc$Mean_Ta < 32 & 4 < m.sc$Time & m.sc$Time < 20])

## Total energy spent on BMR + thermoregulation in 14 daytime hours
tre_total <- tre_h + tre_l + t_bmr

## Nighttime energy expenditure in ml O2/h from broad-bill data, 10 hour night (7pm-5am)
nee <- sum(m.sc$MR_ml_h[m.sc$Time < 5 | m.sc$Time > 18])

## Metabolic rates in ml O2/h
bmr <- 0.2385*60
rmr <- 1.5*bmr
hmr <- 10.3*bmr
flmr <- 0.5*hmr

## Total energy spent on daytime activities in ml O2/h. 
## Assuming ACT = 70% resting + 20% hovering + 10% flying; 14 daylight hours
ACT <- (0.7*14*(rmr-bmr)) + (0.2*14*(hmr-bmr)) + (0.1*14*(flmr-bmr))

## Model estimate in ml O2 per 24h
DEE_model <- tre_total + ACT + nee

## To get a per hour CO2 estimate, I multiply by RQ (assumed to be 0.85) and divide by 24
DEE_model_hr <- DEE_model*0.85/24

## Measured estimate of DEE from DLW
dlw <- 51.3

## Percentage the model is off from the mean DLW estimate
per.off <- ((dlw - DEE_model_hr)/dlw)*100