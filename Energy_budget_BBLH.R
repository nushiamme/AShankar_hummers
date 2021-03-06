## Daily energy budgeting in a temperate hummingbird
## Paper Authors: Anusha Shankar, Donald R. Powers, Susan Wethington, 
# Joseph R. Canepa, Catherine H. Graham
## Script by: Anusha Shankar
## Start date: Wednesday, August 31, 2016
## Last updated: September 22, 2016

library(ggplot2)
library(reshape)
library(car)

## NOTE: masses for SC pre- vs. post-monsoon might be significantly different - check
## from "C:\Users\nushi\Dropbox\DLW_paper\BBLH 2013 Poster Data.xlsx"

setwd("C:\\Users\\nushi\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Tables")
## wd at GFU
setwd("/Users/anshankar/Dropbox/Anusha Committee/BBLH_EnergyBudget/Tables")
## Includes data from XXXX papers.

dlw <- read.csv("C:\\Users\\nushi\\Dropbox\\DLW_paper\\Data\\DLW_data2.csv")
##At GFU
dlw <- read.csv("/Users/anshankar/Dropbox/DLW_paper/Data/DLW_data2.csv")
dlw <- dlw[dlw$Reasonable_not=="Y",]


dlw_bblh <- read.csv("DLW_summary.csv")
dlw_bblh$Site_monsoon <- paste(dlw_bblh$Site, dlw_bblh$Pre_post_monsoon, sep="_") # column combining site and monsoon status
dlw_bblh$Site_monsoon <- factor(dlw_bblh$Site_monsoon, levels=c('HC_Pre','HC_Post','SC_Pre','SC_Post'))
dlw_bblh$Initial_mass_g <- as.numeric(as.character(dlw_bblh$Initial_mass_g))

## TNZ files
bblh_tnz <- read.csv("../Energy budget data/BroadBill.csv")
## Merged N? and N in Excel (first 3 N's were N?) because the points looked similar
bblh_tnz$N_T <- factor(bblh_tnz$N_T, levels=c('T', 'N'))

#### Reading in Torpor files ####
## Pulling in BBLH torpor data - UPDATE
torpor <- read.csv("C:\\Users\\nushi\\Dropbox\\Hummingbird energetics\\Submission_Oct2016\\Torpor_individual_summaries.csv")

torpor$AvgEE_normo_MassCorrected <- torpor$Avg_EE_hourly_normo/(torpor$Mass)
torpor$AvgEE_torpid_MassCorrected <- torpor$Avg_EE_hourly_torpid/(torpor$Mass)
BBLH_torpor <- subset(torpor, Species=="BBLH")

## Reading in merged NEE and DEE dataset including only pre-monsoon DEE data. For all DEE data, use BBLH_merged_summ.csv
bblh_merged <- read.csv("BBLH_merged_premonsoon.csv")
m.bblh <- melt(bblh_merged, id.vars="Site", measure.vars = c("NEE_kJ", "DEE_kJ"))
m.bblh$variable <- factor(m.bblh$variable, levels=c('DEE_kJ', 'NEE_kJ'))

###### General functions #####
## Generic plot theme
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

lm_eqn <- function(table, y, x){
  m <- lm(y ~ x, table);
  eq <- substitute(italic(y) == 
                     a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

## Axis titles
Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))

#### BBLH DEE plots ####
# DEE by pre- vs. post- monsoon
dee_BBLH_monsoon <- ggplot(dlw_bblh, aes(Initial_mass_g, kJ_day)) + my_theme + 
  geom_point(aes(shape=Site_monsoon), size=7) + 
  scale_shape_manual("Site_monsoon", values=c(20,3,2,8)) +
  #geom_text(x = 2.8, y = 40, 
  #label = lm_eqn(dlw_bblh$kJ_day, dlw_bblh$Initial_mass_g), parse=T, size=10) +
  geom_smooth(stat='smooth', method='loess', data=dlw_bblh, 
              aes(Initial_mass_g, kJ_day, group=Site_monsoon, col=Site_monsoon)) +
  #geom_line(stat='smooth', data=dlw_bblh, aes(Initial_mass_g, kJ_day, group=Site_monsoon, col=Site_monsoon),
  #           method="lm",size=2, alpha=0.6) +
  ylab("Daily energy expenditure (kJ/day)") + xlab("Initial mass (g)")
dee_BBLH_monsoon

summary(lm(dlw_bblh$kJ_day[dlw_bblh$Pre_post_monsoon=="Post"] ~ 
             dlw_bblh$Initial_mass_g[dlw_bblh$Pre_post_monsoon=="Post"]))

## DEE by site
dee_BBLH_site <- ggplot(dlw_bblh, aes(Initial_mass_g, kJ_day)) + my_theme +
  geom_point(aes(col=Site), size=7) +  
  scale_shape_manual("Site", values=c(20,3), 
                     labels=c("HC", "SC"), breaks=c("Pre", "Post")) + 
  ylab("Daily energy expenditure (kJ/day)") + xlab("Initial mass (g)")
plot(dee_BBLH_site, fig.align='left')

## DEE by site and pre- and post-monsoon, points different for pre- and -post
dee_BBLH_site_monsoon_plot <- ggplot(dlw_bblh, aes(Site_monsoon, kJ_day)) + geom_boxplot() + 
  my_theme + geom_point(aes(shape=Pre_post_monsoon), size=5, alpha=0.4, col="blue") + 
  scale_shape_manual("Season\n", values=c(20,3), labels=c("Pre-monsoon", "Post-monsoon"), breaks=c("Pre", "Post")) + 
  ylab("Daily energy expenditure (kJ/day)") + xlab("Site") + theme(legend.key.height = unit(3, 'lines'))
dee_BBLH_site_monsoon_plot

## Points all the same
dee_BBLH_site_monsoon_plot <- ggplot(dlw_bblh, aes(Site_monsoon, kJ_day)) + geom_boxplot() + 
  my_theme + geom_point(aes(col=Pre_post_monsoon), size=5, alpha=0.4, col="blue") + 
  scale_shape_manual("Season\n", values=c(20,3), labels=c("Pre-monsoon", "Post-monsoon"), breaks=c("Pre", "Post")) + 
  ylab("Daily energy expenditure (kJ/day)") + xlab("Site")
dee_BBLH_site_monsoon_plot

dee_BBLH_site <- ggplot(dlw_bblh[dlw_bblh$Pre_post_monsoon=="Pre",], aes(Site, kJ_day)) + geom_boxplot() + 
  my_theme + geom_point(size=5, alpha=0.4, col="blue") + 
  scale_shape_manual("Season\n", values=c(20,3), labels=c("Pre-monsoon", "Post-monsoon"), breaks=c("Pre", "Post")) + 
  ylab("Daily energy expenditure (kJ/day)") + xlab("Site")
dee_BBLH_site

#### Torpor plots ####

## Comparing HC and SC BBLH NEE
ggplot(BBLH_torpor, aes(Site, NEE_kJ)) + my_theme +
  geom_boxplot() + geom_point() +
  ylab("Nighttime energy expenditure (kJ)")

ggplot(BBLH_torpor, aes(Site, AvgEE_normo_MassCorrected)) + my_theme +
  geom_boxplot() + geom_point(aes(col=Tc_min_C), size=5) + 
  scale_colour_gradient(low = "blue", high = "red", "Min chamber temperature\n") +
  ylab("Hourly Energy expenditure in normothermic birds (J/h*g)")

#### Combining DLW and torpor plots ####
## Remember- m.bblh only has pre-monsoon data
ggplot(m.bblh, aes(Site, value)) + geom_boxplot(aes(col=variable)) + my_theme + 
  stat_summary(fun.data = give.n, geom = "text", vjust=-1.5, size=5) +
  scale_color_manual("Energy expenditure", labels=c("24-hour daily", "Nighttime"), values=c("black", "#ff0000")) + 
  ylab("kiloJoules")

#### DEE and NEE costs at each site; NEE as proportion of DEE ####
dee.hc <- mean(m.bblh$value[m.bblh$variable=="DEE_kJ" & m.bblh$Site=="HC"], na.rm=T)
dee.sc <- mean(m.bblh$value[m.bblh$variable=="DEE_kJ" & m.bblh$Site=="SC"], na.rm=T)
nee.hc <- mean(m.bblh$value[m.bblh$variable=="NEE_kJ" & m.bblh$Site=="HC"], na.rm=T)
nee.sc <- mean(m.bblh$value[m.bblh$variable=="NEE_kJ" & m.bblh$Site=="SC"], na.rm=T)
dee.hc
dee.sc
nee.hc
nee.sc

nee.hc/dee.hc
nee.sc/dee.sc

#### T-tests for site and monsoon differences ####
t.test(m.bblh$value[m.bblh$variable=="DEE_kJ" & m.bblh$Site=="HC"], 
       m.bblh$value[m.bblh$variable=="DEE_kJ" & m.bblh$Site=="SC"], na.rm=T)
Premonsoon <- dlw[dlw$Pre_post_monsoon=="Pre",]
Postmonsoon <- dlw[dlw$Pre_post_monsoon=="Post",]
t.test(Premonsoon$kJ_day[Premonsoon$Site=="HC"], Premonsoon$kJ_day[Premonsoon$Site=="SC"], paired = F) ## Not significant
t.test(Postmonsoon$kJ_day[Postmonsoon$Site=="HC"], Postmonsoon$kJ_day[Postmonsoon$Site=="SC"], paired = F) # Significant
t.test(Premonsoon$kJ_day, Postmonsoon$kJ_day, paired = F) # No difference
t.dee.sites <- t.test(dlw$kJ_day[dlw$Site=="HC"], dlw$kJ_day[dlw$Site=="SC"],paired = F) ## Significant
t.test(Premonsoon$kJ_day[Premonsoon$Site=="HC"], Postmonsoon$kJ_day[Postmonsoon$Site=="HC"], paired=F) ## Significant
t.test(Premonsoon$kJ_day[Premonsoon$Site=="SC"], Postmonsoon$kJ_day[Postmonsoon$Site=="SC"], paired=F) ##Highly significant
pander(t.dee.sites)
summary(lm(dlw$kJ_day[dlw$Pre_post_monsoon=="Post" & dlw$Site==c("HC", "SC")] ~ 
             dlw$Initial_mass[dlw$Pre_post_monsoon=="Post" & dlw$Site==c("HC", "SC")]))
summary(lm(dlw$kJ_day[dlw$Pre_post_monsoon=="Pre" & dlw$Site==c("HC", "SC")] ~ 
             dlw$Initial_mass[dlw$Pre_post_monsoon=="Pre" & dlw$Site==c("HC", "SC")]))
## Using this May 12, 2018 #http://data.library.virginia.edu/diagnostic-plots/
atest.1 <- aov(kJ_day~Site+Pre_post_monsoon+Initial_mass_g, data=dlw_bblh)
summary(atest.1)
plot(atest.1,1) # residuals vs. fitted. Shows that residuals don't have clear non-linear pattern
plot(atest.1,2) # Q-Q plot. Shows that residuals are normally fitted for the most part. Few outliers to watch for in next few plots
plot(atest.1,3) # Scale-location plots. Line is horizontal, no big slope. Means that residuals are spread evenly among predictors. Shows homoscedasticity
plot(atest.1,5) # Reisduals vs. leverage. All points are well enough away from the dashed red line; shows that no single point is overly influential.
leveneTest(kJ_day~Site*Pre_post_monsoon+Initial_mass_g, data=dlw_bblh)
aov_residuals <- residuals(object = atest.1)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)


t.test(dlw_bblh$Initial_mass_g[dlw_bblh$Site=="HC" & dlw_bblh$Pre_post_monsoon=="Pre"], 
       dlw_bblh$Initial_mass_g[dlw_bblh$Site=="HC" & dlw_bblh$Pre_post_monsoon=="Post"])
       

#### Thermoregulatory costs ####
bblh_MR_temp <- ggplot(bblh_tnz, aes(Temp_C, VO2)) + my_theme +
  geom_point(aes(shape=N_T, color=N_T)) + 
  scale_color_manual("Torpor/ \n Normothermy", labels=c("Torpor", "Normothermy"), values=c("#ff0000", "blue")) +
  scale_shape_manual("Torpor/ \n Normothermy", labels=c("Torpor", "Normothermy"), values=c(1,19)) +
  geom_smooth(stat='smooth', method='lm', data=bblh_tnz[bblh_tnz$N_T == "N",], 
              aes(Temp_C, VO2, group=N_T, col=N_T), alpha=0.2) +
  geom_smooth(stat='smooth', method='loess', data=bblh_tnz[bblh_tnz$N_T=="T",],
              aes(Temp_C, VO2, group=N_T, col=N_T), alpha=0.2) +
  ylab(expression(VO["2"] (ml/min))) + xlab(Temp.lab) + theme(legend.key.height=unit(3,"line")) +
  scale_x_continuous(breaks=seq(0,40,5))
bblh_MR_temp

bblh_MR_normo <- ggplot(bblh_tnz, aes(Temp_C, Normothermic)) + my_theme +
  geom_point() + 
  geom_smooth(stat='smooth', method='lm', data=bblh_tnz, 
              aes(Temp_C, Normothermic)) +
  ylab(expression(VO["2"] (ml/min))) + xlab(Temp.lab) + 
  theme(legend.key.height=unit(3,"line")) +
  scale_x_continuous(breaks=seq(0,40,5))
bblh_MR_normo
lm(bblh_tnz$Normothermic~bblh_tnz$Temp_C)
            
## Just torpor data
bblh_MR_tor <- ggplot(bblh_tnz[bblh_tnz$N_T=="T",], aes(Temp_C, VO2)) + my_theme +
  geom_point(size=5) + 
  geom_smooth(stat='smooth', method='loess', data=bblh_tnz[bblh_tnz$N_T=="T",],
              aes(Temp_C, VO2), col="black", alpha=0.2) +
  ylab("Oxygen consumption (ml/min)") + xlab(Temp.lab) + 
  theme(axis.title.y = element_text(size=25), axis.title.x = element_text(size=25)) +
  scale_x_continuous(breaks=seq(0,40,5))
bblh_MR_tor

## Just torpor data
bblh_MR_tor <- ggplot(bblh_tnz[bblh_tnz$N_T=="T",], aes(Temp_C, VO2)) + my_theme +
  geom_point(col="red", size=4) + 
  geom_smooth(stat='smooth', method='loess', data=bblh_tnz[bblh_tnz$N_T=="T",],
              aes(Temp_C, VO2), col="red", alpha=0.2) +
  ylab("Oxygen consumption (ml/min)") + xlab(Temp.lab) + 
  theme(axis.title.y = element_text(size=25), axis.title.x = element_text(size=25)) +
  scale_x_continuous(breaks=seq(0,40,5))
bblh_MR_tor

bblh_LCT_eqn <- lm(bblh_tnz$Normothermic~bblh_tnz$Temp_C)
