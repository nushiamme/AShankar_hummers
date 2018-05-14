## 3D surface plot of daily energy expenditure vs. activity and temperature

# Sign in to plotly - like Git
library(plotly)
library(ggplot2)
library(rgl)
library(dplyr) # Trying this out for stacking NEE with and without torpor
library(reshape2) # Trying this out for stacking NEE with and without torpor
library(gridExtra)
library(cowplot) #for plot_grid function instead of grid.arrange
library(tidyverse) #for the stacked bar plot- labeled as such in case you want to delete this later

#library(rgl)

setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Tables")## laptop
## wd at GFU
#setwd("/Users/anshankar/Dropbox/Anusha Committee/BBLH_EnergyBudget/Tables")
#energymodels <- read.csv("Trial_EnergyBudget_models_act_thermo.csv")
#energymodels2 <- read.csv("Trial_EnergyBudget_models_act_thermo_redone.csv")
energymodels3 <- read.csv("Trial_EnergyBudget_models_act_thermo_Jul2017.csv") #includes BMR variation but not
#new activity budget scenario
## Modified Trial_EnergyBudget_models_act_thermo_Jul2017_2.csv slightly:
energymodels4 <- read.csv("Trial_EnergyBudget_models_act_thermo_Jan2018.csv") #incl BMR and new act budget scenario and torpor split
energymodels_jan <- read.csv("Jan_all_new_models.csv") #Includes min and max 24h cost by varying activity; per activity, thermo, NEE and BMR scenario
#Includes min and max 24h cost by varying activity; per activity, thermo, NEE and BMR scenario; and adjusting hovering for thermoregulatory substitution
energymodels_may <- read.csv("May_hover_Thermo_adj.csv") 
act_models <- read.csv("Activity_modeled.csv") #Varying HMR, FLMR, RMR
dee_act <- read.csv("DEE_for_activity_models.csv")
percentEB <- read.csv("percent_EB.csv")

dlw_bblh <- read.csv("DLW_summary.csv")

valida_A <- read.csv("Validation_Enrichment_dose_A.csv")
valida_B <- read.csv("Validation_enrichment_eqb_B.csv")
valida_C <- read.csv("Validation_CO2produc_dose_C.csv")

#### General functions ####
my_theme <- theme_classic(base_size = 32) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 10) + 
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

## Place plots side by side with shared legend
grid_arrange_shared_legend_hori <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}

#### Old, ignore? #####
### New data frames or vectors ###
## Range of results of the thermoregulatory models
# Pull out all the minimum costs
vec1 <- energymodels2$Daytime_EE_kJ[energymodels2$Thermoreg_scenario=="Min_cost"]
# Max costs
vec2 <- energymodels2$Daytime_EE_kJ[energymodels2$Thermoreg_scenario=="Max_cost"]
# Difference between the two models
mean(vec2-vec1)
sd(vec2-vec1)
# Randomized models, min and max
vec3 <- energymodels2$Daytime_EE_kJ[energymodels2$Thermoreg_scenario=="Rand_cost_min"]
vec4 <- energymodels2$Daytime_EE_kJ[energymodels2$Thermoreg_scenario=="Rand_cost_max"]
# Difference between the two models
mean(vec4-vec3)
sd(vec4-vec3)

## Range of results of the activity budget models
## Lowest ACT costs
vec5 <- energymodels2$Daytime_EE_kJ[energymodels2$Activity_budget_type=="5_20_75"]
# Max costs
vec6 <- energymodels2$Daytime_EE_kJ[energymodels2$Activity_budget_type=="25_30_45"]
# Difference between the two models
mean(vec6-vec5)
sd(vec6-vec5)

#### Aggregating ####
dlw_summ <- as.data.frame(as.list(aggregate(dlw_bblh$kJ_day,
                                by=list(dlw_bblh$Site_monsoon),
                                FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))

m_energymodels <- as.data.frame(as.list(aggregate(energymodels2$kJ_day,
                                                  by=list(energymodels2$Activity_budget_type,
                                                          energymodels2$NEE_low_high,
                                                          energymodels2$Site_proxy),
                                                  FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(m_energymodels) <- c("Activity_budget_type", "Torpor_use", "Site_proxy",  
                           "Min_kJ_day", "kJ_day", "Max_kJ_day")
m_energymodels$no <- seq(1:length(m_energymodels$Max_kJ_day))

## use for just viewing activity differences, with all thermo and NEE variation incorporated
m_energymodels2 <- as.data.frame(as.list(aggregate(energymodels2$kJ_day,
                                                   by=list(energymodels2$Activity_budget_type,
                                                           energymodels2$Site_proxy),
                                                   FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(m_energymodels2) <- c("Activity_budget_type", "Site_proxy",
                            "Min_kJ_day", "kJ_day", "Max_kJ_day")
m_energymodels2$no <- seq(1:length(m_energymodels2$Max_kJ_day))
m_energymodels2

m_energymodels2$Site_proxy2 <- m_energymodels2$Site_proxy
levels(m_energymodels2$Site_proxy2) <- c("Aa", "Bb", "Cc", "Dd")

#### Jul 2017 - Dec 2017 ####
## Activity modeled - melt dataframe
m_act <- melt(act_models, id.vars = c("Cost_scenario", "Activity_budget_type", 
                                      "HMR_scenario", "FLMR_scenario", "RMR_scenario"), measure.vars = "ACT_kJ_day")
names(m_act)[names(m_act) == 'value'] <- 'Activity_kJ_daytime'
m_act$Cost_scenario <- factor(m_act$Cost_scenario, as.character(unique(m_act$Cost_scenario)))
m_act$Activity_budget_type <- factor(m_act$Activity_budget_type, levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))

m_dee <- melt(dee_act, id.vars = c("Cost_scenario", "Activity_budget_type", 
                                      "DLW_scenario"), measure.vars = "DEE_kJ_day")
m_dee$Cost_scenario <- factor(m_dee$Cost_scenario, as.character(unique(m_dee$Cost_scenario)))

#### Old ####
## With BMR variation
## use for just viewing activity differences, with all thermo and NEE variation incorporated
m_energymodels3 <- as.data.frame(as.list(aggregate(energymodels3$kJ_adjBMR_day,
                                                   by=list(energymodels3$Activity_budget_type,
                                                           energymodels3$Site_proxy),
                                                   FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(m_energymodels3) <- c("Activity_budget_type", "Site_proxy",
                            "Min_kJ_day", "kJ_day", "Max_kJ_day")
m_energymodels3$no <- seq(1:length(m_energymodels3$Max_kJ_day))
m_energymodels3

m_energymodels3$Site_proxy2 <- m_energymodels3$Site_proxy
levels(m_energymodels3$Site_proxy2) <- c("Aa", "Bb", "Cc", "Dd")

#### May 2018 ####
## With activity variation included, and in May with thermo_adjBMR substituted from hovering costs
m_energymodels_may <- as.data.frame(as.list(aggregate(energymodels_may$kJ_adjBMR_day,
                                                       by=list(energymodels_may$Activity_budget_type,
                                                               energymodels_may$BMR_assump,
                                                               energymodels_may$Thermoreg_scenario,
                                                               energymodels_may$Site_proxy),
                                                       FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(m_energymodels_may) <- c("Activity_budget_type", "BMR_category", "Thermoreg_scenario", "Site_proxy",
                                "Min_kJ_day", "kJ_day", "Max_kJ_day")
head(m_energymodels_may)

m_energymodels_may$Activity_budget_type <- factor(m_energymodels_may$Activity_budget_type,
                                                  levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels_may$Activity_bmr_thermo <- paste(m_energymodels_may$Activity_budget_type, 
                                                m_energymodels_may$BMR_category, m_energymodels_may$Thermoreg_scenario, sep= "_")

## With hovering thermo substitution
m_energymodels_may2 <- as.data.frame(as.list(aggregate(energymodels_may$kJ_adjBMR_day_HovThermo_adj,
                                                   by=list(energymodels_may$Activity_budget_type,
                                                           energymodels_may$BMR_assump,
                                                           energymodels_may$Thermoreg_scenario,
                                                           energymodels_may$Site_proxy),
                                                   FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(m_energymodels_may2) <- c("Activity_budget_type", "BMR_category", "Thermoreg_scenario", "Site_proxy",
                            "Min_kJ_day", "kJ_day", "Max_kJ_day")
head(m_energymodels_may2)

m_energymodels_may2$Activity_budget_type <- factor(m_energymodels_may2$Activity_budget_type,
                                               levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels_may2$Activity_bmr_thermo <- paste(m_energymodels_may2$Activity_budget_type, 
                                             m_energymodels_may$BMR_category, m_energymodels_may$Thermoreg_scenario, sep= "_")

## With extremely high activity budget model included
## use for just viewing activity differences, with all thermo and NEE variation incorporated
levels(energymodels4$Thermoreg_scenario)[match("Rand_cost_median",levels(energymodels4$Thermoreg_scenario))] <- "random"
levels(energymodels4$Thermoreg_scenario)[match("Min_cost",levels(energymodels4$Thermoreg_scenario))] <- "minimum"
levels(energymodels4$Thermoreg_scenario)[match("Max_cost",levels(energymodels4$Thermoreg_scenario))] <- "maximum"

m_energymodels4 <- as.data.frame(as.list(aggregate(energymodels4$kJ_adjBMR_day,
                                                   by=list(energymodels4$Activity_budget_type,
                                                           energymodels4$BMR_assump,
                                                           energymodels4$Thermoreg_scenario,
                                                           energymodels4$Site_proxy),
                                                   FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(m_energymodels4) <- c("Activity_budget_type", "BMR_category", "Thermoreg_scenario", "Site_proxy",
                            "Min_kJ_day", "kJ_day", "Max_kJ_day")
m_energymodels4

m_energymodels4$Activity_budget_type <- factor(m_energymodels4$Activity_budget_type,
                                    levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels4$Activity_bmr_thermo <- paste(m_energymodels4$Activity_budget_type, 
                                             m_energymodels4$BMR_category, m_energymodels4$Thermoreg_scenario, sep= "_")

## Without aggregating by BMR category
m_energymodels5 <- as.data.frame(as.list(aggregate(energymodels4$kJ_adjBMR_day,
                                                   by=list(energymodels4$Activity_budget_type,
                                                           energymodels4$Site_proxy),
                                                   FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(m_energymodels5) <- c("Activity_budget_type", "Site_proxy",
                            "Min_kJ_day", "kJ_day", "Max_kJ_day")
m_energymodels5$Activity_budget_type <- factor(m_energymodels5$Activity_budget_type,
                                      levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))

#### Trying out a NEE column for just non-torpid birds
energymodels4$Sitedate_Thermo_Activity <- paste(energymodels4$Scenario_ID, energymodels4$BMR_assump, sep="_")

### NEW - Jan 2018, stacked bar for energy budget
energymodels6 <- energymodels4[energymodels4$BMR_assump != "BMR_min" & energymodels4$BMR_assump != "BMR_max" &
                                 energymodels4$Thermoreg_scenario != "Rand_cost_min" & energymodels4$Thermoreg_scenario != "Rand_cost_max",]
energymodels6$Site_date <- paste(energymodels6$Site, energymodels6$Day, "0", energymodels6$Month, sep="")
energymodels6$Thermoreg_scenario <- paste("T", energymodels6$Thermoreg_scenario, sep="")
energymodels6$Thermo_NEE <- paste(energymodels6$Thermoreg_scenario, energymodels6$NEE_low_high, sep="_")
energymodels6$Activity_budget_type <- factor(energymodels6$Activity_budget_type,
                                             levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels_stack <- melt(energymodels6, 
                             id.vars=c("kJ_adjBMR_day","Activity_budget_type", 
                                       "Site_date", "Thermoreg_scenario"), 
                             measure.vars = c("NEE_addon_noTorpor", "NEE_high_torpor", "Act_kJ_day", "Thermo_adj_kJ_day"))
m_energymodels_stack$Activity_budget_type <- factor(m_energymodels_stack$Activity_budget_type,
                                             levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels_stack$Thermoreg_scenario <- factor(m_energymodels_stack$Thermoreg_scenario,
                                                    levels= c("Tminimum", "Trandom", "Tmaximum"))

### Stacked bar for energy budget, with activity variability
energymodelsmayv2 <- energymodels_may[energymodels_may$BMR_assump != "BMR_min" & energymodels_may$BMR_assump != "BMR_max" &
                                 energymodels_may$Thermoreg_scenario != "Rand_cost_min" & energymodels_may$Thermoreg_scenario != "Rand_cost_max",]
energymodelsmayv2$Site_date <- paste(energymodelsmayv2$Site, energymodelsmayv2$Day, "0", energymodelsmayv2$Month, sep="")
energymodelsmayv2$Thermoreg_scenario <- paste("T", energymodelsmayv2$Thermoreg_scenario, sep="")
energymodelsmayv2$Thermo_NEE <- paste(energymodelsmayv2$Thermoreg_scenario, energymodelsmayv2$NEE_low_high, sep="_")
energymodelsmayv2$Activity_budget_type <- factor(energymodelsmayv2$Activity_budget_type,
                                             levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels_stack2v <- melt(energymodelsmayv2, 
                             id.vars=c("kJ_adjBMR_day_HovThermo_adj", "Activity_budget_type", 
                                       "Site_date", "Thermoreg_scenario"), 
                             measure.vars = c("NEE_addon_noTorpor", "NEE_high_torpor", "Act_kJ_day_minusThermo", "Thermo_adj_kJ_day"))

m_energymodels_stack_minv <- melt(energymodelsmayv2, 
                                 id.vars=c("kJ_min_day_HovThermo_adj", "Activity_budget_type", 
                                           "Site_date", "Thermoreg_scenario"), 
                                 measure.vars = c("NEE_addon_noTorpor", "NEE_high_torpor", "ACT_min_kJ_daytime_minusThermo", "Thermo_adj_kJ_day"))

m_energymodels_stack_maxv <- melt(energymodelsmayv2, 
                                 id.vars=c("kJ_max_day_HovThermo_adj", "Activity_budget_type", 
                                           "Site_date", "Thermoreg_scenario"), 
                                 measure.vars = c("NEE_addon_noTorpor", "NEE_high_torpor", "ACT_max_kJ_daytime_minusThermo", "Thermo_adj_kJ_day"))

m_energymodels_stack2v$Activity_budget_type <- factor(m_energymodels_stack2v$Activity_budget_type,
                                                    levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels_stack2v$Thermoreg_scenario <- factor(m_energymodels_stack2v$Thermoreg_scenario,
                                                  levels= c("TMinimum", "TRandom", "TMaximum"))

m_energymodels_stack_minv$Activity_budget_type <- factor(m_energymodels_stack2v$Activity_budget_type,
                                                     levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels_stack_minv$Thermoreg_scenario <- factor(m_energymodels_stack2v$Thermoreg_scenario,
                                                   levels= c("TMinimum", "TRandom", "TMaximum"))

m_energymodels_stack_maxv$Activity_budget_type <- factor(m_energymodels_stack2v$Activity_budget_type,
                                                     levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels_stack_maxv$Thermoreg_scenario <- factor(m_energymodels_stack2v$Thermoreg_scenario,
                                                   levels= c("TMinimum", "TRandom", "TMaximum"))

activitymodels_24h <- energymodels_may[energymodels_may$BMR_assump != "BMR_min" & energymodels_may$BMR_assump != "BMR_max" &
                                       energymodels_may$Thermoreg_scenario == "Random",]
activitymodels_24h$Site_date <- paste(activitymodels_24h$Site, activitymodels_24h$Day, "0", activitymodels_24h$Month, sep="")
activitymodels_24h$Activity_budget_type <- factor(activitymodels_24h$Activity_budget_type,
                                                levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_activity_stack <- melt(activitymodels_24h, 
                             id.vars=c("Activity_budget_type", 
                                       "Site_date", "Thermoreg_scenario"), 
                             measure.vars = c("kJ_min_day", "kJ_adjBMR_day", "kJ_max_day"))
m_activity_stack$Activity_budget_type <- factor(m_activity_stack$Activity_budget_type,
                                                  levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))

## Getting percentages for the contribution of each component to the activity budget
m_energymodels_stack2$proportion <- (m_energymodels_stack2$value/m_energymodels_stack2$kJ_adjBMR_day)*100

#Summarize the percentages
percent_full_model <- as.data.frame(as.list(aggregate(m_energymodels_stack2$proportion,
                                by=list(m_energymodels_stack2$variable,
                                        m_energymodels_stack2$Thermoreg_scenario),
                                FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(percent_full_model) <- c("Model_component", "Thermoreg_scenario",  
                           "Min_kJ_24h", "Mean_kJ_24h", "Max_kJ_24h")


#### plots ####
#### DLW Validation plots ####
# Enrichment vs. DLW dose (g)
ggplot(valida_A, aes(DLW_dose_g, O_18_Enrichment_ppm, col=Treatment)) + geom_point(size=3, alpha=0.9) + my_theme +
  scale_color_manual(values = c("black", "grey70")) + xlab("DLW dose (g)") + ylab(bquote(~O^18~ 'Enrichment (ppm)')) +
  geom_smooth(method='lm') + theme(legend.key.height=unit(3, 'lines')) + ylim(0,6000)

# Initial enrichment vs equilibration time
ggplot(valida_B, aes(Eqb_time_min, Initial_enrichment_ppm_per_mg)) + geom_point(size=3, alpha=0.9) + my_theme +
  xlab("Equilibration time (min)") + ylab("Initial enrichment (ppm/mg)") +
  geom_smooth(method='lm') + ylim(0,30)

# CO2 production vs. DLW dose (g)
ggplot(valida_C, aes(DLW_dose_g, CO2_production_mL_h)) + geom_point(size=3, alpha=0.9) + my_theme +
  xlab("DLW dose (g)") + ylab(bquote(~CO[2]~ 'production (mL/hr)')) +
  geom_smooth(method='lm') + theme(legend.key.height=unit(3, 'lines')) + ylim(0,80)

#### Activity plots ####
names(percentEB) <- c("Measure", "Activity", "Nighttime energy", "Thermoregulation \n and BMR")
m.EB_percent <- melt(percentEB, id.vars="Measure", 
                     measure.vars=c("Activity", "Nighttime energy", "Thermoregulation \n and BMR"))
ggplot(m.EB_percent, aes(variable, value)) + geom_point(aes(col=Measure), size=4) + my_theme +
  scale_color_manual(values = c("brown1", "black", "cornflowerblue")) +
  theme(axis.text.x = element_text(size=25, vjust=0.5), legend.key.height = unit(3, 'lines')) + 
  xlab("\n Energy budget component") + ylab("Percent of budget \n")

## Trying stacked bar plots for breaking down energy budget, just one site+date at a time
pl_vSC0207 <- ggplot(m_energymodels_stack2v[m_energymodels_stack2v$Site_date=="SC207",], aes(Thermoreg_scenario, y=value, fill=variable)) + 
  facet_grid(~Activity_budget_type, scales='free_x') +
  geom_bar(stat="identity") +
  scale_fill_manual(labels = c("Nighttime no torpor", "Nighttime with torpor", "Daytime activity", "Thermoregulation + BMR"),
                     values = c("#B47ED5", "lavender", "red", "dark blue")) +
  geom_text(aes(x=Thermoreg_scenario, y =kJ_adjBMR_day_HovThermo_adj, label=kJ_adjBMR_day_HovThermo_adj), size=5) +
  xlab("Thermoregulatory scenarios") +
  ylab("kiloJoules per day\n") +
  ggtitle("Sonoita Creek pre-monsoon") +
  my_theme + theme(axis.text.x = element_text(angle=30, size=15, hjust=0.5, vjust=0.5),
                   legend.key.height=unit(3, 'lines'), plot.title = element_text(hjust=0.5)) + 
  guides(fill = guide_legend(title="Energy budget \n component"))

pl_SC0207 <- ggplot(m_energymodels_stack2[m_energymodels_stack2$Site_date=="SC207",], aes(Thermoreg_scenario, y=value, fill=variable)) + 
  facet_grid(~Activity_budget_type, scales='free_x') +
  geom_bar(stat="identity") +
  scale_fill_manual(labels = c("Nighttime no torpor", "Nighttime with torpor", "Daytime activity", "Thermoregulation + BMR"),
                    values = c("#B47ED5", "lavender", "red", "dark blue")) +
  geom_text(aes(x=Thermoreg_scenario, y =kJ_adjBMR_day, label=kJ_adjBMR_day), size=5) +
  xlab("Thermoregulatory scenarios") +
  ylab("kiloJoules per day\n") +
  ggtitle("Sonoita Creek pre-monsoon") +
  my_theme + theme(axis.text.x = element_text(angle=30, size=15, hjust=0.5, vjust=0.5),
                   legend.key.height=unit(3, 'lines'), plot.title = element_text(hjust=0.5)) + 
  guides(fill = guide_legend(title="Energy budget \n component"))

grid.arrange(pl_SC0207, pl_vSC0207)

## Trying stacked bar plots for breaking down energy budget, with mean variable activity, for Harshaw 1306
pl_HC1306 <- ggplot() + 
  geom_bar(data=m_energymodels_stack2[m_energymodels_stack2$Site_date=="HC1306",], aes(Thermoreg_scenario, y=value, fill=variable),
           stat="identity") +
  scale_fill_manual(labels = c("Nighttime less torpor", "Nighttime full torpor", "Mean Daytime activity", "Thermoregulation + BMR"),
                    values = c("#B47ED5", "lavender", "red", "dark blue")) +
  #geom_boxplot(data=m_dee, aes(Cost_scenario, value)) +
  #geom_point(data=m_energymodels_stack2[m_energymodels_stack2$Site_date=="HC1306",], aes(Thermoreg_scenario, y=kJ_adjBMR_day)) +
  facet_grid(~Activity_budget_type, scales='free_x') +
  geom_text(data=m_energymodels_stack2[m_energymodels_stack2$Site_date=="HC1306",], 
            aes(x=Thermoreg_scenario, y =kJ_adjBMR_day, label=kJ_adjBMR_day), size=5) +
  xlab("Thermoregulatory scenarios") +
  ylab("kiloJoules per day\n") +
  ggtitle("Harshaw Creek pre-monsoon") + ylim(0,45) +
  my_theme + theme(axis.text.x = element_text(angle=90, size=15, hjust=0.5),
                   legend.key.height=unit(3, 'lines'), plot.title = element_text(hjust=0.5)) + 
  guides(fill = guide_legend(title="Energy budget \n component"))

pl_vHC1306 <- ggplot() + 
  geom_bar(data=m_energymodels_stack2v[m_energymodels_stack2v$Site_date=="HC1306",], aes(Thermoreg_scenario, y=value, fill=variable),
           stat="identity") +
  scale_fill_manual(labels = c("Nighttime less torpor", "Nighttime full torpor", "Mean Daytime activity", "Thermoregulation + BMR"),
                    values = c("#B47ED5", "lavender", "red", "dark blue")) +
  #geom_boxplot(data=m_dee, aes(Cost_scenario, value)) +
  #geom_point(data=m_energymodels_stack2v[m_energymodels_stack2v$Site_date=="HC1306",], aes(Thermoreg_scenario, y=kJ_adjBMR_day_HovThermo_adj)) +
  facet_grid(~Activity_budget_type, scales='free_x') +
  geom_text(data=m_energymodels_stack2v[m_energymodels_stack2v$Site_date=="HC1306",], 
            aes(x=Thermoreg_scenario, y =kJ_adjBMR_day_HovThermo_adj, label=kJ_adjBMR_day_HovThermo_adj), size=5) +
  xlab("Thermoregulatory scenarios") +
  ylab("kiloJoules per day\n") +
  ggtitle("Harshaw Creek pre-monsoon") + ylim(0,45) +
  my_theme + theme(axis.text.x = element_text(angle=90, size=15, hjust=0.5),
                   legend.key.height=unit(3, 'lines'), plot.title = element_text(hjust=0.5)) + 
  guides(fill = guide_legend(title="Energy budget \n component"))

grid.arrange(pl_vSC0207, pl_vHC1306)

## Trying stacked bar plots for breaking down MIN energy budget, with mean variable activity, for Harshaw 1306
ggplot() + 
  geom_bar(data=m_energymodels_stack_min[m_energymodels_stack_min$Site_date=="HC1306",], aes(Thermoreg_scenario, y=value, fill=variable),
           stat="identity") +
  scale_fill_manual(labels = c("Nighttime less torpor", "Nighttime full torpor", "Min Daytime activity", "Thermoregulation + BMR"),
                    values = c("#B47ED5", "lavender", "red", "dark blue")) +
  geom_boxplot(data=m_dee, aes(Cost_scenario, value)) +
  geom_point(data=m_energymodels_stack_min[m_energymodels_stack_min$Site_date=="HC1306",], aes(Thermoreg_scenario, y=kJ_min_day)) +
  facet_grid(~Activity_budget_type, scales='free_x') +
  xlab("Thermoregulatory scenarios") +
  ylab("kiloJoules per day\n") +
  ggtitle("Harshaw Creek pre-monsoon") + ylim(0,45) +
  my_theme + theme(axis.text.x = element_text(angle=90, size=15, hjust=0.5),
                   legend.key.height=unit(3, 'lines'), plot.title = element_text(hjust=0.5)) + 
  guides(fill = guide_legend(title="Energy budget \n component"))

## Trying stacked bar plots for breaking down MAX energy budget, with mean variable activity, for Harshaw 1306
ggplot() + 
  geom_bar(data=m_energymodels_stack_max[m_energymodels_stack_max$Site_date=="HC1306",], aes(Thermoreg_scenario, y=value, fill=variable),
           stat="identity") +
  scale_fill_manual(labels = c("Nighttime less torpor", "Nighttime full torpor", "Max Daytime activity", "Thermoregulation + BMR"),
                    values = c("#B47ED5", "lavender", "red", "dark blue")) +
  geom_boxplot(data=m_dee, aes(Cost_scenario, value)) +
  geom_point(data=m_energymodels_stack_max[m_energymodels_stack_max$Site_date=="HC1306",], aes(Thermoreg_scenario, y=kJ_max_day)) +
  facet_grid(~Activity_budget_type, scales='free_x') +
  xlab("Thermoregulatory scenarios") +
  ylab("kiloJoules per day\n") +
  ggtitle("Harshaw Creek pre-monsoon") + ylim(0,45) +
  my_theme + theme(axis.text.x = element_text(angle=90, size=15, hjust=0.5),
                   legend.key.height=unit(3, 'lines'), plot.title = element_text(hjust=0.5)) + 
  guides(fill = guide_legend(title="Energy budget \n component"))

## Variable activity, DEE, for Harshaw 1306
p.act <- ggplot() + 
  geom_boxplot(data=m_activity_stack[m_activity_stack$Site_date=="HC1306",], aes(variable, y=value, fill=variable)) +
  #geom_bar(data=m_activity_stack[m_activity_stack$Site_date=="HC1306",], aes(variable, y=value, fill=variable),
   #        stat="identity") +
  #scale_fill_manual(labels = c("Low", "Mean", "High"),
                    #values = c("grey70", "grey50", "grey40")) +
                    #values = c("palevioletred1", "orangered1", "red3")) +
  #scale_x_discrete(labels=c("Low\n", "Mean\n", "High\n")) +
  #geom_boxplot(data=m_dee, aes(Cost_scenario, value)) +
  facet_grid(~Activity_budget_type, scales='free_x') +
  xlab("Activity costs") +
  ylab("kiloJoules per day") +
  ggtitle("Harshaw Creek pre-monsoon") + ylim(0,45) +
  my_theme + theme(axis.text.x = element_text(size=15),
                   legend.key.height=unit(3, 'lines'), plot.title = element_text(hjust=0.5),
                   panel.grid.major.y = element_line( size=.1, color="grey" ),
                   strip.background = element_blank(),
                   strip.text = element_blank()) + 
  guides(fill = guide_legend(title="Activity costs \n per unit time \n"))

p.act <- ggplot(data=m_activity_stack, aes(x=Activity_budget_type, y=value)) + 
  geom_boxplot(aes(group=Activity_budget_type)) +
  #geom_bar(data=m_activity_stack[m_activity_stack$Site_date=="HC1306",], aes(variable, y=value, fill=variable),
  #        stat="identity") +
  #scale_fill_manual(labels = c("Low", "Mean", "High"),
  #values = c("grey70", "grey50", "grey40")) +
  #values = c("palevioletred1", "orangered1", "red3")) +
  #scale_x_discrete(labels=c("Low\n", "Mean\n", "High\n")) +
  #geom_boxplot(data=m_dee, aes(Cost_scenario, value)) +
  #facet_grid(~Activity_budget_type, scales='free_x') +
  xlab("Activity time scenario") +
  ylab("kiloJoules per day") +
  ggtitle("Individual variability in \nactivity costs") + ylim(0,45) +
  my_theme + theme(axis.text.x = element_text(size=20, angle=30, vjust=0.5),
                   legend.key.height=unit(3, 'lines'), plot.title = element_text(hjust=0.5),
                   panel.grid.major.y = element_line(size=.1, color="grey" ))

## DEE plot
p.dlw <- ggplot() +
  geom_boxplot(data=dlw_bblh, aes(Site_proxy, kJ_day)) +
  scale_x_discrete(breaks=c('A','B','C','D'),
                   labels=c("Harshaw Pre", "Harshaw Post", "Sonoita Pre", "Sonoita Post")) +
  xlab("Site and monsoon status") +
  ylab("kiloJoules per day") +
  ylim(0,45) +
  ggtitle("Doubly Labeled Water \n Daily energy expenditure") +
  my_theme + theme(axis.text.x = element_text(size=20, angle=30, vjust=0.5),
                   legend.key.height=unit(3, 'lines'), plot.title = element_text(hjust=0.5),
                   panel.grid.major.y = element_line(size=.1, color="grey"))

plot_grid(p.dlw, p.act)

#Activity modeled
ggplot(m_act, aes(Cost_scenario, Activity_kJ_daytime)) + facet_grid(~Activity_budget_type, scales='free_x') + 
  my_theme + geom_point(aes(col=HMR_scenario, shape=FLMR_scenario), size=4) + 
  theme(axis.text.x=element_text(angle=90, size=10), legend.key.height=unit(3, 'lines')) +
  guides(colour = guide_legend(override.aes = list(size=4)), size=F)

ggplot() + 
  geom_point(data=m_act, aes(Cost_scenario, Activity_kJ_daytime, col=HMR_scenario, shape=FLMR_scenario), size=4) +
  geom_boxplot(data=m_dee, aes(Cost_scenario, value)) +
  facet_grid(~Activity_budget_type, scales='free_x') + my_theme +
  ylab("kiloJoules per day") +
  theme(axis.text.x=element_text(angle=90, size=10), legend.key.height=unit(3, 'lines')) +
  guides(colour = guide_legend(override.aes = list(size=4)), size=F)

#### Thermoreg variation ####
## With quantiles to select min and max thermo costs
ggplot(energymodels, aes(Thermoreg_scenario, Daytime_EE)) + 
  geom_point(aes(col=Site), size=3) +
  facet_grid(~Activity_budget_type) + theme_classic(base_size = 20) + 
  theme(panel.border = element_rect(colour = "black", fill=NA), 
        axis.text.x = element_text(angle=90, vjust=-0.05),
        strip.text.x = element_text(size = 15)) 

## Selecting just top highest and lowest thermo costs
#energymodels2$Thermoreg_scenario <- factor(energymodels2$Thermoreg_scenario, 
#                                          levels= c("Min_cost", "Rand_cost_min", "Rand_cost_median", 
#                                                   "Rand_cost_max", "Max_cost"))
ggplot(energymodels2, aes(Site_proxy, Daytime_EE_kJ)) + 
  geom_point(aes(col=Thermoreg_scenario), size=3, alpha=0.7) +  
  scale_colour_brewer(palette="Set1", guide = guide_legend(title = "Thermoregulatory \n model")) +
  facet_grid(~Activity_budget_type) + theme_classic(base_size = 25) + 
  scale_x_discrete(breaks=c('A','B','C','D'),
                   labels=c("Hawshaw Pre", "Harshaw Post", "Sonoita Pre", "Sonoita Post")) +
  theme(panel.border = element_rect(colour = "black", fill=NA), 
        axis.text.x = element_text(angle=45, margin=margin(30,0,0,0), hjust = 0.75),
        strip.text.x = element_text(size = 20), plot.title = element_text(hjust = 0.5, size=20),
        legend.key.size = unit(1.5, 'lines'), legend.title.align=0.5) + 
  xlab("Site and Monsoon status") + ylab("Daytime energy expenditure (kJ)") +
  ggtitle("Daytime activity costs Hover_Fly_Perch")

energymodels2$NEE_low_high <- as.factor(energymodels2$NEE_low_high)
levels(energymodels2$NEE_low_high) <- c("No torpor used", "Torpor used")

# Whole model with DLW, activity costs, and thermoregulatory costs
ggplot(NULL, aes(Site_proxy, kJ_day)) + my_theme +
  facet_grid(.~Activity_budget_type) +
  geom_boxplot(data=dlw_bblh, aes(Site_proxy, kJ_day), alpha=0.5, fill="light grey") +
  geom_point(data=energymodels2, aes(Site_proxy, kJ_day,
                                     col=Thermoreg_scenario, shape=NEE_low_high), size=5, alpha=0.5) +  
  scale_colour_brewer(palette="Set1", guide = guide_legend(title = "Thermoregulatory \n model")) +
  scale_shape_discrete(guide=guide_legend(title="Nighttime energy expenditure")) +
  scale_x_discrete(breaks=c('A','B','C','D'),
                   labels=c("Harshaw Pre", "Harshaw Post", "Sonoita Pre", "Sonoita Post")) +
  theme(axis.text.x = element_text(angle=45, margin=margin(30,0,0,0), hjust=0.75),
        #strip.text.x = element_text(size = 20), plot.title = element_text(hjust = 0.5, size=20),
        legend.key.size = unit(1.5, 'lines'), legend.title.align=0.5) + 
  xlab("Site and Monsoon status") + ylab("Daily energy expenditure (kJ)") +
  ggtitle("Daytime activity costs Hover_Fly_Perch")

# Whole model with DLW, activity costs, and thermoregulatory costs. Trying to make separate points and ranges for each scenario
ggplot(NULL, aes(Site_proxy, kJ_day)) + my_theme +
  geom_boxplot(data=dlw_bblh, aes(Site_proxy, kJ_day), alpha=0.5, fill="light grey") +
  geom_point(data=energymodels2, aes(Site_proxy, kJ_day,
                                     color=Activity_budget_type, shape=NEE_low_high), size=5, alpha=0.5) +  
  geom_linerange(data=energymodels2[energymodels2$Activity_budget_type=="15_15_70",], 
                 aes(Site_proxy, ymin= min(kJ_day), ymax=max(kJ_day),
                     color=Activity_budget_type, size=NEE_low_high), alpha=0.5, size=2) +  
  scale_colour_brewer(palette="Set1", guide = guide_legend(title = "Activity scenario")) +
  scale_shape_discrete(guide=guide_legend(title="Nighttime energy expenditure")) +
  scale_x_discrete(breaks=c('A','B','C','D'),
                   labels=c("Harshaw Pre", "Harshaw Post", "Sonoita Pre", "Sonoita Post")) +
  theme(axis.text.x = element_text(angle=45, margin=margin(30,0,0,0), hjust=0.75),
        strip.text.x = element_text(size = 20), plot.title = element_text(hjust = 0.5, size=20),
        legend.key.size = unit(1.5, 'lines'), legend.title.align=0.5) + 
  xlab("Site and Monsoon status") + ylab("Daily energy expenditure (kJ)") +
  ggtitle("Daytime activity costs Hover_Fly_Perch")

ggplot(energymodels2, aes(Site_proxy, kJ_day)) + my_theme +
  geom_point(aes(Site_proxy, kJ_day,
                 color=Activity_budget_type, shape=NEE_low_high), size=5, alpha=0.5) +  
  geom_linerange(data=energymodels2[energymodels2$Activity_budget_type=="15_15_70",], 
                 aes(Site_proxy, ymin= min(kJ_day), ymax=max(kJ_day),
                     color=Activity_budget_type, size=NEE_low_high), alpha=0.5, size=2) +  
  geom_linerange(data=energymodels2[energymodels2$Activity_budget_type=="15_15_70",], 
                 aes(Site_proxy, ymin= min(kJ_day), ymax=max(kJ_day),
                     color=Activity_budget_type, size=NEE_low_high), alpha=0.5, size=2) +  
  scale_colour_brewer(palette="Set1", guide = guide_legend(title = "Activity scenario")) +
  scale_shape_discrete(guide=guide_legend(title="Nighttime energy expenditure")) +
  scale_x_discrete(breaks=c('A','B','C','D'),
                   labels=c("Harshaw Pre", "Harshaw Post", "Sonoita Pre", "Sonoita Post")) +
  theme(axis.text.x = element_text(angle=45, margin=margin(30,0,0,0), hjust=0.75),
        strip.text.x = element_text(size = 20), plot.title = element_text(hjust = 0.5, size=20),
        legend.key.size = unit(1.5, 'lines'), legend.title.align=0.5) + 
  xlab("Site and Monsoon status") + ylab("Daily energy expenditure (kJ)") +
  ggtitle("Daytime activity costs Hover_Fly_Perch")

## Good plot with adjacent dlw and model vals
ggplot(NULL, aes(Site_proxy, kJ_day)) + my_theme +
  geom_boxplot(data=dlw_bblh,aes(Site_proxy, kJ_day), alpha=0.5, fill="grey90",  width = 0.5) + 
  geom_linerange(data=m_energymodels2, aes(x=Site_proxy2, ymin = Min_kJ_day, ymax = Max_kJ_day,
                                           color = Activity_budget_type), 
                 position=position_dodge(width=0.4), size = 5, alpha = 0.6) + 
  geom_point(data=m_energymodels2, aes(Site_proxy2, kJ_day, color = Activity_budget_type),
             position=position_dodge(width=0.4), size=5) +
  scale_color_manual(values = c('olivedrab3', 'orangered2', 'slateblue4')) +
  scale_x_discrete(breaks=c('A', 'Aa', 'B', 'Bb', 'C', 'Cc', 'D', 'Dd'), 
                   labels=c("Harshaw Pre", " ", "Harshaw Post", " ", "Sonoita Pre",
                            " ", "Sonoita Post", " ")) +
  ylim(9, 41) + my_theme + theme(legend.key.size = unit(2, 'lines'), 
                                 axis.ticks = element_blank(), axis.text.x = element_text(hjust=-0.1)) + 
  xlab("Site and monsoon status") + ylab("Daily Energy Expenditure (kJ)")

#### Final plot for IUPS 2017 poster ####
## Good plot with adjacent dlw and model vals
## and including variation in BMR, including suggestions from Simone
ggplot(NULL, aes(Site_proxy, kJ_day)) +
  geom_boxplot(data=dlw_bblh,aes(Site_proxy, kJ_day), fill="grey90",  width = 0.5, lwd=1) + 
  geom_linerange(data=m_energymodels5, 
                 aes(x=Site_proxy, ymin = Min_kJ_day, ymax = Max_kJ_day,
                                           color = Activity_budget_type), 
                 position=position_dodge(width=0.4), size = 4, alpha=0.2) + 
  geom_linerange(data=m_energymodels4[m_energymodels4$BMR_category=="BMR_mean",],
                 aes(x=Site_proxy, ymin = Min_kJ_day, ymax = Max_kJ_day, 
                                           color = Activity_budget_type), 
                 position=position_dodge(width=0.4), 
                 size = 6, alpha = 0.2) + 
  geom_point(data=m_energymodels4[m_energymodels4$BMR_category=="BMR_mean",],
             aes(Site_proxy, kJ_day, color = Activity_budget_type),
             position=position_dodge(width=0.4), size=4) +
  scale_color_manual(values = c('darkgreen', 'orangered2', 'slateblue4', 'violetred3'), 
                     guide = guide_legend(title = "Activity budget \n percent time \n hover_fly_perch")) +
  scale_x_discrete(breaks=c('A', 'B', 'C', 'D'), 
                   labels=c("Harshaw \n Pre", "Harshaw \n Post", "Sonoita \n Pre",
                            "Sonoita \n Post")) +
  ylim(9, 41) + my_theme + theme(legend.key.size = unit(4, 'lines'), 
                                 legend.key.height = unit(4, 'lines'),
                                 legend.margin = margin(t=0.5, unit='cm'),
                                 legend.title.align = 0.5,
                                 legend.text.align = 0.5,
                                 legend.text=element_text(size=32),
                                 axis.ticks.x = element_blank(),
                                 axis.ticks.y = element_line(size=2),
                                 axis.text = element_text(color = 'black', size=32, hjust=0.5),
                                 axis.title = element_text(face='bold'),
                                 axis.title.y = element_text(hjust=0.5)) +
  xlab("Site and monsoon status") + ylab("Daily \n energy expenditure (kJ)")

## Plotting just boxplot but keeping scaling of graph by setting alpha = 0 on the other layers
ggplot(NULL, aes(Site_proxy, kJ_day)) +
  geom_boxplot(data=dlw_bblh,aes(Site_proxy, kJ_day), fill="grey90",  width = 0.5, lwd=1) + 
  geom_linerange(data=m_energymodels5, 
                 aes(x=Site_proxy, ymin = Min_kJ_day, ymax = Max_kJ_day,
                     color = Activity_budget_type), 
                 position=position_dodge(width=0.4), size = 3, alpha=0) + 
  scale_color_manual(values = c('olivedrab3', 'orangered2', 'slateblue4', 'violet'), 
                     guide = guide_legend(title = "Activity \n budget type")) +
  scale_x_discrete(breaks=c('A', 'B', 'C', 'D'), 
                   labels=c("Harshaw Pre", "Harshaw Post", "Sonoita Pre",
                            "Sonoita Post")) +
  ylim(9, 41) + my_theme + theme(legend.key.size = unit(2, 'lines'), 
                                 legend.key.height = unit(3, 'lines'),
                                 axis.ticks = element_blank(), 
                                 legend.title.align = 0.5) +
  xlab("Site and monsoon status") + ylab("24-hour Energy Expenditure (kJ)")

## Just model points
model_plot <- ggplot(data=m_energymodels2, aes(Site_proxy, kJ_day)) + my_theme +
  geom_linerange(aes(x=Site_proxy, ymin = Min_kJ_day, ymax = Max_kJ_day,
                     color = Activity_budget_type), 
                 position=position_dodge(width=0.4), size = 3, alpha = 0.5) + 
  geom_point(data=m_energymodels2, aes(Site_proxy2, kJ_day, color = Activity_budget_type), size=3,
             position=position_dodge(width=0.4)) +
  scale_x_discrete(breaks=c('A', 'B', 'C', 'D'), 
                   labels=c("Harshaw Pre", "Harshaw Post", "Sonoita Pre", "Sonoita Post")) +
  ylim(9, 41) + my_theme + theme(legend.key.size = unit(2, 'lines'), legend.position = "bottom") + 
  xlab("Site and monsoon status") + ylab("Daily Energy Expenditure (kJ)")

## Just boxplots from DLW
dlw_plot <- ggplot(data=dlw_bblh,aes(Site_proxy, kJ_day)) + my_theme2 +
  geom_boxplot(alpha=0.5, fill="light grey") + 
  scale_x_discrete(breaks=c('A', 'B', 'C', 'D'), 
                   labels=c("Harshaw Pre", "Harshaw Post", "Sonoita Pre", "Sonoita Post")) +
  ylim(9, 41) + theme(legend.key.size = unit(2, 'lines')) + 
  xlab("Site and monsoon status") + 
  ylab("24-hour Energy Expenditure (kJ)")
dlw_plot
grid_arrange_shared_legend_hori(model_plot, dlw_plot)

### Trial
ggplot(NULL, aes(Site_proxy, kJ_day)) + my_theme +
  geom_boxplot(data=dlw_bblh,aes(Site_proxy, kJ_day), alpha=0.5, fill="light grey") + 
  scale_x_discrete(breaks=c('A','B','C','D'), labels=c("Harshaw Pre", "Harshaw Post", "Sonoita Pre", "Sonoita Post")) +
  geom_linerange(data=m_energymodels, aes(x=Site_proxy, ymin = Min_kJ_day, ymax = Max_kJ_day, color = Activity_budget_type, 
                                            linetype=Torpor_use), position=position_dodge(width=0.4), alpha = 0.5, size=2) +
  scale_linetype_manual(values=c("solid", "solid")) +
  geom_point(data=m_energymodels, aes(Site_proxy, kJ_day, color = Activity_budget_type, shape=Torpor_use),
             position=position_dodge(width=0.4), size=4) +
  scale_shape_manual(values=c(19, 2)) +
  my_theme + theme(legend.position = "bottom", legend.direction = "vertical", legend.key.size = unit(2, 'lines'))

ggplot(NULL, aes(Site_proxy, kJ_day)) + 
  geom_boxplot(data=dlw_bblh, aes(Site_proxy, kJ_day), alpha=0.5) +
  #geom_point(data=dlw_bblh, aes(Site_proxy, kJ_day), size=5, alpha=0.1) +
  geom_point(data=energymodels2, aes(Site_proxy, kJ_day, 
                                     col=Thermoreg_scenario, shape=NEE_low_high), size=3, alpha=0.7) +  
  scale_colour_brewer(palette="Set1", guide = guide_legend(title = "Thermoregulatory \n model")) +
  scale_shape_discrete(guide=guide_legend(title="Nighttime energy \n expenditure")) +
  facet_grid(.~Activity_budget_type) + theme_classic(base_size = 25) + 
  scale_x_discrete(breaks=c('A','B','C','D'),
                   labels=c("Harshaw Pre", "Harshaw Post", "Sonoita Pre", "Sonoita Post")) +
  theme(panel.border = element_rect(colour = "black", fill=NA), 
        axis.text.x = element_text(angle=45, margin=margin(30,0,0,0), hjust=0.75),
        strip.text.x = element_text(size = 20), plot.title = element_text(hjust = 0.5, size=20),
        legend.key.size = unit(1.5, 'lines'), legend.title.align=0.5) + 
  guides(fill=guide_legend(keywidth=0.1, keyheight =0.5, default.unit="inch")) +
  xlab("Site and Monsoon status") + ylab("Daily energy expenditure (kJ)") +
  ggtitle("Daytime activity costs Hover_Fly_Perch")

dlw_bblh
ggplot(NULL, aes(Site_proxy, Daytime_EE_kJ)) + 
  geom_boxplot(data=dlw_bblh, aes(Site_proxy, kJ_day), alpha=0.5) +
  geom_point(data=energymodels2, aes(col=Activity_budget_type), size=3, alpha=0.7) + 
    theme_classic(base_size = 25) + 
  scale_colour_brewer(palette="Set1") +
  scale_x_discrete(breaks=c('A','B','C','D'),
                   labels=c("HC Pre", "HC Post", "SC Pre", "SC Post")) +
  theme(panel.border = element_rect(colour = "black", fill=NA), 
        axis.text.x = element_text(angle=45, margin=margin(30,0,0,0)),
        strip.text.x = element_text(size = 20)) 

dlw_bblh$ind_band <- dlw_bblh$Band_no
dlw_bblh$ind_band[dlw_bblh$ind_band==""] <- NA
## Just DLW boxplots and points with recap individuals colored for Figure 2 (as of April 3, 2017)
ggplot(dlw_bblh, aes(Site_proxy, kJ_day)) + 
  geom_boxplot(alpha=0.5, fill="light grey") +
  geom_point(aes(col=Band_no, size=Band_no), alpha=0.9) + my_theme +
  geom_line(data=dlw_bblh[!is.na(dlw_bblh$ind_band),], aes(group=ind_band, col=ind_band), size=1) +
  scale_colour_manual(values=c("black", "red", "green", "purple")) +
  scale_size_manual(values=c(4, 6, 6, 6)) +
  scale_x_discrete(breaks=c('A','B','C','D'),
                   labels=c("Harshaw \n Pre", "Harshaw \n Post", "Sonoita \n Pre", "Sonoita \n Post")) +
  stat_summary(fun.data = give.n, geom = "text", hjust=-0.5, vjust=-1.8, size=9) +
  theme(legend.position = 'none', axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(size=2),
        axis.text = element_text(color = 'black', size=32, hjust=0.5),
        axis.title = element_text(face='bold'),
        axis.title.y = element_text(hjust=0.5)) +
  xlab("Site and monsoon status") + 
  ylab("Daily \n energy expenditure (kJ)")

df.list <- as.data.frame(x1 = energymodels$Thermoreg_mlO2_daytime,
                y1 = energymodels$Activity_cost_mlO2_daytime,
                z1 = energymodels$Daytime_EE)

plot3d(x = energymodels$Thermoreg_mlO2_daytime,
       y = energymodels$Activity_cost_mlO2_daytime,
       z = energymodels$Daytime_EE, type="s", col="red", xlab="Thermoreg", ylab="Activity", zlab="Daytime EE",
       size=2, radius=10, box=F)
quads3d(x=209.6:315.9, y=358.53:814.64,
        z=568.13:1130.54, col="purple")


x_vec = c(seq(-5, 4.9, 0.1))
x_matrix = matrix(c(x_vec), nrow = 100, ncol = 1)
y_matrix = matrix(c(x_vec), nrow = 1, ncol = 100)

data1 <- list(
  x = x_vec,
  y = x_vec,
  z = matrix(c(cos(x_matrix %*% y_matrix) + sin(x_matrix %*% y_matrix)), nrow = 100, ncol = 100),
  type = "surface")

layout <- list(
  title = "Waaaves in r",
  scene = list(bgcolor = "rgb(244, 244, 248)"))

response <- plot_ly(data1$x, data1$y, data1$z, type='surface')


df.list <- list(x = 1:100,
                y = 500:599,
                z = matrix(rnorm(10000), nrow = 100))

df.dataframe <- data.frame(x1 = 1:100,
                           y1 = 500:599,
                           z1 = sample(1:200, size = 100))


# Works fine
plot_ly(x = df.list$x1, y = df.list$y1, z = df.list$z1, type = "surface")


## Half-torus script
par(mar = c(2, 2, 2, 2))
par(mfrow = c(1, 1))
R <- 3
r <- 2
x <- seq(0, 2*pi,length.out=50)
y <- seq(0, pi,length.out=50)
M <- mesh(x, y)

alpha <- M$x
beta <- M$y


surf3D(x = energymodels$Thermoreg_mlO2_daytime,
       y = energymodels$Activity_cost_mlO2_daytime,
       z = energymodels$Daytime_EE,
       colkey=FALSE
       )

surf3D(x = 1,
       y = data.frame(c(energymodels$Thermoreg_mlO2_daytime, energymodels$Daytime_EE)),
       z = energymodels$Activity_cost_mlO2_daytime,
       colvar=x
       )

x2 <- energymodels$Thermoreg_mlO2_daytime
y2 <- energymodels$Daytime_EE
z2 <- energymodels$Activity_cost_mlO2_daytime

M2 <- mesh(x2,y2)
alpha2 <- M2$x2
beta2 <- M2$y2

surf3D(x = alpha2,
       y = beta2,
       z = r * sin(alpha),
       colkey=FALSE,
       bty="b2",
       main="Half of a Torus")
