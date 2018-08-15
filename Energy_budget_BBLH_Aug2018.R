## 3D surface plot of daily energy expenditure vs. activity and temperature

# Sign in to plotly - like Git
#library(plotly)
library(ggplot2)
#library(rgl)
library(dplyr) # Trying this out for stacking NEE with and without torpor
library(reshape2) # Trying this out for stacking NEE with and without torpor
library(gridExtra)
#library(cowplot) #for plot_grid function instead of grid.arrange
#library(tidyverse) #for the stacked bar plot- labeled as such in case you want to delete this later


setwd("C:\\Users\\nushi\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Tables")

## Read in files 
energymodels2 <- read.csv("Trial_EnergyBudget_models_act_thermo_redone.csv")
# CHECK #energymodels3 <- read.csv("Trial_EnergyBudget_models_act_thermo_Jul2017.csv") #includes BMR variation but not
#new activity budget scenario
## Modified Trial_EnergyBudget_models_act_thermo_Jul2017_2.csv slightly:
energymodels4 <- read.csv("Trial_EnergyBudget_models_act_thermo_Jan2018.csv") #incl BMR and new act budget scenario and torpor split
energymodels_jan <- read.csv("Jan_all_new_models.csv") #Includes min and max 24h cost by varying activity; per activity, thermo, NEE and BMR scenario
#Includes min and max 24h cost by varying activity; per activity, thermo, NEE and BMR scenario; and adjusting hovering for thermoregulatory substitution
energymodels_may <- read.csv("May_hover_Thermo_adj.csv") 
#act_models <- read.csv("Activity_modeled.csv") #Varying HMR, FLMR, RMR
dee_act <- read.csv("DEE_for_activity_models.csv")
percentEB <- read.csv("percentEB_May.csv")
dlw_bblh <- read.csv("DLW_summary.csv")

# Files for DLW validation plots for supplement
valida_A <- read.csv("Validation_Enrichment_dose_A.csv")
valida_B <- read.csv("Validation_enrichment_eqb_B.csv")
valida_C <- read.csv("Validation_CO2produc_dose_C.csv")

#### General functions ####
my_theme <- theme_classic(base_size = 32) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 25) + 
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

## Place plots side by side with shared legend- CHECK if needed
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


#### Aggregating NOT READING IN NOW - CHECK ####
dlw_summ <- as.data.frame(as.list(aggregate(dlw_bblh$kJ_day,
                                by=list(dlw_bblh$Site_monsoon),
                                FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))


#### Jul 2017 - Dec 2017 ####
## Activity modeled - melt dataframe -  CHECK
m_act <- melt(act_models, id.vars = c("Cost_scenario", "Activity_budget_type", 
                                      "HMR_scenario", "FLMR_scenario", "RMR_scenario"), measure.vars = "ACT_kJ_day")
names(m_act)[names(m_act) == 'value'] <- 'Activity_kJ_daytime'
m_act$Cost_scenario <- factor(m_act$Cost_scenario, as.character(unique(m_act$Cost_scenario)))
m_act$Activity_budget_type <- factor(m_act$Activity_budget_type, levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))

m_dee <- melt(dee_act, id.vars = c("Cost_scenario", "Activity_budget_type", 
                                      "DLW_scenario"), measure.vars = "DEE_kJ_day")
m_dee$Cost_scenario <- factor(m_dee$Cost_scenario, as.character(unique(m_dee$Cost_scenario)))


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

m_energymodels_act <- as.data.frame(as.list(aggregate(energymodels_may$kJ_adjBMR_day,
                                                      by=list(energymodels_may$Activity_budget_type,
                                                              energymodels_may$Site_proxy),
                                                      FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(m_energymodels_act) <- c("Activity_budget_type", "Site_proxy",
                               "Min_kJ_day", "kJ_day", "Max_kJ_day")
head(m_energymodels_act)

m_energymodels_therm <- as.data.frame(as.list(aggregate(energymodels_may$kJ_adjBMR_day,
                                                      by=list(energymodels_may$Activity_budget_type,
                                                              energymodels_may$Site_proxy,
                                                              energymodels_may$Thermoreg_scenario),
                                                      FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(m_energymodels_therm) <- c("Activity_budget_type", "Site_proxy", "Thermoreg_scenario",
                               "Min_kJ_day", "kJ_day", "Max_kJ_day")
head(m_energymodels_therm)

#### AUGUST 2018 ####
m_energymodels_aug_min <- as.data.frame(as.list(aggregate(energymodels_may$kJ_min_day_HovThermo_adj,
                                                      by=list(energymodels_may$Activity_budget_type,
                                                              energymodels_may$BMR_assump,
                                                              energymodels_may$Thermoreg_scenario,
                                                              energymodels_may$Site_proxy),
                                                      FUN = function(x) mn = min(x))))
names(m_energymodels_aug_min) <- c("Activity_budget_type", "BMR_category", "Thermoreg_scenario", "Site_proxy",
                               "Min_kJ_day")
head(m_energymodels_aug_min)

m_energymodels_aug_mean <- as.data.frame(as.list(aggregate(energymodels_may$kJ_adjBMR_day_HovThermo_adj,
                                                      by=list(energymodels_may$Activity_budget_type,
                                                              energymodels_may$BMR_assump,
                                                              energymodels_may$Thermoreg_scenario,
                                                              energymodels_may$Site_proxy),
                                                      FUN = function(x) mi = mean(x))))
names(m_energymodels_aug_mean) <- c("Activity_budget_type", "BMR_category", "Thermoreg_scenario", "Site_proxy",
                               "kJ_day")

m_energymodels_aug_max <- as.data.frame(as.list(aggregate(energymodels_may$kJ_max_day_HovThermo_adj,
                                                      by=list(energymodels_may$Activity_budget_type,
                                                              energymodels_may$BMR_assump,
                                                              energymodels_may$Thermoreg_scenario,
                                                              energymodels_may$Site_proxy),
                                                      FUN = function(x) mx = max(x))))
names(m_energymodels_aug_max) <- c("Activity_budget_type", "BMR_category", "Thermoreg_scenario", "Site_proxy",
                               "Max_kJ_day")

m_energymodels_aug <- merge(m_energymodels_aug_min,m_energymodels_aug_mean,
      by=c("Activity_budget_type", "BMR_category", "Thermoreg_scenario", "Site_proxy"))

m_energymodels_aug <- merge(m_energymodels_aug,m_energymodels_aug_max,
                            by=c("Activity_budget_type", "BMR_category", "Thermoreg_scenario", "Site_proxy"))


m_energymodels_aug$Activity_budget_type <- factor(m_energymodels_aug$Activity_budget_type,
                                                  levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels_aug$Activity_bmr_thermo <- paste(m_energymodels_aug$Activity_budget_type, 
                                                m_energymodels_aug$BMR_category, m_energymodels_aug$Thermoreg_scenario, sep= "_")


mm_energymodels_aug <- melt(m_energymodels_aug, 
                            id.vars = c("Activity_budget_type", "BMR_category", "Thermoreg_scenario", "Site_proxy"),
                            measure.vars = c("Min_kJ_day", "kJ_day", "Max_kJ_day"))
names(mm_energymodels_aug)[names(mm_energymodels_aug) == 'variable'] <- 'min_mean_max_kJ'
names(mm_energymodels_aug)[names(mm_energymodels_aug) == 'value'] <- 'kJ_day'

## Keeping BMR and thermo at average and only allowing unit activity costs to vary, per activity budget scenario
mm_energymodels_var_act<- mm_energymodels_aug[mm_energymodels_aug$BMR_category=="BMR_mean" & 
                                                mm_energymodels_aug$Thermoreg_scenario=="Random",]
m_energymodels_act <- as.data.frame(as.list(aggregate(mm_energymodels_var_act$kJ_day,
                                                      by=list(mm_energymodels_var_act$Activity_budget_type,
                                                              mm_energymodels_var_act$Site_proxy),
                                                      FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))

names(m_energymodels_act) <- c("Activity_budget_type", "Site_proxy",
                               "Min_kJ_day", "kJ_day", "Max_kJ_day")
head(m_energymodels_act)


## Allowing everything to vary, aggregate by thermoregulatory scenario and activity scenario
m_energymodels_therm <- as.data.frame(as.list(aggregate(mm_energymodels_aug$kJ_day,
                                                        by=list(mm_energymodels_aug$Activity_budget_type,
                                                                mm_energymodels_aug$Site_proxy,
                                                                mm_energymodels_aug$Thermoreg_scenario),
                                                        FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(m_energymodels_therm) <- c("Activity_budget_type", "Site_proxy", "Thermoreg_scenario",
                                 "Min_kJ_day", "kJ_day", "Max_kJ_day")
head(m_energymodels_therm)

## Allowing everything to vary, aggregate by thermoregulatory scenario and activity scenario
m_energymodels_aug_mean$Activity_budget_type <- factor(m_energymodels_aug_mean$Activity_budget_type,
                                                  levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels_avg_act <- as.data.frame(as.list(aggregate(m_energymodels_aug_mean$kJ_day,
                                                        by=list(m_energymodels_aug_mean$Activity_budget_type,
                                                                m_energymodels_aug_mean$Site_proxy,
                                                                m_energymodels_aug_mean$Thermoreg_scenario),
                                                        FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(m_energymodels_avg_act) <- c("Activity_budget_type", "Site_proxy", "Thermoreg_scenario",
                                 "Min_kJ_day", "kJ_day", "Max_kJ_day")
head(m_energymodels_avg_act)

### Stacked bar for energy budget, with activity variability
energymodelsaugv2 <- energymodels_may[energymodels_may$BMR_assump != "BMR_min" & energymodels_may$BMR_assump != "BMR_max" &
                                 energymodels_may$Thermoreg_scenario != "Rand_cost_min" & energymodels_may$Thermoreg_scenario != "Rand_cost_max",]
energymodelsaugv2$Site_date <- paste(energymodelsaugv2$Site, energymodelsaugv2$Day, "0", energymodelsaugv2$Month, sep="")
energymodelsaugv2$Thermoreg_scenario <- paste("T", energymodelsaugv2$Thermoreg_scenario, sep="")
energymodelsaugv2$Thermo_NEE <- paste(energymodelsaugv2$Thermoreg_scenario, energymodelsaugv2$NEE_low_high, sep="_")
energymodelsaugv2$Activity_budget_type <- factor(energymodelsaugv2$Activity_budget_type,
                                             levels= c("5_20_75", "15_15_70", "25_30_45", "40_40_20"))
m_energymodels_stack2v <- melt(energymodelsaugv2, 
                             id.vars=c("kJ_adjBMR_day_HovThermo_adj", "Activity_budget_type", 
                                       "Site_date", "Thermoreg_scenario"), 
                             measure.vars = c("NEE_addon_noTorpor", "NEE_high_torpor", "Act_kJ_day_minusThermo", "Thermo_adj_kJ_day"))

m_energymodels_stack_minv <- melt(energymodelsaugv2, 
                                 id.vars=c("kJ_min_day_HovThermo_adj", "Activity_budget_type", 
                                           "Site_date", "Thermoreg_scenario"), 
                                 measure.vars = c("NEE_addon_noTorpor", "NEE_high_torpor", "ACT_min_kJ_daytime_minusThermo", "Thermo_adj_kJ_day"))

m_energymodels_stack_maxv <- melt(energymodelsaugv2, 
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

activitymodels_24h <- energymodels_aug[energymodels_may$BMR_assump != "BMR_min" & energymodels_may$BMR_assump != "BMR_max" &
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
m_energymodels_stack2v$proportion <- (m_energymodels_stack2v$value/m_energymodels_stack2v$kJ_adjBMR_day)*100

#Summarize the percentages ## MAY 2018
percent_full_model <- as.data.frame(as.list(aggregate(m_energymodels_stack2v$proportion,
                                by=list(m_energymodels_stack2v$variable,
                                        m_energymodels_stack2v$Thermoreg_scenario),
                                FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(percent_full_model) <- c("Model_component", "Thermoreg_scenario",  
                           "Min_percentkJ_24h", "Mean_percentkJ_24h", "Max_percentkJ_24h")

#Summarize the percentages for SC 0207 ## MAY 2018
kJ_split_full_model <- as.data.frame(as.list(aggregate(m_energymodels_stack2v$value,
                                                      by=list(m_energymodels_stack2v$variable,
                                                              m_energymodels_stack2v$Thermoreg_scenario),
                                                      FUN = function(x) c(mi = min(x), mn = mean(x), mx = max(x)))))
names(kJ_split_full_model) <- c("Model_component", "Thermoreg_scenario",  
                               "Min_kJ_24h", "Mean_kJ_24h", "Max_kJ_24h")
write.csv(kJ_split_full_model,file="kJ_splitEB_May.csv")

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
names(percentEB) <- c("Measure", "Activity", "Nighttime energy", "Thermoregulation\n and BMR", "BMR_alone", "Thermoreg_alone", "total_ignore")
m.EB_percent <- melt(percentEB, id.vars="Measure", 
                     measure.vars=c("Activity", "Nighttime energy", "Thermoregulation\n and BMR"))
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
  geom_bar(data=m_energymodels_stack_minv[m_energymodels_stack_minv$Site_date=="HC1306",], aes(Thermoreg_scenario, y=value, fill=variable),
           stat="identity") +
  scale_fill_manual(labels = c("Nighttime less torpor", "Nighttime full torpor", "Min Daytime activity", "Thermoregulation + BMR"),
                    values = c("#B47ED5", "lavender", "red", "dark blue")) +
  geom_boxplot(data=m_dee, aes(Cost_scenario, value)) +
  geom_point(data=m_energymodels_stack_minv[m_energymodels_stack_minv$Site_date=="HC1306",], aes(Thermoreg_scenario, y=kJ_min_day_HovThermo_adj)) +
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

## Variable activity, DEE, for Harshaw 1306. Don't really work.
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

## Figure 4
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


## Figure 2a
dlw_bblh$ind_band <- dlw_bblh$Band_no
dlw_bblh$ind_band[dlw_bblh$ind_band==""] <- NA
## Just DLW boxplots and points with recap individuals colored for Figure 2 (as of April 3, 2017)
dlw_indiv <- ggplot(dlw_bblh, aes(Site_proxy, kJ_day)) + 
  geom_boxplot(alpha=0.5, fill="light grey") +
  geom_point(aes(col=Band_no, size=Band_no), alpha=0.9) + my_theme2 +
  geom_line(data=dlw_bblh[!is.na(dlw_bblh$ind_band),], aes(group=ind_band, col=ind_band), size=1) +
  scale_colour_manual(values=c("black", "red", "green", "purple")) +
  scale_size_manual(values=c(4, 6, 6, 6)) +
  scale_x_discrete(breaks=c('A','B','C','D'),
                   labels=c("Harshaw \n Dry", "Harshaw\nEarly-wet", "Sonoita \n Dry", "Sonoita \n Early-wet")) +
  stat_summary(fun.data = give.n, geom = "text", hjust=-0.5, vjust=-1.8, size=9) +
  theme(legend.position = 'none', axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(size=2),
        axis.text = element_text(color = 'black', hjust=0.5),
        axis.title = element_text(face='bold'),
        axis.title.y = element_text(hjust=0.5)) + ylim(9,41) +
  xlab("Site and season") + 
  ylab("Daily \n energy expenditure (kJ)")

## Figure 2b
## AUGUST 2018 plot adjusting Hovering and thermo, and including individual variation in activity costs
all_model_plot <- ggplot(NULL, aes(Site_proxy, kJ_day)) +
  geom_boxplot(data=dlw_bblh,aes(Site_proxy, kJ_day), fill="grey90",  width = 0.5, lwd=1) + 
  geom_linerange(data=m_energymodels_therm, #with everything varying
                 aes(Site_proxy, ymin = Min_kJ_day, ymax = Max_kJ_day, 
                     color = Activity_budget_type),
                 position=position_dodge(width=0.5),
                 size = 2, alpha = 0.4) + 
  geom_linerange(data=m_energymodels_act, #with only activity costs varying
                 aes(x=Site_proxy, ymin = Min_kJ_day, ymax = Max_kJ_day, 
                     color = Activity_budget_type), 
                 position=position_dodge(width=0.5), 
                 size = 5, alpha = 0.4) + 
  geom_linerange(data=m_energymodels_avg_act, #with average activity costs
                 aes(Site_proxy, ymin = Min_kJ_day, ymax = Max_kJ_day, 
                     color = Activity_budget_type), 
                 position=position_dodge(width=0.5), 
                 size = 5) +
  scale_color_manual(values = c('darkgreen', 'orangered2', 'slateblue4', 'violetred3'), 
                     guide = guide_legend(title = "Activity budget \n percent time \n hover_fly_perch")) +
  scale_x_discrete(breaks=c('A', 'B', 'C', 'D'), 
                   labels=c("Harshaw \n Dry", "Harshaw\nEarly-wet", "Sonoita \n Dry",
                            "Sonoita \n Early-wet")) +
  ylim(9, 41) + my_theme2 + theme(legend.key.size = unit(4, 'lines'), 
                                  legend.key.height = unit(4, 'lines'),
                                  legend.margin = margin(t=0.5, unit='cm'),
                                  legend.title.align = 0.5,
                                  legend.text.align = 0.5,
                                  #legend.text=element_text(size=32),
                                  axis.ticks.x = element_blank(),
                                  axis.ticks.y = element_line(size=2),
                                  axis.text = element_text(color = 'black', hjust=0.5),
                                  axis.title = element_text(face='bold'),
                                  axis.title.y = element_text(hjust=0.5)) +
  xlab("Site and season") + ylab("")#ylab("Daily \n energy expenditure (kJ)")


## Figure 2, arranging DLW plot with all model plot
grid.arrange(dlw_indiv, all_model_plot, nrow=1, widths = c(1,2))


## Just boxplots from DLW
dlw_plot <- ggplot(data=dlw_bblh,aes(Site_proxy, kJ_day)) + my_theme2 +
  geom_boxplot(alpha=0.5, fill="light grey") + 
  scale_x_discrete(breaks=c('A', 'B', 'C', 'D'), 
                   labels=c("Harshaw Pre", "Harshaw Post", "Sonoita Pre", "Sonoita Post")) +
  ylim(9, 41) + theme(legend.key.size = unit(2, 'lines')) + 
  xlab("Site and monsoon status") + 
  ylab("24-hour Energy Expenditure (kJ)")
dlw_plot


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
