## Started fresh, 15 November, 2015
## Torpor paper, A. Shankar, R. Schroeder et al.

library(ggplot2)
library(reshape)
library(gridExtra)
library(grid)
library(wq)

## setwd and read in file
#wdMac<- setwd("/Users/anushashankar/Dropbox/Hummingbird energetics/Tables_for_paper")
#wdMac
wdMS <- setwd("C:\\Users\\ANUSHA\\Dropbox\\Hummingbird energetics\\Tables_for_paper")
wdMS
torpor <- read.csv("Torpor_table_plot_Dec9.csv")
#names(torpor)

## Adding column dividing NEE by 2/3*Mass to correct for mass with allometric scaling
torpor$NEE_MassCorrected<- torpor$NEE_kJ/((2/3)*torpor$Mass)

## Adding columns to correct for mass in Avg EE normo, Min EE normo, torpid, etc.
torpor$AvgEE_normo_MassCorrected <- torpor$Avg_EE_hourly_normo/((2/3)*torpor$Mass)
torpor$MinEE_normo_MassCorrected <- as.numeric(torpor$Min_EE_normo)/((2/3)*torpor$Mass)
torpor$AvgEE_torpid_MassCorrected <- torpor$Avg_EE_hourly_torpid/((2/3)*torpor$Mass)
torpor$MinEE_torpid_MassCorrected <- as.numeric(torpor$Min_EE_torpid)/((2/3)*torpor$Mass)

# Line to arrange Site facets in sensible order
torpor$Site_new <- factor(torpor$Site, levels=c('HC','SC','SWRS','MQ','SL'))

## Subset just BBLH data
BBLH_torpor <- subset(torpor, Species=="BBLH")

## Subset just GCB data
GCB_torpor <- subset(torpor, Species=="GCB")

##To subset variables within melted data frame and plot in ggplot, 
##add to ggplot(x=,*,subset=.(variable=="NEE_kJ")*)## Function to arrange plots

#### General functions ####
lay_out = function(...) {    
  x <- list(...)
  n <- max(sapply(x, function(x) max(x[[2]])))
  p <- max(sapply(x, function(x) max(x[[3]])))
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))    
  
  for (i in seq_len(length(x))) {
    print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]], 
                                           layout.pos.col = x[[i]][[3]]))
  }
} 

# Function to return sample sizes
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

## Function for adding a regression equation to graphs
## (Where table= the file name, y= column name for y in the equation and x= column name for x)
lm_eqn <- function(table, y, x){
  m <- lm(y ~ x, table);
  eq <- substitute(italic(y) == 
                     a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

#code for including degree symbol in axis labels
Tc.xlab <- expression(atop(paste("Chamber Temperature (", degree,"C)")))
Ta.xlab <- expression(atop(paste("Ambient Temperature (", degree,"C)")))
Tc_min.xlab <- expression(atop(paste("Minimum Chamber Temperature (", degree,"C)")))

#### Basic NEE and hours plots ####
## Plot for Nighttime energy expenditure, by species
energy_plot <- ggplot(torpor, aes(Species, NEE_kJ)) +  theme_bw() +
  geom_boxplot(aes(col=Species)) + facet_grid(.~Site_new, scale="free_x", space="free") + 
  ylab("Nighttime energy expenditure (kJ)") + theme(legend.position="none") +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) +
  stat_summary(fun.data = give.n, geom = "text", vjust=-5)
energy_plot

## Plot for hours spent torpid
hours_plot <- ggplot(torpor, aes(Species, Hours_torpid)) +  theme_bw() +
  geom_boxplot(aes(col=Species)) + facet_grid(.~Site_new, scale="free_x", space="free") + 
  ylab("Hours Torpid") + theme(legend.position="none") +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) +
  stat_summary(fun.data = give.n, geom = "text", vjust=-4.75)
hours_plot

## Plot for Mass-corrected Nighttime energy expenditure, by species
energyM_plot <- ggplot(torpor, aes(Species, NEE_MassCorrected)) +  theme_bw() +
  geom_boxplot(aes(col=Species)) + facet_grid(.~Site_new, scale="free_x", space="free") + 
  ylab("Nighttime energy expenditure Mass-corrected (kJ/g)") + theme(legend.position="none") +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) +
  stat_summary(fun.data = give.n, geom = "text", vjust=-5)
energyM_plot

## Energy vs. hours torpid, species labeled
energyM_hours <- ggplot(torpor, aes(Hours_torpid, NEE_MassCorrected)) +  theme_bw() +
  geom_point(aes(shape = factor(Species)), size=4) + 
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  geom_smooth(method=lm, color="black") +
  geom_text(x = 5, y = 4.5, label = lm_eqn(torpor, torpor$NEE_MassCorrected, 
                                             torpor$Hours_torpid), parse=T, size=7) +
  labs(shape='Species') + scale_color_brewer(palette = "Set1") + theme_bw() +
  ylab("Nighttime energy expenditure Mass-corrected (kJ/g)") + xlab("Hours torpid") +
  theme(axis.title.x = element_text(size=18, face="bold"),
        axis.text.x = element_text(size=16),
        axis.title.y = element_text(size=18, face="bold"), axis.text.y = element_text(size=16))
energyM_hours

## Comparing NEE and hours plots
grid.arrange(energyM_plot, hours_plot, nrow=1, ncol=2)

## Comparing energy plots with and without mass-correction
grid.arrange(energy_plot, energyM_plot, nrow=1, ncol=2)

## NEE plot by temperature
energy_temp <- ggplot(torpor, aes(as.numeric(Tc_mean_C), NEE_kJ)) + 
  geom_point(aes(shape = factor(Species)), size=4) + 
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  labs(shape='Species') + xlim(4, 30) +
  scale_color_brewer(palette = "Set1") + theme_bw() + 
  geom_text(aes(label=Torpid_not, hjust=2), size=5, show_guide=F,
            fontface="bold") +
  ylab("Nighttime energy expenditure (kJ)") + xlab(Tc.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
energy_temp 

## Mass-corrected NEE plot by temperature
energyM_temp <- ggplot(torpor, aes(as.numeric(Tc_mean_C), NEE_MassCorrected)) + 
  geom_point(aes(shape = factor(Species)), size=4) + 
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  #scale_shape_manual(values=1:nlevels(torpor$Species)) +
  labs(shape='Species') + xlim(0, 30) +
  scale_color_brewer(palette = "Set1") + theme_bw() + 
  geom_text(aes(label=Torpid_not, hjust=2), size=5, show_guide=F,
            fontface="bold") +
  ylab("Nighttime energy expenditure Mass-corrected (kJ/g)") + xlab(Tc.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
energyM_temp 

## Mass-corrected NEE plot by MINIMUM Tc
energyM_Tcmin <- ggplot(torpor, aes(as.numeric(Tc_min_C), NEE_MassCorrected)) + 
  geom_point(aes(shape = factor(Species)), size=4) + 
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  geom_smooth(method=lm, color="black") +
  geom_text(x = 16, y = 4.5, label = lm_eqn(torpor, torpor$NEE_MassCorrected, 
                                            torpor$Tc_min_C), parse=T) +
  labs(shape='Species') + #xlim(5, 25) +
  scale_color_brewer(palette = "Set1") + theme_bw() + 
  geom_text(aes(label=Torpid_not, hjust=2), size=5, show_guide=F,
            fontface="bold") +
  ylab("Nighttime energy expenditure Mass-corrected (kJ/g)") + xlab(Tc_min.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
energyM_Tcmin

## NEE vs. temp, with and without mass-correction
grid.arrange(energy_temp, energyM_temp, nrow=1, ncol=2)

## NEE plot by chamber temperature, facet by site and color by species
energy_temp_site <- ggplot(torpor, aes(as.numeric(Tc_mean_C), NEE_kJ)) + 
  geom_point(aes(shape = factor(Species)), size=4) + 
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  #scale_shape_manual(values=1:nlevels(torpor$Species)) +
  labs(shape='Species') + xlim(-7, 45) + theme_bw() + 
  #scale_color_brewer(palette = "Set1") 
  geom_text(aes(label=Torpid_not, hjust=1.75, fontface="bold"),size=5) +
  facet_grid(.~Site_new) +
  ylab("Nighttime energy expenditure (kJ)") + xlab(Tc.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
energy_temp_site

## Plot both energy_temp plots together
lay_out(list(energy_temp, 1, 1), 
        list(energy_temp_site, 1, 2))

## Mass-corrected NEE plot by chamber temperature, facet by site and color by species
energyM_temp_site <- ggplot(torpor, aes(as.numeric(Tc_mean_C), NEE_MassCorrected)) + 
  geom_point(aes(shape = factor(Species)), size=4) + 
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  #scale_shape_manual(values=1:nlevels(torpor$Species)) +
  labs(shape='Species') + xlim(-7, 45) +
  scale_color_brewer(palette = "Set1") + theme_bw() + 
  geom_text(aes(label=Torpid_not, hjust=1.75, fontface="bold"),size=5) +
  facet_grid(.~Site_new) +
  ylab("Nighttime energy expenditure Mass-corrected (kJ/g)") + xlab(Tc.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
energyM_temp_site

## Plot both energy_temp with and without mass-correction together. i.e. 4 plots
lay_out(list(energy_temp, 1, 1), 
        list(energy_temp_site, 1, 2),
        list(energyM_temp, 2, 1),
        list(energyM_temp_site, 2, 2))

#### Non-mass-corrected min and avg graphs ####
## Min normo EE by Tc
min_normo_EE <- ggplot(torpor, aes(as.numeric(Tc_mean_C), as.numeric(Min_EE_normo))) +  theme_bw() + 
  geom_point(aes(shape = factor(Species)), size=4) +  labs(shape ='Species') +
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + xlim(5, 32) +
  geom_text(aes(label=Torpid_not, hjust=1.75, fontface="bold"),size=5) +
  ylab("Min EE normothermic") + xlab(Tc.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
min_normo_EE

## Minimum hourly energy expenditure while torpid by Tc
min_torpid_EE <- ggplot(torpor, aes(as.numeric(Tc_mean_C), as.numeric(Min_EE_torpid))) + theme_bw() + 
  geom_point(aes(shape = factor(Species)), size=4) + labs(shape ='Species') +
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + #xlim(-7, 50) +
  ylab("Min EE torpid") + xlab(Tc.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
min_torpid_EE

## Average hourly energy expenditure while normothermic by Tc
#avg_normo_EE <- ggplot(torpor, aes(as.numeric(Tc_mean_C), Avg_EE_hourly_normo)) +  theme_bw() + 
#  geom_point(aes(shape = factor(Species)), size=4) + labs(shape ='Species') +
#  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
#  scale_color_brewer(palette = "Set1")  + #xlim(-7, 35) +
#  ylab("Avg EE normothermic") + xlab(Tc.xlab) +
#  theme(axis.title.x = element_text(size=16, face="bold"),
#        axis.text.x = element_text(size=14),
#        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
#avg_normo_EE

## Avg EE normo, with points labeled as Normo or Torpid birds; to see if overall EE is lower or
## higher for birds that tend to go into torpor
avg_normo_EE <- ggplot(torpor, aes(as.numeric(Tc_mean_C), Avg_EE_hourly_normo)) +  theme_bw() + 
  geom_point(aes(shape = factor(Species)), size=4) + labs(shape ='Species') +
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + xlim(5, 30) +
  #facet_grid(.~Site,space="free") +
  geom_text(aes(label=Torpid_not, hjust=1.75, fontface="bold"),size=5) +
  ylab("Avg EE normothermic") + xlab(Tc.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
avg_normo_EE

## Average hourly energy expenditure while torpid
avg_torpid_EE <- ggplot(torpor, aes(Tc_mean_C, Avg_EE_hourly_torpid)) +  theme_bw() + 
  geom_point(aes(shape = factor(Species)), size=4) + labs(shape ='Species') +
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + #xlim(5, 30) +
  #facet_grid(.~Site,space="free") +
  ylab("Avg EE torpid") + xlab(Tc.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
avg_torpid_EE

######### Mass-corrected Min and avg graphs ####
## Mass-corrected Min normo EE by Tc
m_min_normo_EE <- ggplot(torpor, aes(as.numeric(Tc_mean_C), 
                                     as.numeric(MinEE_normo_MassCorrected))) +  
  theme_bw() + geom_point(aes(shape = factor(Species)), size=4) +  labs(shape ='Species') +
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + xlim(5, 32) +
  geom_text(aes(label=Torpid_not, hjust=1.75, fontface="bold"),size=5) +
  ylab("Min EE normothermic (kJ/g)") + xlab(Tc.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
m_min_normo_EE

## Mass-corrected Minimum hourly energy expenditure while torpid by Tc
m_min_torpid_EE <- ggplot(torpor, aes(as.numeric(Tc_mean_C), 
                                      as.numeric(MinEE_torpid_MassCorrected))) + 
  theme_bw() + geom_point(aes(shape = factor(Species)), size=4) + labs(shape ='Species') +
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  geom_smooth(method=lm, color="black") +
  geom_text(x = 16, y = 0.6, label = lm_eqn(torpor, torpor$AvgEE_normo_MassCorrected, 
                                            torpor$Tc_min_C), parse=T) +
  scale_color_brewer(palette = "Set1") + #xlim(-7, 50) +
  ylab("Min EE torpid (kJ/g)") + xlab(Tc.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
m_min_torpid_EE

## Mass-corrected Average hourly energy expenditure while normothermic
m_avg_normo_EE <- ggplot(torpor, aes(as.numeric(Tc_mean_C), AvgEE_normo_MassCorrected)) + 
  theme_bw() + geom_point(aes(shape = factor(Species)), size=4) + labs(shape ='Species') +
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + xlim(5, 30) +
  #facet_grid(.~Site,space="free") +
  geom_text(aes(label=Torpid_not, hjust=1.75, fontface="bold"),size=5) +
  ylab("Avg EE normothermic (kJ/g)") + xlab(Tc.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
m_avg_normo_EE

## Mass-corrected Average hourly energy expenditure while torpid
m_avg_torpid_EE <- ggplot(torpor, aes(Tc_mean_C, AvgEE_torpid_MassCorrected)) +  theme_bw() + 
  geom_point(aes(shape = factor(Species)), size=4) + labs(shape ='Species') +
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + #xlim(5, 30) +
  #facet_grid(.~Site,space="free") +
  ylab("Avg EE torpid (kJ/g)") + xlab(Tc.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
m_avg_torpid_EE

## Arranging mass-corrected graphs
grid.arrange(m_avg_normo_EE, m_avg_torpid_EE, 
             m_min_normo_EE, m_min_torpid_EE, nrow=2, ncol=2) # avg and min EE
lay_out(list(m_avg_normo_EE_labeled, 1, 1), 
        list(m_avg_torpid_EE, 2, 1)) # Only average EEs

lay_out(list(m_avg_normo_EE, 1, 1),
        list(m_avg_torpid_EE, 1, 2),
        list(m_min_normo_EE, 2, 1), 
        list(m_min_torpid_EE, 2, 2))

#### Mass-corrected mins and avgs, plotted against Tc min instead of Tc mean with regressions ####
## Mass-corrected Min normo EE by Tc min
m_min_normo_EE_Tcmin_eq <- ggplot(torpor, aes(as.numeric(Tc_min_C), 
                                              as.numeric(MinEE_normo_MassCorrected))) +  
  theme_bw() + geom_point(aes(shape = factor(Species)), size=4) +  labs(shape ='Species') +
  geom_smooth(method=lm, color="black") +
  geom_text(x = 19, y = 0.38, label = lm_eqn(torpor, torpor$MinEE_normo_MassCorrected, 
                                            torpor$Tc_min_C), parse=T) +
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + xlim(0, 32) +
  geom_text(aes(label=Torpid_not, hjust=1.75, fontface="bold"),size=5) +
  ylab("Min EE normothermic (kJ/g)") + xlab(Tc_min.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
m_min_normo_EE_Tcmin_eq

## Mass-corrected Minimum hourly energy expenditure while torpid by Tc
m_min_torpid_EE_Tcmin_eq <- ggplot(torpor, aes(as.numeric(Tc_min_C), 
                                               as.numeric(MinEE_torpid_MassCorrected))) + 
  theme_bw() + geom_point(aes(shape = factor(Species)), size=4) + labs(shape ='Species') +
  geom_smooth(method=lm, color="black") +
  geom_text(x = 15, y = 0.06, label = lm_eqn(torpor, torpor$MinEE_torpid_MassCorrected, 
                                            torpor$Tc_min_C), parse=T) +
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + #xlim(-7, 50) +
  ylab("Min EE torpid (kJ/g)") + xlab(Tc_min.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
m_min_torpid_EE_Tcmin_eq

## Mass-corrected Average hourly energy expenditure while normothermic
m_avg_normo_EE_Tcmin_eq <- ggplot(torpor, aes(as.numeric(Tc_min_C), AvgEE_normo_MassCorrected)) + 
  theme_bw() + geom_point(aes(shape = factor(Species)), size=4) + labs(shape ='Species') +
  geom_smooth(method=lm, color="black") +
  geom_text(x = 16, y = 0.6, label = lm_eqn(torpor, torpor$AvgEE_normo_MassCorrected, 
                                            torpor$Tc_min_C), parse=T, size=7) +
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + xlim(0, 30) +
  #facet_grid(.~Site,space="free") +
  geom_text(aes(label=Torpid_not, hjust=1.75, fontface="bold"),size=5) +
  ylab("Avg EE normothermic (kJ/g)") + xlab(Tc_min.xlab) +
  theme(axis.title.x = element_text(size=18, face="bold"),
        axis.text.x = element_text(size=16),
        axis.title.y = element_text(size=18, face="bold"), axis.text.y = element_text(size=16)) 
m_avg_normo_EE_Tcmin_eq

## Mass-corrected Average hourly energy expenditure while torpid
m_avg_torpid_EE_Tcmin_eq <- ggplot(torpor, aes(Tc_min_C, AvgEE_torpid_MassCorrected)) +  theme_bw() + 
  geom_point(aes(shape = factor(Species)), size=4) + labs(shape ='Species') +
  geom_smooth(method=lm, color="black") +
  geom_text(x = 13, y = 0.095, label = lm_eqn(torpor, torpor$AvgEE_torpid_MassCorrected, 
                                            torpor$Tc_min_C), parse=T) +
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + 
  #facet_grid(.~Site,space="free") +
  ylab("Avg EE torpid (kJ/g)") + xlab(Tc_min.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
m_avg_torpid_EE_Tcmin_eq

## Arranging mass-corrected vs. Tc min graphs
lay_out(list(m_avg_normo_EE_Tcmin_eq, 1, 1),
        list(m_avg_torpid_EE_Tcmin_eq, 1, 2),
        list(m_min_normo_EE_Tcmin_eq, 2, 1), 
        list(m_min_torpid_EE_Tcmin_eq, 2, 2))

#### Overall regressions ####

## All species, Avg normo vs Tc regression
m_AvgEE_normo_Tcmin_eq <- ggplot(torpor, aes(as.numeric(Tc_mean_C), AvgEE_normo_MassCorrected)) + 
  theme_bw() + geom_point(aes(shape = factor(Species)), size=4) + labs(shape ='Species') +
  geom_smooth(method=lm, color="black") +
  geom_text(x = 16, y = 0.6, label = lm_eqn(torpor, torpor$AvgEE_normo_MassCorrected, 
                                            torpor$Tc_min_C), parse=T) +
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + xlim(7, 30) +
  geom_text(aes(label=Torpid_not, hjust=1.75, fontface="bold"),size=5) +
  ylab("Avg EE normothermic (kJ/g)") + xlab(Tc.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
m_AvgEE_normo_Tcmin_eq


#### Species- specific graphs ####
## BBLH Avg mass-corrected hourly normothermic EE vs. min Tc with regression line
m_BBLH_avgEE_normo_Tcmin_eq <- ggplot(BBLH_torpor, aes(as.numeric(Tc_min_C), 
                                                    AvgEE_normo_MassCorrected)) + 
  theme_bw() + geom_point() + geom_smooth(method=lm, color="black") +
  geom_text(x = 16, y = 0.38, label = lm_eqn(BBLH_torpor, BBLH_torpor$AvgEE_normo_MassCorrected, 
                                             BBLH_torpor$Tc_min_C), parse=T) +
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + #xlim(0, 30) +
  geom_text(aes(label=Torpid_not, hjust=1.75, fontface="bold"),size=5) +
  ylab("Avg BBLH normothermic EE (kJ/g)") + xlab(Tc_min.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
m_BBLH_avgEE_normo_Tcmin_eq

## BBLH Avg mass-corrected hourly torpid EE vs. min Tc with regression line
m_BBLH_avgEE_torpid_Tcmin_eq <- ggplot(BBLH_torpor, aes(as.numeric(Tc_min_C), 
                                                     AvgEE_torpid_MassCorrected)) + 
  theme_bw() + geom_point() + geom_smooth(method=lm) +
  geom_text(x = 14, y = 0.07, label = lm_eqn(BBLH_torpor, BBLH_torpor$AvgEE_torpid_MassCorrected, 
                                             BBLH_torpor$Tc_min_C), parse=T, size=6) +
  #scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  #scale_color_brewer(palette = "Set1") + #xlim(0, 30) +
  #geom_text(aes(label=Torpid_not, hjust=1.75, fontface="bold"),size=5) +
  ylab("Avg BBLH EE torpid (kJ/g)") + xlab(Tc_min.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
m_BBLH_avgEE_torpid_Tcmin_eq

## BBLH Min mass-corrected hourly normothermic EE vs. min Tc with regression line
m_BBLH_minEE_normo_Tcmin_eq <- ggplot(BBLH_torpor, aes(as.numeric(Tc_min_C), 
                                                    MinEE_normo_MassCorrected)) + 
  theme_bw() + geom_point() + geom_smooth(method=lm, color="black") +
  geom_text(x = 16, y = 0.32, label = lm_eqn(BBLH_torpor, BBLH_torpor$MinEE_normo_MassCorrected, 
                                           BBLH_torpor$Tc_min_C), parse=T) +
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + #xlim(0, 30) +
  geom_text(aes(label=Torpid_not, hjust=1.75, fontface="bold"),size=5) +
  ylab("Minimum BBLH normothermic EE (kJ/g)") + xlab(Tc_min.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
m_BBLH_minEE_normo_Tcmin_eq

## BBLH Min mass-corrected hourly torpid EE vs. min Tc with regression line
m_BBLH_minEE_torpid_Tcmin_eq <- ggplot(BBLH_torpor, aes(as.numeric(Tc_min_C), 
                                                     MinEE_torpid_MassCorrected)) + 
  theme_bw() + geom_point() + geom_smooth(method=lm) +
  geom_text(x = 14, y = 0.024, label = lm_eqn(BBLH_torpor, BBLH_torpor$MinEE_torpid_MassCorrected, 
                                          BBLH_torpor$Tc_min_C), parse=T, size=6) +
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + #xlim(0, 30) +
  geom_text(aes(label=Torpid_not, hjust=1.75, fontface="bold"),size=5) +
  ylab("Minimum BBLH EE torpid (kJ/g)") + xlab(Tc_min.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
m_BBLH_minEE_torpid_Tcmin_eq

lay_out(list(m_BBLH_avgEE_normo_Tcmin_eq, 1, 1),
        list(m_BBLH_avgEE_torpid_Tcmin_eq, 1, 2),
        list(m_BBLH_minEE_normo_Tcmin_eq, 2, 1), 
        list(m_BBLH_minEE_torpid_Tcmin_eq, 2, 2))

## GCB
m_GCB_avgEE_torpid_Tcmin_eq <- ggplot(GCB_torpor, aes(as.numeric(Tc_min_C), AvgEE_torpid_MassCorrected)) +
  theme_bw() + geom_point() + geom_smooth(method=lm) +
  geom_text(x=21, y=0.10, label=lm_eqn(GCB_torpor, GCB_torpor$AvgEE_torpid_MassCorrected,
                                       GCB_torpor$Tc_min_C), parse=T,size=6) +
  ylab("Avg GCB EE torpid (kJ/g)") + xlab(Tc_min.xlab) +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
m_GCB_avgEE_torpid_Tcmin_eq

#### Statistics ####

## Linear and non-linear models
## Regressions
multiple_regression_NEE <- lm(NEE_kJ ~ Tc_mean_C + Tc_min_C + Mass, data=torpor)
summary(multiple_regression_NEE) # show results
# Other useful functions 
coefficients(multiple_regression_NEE) # model coefficients
confint(multiple_regression_NEE, level=0.95) # CIs for model parameters 
fitted(multiple_regression_NEE) # predicted values
residuals(multiple_regression_NEE) # residuals
anova(multiple_regression_NEE) # anova table 
vcov(multiple_regression_NEE) # covariance matrix for model parameters 
influence(multiple_regression_NEE) # regression diagnostics

## Avg normo EE anova (non-mass-corrected)
mul_regr_AvgEEnormo <- lm(Avg_EE_hourly_normo ~ Tc_mean_C + Tc_min_C + Mass, data=torpor)
anova(mul_regr_AvgEEnormo)

##### WEIRD- mean Tc and Mass have a significant effect on Average mass-corrected normo hourly EE
mul_regr_m_AvgEEnormo <- lm(AvgEE_normo_MassCorrected ~ Tc_mean_C + Tc_min_C + Mass, data=torpor)
anova(mul_regr_m_AvgEEnormo)

## Avg non-mass-corrected normo EE for BBLH anova
mul_regr_m_AvgEEnormo_BBLH <- lm(AvgEE_normo_MassCorrected ~ Tc_mean_C + Tc_min_C + 
                                   Mass, data=BBLH_torpor)
anova(mul_regr_m_AvgEEnormo_BBLH)

## Avg non-mass-corrected torpid EE for BBLH anova
mul_regr_m_AvgEEtorpid_BBLH <- lm(AvgEE_torpid_MassCorrected ~ Tc_mean_C + Tc_min_C + 
                                    Mass, data=BBLH_torpor)
anova(mul_regr_m_AvgEEtorpid_BBLH)

## GAMs
gam(torpor)