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
torpor <- read.csv("Torpor_table_plot2.csv")
#names(torpor)

## Melting file
#m.tor <- melt(torpor, id.vars = c("Site","Species","Day","Month","Year","Daytime_Ta_mean_C"), 
#               measure.vars = c("NEE_kJ","Hours_torpid"))
##"Time_of_entry","Hours_torpid","Hours_normo","Nighttime_Ta_mean_C","Tc_mean_C",
##"Min_EE_torpid","Min_EE_normo",
##m.NEE <- m.tor[m.tor$variable=="NEE_kJ",]

## Adding column dividing NEE by 2/3*Mass to correct for mass with allometric scaling
torpor$NEE_MassCorrected<- torpor$NEE_kJ/((2/3)*torpor$Mass)

## Adding columns to correct for mass in Avg EE normo, Min EE normo, torpid, etc.
torpor$AvgEE_normo_MassCorrected<- torpor$EE_per_hour_normo/((2/3)*torpor$Mass)
torpor$MinEE_normo_MassCorrected<- torpor$Min_EE_normo/((2/3)*torpor$Mass)
torpor$AvgEE_torpid_MassCorrected<- torpor$EE_per_hour_torpid/((2/3)*torpor$Mass)
torpor$MinEE_torpid_MassCorrected<- torpor$EE_per_hour_torpid/((2/3)*torpor$Mass)

# Line to arrange Site facets in sensible order
torpor$Site_new = factor(torpor$Site, levels=c('HC','SC','SWRS','MQ','SL'))

## Function to arrange plots
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

## Function to return sample sizes
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

## Plot for Nighttime energy expenditure, by species
energy_plot <- ggplot(torpor, aes(Species, NEE_kJ)) +  theme_bw() +
  geom_boxplot(aes(col=Species)) + facet_grid(.~Site_new, scale="free_x", space="free") + 
  ylab("Nighttime energy expenditure (kJ)") + theme(legend.position="none") +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) +
  stat_summary(fun.data = give.n, geom = "text", vjust=-6)
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
grid.arrange(energy_plot, hours_plot, nrow=1, ncol=2)

## Plot for Mass-corrected Nighttime energy expenditure, by species
energyM_plot <- ggplot(torpor, aes(Species, NEE_MassCorrected)) +  theme_bw() +
  geom_boxplot(aes(col=Species)) + facet_grid(.~Site_new, scale="free_x", space="free") + 
  ylab("Nighttime energy expenditure Mass-corrected (kJ/g)") + theme(legend.position="none") +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) +
  stat_summary(fun.data = give.n, geom = "text", vjust=-5)
energyM_plot

## Comparing energy plots with and without mass-correction
grid.arrange(energy_plot, energyM_plot, nrow=1, ncol=2)

## NEE plot by temperature
energy_temp <- ggplot(torpor, aes(as.numeric(Tc_mean_C), NEE_kJ)) + 
  geom_point(aes(shape = factor(Species)), size=4, show_guide=F) + 
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  #scale_shape_manual(values=1:nlevels(torpor$Species)) +
  labs(shape='Species') + xlim(-2, 45) +
  scale_color_brewer(palette = "Set1") + theme_bw() + 
  geom_text(aes(label=Torpid_not, hjust=2), size=5, show_guide=F,
            fontface="bold") +
  ylab("Nighttime energy expenditure (kJ)") + 
  xlab("Chamber Temperature deg. C") +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
energy_temp 

## Mass-corrected NEE plot by temperature
energyM_temp <- ggplot(torpor, aes(as.numeric(Tc_mean_C), NEE_MassCorrected)) + 
  geom_point(aes(shape = factor(Species)), size=4) + 
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  #scale_shape_manual(values=1:nlevels(torpor$Species)) +
  labs(shape='Species') + xlim(-2, 45) +
  scale_color_brewer(palette = "Set1") + theme_bw() + 
  geom_text(aes(label=Torpid_not, hjust=2), size=5, show_guide=F,
            fontface="bold") +
  ylab("Nighttime energy expenditure Mass-corrected (kJ/g)") + 
  xlab("Chamber Temperature deg. C") +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
energyM_temp 

## NEE vs. temp, with and without mass-correction
grid.arrange(energy_temp, energyM_temp, nrow=1, ncol=2)

## NEE plot by chamber temperature, facet by site and color by species
energy_temp_site <- ggplot(torpor, aes(as.numeric(Tc_mean_C), NEE_kJ)) + 
  geom_point(aes(shape = factor(Species)), size=4) + 
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  #scale_shape_manual(values=1:nlevels(torpor$Species)) +
  labs(shape='Species') + xlim(-7, 45) +
  scale_color_brewer(palette = "Set1") + theme_bw() + 
  geom_text(aes(label=Torpid_not, hjust=1.75, fontface="bold"),size=5) +
  facet_grid(.~Site_new) +
  ylab("Nighttime energy expenditure (kJ)") + 
  xlab("Chamber Temperature deg. C") +
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
  ylab("Nighttime energy expenditure Mass-corrected (kJ/g)") + 
  xlab("Chamber Temperature deg. C") +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
energyM_temp_site

## Plot both energy_temp with and without mass-correction together. i.e. 4 plots
lay_out(list(energy_temp, 1, 1), 
        list(energy_temp_site, 1, 2),
        list(energyM_temp, 2, 1),
        list(energyM_temp_site, 2, 2))

# Average normo hourly EE, by Tc
avg_normo_EE <- ggplot(torpor, aes(as.numeric(Tc_mean_C), EE_per_hour_normo)) +  theme_bw() + 
  geom_point(aes(shape = factor(Species)), size=4) + 
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1")  + xlim(-7, 35) +
  #facet_grid(.~Site,space="free") +
  ylab("Avg EE normothermic (kJ/hr)") + xlab("Tc mean (C)") +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
avg_normo_EE

# Average hourly EE when in torpor, by Tc
avg_torpid_EE <- ggplot(torpor, aes(Tc_mean_C, EE_per_hour_torpid)) +  theme_bw() + 
  geom_point(aes(col=Species), size=3) + #theme(legend.position="none") +
  scale_color_brewer(palette = "Set1") +
  #facet_grid(.~Site,space="free") +
  ylab("Avg EE torpid (kJ/hr)") + xlab("Tc mean (C)") +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
avg_torpid_EE

## Min normo EE by Tc
min_normo_EE <- ggplot(torpor, aes(as.numeric(Tc_mean_C), Min_EE_normo)) +  theme_bw() + 
  geom_point(aes(shape = factor(Species)), size=4) + 
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + #xlim(-7, 50) +
  #facet_grid(.~Site,space="free") + 
  ylab("Min EE normothermic (kJ/hr)") + 
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
min_normo_EE

## Min torpid EE by Tc
min_torpid_EE <- ggplot(torpor, aes(Tc_mean_C, Min_EE_torpid)) +  theme_bw() + 
  geom_point(aes(col=Species), size=3) + scale_color_brewer(palette = "Set1") +
  #facet_grid(.~Site,space="free") + 
  ylab("Min EE torpid (kJ/hr)") + 
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
min_torpid_EE
grid.arrange(min_normo_EE, min_torpid_EE, nrow=2, ncol=1)
grid.arrange(avg_normo_EE, avg_torpid_EE, min_normo_EE, min_torpid_EE, nrow=2, ncol=2)

## Avg and min graphs, mass corrected
# Average normo hourly EE, by Tc
avg_normo_EE <- ggplot(torpor, aes(as.numeric(Tc_mean_C), AvgEE_normo_MassCorrected)) +  theme_bw() + 
  geom_point(aes(shape = factor(Species)), size=4) + 
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1")  + xlim(-7, 35) +
  #facet_grid(.~Site,space="free") +
  ylab("Avg EE normothermic (kJ/g*hr)") + xlab("Tc mean (C)") +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
avg_normo_EE

# Average hourly EE when in torpor, by Tc
avg_torpid_EE <- ggplot(torpor, aes(Tc_mean_C, AvgEE_torpid_MassCorrected)) +  theme_bw() + 
  geom_point(aes(col=Species), size=3) + #theme(legend.position="none") +
  scale_color_brewer(palette = "Set1") +
  #facet_grid(.~Site,space="free") +
  ylab("Avg EE torpid (kJ/g*hr)") + xlab("Tc mean (C)") +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
avg_torpid_EE

## Min normo EE by Tc
min_normo_EE <- ggplot(torpor, aes(as.numeric(Tc_mean_C), MinEE_normo_MassCorrected)) +  theme_bw() + 
  geom_point(aes(shape = factor(Species)), size=4) + 
  scale_shape_manual(values=c(3,1,2,0,15,16,17,23)) +
  scale_color_brewer(palette = "Set1") + #xlim(-7, 50) +
  #facet_grid(.~Site,space="free") + 
  ylab("Min EE normothermic (kJ/g*hr)") +  xlab("Tc mean (C)") +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
min_normo_EE

## Min torpid EE by Tc
min_torpid_EE <- ggplot(torpor, aes(Tc_mean_C, MinEE_torpid_MassCorrected)) +  theme_bw() + 
  geom_point(aes(col=Species), size=3) + scale_color_brewer(palette = "Set1") +
  #facet_grid(.~Site,space="free") + 
  ylab("Min EE torpid (kJ/g*hr)") +  xlab("Tc mean (C)") +
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
min_torpid_EE
temp <- ggplot(torpor, aes(Daytime_Ta_mean_C,Day)) + geom_point() + theme_bw()
temp
geom_boxplot(aes(Species, Hours_torpid)) + geom_point()

##To subset variables within melted data frame and plot in ggplot, 
##add to ggplot(x=,*,subset=.(variable=="NEE_kJ")*)

#geom_point(aes(Tc_mean_C, Min_EE_torpid), col=Species) + scale_shape(solid=FALSE) +

##+ geom_point(aes(y=Daytime_Ta_mean_C, col=Daytime_Ta_mean_C))
#scale_size(guide = 'none') + 
#geom_point(aes(col=Site, size=2)) + scale_shape_identity() + 
#scale_color_manual(values = c("grey60", "black"), guide = FALSE) + 