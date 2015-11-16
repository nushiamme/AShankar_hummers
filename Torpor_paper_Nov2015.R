## Started fresh, 15 November, 2015
## Torpor paper, A. Shankar, R. Schroeder et al.

library(ggplot2)
library(reshape)
library(gridExtra)


## setwd and read in file
setwd("C:\\Users\\ANUSHA\\Dropbox\\Hummingbird energetics\\Tables_for_paper")
torpor <- read.csv("Torpor_table_plot.csv")
names(torpor)

m.tor <- melt(torpor, id.vars = c("Site","Species","Day","Month","Year","Daytime_Ta_mean_C"), 
              measure.vars = c("NEE_kJ","Hours_torpid"))
##"Time_of_entry","Hours_torpid","Hours_normo","Nighttime_Ta_mean_C","Tc_mean_C",
##"Min_EE_torpid","Min_EE_normo",
##m.NEE <- m.tor[m.tor$variable=="NEE_kJ",]

# Line to arrange Site facets in sensible order
torpor$Site_new = factor(torpor$Site, levels=c('HC','SC','SWRS','MQ','SL'))

give.n <- function(Species){
  return(c(y = mean(Species), label = length(Species)))
}

## Plot for Nighttime energy expenditure, by species
energy_plot <- ggplot(torpor, aes(Species, NEE_kJ)) +  theme_bw() +
  geom_boxplot() + facet_grid(.~Site_new, scale="free_x", space="free") + 
  ylab("Nighttime energy expenditure (kJ)") + 
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) +
  stat_summary(fun.data = give.n, geom = "text")
energy_plot

## Plot for hours spent torpid
hours_plot <- ggplot(torpor, aes(Species, Hours_torpid)) +  theme_bw() +
  geom_boxplot() + facet_grid(.~Site_new, scale="free_x", space="free") + 
  ylab("Hours Torpid") + 
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14))
hours_plot
grid.arrange(energy_plot, hours_plot, nrow=1, ncol=2)

## NEE plot by temperature
energy_temp <- ggplot(torpor, aes(Tc_mean_C, NEE_kJ)) +  theme_bw() +
  geom_point(aes(col=Species), size=3) + scale_color_brewer(palette = "Set1") +
  facet_grid(.~Site_new,space="free") + 
  ylab("Nighttime energy expenditure (kJ)") + 
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
energy_temp

## NEE plot by chamber temperature, facet by site and color by species
energy_temp2 <- ggplot(torpor, aes(Tc_mean_C, NEE_kJ)) +  theme_bw() +
  geom_point(aes(col=Species), size=3) + scale_color_brewer(palette = "Set1") +
  #facet_grid(.~Site,space="free") + 
  ylab("Nighttime energy expenditure (kJ)") + 
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
energy_temp2

## Min normo EE by Tc
min_normo_EE <- ggplot(torpor, aes(Tc_mean_C, Min_EE_normo)) +  theme_bw() + 
  geom_point(aes(col=Species), size=3) + theme(legend.position="none") +
  scale_color_brewer(palette = "Set1") +
  #facet_grid(.~Site,space="free") + 
  ylab("Min EE normothermic") + 
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
min_normo_EE

## Min torpid EE by Tc
min_torpid_EE <- ggplot(torpor, aes(Tc_mean_C, Min_EE_torpid)) +  theme_bw() + 
  geom_point(aes(col=Species), size=3) + scale_color_brewer(palette = "Set1") +
  #facet_grid(.~Site,space="free") + 
  ylab("Min EE torpid") + 
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14)) 
min_torpid_EE
grid.arrange(min_normo_EE, min_torpid_EE, nrow=2, ncol=1)

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
  
hours_torpor_plot <- ggplot(torpor, aes(Site, NEE_kJ)) + theme_bw() + geom_boxplot() +
  geom_point(aes(col=Site, size=2)) + #scale_size(guide = 'none') + scale_shape_identity() +
  #scale_color_manual(values = c("grey60", "black"), guide = FALSE) + 
  theme(axis.title.x = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16, face="bold"), axis.text.y = element_text(size=14))
hours_torpor_plot

