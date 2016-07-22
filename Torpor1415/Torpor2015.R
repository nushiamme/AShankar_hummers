## Processing torpor data from 2015 field season to incorporate with previous torpor data
## Anusha Shankar
## Started February 22, 2016

##Packages
library(ggplot2)
library(reshape)
library(gridExtra)
library(grid)
library(wq)
library(gam)
library(foreign)
library(MASS)
library(devtools)
#library(plotflow) #Useful function reorder_by() might be useful for ordering variables by others

## Set working directory and read in .csv file
wdMS <- setwd("C:\\Users\\ANUSHA\\Dropbox\\Hummingbird energetics\\Tables_for_paper")
wdMS
torpor2015 <- read.csv("Torpor2015.csv")
litstudy <- read.csv("LitStudy_combined.csv")
litnew <- read.csv("LitStudy_andKruger.csv")
krugertab <- read.csv("Lit_Kruger1982.csv")
k_melt <- read.csv("Lit_Kruger1982_modified.csv")

m.krug <- melt(krugertab, id.vars = c("Species", "Sex", "Mean_mass_g", "Temp"), 
     measure.vars = c("MR_day_J_g_hr", "MR_night_J_g_hr", "MR_torpor_J_g_hr"))
names(m.krug) <- c("Species", "Sex", "Mass", "Temp", "Measure", "Value")

## Min chamber temo in deg. C axis label
Tc_min.xlab <- expression(atop(paste("Minimum Chamber Temperature (", degree,"C)")))

## Making my easy-theme
my_theme <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(vjust = 2),
        panel.border = element_rect(colour = "black", fill=NA))

## Function to return sample sizes
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

## USeful for introduction !!!!!!!!!!!!!!!!
## McKechnie, A.E. and B.G. Lovegrove. 2002. Avian Facultative Hypothermic Responses: a Review. 
      # The Condor 104: 705.
## The capacity for shallow hypothermia (rest-phase hypothermia) occurs throughout the avian phylogeny,
## but the capacity for pronounced hypothermia (torpor) appears to be restricted to certain taxa. 
## Families in which torpor has been reported include the Todidae, Coliidae, Trochilidae, Apodidae, 
## Caprimulgidae, and Columbidae.

## Subsetting files
#agcu <- torpor2015[torpor2015$Species=="AGCU",]
#mety <- torpor2015[torpor2015$Species=="METY",]
tor_sub <- torpor2015[torpor2015$Species=="AGCU" | torpor2015$Species=="METY",]

##### Set time as a factor  ######
#agcu_indiv <- torpor2015[torpor2015$BirdID=="EG15_0104_AGCU",]
#agcu_indiv$Time <- factor(agcu_indiv$Time, levels=agcu_indiv$Time)

#mety$Time <- factor(mety$Time, levels=mety$Time)
#mety_indiv <- torpor2015[torpor2015$BirdID=="EG15_1028_METY",]
#mety_indiv$Time <- factor(mety_indiv$Time, levels=mety_indiv$Time)

##METY days - 0910, 1028, 1130, 1209, 1211, 1212, 1219
##AGCU days - 0826, 1023, 1220, 1223, 0104

#### Kruger et al. 1982 study, plotting values for 22 species ####
krugerplot <- ggplot(m.krug, aes(Temp, Value, group=interaction(Measure,Species))) + my_theme + 
  geom_line(aes(col=Measure)) +
  xlab("Ambient temperature (deg. C)") + ylab("Energy expenditure (J/g*hr)") +
  scale_y_continuous(breaks=c(0,50,100,200,400,600)) + theme(panel.grid.major.y = element_line(size=.1, color="grey"))
krugerplot


## Kruger's data, selecting particular species
krugerplot_sp <- ggplot(m.krug[m.krug$Species=="Aglaectis cupripennis",], aes(Temp, Value)) + my_theme +
  geom_point(aes(col=Measure, size=Mass)) + ylab("Energy expenditure (J/g*hr)") + 
  scale_y_continuous(breaks=c(0,50,100,200,400,600)) + theme(panel.grid.major.y = element_line(size=.1, color="grey"))
krugerplot_sp

### Plot literature review values ######
litplot <- ggplot(litstudy, aes(Tc_min, EE_J)) +  
  theme_bw(base_size = 20) + geom_point(aes(col=Torpid_not, shape=Study_lit), size=4) +
  scale_shape_manual(values=c(20,3)) + facet_grid(~Mass_categ) +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) + xlab(Tc_min.xlab) +
  ylab("Energy expenditure (J)")
litplot
grid.text(unit(0.5,"npc"),0.99,label = "Mass in grams", gp=gpar(fontsize=20))

## With Kruger et al. 1982 data added in
litplotnew <- ggplot(litnew, aes(Temp, EE_J/Mass)) +  
  theme_bw(base_size = 20) + geom_point(aes(col=Torpid_not, shape=Study_lit), size=4) +
  scale_shape_manual(values=c(20,3)) + #facet_grid(~Mass) +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) + xlab(Tc_min.xlab) +
  ylab("Energy expenditure (J/g*hr)")
litplotnew

## Without the Unknown points from La Paz
lits <- ggplot(litnew[litnew$Torpid_not!="UK",], aes(Temp, EE_J/Mass)) +  
  theme_bw(base_size = 20) + geom_point(aes(col=Torpid_not, shape=Study_lit), size=4) +
  scale_shape_manual(values=c(20,3)) + #facet_grid(~Mass) +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) + xlab(Tc_min.xlab) +
  ylab("Energy expenditure (J/g*hr)")
lits

## Just La paz data
coir_plot <- ggplot(litstudy[litstudy$Species=="COIR",], aes(Tc_min, EE_J)) +  
  theme_bw(base_size = 20) + geom_point(aes(col=Species), size=4) + 
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) + xlab(Tc_min.xlab) +
  ylab("Energy expenditure (J)") + xlim(7,15)
coir_plot

agcu_plot <- ggplot(litstudy[litstudy$Species=="AGCU",], aes(Tc_min, EE_J)) +  
  theme_bw(base_size = 20) + geom_point(aes(col=Species), size=4) + 
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) + xlab(Tc_min.xlab) +
  ylab("Energy expenditure (J)") + xlim(7,15)
agcu_plot

hevi_plot <- ggplot(litstudy[litstudy$Species=="HEVI",], aes(Tc_min, EE_J)) +  
  theme_bw(base_size = 20) + geom_point(aes(col=Species), size=4) + 
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) + xlab(Tc_min.xlab) +
  ylab("Energy expenditure (J)") + xlim(7,15)
hevi_plot

mety_plot <- ggplot(litstudy[litstudy$Species=="METY",], aes(Tc_min, EE_J)) +  
  theme_bw(base_size = 20) + geom_point(aes(col=Species), size=4) + 
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) + xlab(Tc_min.xlab) +
  ylab("Energy expenditure (J)") + xlim(7,15)
mety_plot

grid.arrange(coir_plot, agcu_plot, mety_plot, hevi_plot, 
             nrow=2, ncol=2)
### TO DO use Hainsworth AZ birds and Lasiewski 1963 torpor cutoffs, color points by whether they're 1. presumed torpid,
## 2. presumed normothermic, and 3. the weirdos.
grid.arrange(coir_plot, agcu_plot, mety_plot, hevi_plot, 
             nrow=2, ncol=2)

## Regressing species' energy expenditure vs. Tc_min
summary(lm(EE_J~Tc_min, data=litstudy[litstudy$Species=="AGCU",]))

grid.text(unit(0.5,"npc"),0.99,label = "Mass in grams", gp=gpar(fontsize=20))

litstudy_med <- litstudy[litstudy$Mass_categ==7.5,]
litstudy_sm <- litstudy[litstudy$Mass_categ==3,]
litplot_med <- ggplot(litstudy_med, aes(Tc_min, EE_J)) +  
  theme_bw(base_size = 30) + geom_point(aes(col=Torpid_not, shape=Study_lit), size=6) +
  scale_shape_manual("Source\n", values=c(20,3), labels=c("Literature", "This Study")) +
  scale_color_brewer("Energetic state", palette="Set1", 
                     labels=c("Normothermic", "Torpid", "Shallow hypothermia?")) +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.key.height=unit(3,"line")) + xlab(Tc_min.xlab) +
  ylab("Energy expenditure (J)")
litplot_med

litplot_sm <- ggplot(litstudy_sm, aes(Tc_min, EE_J)) +  
  theme_bw(base_size = 20) + geom_point(aes(col=Species, shape=Study_lit), size=4) +
  scale_shape_manual(values=c(20,3)) +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA))
litplot_sm

####### Subsetting some columns from tor_sub and ordering them) ######
o.tor_sub <- na.omit(tor_sub[, c("Hourly", "Time", "EE_J", "BirdID","Species", "Ta_day_min", 
                                 "Ta_day_avg", "Ta_day_max", "Ta_night_min", "Tc_avg", "Tc_min")])

o.tor_sub$Hourly <- factor(o.tor_sub$Hourly, levels=o.tor_sub$Hourly)

o.tor_sub$BirdID <- factor(o.tor_sub$BirdID, 
                           levels = c("EG15_0826_AGCU", "EG15_0910_METY", "EG15_1023_AGCU",
                                      "EG15_1028_METY", "EG15_1130_METY", "EG15_1209_METY",
                                      "EG15_1211_METY","EG15_1212_METY", "EG15_1219_METY",
                                      "EG15_1220_AGCU", "EG15_1223_AGCU", "EG15_0104_AGCU"))

## Without subsetting out the METYs and AGCU's - all of La Paz data
o.tor <- na.omit(torpor2015[, c("Hourly", "Time", "EE_J", "BirdID","Species", "Ta_day_min", 
                             "Ta_day_avg", "Ta_day_max", "Ta_night_min", "Tc_avg", "Tc_min")])

o.tor$Hourly <- factor(o.tor$Hourly, levels=o.tor$Hourly)

o.tor$BirdID <- factor(o.tor$BirdID, 
                           levels = c("EG15_0826_AGCU", "EG15_0910_METY", "EG15_1023_AGCU",
                                      "EG15_1028_METY", "EG15_1130_METY", "EG15_1209_METY",
                                      "EG15_1211_METY","EG15_1212_METY", "EG15_1219_METY",
                                      "EG15_1220_AGCU", "EG15_1223_AGCU", "EG15_0104_AGCU"))

## Plotting EE per hour by time, and labeling with chamber temperature
energy_metyagcu <- ggplot(o.tor_sub, aes(Hourly, EE_J)) + theme_bw(base_size=18) +
  geom_line(aes(group=BirdID, col=Species), size=1.5) + facet_wrap(~BirdID, scales="free_x") +
  geom_point() + geom_text(aes(label=Tc_min), vjust=-1) + 
  geom_text(aes(label=Ta_day_min), col="red", vjust=1) +
  #annotate("text", x=7, y=2100, label= paste("Ta daytime min = ", o.tor_sub$Ta_day_min)) + 
  ylab("Hourly energy expenditure (J)") + scale_color_manual(values=c("#000080", "#ff0000")) +
  scale_y_continuous(breaks=c(0,100,200,300,500,1000,1500,2000))+
  theme(axis.text.x = element_text(angle=30, hjust=1), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size=.1, color="grey"),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) +
  xlab("Hour step (Birdno_ArmyTime)") # + scale_x_discrete(labels=o.tor_sub$Time)
energy_metyagcu

## just agcu
energy_metyagcu <- ggplot(o.tor_sub, aes(Hourly, EE_J)) + theme_bw(base_size=18) +
  geom_line(aes(group=BirdID, col=Species), size=1.5) + facet_wrap(~BirdID, scales="free_x") +
  geom_point() + geom_text(aes(label=Tc_min), vjust=-1) + 
  geom_text(aes(label=Ta_day_min), col="red", vjust=1) +
  #annotate("text", x=7, y=2100, label= paste("Ta daytime min = ", o.tor_sub$Ta_day_min)) + 
  ylab("Hourly energy expenditure (J)") + scale_color_manual(values=c("#000080", "#ff0000")) +
  scale_y_continuous(breaks=c(0,100,200,300,500,1000,1500,2000))+
  theme(axis.text.x = element_text(angle=30, hjust=1), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size=.1, color="grey"),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) +
  xlab("Hour step (Birdno_ArmyTime)") # + scale_x_discrete(labels=o.tor_sub$Time)
energy_metyagcu

## Plotting EE per hour by time, and labeling with chamber temperature
energy_all <- ggplot(o.tor, aes(Hourly, EE_J)) + theme_bw(base_size=18) +
  geom_line(aes(group=BirdID, col=Species), size=1.5) + facet_wrap(~BirdID, scales="free_x") +
  geom_point() + geom_text(aes(label=Tc_min), vjust=-1) + 
  geom_text(aes(label=Ta_day_min), col="red", vjust=1) +
  #annotate("text", x=7, y=2100, label= paste("Ta daytime min = ", o.tor_sub$Ta_day_min)) + 
  ylab("Hourly energy expenditure (J)") + #scale_color_manual(values=c("#000080", "#ff0000")) +
  scale_y_continuous(breaks=c(0,100,200,300,500,1000,1500,2000))+
  theme(axis.text.x = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size=.1, color="grey"),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) +
  xlab("Hour step (Birdno_ArmyTime)") # + scale_x_discrete(labels=o.tor_sub$Time)
energy_all

#Plot EE over night for agcu
energy15_agcu <- ggplot(na.omit(agcu_indiv[, c("Time", "EE_J", "BirdID")]),aes(Time, EE_J)) +
  theme_bw(base_size=30) + geom_line(aes(group=BirdID, col=BirdID), size=2) + 
  scale_color_manual(values="purple") +
  ylab("Hourly energy expenditure (J)")
energy15_agcu 

#Plot EE over night for mety
energy15_mety <- ggplot(na.omit(mety_indiv[, c("Time", "EE_J", "BirdID")]), aes(Time, EE_J)) + 
  theme_bw(base_size=30) +  geom_line(aes(group=BirdID, col = BirdID), size=2) + 
  ylab("Hourly energy expenditure (J)")
energy15_mety

## Plot NEE
energy_plot <- ggplot(m.nee, aes(Species, value)) +  theme_bw(base_size = 30) +
  geom_boxplot(size=2) + geom_point(aes(col=Species), size=6) +
  ylab("Nighttime energy expenditure (kJ)")
energy_plot



###### For later #######
## Adding column dividing NEE by 2/3*Mass to correct for mass with allometric scaling
torpor$NEE_MassCorrected<- torpor$NEE_kJ/((2/3)*torpor$Mass)

## Adding columns to correct for mass in Avg EE normo, Min EE normo, torpid, etc.
torpor$AvgEE_normo_MassCorrected <- torpor$Avg_EE_hourly_normo/((2/3)*torpor$Mass)
torpor$MinEE_normo_MassCorrected <- as.numeric(torpor$Min_EE_normo)/((2/3)*torpor$Mass)
torpor$AvgEE_torpid_MassCorrected <- torpor$Avg_EE_hourly_torpid/((2/3)*torpor$Mass)
torpor$MinEE_torpid_MassCorrected <- as.numeric(torpor$Min_EE_torpid)/((2/3)*torpor$Mass)