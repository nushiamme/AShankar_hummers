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
require(dplyr)
#library(plotflow) #Useful function reorder_by() might be useful for ordering variables by others

## Set working directory and read in .csv file
setwd("C:\\Users\\nushi\\Dropbox\\Hummingbird energetics\\Tables_for_paper")

torpor2015 <- read.csv("Torpor_METY_AGCU_2015_detailed_hourly.csv")
litstudy <- read.csv("LitStudy_combined.csv")
litnew <- read.csv("LitStudy_andKruger2.csv")
krugertab <- read.csv("Lit_Kruger1982.csv")
k_melt <- read.csv("Lit_Kruger1982_modified.csv")
##In Jan 2018, have to check with other Kruger files that this is complete
litjan <- read.csv("LitStudy_andKruger.csv")

##Code for Anita project - 2015 and 2016 torpor datacd "E:\cd
tor_am <- read.csv("C:\\Users\\ANUSHA\\Dropbox\\Data 2015\\all_torpor_data.csv")

tor_am$NEE_MC <- tor_am$NEE/(tor_am$Av_mass^(2/3))

n_fun <- function(Species){
  return(data.frame(y = median(Species), label = paste0("n = ",length(Species)),"\n"))
}
ggplot(tor_am, aes(Species, NEE_MC))  + geom_boxplot() + geom_point(aes(col=Species)) + geom_point(aes(Species, mean(NEE_MC))) +  
  stat_summary(fun.data = n_fun, geom = "text", vjust = -2, size = 8) + my_theme
### end code for Anita project

## For Nat Geo demo
gcbnight <- read.csv("Plotting_DailyGraphs_torpor_in_R//E14_0720_GCB.csv")
gcbsumm <- read.csv("Plotting_DailyGraphs_torpor_in_R//E14_0720_GCB_summary.csv")
birdsumms <- read.csv("Plotting_DailyGraphs_torpor_in_R//E14_bird_summaries_toplot.csv")

m.krug <- melt(krugertab, id.vars = c("Species", "Sex", "Mean_mass_g", "Temp"), 
     measure.vars = c("MR_day_J_g_hr", "MR_night_J_g_hr", "MR_torpor_J_g_hr"))
names(m.krug) <- c("Species", "Sex", "Mass", "Temp", "Measure", "Value")
levels(litstudy$Torpid_not) <- c("Normothermic", "Torpid", "Unknown")

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

## USeful for introduction !!!!!!!!!!!!!!!! #####
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

#### Literature and some Lit-study plots #####
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

### Plot literature review plot study values ######
litplotstudy <- ggplot(litstudy, aes(Tc_min, EE_J)) +  
  geom_point(aes(col=Torpid_not, shape=Study_lit), size=4) +
  scale_shape_manual(values=c(3,20)) + facet_grid(~Mass_categ) + my_theme +
  scale_color_manual(values=c('black', '#ff3333', '#9999ff')) +
  theme(legend.key.height = unit(3,"line"), plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=15)) + 
  xlab(Tc_min.xlab) + ylab("Energy expenditure (J/h)") + ggtitle("Mass category")
litplotstudy

### Plot literature review plot study values, just 7.5g mass category ######
litplotstudy <- ggplot(litstudy[litstudy], aes(Tc_min, EE_J)) +  
  geom_point(aes(col=Torpid_not, shape=Study_lit), size=4) +
  scale_shape_manual(values=c(3,20)) + facet_grid(~Mass_categ) + my_theme +
  scale_color_manual(values=c('black', '#ff3333', '#9999ff')) +
  theme(legend.key.height = unit(3,"line"), plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=15)) + 
  xlab(Tc_min.xlab) + ylab("Energy expenditure (J/h)") + ggtitle("Mass category")
litplotstudy

levels(litjan$Torpid_not) <- c("Normothermic", "N_day", "Normothermic", "Torpid", "Unknown")
old.lvl<-levels(litjan$Mass_categ)
litjan$Mass_categ<-factor(litjan$Mass_categ, 
                          levels=c(sort(old.lvl[old.lvl!="20"], decreasing=F), "20"))
### Plot literature review plot study values with kruger ######
litplotstudy_jan <- ggplot(litjan[litjan$Torpid_not !="N_day" &
                                    litjan$Mass_categ%in% c("2_4", "6_8","8_10"),], aes(Temp, EE_J)) +  
  geom_point(aes(col=Torpid_not, shape=Study_lit, alpha=Study_lit), size=4) +
  scale_alpha_manual(values=c(0.3,1)) +
  scale_shape_manual(values=c(20,3)) + facet_grid(~Mass_categ) + my_theme +
  scale_color_manual(values=c('black', '#ff3333', '#0066CC','green')) +
  theme(legend.key.height = unit(3,"line"), plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=15)) + ylim(0,3300) +
  xlab(Tc_min.xlab) + ylab("Energy expenditure (J/h)") + ggtitle("Mass category (g)")
litplotstudy_jan


### Plot just literature review values with kruger, just 6-8g category ######
litplot_jan_6to8 <- ggplot(litjan[litjan$Torpid_not !="N_day" & litjan$Mass_categ=="6_8" &
                                    litjan$Study_lit=="Lit",], aes(Temp, EE_J)) +  
  geom_point(aes(col=Torpid_not), shape=20, alpha=0.5, size=4) +
  geom_smooth(data=litjan[litjan$Torpid_not=="Normothermic" & litjan$Mass_categ=="6_8" & 
                       litjan$Study_lit=="Lit",], method="lm", col="black", alpha=0.3) +
  geom_smooth(data=litjan[litjan$Torpid_not=="Torpid" & litjan$Mass_categ=="6_8" & 
                            litjan$Study_lit=="Lit" & litjan$Temp<28,], method="loess", col="black", alpha=0.3) +
  #scale_alpha_manual(values=c(0.3,1)) +
  #scale_shape_manual(values=c(20,3)) + 
  my_theme + #facet_grid(~Mass_categ) + 
  scale_color_manual(values=c('black', '#ff3333', '#9999ff')) +
  theme(legend.key.height = unit(3,"line"), plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=20)) + ylim(0,3300) +
  xlab(Tc_min.xlab) + ylab("Energy expenditure (J/h)") #+ ggtitle("Mass category (g)")
litplot_jan_6to8

### Plot literature review plot study values with kruger, just 6-8g category ######
litplotstudy_jan_6to8 <- ggplot(litjan[litjan$Torpid_not !="N_day" &
                                         litjan$Mass_categ%in% c("6_8"),], aes(Temp, EE_J)) +  
  geom_point(aes(col=Torpid_not, shape=Study_lit, alpha=Study_lit), size=4) +
  geom_smooth(data=litjan[litjan$Torpid_not=="Normothermic" & litjan$Mass_categ=="6_8" & 
                            litjan$Study_lit=="Lit",], method="lm", col="black", alpha=0.3) +
  geom_smooth(data=litjan[litjan$Torpid_not=="Torpid" & litjan$Mass_categ=="6_8" & 
                            litjan$Study_lit=="Lit" & litjan$Temp<28,], method="loess", col="black", alpha=0.3) +
  scale_alpha_manual(values=c(0.5,1)) +
  scale_shape_manual(values=c(20,3)) + my_theme + #facet_grid(~Mass_categ) + 
  scale_color_manual(values=c('black', '#ff3333', '#9999ff')) +
  theme(legend.key.height = unit(3,"line"), plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=15)) + ylim(0,3300) +
  xlab(Tc_min.xlab) + ylab("Energy expenditure (J/h)") #+ ggtitle("Mass category (g)")
litplotstudy_jan_6to8


## Subsetting just lit vals, including Kruger
litplot_jan <- ggplot(litjan[litjan$Torpid_not %in% c("Normothermic","Torpid") &
                                              litjan$Study_lit=="Lit",],
                                     aes(Temp, EE_J)) +  
  geom_point(aes(shape=Study_lit, col=Torpid_not), size=4, alpha=0.7) +
  facet_grid(~Mass_categ) + my_theme +
  scale_shape_manual(values=c(20,3)) + 
  scale_color_manual(values=c('black', '#ff3333', '#9999ff')) +
  theme(legend.key.height = unit(3,"line"), plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=15)) + 
  guides(colour = guide_legend(override.aes = list(size=4))) +
  xlab(Tc_min.xlab) + ylab("Energy expenditure (J)") + ggtitle("Mass category (g)")
litplot_jan

## Subsetting just the known values; excluding uncategorized
## Jan vals with Kruger
litplotstudy_subset_NT_jan <- ggplot(litjan[litjan$Torpid_not %in% c("Normothermic","Torpid"),],
                                 aes(Temp, EE_J)) +  
  geom_point(aes(col=Torpid_not, shape=Study_lit), size=4, alpha=0.7) +
  facet_grid(~Mass_categ) + my_theme +
  scale_shape_manual(values=c(20,3)) + 
  scale_color_manual(values=c('black', '#ff3333', '#9999ff')) +
  theme(legend.key.height = unit(3,"line"), plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=15)) + 
  xlab(Tc_min.xlab) + ylab("Energy expenditure (J)") + ggtitle("Mass category (g)")
litplotstudy_subset_NT_jan

litplotstudy_subset_NT_jan <- ggplot(litjan[litjan$Torpid_not %in% c("Normothermic","Torpid") &
                                              litjan$Mass_categ%in% c("2_4", "6_8","8_10"),],
                                     aes(Temp, EE_J)) +  
  geom_point(aes(col=Torpid_not, shape=Study_lit), size=4, alpha=0.7) +
  facet_grid(~Mass_categ) + my_theme +
  scale_shape_manual(values=c(20,3)) + 
  scale_color_manual(values=c('black', '#ff3333', '#9999ff')) +
  theme(legend.key.height = unit(3,"line"), plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=15)) + 
  xlab(Tc_min.xlab) + ylab("Energy expenditure (J)") + ggtitle("Mass category (g)")
litplotstudy_subset_NT_jan


## Subsetting three mass categories, just the known values; excluding uncategorized
litplotstudy_subset_NT <- ggplot(litstudy[litstudy$Mass_categ %in% c(3,6,7.5) &
                                         litstudy$Torpid_not %in% c("Normothermic","Torpid"),],
                              aes(Tc_min, EE_J)) +  
  geom_point(aes(col=Torpid_not, shape=Study_lit), size=4) +
  scale_shape_manual(values=c(20,3)) + facet_grid(~Mass_categ) + my_theme +
  scale_color_manual(values=c('black', '#ff3333', '#9999ff')) +
  theme(legend.key.height = unit(3,"line"), plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=15)) + 
  xlab(Tc_min.xlab) + ylab("Energy expenditure (J)") + ggtitle("Mass category")
litplotstudy_subset_NT

## Subsetting three mass categories
litplotstudy_subset <- ggplot(litstudy[litstudy$Mass_categ %in% c(3,6,7.5),], aes(Tc_min, EE_J)) +  
  geom_point(aes(col=Torpid_not, shape=Study_lit), size=4) +
  scale_shape_manual(values=c(20,3)) + facet_grid(~Mass_categ) + my_theme +
  scale_color_manual(values=c('black', '#ff3333', '#9999ff')) +
  theme(legend.key.height = unit(3,"line"), plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=15)) + 
  xlab(Tc_min.xlab) + ylab("Energy expenditure (J)") + ggtitle("Mass category")
litplotstudy_subset

## Plot just lit values
litplot <- ggplot(litstudy[litstudy$Study_lit=="Lit",], aes(Tc_min, EE_J)) +  
  my_theme + geom_point(aes(col=Torpid_not), size=3) +
  scale_color_manual(values=c('black', '#ff3333')) + 
  facet_grid(.~Mass_categ) + ylim(0,3300) +
  theme(legend.key.height = unit(3,"line"), plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=15)) +
  xlab(Tc_min.xlab) + ylab("Energy expenditure (J)") + ggtitle("Mass category")
litplot

## With Kruger et al. 1982 data added in
litplotnew <- ggplot(litnew, aes(Temp, EE_J/Mass)) +  
  theme_bw(base_size = 20) + geom_point(aes(col=Torpid_not, shape=Study_lit), size=4) +
  scale_shape_manual(values=c(20,3)) + facet_grid(~Mass) +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) + xlab(Tc_min.xlab) +
  ylab("Energy expenditure (J/g*hr)")
litplotnew

## All lit data, jut plotting torpor points
litplotnew_T <- ggplot(litnew[litnew$Torpid_not=="T",], aes(Temp, EE_J/Mass)) +  
  theme_bw(base_size = 20) + geom_point(aes(col=Mass, shape=Study_lit), size=5) +
  scale_shape_manual(values=c(20,3)) + my_theme + xlab(Tc_min.xlab) +
  scale_colour_gradientn(colours=rainbow(3)) + ggtitle("Energy expenditure in torpor") +
  ylab("Energy expenditure (J/g*hr)")
litplotnew_T

#Testing slopes
summary(lm(EE_J/Mass~Temp, data=litnew[litnew$Torpid_not=="T" & litnew$Temp > 18,]))
summary(lm(EE_J/Mass~Temp, data=litnew[litnew$Torpid_not=="T" & litnew$Temp < 18,]))

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

#### Nightly hourly EE plots #####

##### For EC2014 birds, making plots for Nat Geo demonstration ####
## Plotting EE per hour by time, and labeling with chamber temperature
energy_gcb <- ggplot(gcbnight, aes(SampleNo, EE_J)) + theme_bw() +
  geom_line(aes(group=BirdID)) + facet_grid(TimeSlot~., scales = "free_x") +
  ylab("Energy expenditure (J)")
energy_gcb

## Short code to produce and save multiple plots from one ggplot snippet, by factor TimeSlot
tryplot <- ggplot(data = gcbnight, aes(SampleNo, EE_J)) + theme_bw() +
  geom_line() +  ylab("Energy expenditure (J)") + ylim(-5,50) + xlab("Time (seconds)") +
  scale_y_continuous(breaks=c(0,2,5,10,20,30,40,50)) + theme(panel.grid.major.y = element_line(size=.1, color="grey"))

plotbunch_gcb <- gcbnight %>%
  group_by(TimeSlot) %>%
  do(plots = tryplot %+% . + facet_wrap(~TimeSlot))
plotbunch_gcb$plots[1]

gcbplots <- ggplot(data = gcbnight, aes(SampleNo, EE_J)) + theme_bw() +
  geom_line() +  ylab("Energy expenditure (J)") + ylim(-5,50) + xlab("Time (seconds)") +
  theme(panel.grid.major.y = element_line(size=.1, color="grey"))
gcbplots

plotbunch_gcb <- gcbnight %>%
  group_by(TimeSlot) %>%
  do(plots = gcbplots %+% . + facet_wrap(~TimeSlot))
pdf("EC14_GCB_0720.pdf", width=10, height = 7)
plotbunch_gcb$plots
dev.off()

Two_gcb_slots <- gcbnight[gcbnight$TimeSlot==9,] # | gcbnight$TimeSlot==5 | gcbnight$TimeSlot==7,]
m.gcbslots <- melt(Two_gcb_slots, id.vars = c("BirdID", "TimeSlot"), measure.vars = "EE_J")
m.gcbslots$TimeSlot <- as.factor(m.gcbslots$TimeSlot)
gcbfacet <- ggplot(m.gcbslots, aes(x = seq(1, length(m.gcbslots$value)), value)) + 
  my_theme +
  geom_line(col="red") +  ylab("Energy expenditure (J)") + ylim(-5,50) + xlab("Time (seconds)")
gcbfacet

gcbsumm$Hour <- factor(gcbsumm$Hour, levels=gcbsumm$Hour)

gcbsummplot <- ggplot(gcbsumm, aes(HourID, EE_J)) + my_theme + geom_point(size=3) + geom_line(aes(group="identity")) + 
  ylab("Energy expenditure (J)") + xlab("Hour") + scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks=c(-100,0,100,200,500,1000,1500)) + 
  theme(panel.grid.major.y = element_line(size=.1, color="grey"))
gcbsummplot

birdsumms$BirdID <- factor(birdsumms$BirdID, levels=birdsumms$BirdID)

birdsummsplot <- ggplot(birdsumms, aes(HourID, EE_J)) + my_theme + 
  geom_point(size=3) + geom_line(aes(group="identity")) + 
  ylab("Energy expenditure (J)") + xlab("Hour") + ylim(-10,2800) + scale_x_continuous(breaks = 1:11)

birdbunch <- birdsumms %>%
  group_by(BirdID) %>%
  do(plots = birdsummsplot %+% . + facet_wrap(~BirdID))
pdf("EC14_birdsumms.pdf", width=10, height = 7)
birdbunch$plots
dev.off()

### For La Paz birds ####
lapaz_hourly <- ggplot(data = o.tor, aes(Hourly, EE_J)) + theme_bw() + geom_line(aes(group="identity")) +
  geom_point() +  ylab("Energy expenditure (J)") + xlab("Hour") #+ ylim(-10,3000)

plotbunch_lapaz <- o.tor %>%
  group_by(BirdID) %>%
  do(plots = lapaz_hourly %+% . + facet_wrap(~BirdID))
pdf("LaPaz_plotbunch.pdf", width=10, height = 7)
plotbunch_lapaz$plots
dev.off()


## Plotting EE per hour by time, and labeling with chamber temperature
energy_metyagcu <- ggplot(o.tor_sub, aes(Hourly, EE_J)) + theme_bw(base_size=18) +
  geom_line(aes(group=BirdID, col=Species), size=1.5) + facet_wrap(~BirdID, scales="free_x") +
  geom_point() + geom_text(aes(label=Tc_min), vjust=-1) + 
  #geom_text(aes(label=Ta_day_min), col="red", vjust=1) +
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

#For Nat His talk
energy_mety <- ggplot(o.tor_sub[o.tor_sub$Species=="METY",], aes(Hourly, EE_J)) + theme_bw(base_size=18) +
  geom_line(aes(group=BirdID, col=Species), size=1.5) + facet_wrap(~BirdID, scales="free_x") +
  geom_point() + #geom_text(aes(label=Tc_min), vjust=-1) + 
  #geom_text(aes(label=Ta_day_min), col="red", vjust=1) +
  #annotate("text", x=7, y=2100, label= paste("Ta daytime min = ", o.tor_sub$Ta_day_min)) + 
  ylab("Hourly energy expenditure (J)") + scale_color_manual(values=c("#000080", "#ff0000")) +
  scale_y_continuous(breaks=c(0,100,200,300,500,1000,1500,2000))+
  theme(axis.text.x = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size=.1, color="grey"),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(), strip.text = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) +
  xlab("Nighttime Hour") # + scale_x_discrete(labels=o.tor_sub$Time)
energy_mety

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