## 3D surface plot of daily energy expenditure vs. activity and temperature

# Sign in to plotly - like Git
library(plotly)
library(ggplot2)
library(rgl)
library(gridExtra)

#library(rgl)

setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Tables")## laptop
#energymodels <- read.csv("Trial_EnergyBudget_models_act_thermo.csv")

energymodels2 <- read.csv("Trial_EnergyBudget_models_act_thermo_redone.csv", sep=";")

dlw_bblh <- read.csv("DLW_summary.csv")

#### General functions ####
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 15) + 
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


#### New data frames or vectors ####
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

#### plots ####
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
dlw_plot <- ggplot(data=dlw_bblh,aes(Site_proxy, kJ_day)) + my_theme +
  geom_boxplot(alpha=0.5, fill="light grey") + 
  scale_x_discrete(breaks=c('A', 'B', 'C', 'D'), 
                   labels=c("Harshaw Pre", "Harshaw Post", "Sonoita Pre", "Sonoita Post")) +
  ylim(9, 41) + my_theme + theme(legend.key.size = unit(2, 'lines')) + xlab("Site and monsoon status") + 
  ylab("Daily Energy Expenditure (kJ)")

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

## Just DLW boxplots and points with recap individuals colored for Figure 2 (as of April 3, 2017)
ggplot(dlw_bblh, aes(Site_proxy, kJ_day)) + 
  geom_boxplot(alpha=0.5, fill="light grey") +
  geom_point(aes(col=Band_no, size=Band_no), alpha=0.7) + 
  theme_classic(base_size = 25) + 
  scale_colour_manual(values=c("black", "red", "green", "purple")) +
  scale_size_manual(values=c(2, 4, 4, 4)) +
  scale_x_discrete(breaks=c('A','B','C','D'),
                   labels=c("Harshaw Pre", "Harshaw Post", "Sonoita Pre", "Sonoita Post")) +
  stat_summary(fun.data = give.n, geom = "text", hjust=-1.5, vjust=-2.5, size=5) +
  theme(panel.border = element_rect(colour = "black", fill=NA), 
        axis.text.x = element_text(margin=margin(30,0,0,0), hjust=0.75),
        strip.text.x = element_text(size = 20), legend.position = 'none') + 
  xlab("Site and monsoon status") + 
  ylab("24-hour energy expenditure (kJ)")

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
