## 3D surface plot of daily energy expenditure vs. activity and temperature

# Sign in to plotly - like Git
library(plotly)
py <- plot_ly()
library(ggplot2)
library(rgl)

#library(rgl)

setwd("C:\\Users/shankar/Dropbox/Anusha Committee/BBLH_EnergyBudget/")
energymodels <- read.csv("Trial_EnergyBudget_models_act_thermo.csv")

energymodels2 <- read.csv("Trial_EnergyBudget_models_act_thermo_redone.csv")

dlw_bblh <- read.csv("DLW_summary.csv")

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
ggplot(NULL, aes(Site_proxy, kJ_day)) + 
  geom_boxplot(data=dlw_bblh, aes(Site_proxy, kJ_day), alpha=0.5, fill="light grey") +
  geom_point(data=energymodels2, aes(Site_proxy, kJ_day, 
                                     col=Thermoreg_scenario, shape=NEE_low_high), size=5, alpha=0.5) +  
  scale_colour_brewer(palette="Set1", guide = guide_legend(title = "Thermoregulatory \n model")) +
  scale_shape_discrete(guide=guide_legend(title="Nighttime energy expenditure")) +
  facet_grid(.~Activity_budget_type) + theme_classic(base_size = 25) + 
  scale_x_discrete(breaks=c('A','B','C','D'),
                   labels=c("HC Pre", "HC Post", "SC Pre", "SC Post")) +
  theme(panel.border = element_rect(colour = "black", fill=NA), 
        axis.text.x = element_text(angle=45, margin=margin(30,0,0,0)),
        strip.text.x = element_text(size = 20), plot.title = element_text(hjust = 0.5, size=20),
        legend.key.size = unit(1.5, 'lines'), legend.title.align=0.5) + 
  xlab("Site and Monsoon status") + ylab("Daily energy expenditure (kJ)") +
  ggtitle("Daytime activity costs Hover_Fly_Perch")

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
