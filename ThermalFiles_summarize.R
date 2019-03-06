## Processing thermal images and video from May-Jun 2018, Southwestern Research Station
## Code Author: Anusha Shankar; started July 2018

#library(here)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(viridis) # Colors
library(scales) # for stacked bar as percentages
library(lme4) # Running multilevel mixed models
library(lattice) ## qqplot to look at lmer model residuals
library(lmerTest)
library(emmeans)
library(MASS) ## T check the distribution of the data
library(car) ## To check the distribution of the data
library(changepoints.np) ## Not YET installed; for automating change points
#library(arm) ## Std errors from random effects in lmer models

wd <- file.path("E:", "Google Drive", "IR_2018_csv")
setwd(wd)
thermal_maxes_melted <- read.csv("E:\\Google Drive\\IR_2018_csv\\Thermal_maxes_all_Oct.csv")
#thermal_maxes_melted <- read.csv("E:\\Google Drive\\IR_2018_csv\\Melted_thermal_maxes_all.csv")
categories <- read.csv("Category_thresholds.csv")
datatry <- read.csv("Interpolated_Thermal.csv")
categ_percentage <- read.csv("Category_percentages.csv")
masses <- read.csv("Bird_masses.csv")
thermal_maxes_melted$Category <- factor(thermal_maxes_melted$Category, levels = c("Normothermic", "Shallow Torpor", "Transition", "Deep Torpor"))
#thermal_maxes_melted$variable <- gsub('MA', 'RI', thermal_maxes_melted$variable) ## Changing species code for RIHU from MAHU to RIHU
#thermal_maxes_melted$Species <- gsub('MA', 'RI', thermal_maxes_melted$Species)
masses$Indiv_ID <- gsub('MA', 'RI', masses$Indiv_ID) ## Changing species code for RIHU from MAHU to RIHU
masses$Species <- gsub('MA', 'RI', masses$Species)
categories$Individual <- gsub('MA', 'RI', categories$Individual) ## Changing species code for RIHU from MAHU to RIHU
categories$Species <- gsub('MA', 'RI', categories$Species)


#### TRY THIS for automated change points #####
cpt.np(test.int, method='PELT', minseglen=1,nquantiles =8*log(length(test.int)))

## Only read this in if categories or out_all files change
#thermal_maxes_NoCateg <- read.csv("E:\\Google Drive\\IR_2018_csv\\Melted_thermal_maxes_all_Oct.csv")

#bird.folders <- list.dirs(wd, recursive=T)[-1]

## Generic plot theme
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 30) + 
  theme(axis.line = element_line(colour = "black"),
        text=element_text(family="Cambria"))


## Axis labels
Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))

bird.folders.2018 <- c("BCHU01_0521", "BCHU02_0526", "BCHU03_0530", "BCHU04_0607", "BCHU05_0607",
                  "BLHU01_0521", "BLHU03_0522", "BLHU04_0523", "BLHU05_0523", "BLHU06_0526", "BLHU07_0529", "BLHU08_0601", 
                  "BLHU09_0603", "BLHU12_0605", "BLHU13_0605", 
                  "MAHU02_0520", "MAHU03_0527", "MAHU05_0529", "MAHU06_0530", "MAHU10_0603", "MAHU12_0606", "MAHU13_0606")

bird.folders.2017 <- c("BC01_0610", "BC02_0612", "BC03_0617",
                  "BL01_0610", "BL02_0612", "BL03_0614", "BL04_0615",
                  "MA02_0611", "MA05_0615", "MA06_0616", "MA07_0617", "MA08_0619")

bird.folders.all <- c("BCHU01_0521", "BCHU02_0526", "BCHU03_0530", "BCHU04_0607", #"BCHU05_0607",
                      "BLHU01_0521", "BLHU03_0522", "BLHU04_0523", "BLHU05_0523", "BLHU06_0526", "BLHU07_0529", "BLHU08_0601", 
                      "BLHU09_0603", "BLHU12_0605", "BLHU13_0605", 
                      "MAHU02_0520", "MAHU03_0527", "MAHU05_0529", "MAHU06_0530", "MAHU10_0603", "MAHU12_0606", "MAHU13_0606",
                      "BC01_0610", "BC02_0612", "BC03_0617",
                      "BL01_0610", "BL02_0612", "BL03_0614", "BL04_0615",
                      "MA02_0611", "MA05_0615", "MA06_0616", "MA07_0617", "MA08_0619")

single <- "MAHU10_0603"

#### Do not run again unless base thermal files change ####

for(i in bird.folders.all) {
setwd(paste0(wd, "/", i))


#### Compile csv's and process ####
## Using plyr
paths <- dir(pattern = "\\.csv$")
names(paths) <- basename(paths)

ThermFiles <- lapply(paths, read.csv, header=F)

### Creating a summary data frame of 
# Can also create automatic lists of summaries: lapply(ThermFiles_na_omit[[i]], summary)
Thermsumm <- data.frame(matrix(NA, nrow=length(ThermFiles), ncol=5))
names(Thermsumm) <- c("Min", "Mean", "Max", "File") #,"sd"
Thermsumm$File <- noquote(names(ThermFiles))
for(i in 1:length(ThermFiles)) { 
  ThermFiles_na_omit <- vector("list",length(ThermFiles))
  names(ThermFiles_na_omit) <- names(ThermFiles)
  ThermFiles_na_omit[[i]]<- na.omit(data.frame(stack(ThermFiles[[i]][1:ncol(ThermFiles[[i]])])))[1]
  Thermsumm[i,1] <- min(ThermFiles_na_omit[[i]]$values)
  Thermsumm[i,2] <- mean(ThermFiles_na_omit[[i]]$values)
  Thermsumm[i,3] <- max(ThermFiles_na_omit[[i]]$values)
  #Thermsumm[i,4] <- sd(ThermFiles_na_omit[[i]]$values)
  ## Splitting the file name to get IndivID, date, and time
  Thermsumm$Indiv_ID <- 
    unlist(lapply(strsplit(as.character(Thermsumm$File), "\\_"), "[", 1))
  Thermsumm$Date <- 
    unlist(lapply(strsplit(as.character(Thermsumm$File), "\\_"), "[", 2))
  Thermsumm$Time <- 
    unlist(lapply(strsplit(as.character(Thermsumm$File), "\\_"), "[", 3))
  Thermsumm$Time <- 
    unlist(lapply(strsplit(as.character(Thermsumm$Time), "\\."), "[", 1))
  m.thermsumm <- melt(Thermsumm, id.vars=c("Indiv_ID", "Date", "Time"), 
                    measure.vars = c("Min", "Mean","Max")) #, "sd"
  m.thermsumm$Hour <- substr(m.thermsumm$Time,1,2)
  m.thermsumm$Hour <- factor(m.thermsumm$Hour, 
                            levels= c("19", "20", "21", "22", "23", "24", "01", "02",
                                      "03", "04", "05", "06"), ordered=T)
  file.name <- paste(m.thermsumm$Indiv_ID[1], "_", m.thermsumm$Date[1], "_summ.rds", sep="")
  saveRDS(m.thermsumm,file.name)
  }
}

## Compiling all the RDS files into a single list, so I can summarize the temperatures
all_thermal <- data.frame(matrix(ncol = length(bird.folders.all), nrow=120))
colnames(all_thermal) <- bird.folders.all
all_amb <- data.frame(matrix(ncol = length(bird.folders.all), nrow=120))
colnames(all_amb) <- bird.folders.all
#all_thermal <- ls()

## Collating all the birds' max temps into one object, and min temps into another
for(i in bird.folders.all) {
  setwd(paste0(wd, "/", i))
  out<- readRDS(file=paste(i, "_summ.rds", sep=""))
  var1 <- out$value[out$variable=="Max"]
  n <- length(var1)
  all_thermal[[paste(i)]] <- c(var1, rep(NA,120-n))
  
  
  var2 <- out$value[out$variable=="Min"]
  n <- length(var2)
  all_amb[[paste(i)]] <- c(var2, rep(NA,120-n))
}

m.all_thermal <- melt(all_thermal, na.rm=T)
setwd("E:/Google Drive/IR_2018_csv")
write.csv(m.all_thermal,file = "Melted_thermal_maxes2.csv")
m.all_amb <- melt(all_amb,na.rm=T)

#### DON'T USE - old ####
## To plot amb vs. Ts for all birds
surface_amb <- data.frame(matrix(ncol = 2*length(bird.folders.all), nrow=120))
colnames(surface_amb) <- bird.folders.all
colnames(surface_amb)[35:68] <- paste(bird.folders.all, "_amb", sep="")
rownames(surface_amb) <- as.numeric(seq(1:120))

surface <- data.frame(matrix(ncol = length(bird.folders.all), nrow=120))
colnames(surface) <- bird.folders.all
rownames(surface) <- as.numeric(seq(1:120))
amb <- data.frame(matrix(ncol = length(bird.folders.all), nrow=120))
colnames(amb) <- paste(bird.folders.all, "_amb", sep="")
rownames(amb) <- as.numeric(seq(1:120))


for(i in bird.folders.all) {
  setwd(paste0(wd, "/", i))
  out<- readRDS(file=paste(i, "_summ.rds", sep=""))
  var1 <- out$value[out$variable=="Max"]
  n <- length(var1)
  surface[[paste(i)]] <- c(var1, rep(NA,120-n))
  
  var2 <- out$value[out$variable=="Min"]
  n <- length(var2)
  amb[[paste(i, "_amb", sep="")]] <- c(var2, rep(NA,120-n))
}
surface_amb <- merge(surface, amb, by=0, all=TRUE)
surface_amb <- surface_amb[order(as.numeric(as.character(surface_amb$Row.names))),]
row.names(surface_amb) <- surface_amb$Row.names
surface_amb <- subset(surface_amb, select = -Row.names)
head(surface_amb)
#### Don't use before this ####


## Stacking all the individual birds' data, keeping all melted columns from earlier (hour, min, max, etc.)
out_all <- data.frame(matrix(ncol = 6, nrow=109*length(bird.folders.all)))
names(out_all) <- c("Indiv_ID", "Date", "Time", "variable", "value", "Hour")
for(i in bird.folders.all) {
  setwd(paste0(wd, "/", i))
  out<- readRDS(file=paste(i, "_summ.rds", sep=""))
  out_all <- rbind(out,out_all)
}
dim(out_all) ## Check dimensions
out_all <- out_all[complete.cases(out_all),] ## Remove rows with NAs
dim(out_all) ## Check dimensions
out_amb <- out_all[out_all$variable=="Min",] ## Make a separate data frame with just minimum (~= ambient) values
#out_mean <- out_all[out_all$variable=="Mean",] ## Make a separate data frame with just mean Ts values
out_max <- out_all[out_all$variable=="Max",] ## Make a separate data frame with just maximum (~= surface) values
out_full <- merge(out_amb,out_max, by = c("Indiv_ID", "Date", "Time", "Hour")) ## Merge the two
out_full <- subset(out_full, select = -c(variable.x, variable.y)) ## Remove unnecessary columns
names(out_full) <- c("Indiv_ID", "Date", "Time", "Hour", "Amb_Temp", "Surf_Temp")
out_full$Year <- 0 ## Making a year column to make Indiv_ID in out_full match individual column in categories DF
head(out_full)
out_full$pasted <- paste(out_full$Indiv_ID, "_", out_full$Date, sep="")
out_full$Year[which(!is.na(match(out_full$pasted,bird.folders.2017)))] <- 17
out_full$Year[which(!is.na(match(out_full$pasted,bird.folders.2018)))] <- 18
out_full$Indiv_ID <- lapply(out_full$Indiv_ID, function(x) {
  gsub("BC0", "BCHU0", x)
})
out_full$Indiv_ID <- lapply(out_full$Indiv_ID, function(x) {
  gsub("BL0", "BLHU0", x)
})
out_full$Indiv_ID <- lapply(out_full$Indiv_ID, function(x) {
  gsub("MA0", "MAHU0", x)
})
out_full$pasted <- paste(out_full$Indiv_ID, "_", out_full$Date, out_full$Year, sep="")
out_full$pasted <- gsub('MA', 'RI', out_full$pasted) ## Changing species code for RIHU from MAHU to RIHU
head(out_full)
#out_full <- out_full[out_full$pasted != "BCHU05_060718",]

## Loops to fill in a "Category" column in the out_full dataset so that each surface temperature is 
## assigned a category according to individual thresholds laid out in the categories DF

out_full$Category <- 0

for(i in 1:nrow(out_full)) {
    categ <- categories[categories$Individual==out_full$pasted[i],]
    if(out_full$Surf_Temp[i] > categ$Normo_min) {
      out_full$Category[i] <- "Normothermic"
    } else if(!is.na(categ$Shallow_min) & out_full$Surf_Temp[i] > categ$Shallow_min) {
      out_full$Category[i] <- "Shallow Torpor"
    } else if(is.na(categ$Shallow_min) & !is.na(categ$Shallow_max) & out_full$Surf_Temp[i] < categ$Shallow_max) {
      out_full$Category[i] <- "Shallow Torpor"
    } else if(!is.na(categ$Transition_min) & out_full$Surf_Temp[i] > categ$Transition_min) {
      out_full$Category[i] <- "Transition"
    } else if(is.na(categ$Transition_min) & !is.na(categ$Transition_max) & out_full$Surf_Temp[i] < categ$Transition_max) {
      out_full$Category[i] <- "Transition"
    } else if(!is.na(categ$Torpor_max) & out_full$Surf_Temp[i] < categ$Torpor_max) {
      out_full$Category[i] <- "Deep Torpor"
    }
}

#### Ignore ####
thermal_maxes_NoCateg$Category <- 0

for(i in 1:nrow(thermal_maxes_NoCateg)) {
  categ <- categories[categories$Individual==thermal_maxes_NoCateg$Indiv_ID[i],]
  if(thermal_maxes_NoCateg$value[i] > categ$Normo_min) {
    thermal_maxes_NoCateg$Category[i] <- "Normothermic"
  } else if(!is.na(categ$Shallow_min) & thermal_maxes_NoCateg$value[i] > categ$Shallow_min) {
    thermal_maxes_NoCateg$Category[i] <- "Shallow Torpor"
  } else if(is.na(categ$Shallow_min) & !is.na(categ$Shallow_max) & thermal_maxes_NoCateg$value[i] < categ$Shallow_max) {
    thermal_maxes_NoCateg$Category[i] <- "Shallow Torpor"
  } else if(!is.na(categ$Transition_min) & thermal_maxes_NoCateg$value[i] > categ$Transition_min) {
    thermal_maxes_NoCateg$Category[i] <- "Transition"
  } else if(is.na(categ$Transition_min) & !is.na(categ$Transition_max) & thermal_maxes_NoCateg$value[i] < categ$Transition_max) {
    thermal_maxes_NoCateg$Category[i] <- "Transition"
  } else if(!is.na(categ$Torpor_max) & thermal_maxes_NoCateg$value[i] < categ$Torpor_max) {
    thermal_maxes_NoCateg$Category[i] <- "Deep Torpor"
  }
}
write.csv(thermal_maxes_NoCateg, "Thermal_maxes_all_Oct.csv")
#### Ignore before this ####

## Add a column for capture masses
out_full$Cap_mass <- 0

for(i in 1:nrow(out_full)) {
    out_full$Cap_mass[i] <- masses$Capture_mass_g[masses$Indiv_ID==out_full$pasted[i]]
}

## Running an ancova on Surface ~ Ambient temperature
out_full$Indiv_numeric <- cumsum(!duplicated(out_full$pasted)) ## Making individual column numeric for the ancova, but this turns out to be unnecessary
out_full$Species <- substr(out_full$Indiv_ID, 1, 4) ## Making a species column
out_full$Species_numeric <- cumsum(!duplicated(out_full$Species))

## Ignore for now
out_full$Category_Traditional <- out_full$Category
out_full$Category_Traditional[out_full$Category=="Shallow"] <- "Normothermic"
out_full$Category_Traditional2 <- out_full$Category
out_full$Category_Traditional2[out_full$Category=="Shallow"] <- "Torpor"

#### GOOD interpolate function RERUN with new CATEGORIES for BLHU04..17 ####

datatry <- data.frame()

for(j in 1:length(unique(out_full$pasted))) {
  for(i in unique(out_full$pasted)) {  
  ## Create trial column
    trial <- out_full[out_full$pasted==i,]
    trial$Time <- as.numeric(as.character(trial$Time))
    times <- c(seq(1930,2400,1), seq(100,530,1))
    sur_temps <- trial$Surf_Temp
    amb_temps <- trial$Amb_Temp
    #temps <- as.data.frame(trial$value)
    names(sur_temps) <- "Surf_Temp"
    names(amb_temps) <- "Amb_Temp"
    time2 <- trial$Time
    
    ## Interpolate missing surface temperatures
    sfun <- approxfun(time2, sur_temps, rule = 2)
    
    ## Interpolate missing ambient temperatures
    afun <- approxfun(time2, amb_temps, rule = 2)
    
    ## Ordering time in the interpolated data frame
    TimeOrder1 <- seq(from = 1900, to = 2459, by = 1) ## Create seq for first half of night
    TimeOrder2 <- seq(from = 100, to = 559, by = 1) ## Seq for second half of night
    TimeOrder <- c(TimeOrder1, paste0("0", TimeOrder2)) ## Combine them, making all same length
    TimeOrder <- factor(TimeOrder, as.character(TimeOrder)) ## Make into a factor in the correct order
    
    Time_unordered<- as.factor(format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 min"),"%H%M", tz="GMT")) ## Make minute-by-minute to compare
    
    #TimeFinal <- droplevels(na.omit(TimeOrder[match(Time_unordered, TimeOrder,nomatch=NA)])) ## Final time variable
    birdTime <- as.character(times) ## Save the interpolated time as a separate vector
    birdTime <- str_pad(birdTime, width=4, side="left", pad="0") ## Make sure all times are 4 characters long, otherwise pad in front with zero
    ## Compile interpolated df
    interTemp <- data.frame(Indiv_pasted = i,
                            Surf_Temp = sfun(times),
                            Amb_Temp = afun(times),
                            Cap_mass = 0)
    
    interTemp$Category <- 0
    for(k in 1:nrow(interTemp)) {
        categ <- categories[categories$Individual==i,]
        if(interTemp$Surf_Temp[k] > categ$Normo_min) {
          interTemp$Category[k] <- "Normothermic"
        } else if(!is.na(categ$Shallow_min) & interTemp$Surf_Temp[k] > categ$Shallow_min) {
          interTemp$Category[k] <- "Shallow"
        } else if(is.na(categ$Shallow_min) & !is.na(categ$Shallow_max) & interTemp$Surf_Temp[k] < categ$Shallow_max) {
          interTemp$Category[k] <- "Shallow"
        } else if(!is.na(categ$Transition_min) & interTemp$Surf_Temp[k] > categ$Transition_min) {
          interTemp$Category[k] <- "Transition"
        } else if(is.na(categ$Transition_min) & !is.na(categ$Transition_max) & interTemp$Surf_Temp[k] < categ$Transition_max) {
          interTemp$Category[k] <- "Transition"
        } else if(!is.na(categ$Torpor_max) & interTemp$Surf_Temp[k] < categ$Torpor_max) {
          interTemp$Category[k] <- "Torpor"
        }
        interTemp$Cap_mass[k] <- masses$Capture_mass_g[masses$Indiv_ID==interTemp$Indiv_pasted[k]]
      }
    interTemp$Time <- TimeOrder[match(birdTime,TimeOrder,nomatch=NA)] ## Match times to correct ordered time and save into new column in interpolated dataset
    interTemp$Species <- substr(interTemp$Indiv_pasted, 1, 4)
    datatry <- rbind(datatry, interTemp) # add it to your df
    }
}

## Check if there's any NAs. If sfun and afun have rule=1, yields NAs
sum(is.na(datatry$Amb_Temp)) #Good if output is 0
sum(is.na(datatry$Surf_Temp)) #Good if output is 0
## Add a column for capture masses
#for(i in 1:nrow(datatry)) {
#    datatry$Cap_mass[i] <- masses$Capture_mass_g[masses$Indiv_ID==datatry$Indiv_pasted[i]]
#}

write.csv(datatry, file = "E:\\Google Drive\\IR_2018_csv\\Interpolated_Thermal.csv")

####Don't rerun before this unless you need to interpolate again ####

#datatry <- dplyr::bind_rows(datalist) ## Would be faster if I figure out how to do make a list in the loop and compile here.

## Summarize proportion of time spent in each state, by species
#data_species_summ <- datatry %>% 
 # group_by(Category, Species) %>%
  #summarise(prop_values = (length(Category)/length(datatry$Surf_Temp))*100)
#data_species_summ

data_species_summ <- datatry %>%
  count(Species, Category) %>%
  group_by(Species) %>%
  mutate(perc = (n / sum(n))*100)

## By individual
data_indiv_summ <- datatry %>%
  group_by(Indiv_pasted, Category) %>%
  summarise (n = length(Category))
data_indiv_summ

## Summarize (binary) whether individuals used a particular category or not
indiv_categ_count <- ddply(data_indiv_summ, c("Indiv_pasted"), summarise, 
                   Normo=sum(Category=="Normothermic"), Shallow=sum(Category=="Shallow"),
                   Transition=sum(Category=="Transition"), Torpor=sum(Category=="Torpor"))
sum(indiv_categ_count$Normo) #no. indivs that used normo; should be all (33)
sum(indiv_categ_count$Shallow) #no. indivs that used shallow
sum(indiv_categ_count$Transition) #no. indivs that used transition
sum(indiv_categ_count$Torpor) #no. indivs that used deep torpor

# Summarise number of records per individual in each category. Using interpolated data
casted_indiv <-dcast(Indiv_pasted~Category,data=datatry, fun.aggregate= length,value.var = 'Category')
prop_indiv_time <- data.frame(matrix(ncol = 5, nrow=nrow(casted_indiv)))
names(prop_indiv_time) <- c("Indiv_pasted", "Normothermic", "Shallow", "Transition", "Torpor")
prop_indiv_time$Indiv_pasted <- casted_indiv$Indiv_pasted
for(i in 1:nrow(casted_indiv)) {
  prop_indiv_time$Normothermic[i] <- round((casted_indiv$Normothermic[i]/(sum(casted_indiv$Normothermic[i], 
                                                                  casted_indiv$Shallow[i], casted_indiv$Transition[i],
                                                                  casted_indiv$Torpor[i])))*100,0) 
  prop_indiv_time$Shallow[i] <- round((casted_indiv$Shallow[i]/(sum(casted_indiv$Normothermic[i], 
                                                                   casted_indiv$Shallow[i], casted_indiv$Transition[i],
                                                                   casted_indiv$Torpor[i])))*100,0) 
  prop_indiv_time$Transition[i] <- round((casted_indiv$Transition[i]/(sum(casted_indiv$Normothermic[i], 
                                                                   casted_indiv$Shallow[i], casted_indiv$Transition[i],
                                                                   casted_indiv$Torpor[i])))*100,0) 
  prop_indiv_time$Torpor[i] <- round((casted_indiv$Torpor[i]/(sum(casted_indiv$Normothermic[i], 
                                                                   casted_indiv$Shallow[i], casted_indiv$Transition[i],
                                                                  casted_indiv$Torpor[i])))*100,0)
}
prop_indiv_time

## Melted dataframe for proportion of time spent in diff categories by species. Uses interpolated data
m.prop <- melt(prop_indiv_time, id.vars = "Indiv_pasted", measure.vars = c("Normothermic", "Shallow", "Transition", "Torpor"))
tail(m.prop)
names(m.prop)[names(m.prop) == 'value'] <- 'freq'
m.prop$Species <- substr(m.prop$Indiv_pasted, 1, 4)
m.prop$Species <- as.factor(as.character(m.prop$Species))

## Not using individual-level models, doesn't make any sense to.
#Trying to test how species are different, not individuals
#mod_glm_freq <- glmer(freq~variable*Species + (1|Indiv_pasted), data=m.prop, family=poisson())
#mod_glm_freq1 <- glmer(freq~variable*Species + (variable|Indiv_pasted), data=m.prop, family=poisson())

## USE THIS
mod_glm_freq_sp <- glm(freq~variable*Species-1, data=m.prop, family=poisson())
summary(mod_glm_freq_sp)
coef(mod_glm_freq_sp)

## Because residual variance >> degrees of freedom, trying a quasipoisson
## But the dispersion parameter is still 12.5, which is much greater than 1, meaning it's overdispersed
mod_glm_freq_sp_quasi <- glm(freq~variable*Species-1, data=m.prop, family=quasipoisson())
summary(mod_glm_freq_sp_quasi)

## Running  a negative binomial model, definitely the best. No overdispserion now.
mod_glm_freq_sp_nb <- glm.nb(freq~variable*Species-1, data=m.prop)
summary(mod_glm_freq_sp_nb)


## Don't use this, it doesn't make any sense to
mod_glm_freq_Categ <- glm(freq~variable-1, data=m.prop, family=poisson())
summary(mod_glm_freq_Categ)
coef(mod_glm_freq_Categ)

m.prop$predicted <- predict(mod_glm_freq_sp)
plot(mod_glm_freq_sp)
#aov(mod_glm_freq_sp, mod_glm_freq_Categ)

ggplot(m.prop, aes(Species,predicted)) + my_theme + geom_bar(aes(fill=variable), position = "fill", stat="identity") +
  #facet_grid(.~Species, scales = "free_x",space = "free_x") +
  xlab("Species") + ylab("Percentages") +
  scale_fill_manual(values=my_colors2, name="Category") +
  scale_y_continuous(labels = percent_format()) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(legend.key.height = unit(3, 'lines'))

mod_glm_freq1<- glm(freq~variable+Species, data=m.prop, family=poisson())

summary(mod_glm_freq)
coef(mod_glm_freq)
anova(mod_glm_freq)

#### Models for proportion of time spent in different categories
datatry$Category <- factor(datatry$Category, levels = c("Normothermic", "Shallow", "Transition", "Torpor"))
my_colors2 <- c("#23988aff", "#F38BA8", "#440558ff", "#9ed93aff")
ggplot(datatry, aes(Indiv_pasted, Surf_Temp)) + my_theme + geom_point(aes(col=Category), alpha=0.8) +  
  facet_grid(.~Species, scales = "free_x",space = "free_x") +
  ylab(Temp.lab) + xlab("Individual") + 
  #scale_color_manual(values = c('black','deepskyblue2', 'palegreen4', 'red')) +
  scale_color_manual(values=my_colors2) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5), axis.text.y=element_text(size=15),
        legend.key.height = unit(3, 'lines'))

m.categ <- melt(categ_percentage, id.vars="Species", measure.vars = c("Normothermic", "Shallow_torpor", "Transition", "Torpor"))
m.categ$variable <- revalue(m.categ$variable, c("Shallow_torpor"="Shallow Torpor", "Torpor"="Deep Torpor"))

## Stacked bars for proportion of time spent in each category per species
ggplot(m.categ, aes(Species,value)) + my_theme + geom_bar(aes(fill=variable), position = "fill", stat="identity") +
  #facet_grid(.~Species, scales = "free_x",space = "free_x") +
  xlab("Species") + ylab("Percentages") +
  scale_fill_manual(values=my_colors2, name="Category") +
  scale_y_continuous(labels = percent_format()) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(legend.key.height = unit(3, 'lines'))

## IGNORE
## To compare interpolated data with test data:
#trial$Time3 <- TimeOrder[match(str_pad(as.character(trial$Time, width=4, side="left", pad="0"),TimeOrder,nomatch=NA)] 
## Plot interpolated data
ggplot(interTemp, aes(Time2, Temp)) + 
  geom_point(alpha = 0.2, col = "red") +
  ggtitle("Temperature time") +
  my_theme + 
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5))


out_full$Category <- as.factor(as.character(out_full$Category))

#### Models ####
# Checking the distribution of the data
out_full$Surf_Temp_test <- out_full$Surf_Temp + 1
qqp(out_full$Surf_Temp_test, "norm") ## SO not normal
qqp(out_full$Surf_Temp_test, "lnorm") ## VERY not log normal either
nbinom <- fitdistr(out_full$Surf_Temp, "Negative Binomial")
qqp(out_full$Surf_Temp, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
poisson <- fitdistr(out_full$Surf_Temp_test, "Poisson")
qqp(out_full$Surf_Temp_test, "pois", poisson$estimate)

ggplot(out_full, aes(pasted, Surf_Temp)) + geom_boxplot() + facet_grid(.~Category, scales = "free_x") + my_theme + theme(axis.text.x = element_text(angle=90))

## Nov 4, 2018. Trying out a multilevel model with random intercepts and fixed slope
mod_mixed <- lmer(Surf_Temp ~ Amb_Temp + (1|Category), data=out_full)
summary(mod_mixed)
coef(mod_mixed) ## Very nice, to see slopes and intercepts
plot(mod_mixed)
plot(ranef(mod_mixed)) ## plotting random effects of model
plot(residuals(mod_mixed)) ## plot residuals of model

## Ignore
mod_categ <- lmer(Surf_Temp~ (Amb_Temp|Category), data=out_full)
summary(mod_categ)
plot(mod_categ)
coef(mod_categ)

## Random intercepts and random slopes, no species in this equation
mod_mixed_2 <- lmer(Surf_Temp ~ Amb_Temp + (Amb_Temp|Category), data=out_full)
summary(mod_mixed_2)
coef(mod_mixed_2) ## Very nice, to see slopes and intercepts
plot(mod_mixed_2)

## Accounting for individual and species, Random intercepts and random slopes. Was doing Species_numeric/Indiv_numeric
## (1|Categ) would allow intercepts to vary by category, but not slopes
## (Amb_Temp|Categ) allows slopes and intercepts to vary by category
mod_mixed_3 <- lmer(Surf_Temp ~ Amb_Temp + (Amb_Temp|Category) + (Amb_Temp|Species_numeric), data=out_full)
summary(mod_mixed_3)
coef(mod_mixed_3) ## Very nice, to see slopes and intercepts
plot(mod_mixed_3)

## Same as above but without individual, and now including mass.
## This is the final full model
mod_mixed_4 <- lmer(Surf_Temp ~ Amb_Temp + (Amb_Temp|Category) + Cap_mass + (Amb_Temp|Species_numeric), data=out_full)
summary(mod_mixed_4)
coef(mod_mixed_4) ## Very nice, to see slopes and intercepts
plot(mod_mixed_4)
anova(mod_mixed_3, mod_mixed_4)
## For confidence intervals
confint(mod_mixed_4,level = 0.95, method="Wald")
#lsmeans(mod_mixed_4, "Category")
qqmath(~resid(mod_mixed_4))

## Leave this out, cos amb temp has to change by categ
mod_mixed_intercept <- lmer(Surf_Temp ~ Amb_Temp + (1|Category) + (1|Species_numeric), data=out_full)
summary(mod_mixed_intercept)
coef(mod_mixed_intercept) ## Very nice, to see slopes and intercepts
plot(mod_mixed_intercept)

## Accounting for individual and species, Random intercepts and random slopes.
## Gives identical results to above. So Indiv ID doesn't make a difference
## Ignore.
mod_mixed_5 <- lmer(Surf_Temp ~ Amb_Temp + (Amb_Temp|Category) + Cap_mass +(Amb_Temp|Species_numeric/Indiv_numeric), data=out_full)
summary(mod_mixed_5)
coef(mod_mixed_5) ## Very nice, to see slopes and intercepts
plot(mod_mixed_5)

an.mod <- anova(mod_mixed_2,mod_mixed_3, mod_mixed_4)
an.mod


tt <- getME(mod_mixed,"theta")
ll <- getME(mod_mixed,"lower")
min(tt[ll==0])

## including + (1|Indiv_numeric) or + (1|Species_numeric) yields singularities, meaning they are unnecessary variables
mod.surf_amb <- lm(Surf_Temp~ Amb_Temp + Category, data=out_full) ## Take out pooled intercept
aov(mod.surf_amb)
summary(mod.surf_amb)
plot(mod.surf_amb)

## Including traditional categories (i.e. considering Shallow is Normo)
mod.surf_amb_trad <- lm(Surf_Temp~Amb_Temp + Category_Traditional, data=out_full)

summary(mod.surf_amb_trad)
plot(mod.surf_amb_trad)

## Including traditional categories (i.e. considering Shallow is torpid)
mod.surf_amb_trad2 <- lm(Surf_Temp~Amb_Temp + Category_Traditional2, data=out_full)
summary(mod.surf_amb_trad2)
plot(mod.surf_amb_trad2)

## Not including Category as a covariate
mod.surf_amb_noCateg <- lm(Surf_Temp~Amb_Temp, data=out_full)
summary(mod.surf_amb_noCateg)
plot(mod.surf_amb_trad2)

# Other useful functions 
coefficients(mod.surf_amb) # model coefficients
confint(mod.surf_amb, level=0.95) # CIs for model parameters 
anova(mod.surf_amb) # anova table 
vcov(mod.surf_amb) # covariance matrix for model parameters 


out_full$Category <- factor(out_full$Category, levels = c("Normothermic", "Shallow Torpor", "Transition", "Deep Torpor"))
#my_colors <- c("#85d349ff", "#440154ff", "#fde725ff", "#23988aff")
my_colors2 <- c("#23988aff", "#F38BA8", "#440558ff", "#9ed93aff")
## Plot surface vs ambient temperature
ggplot(out_full, aes(Amb_Temp, Surf_Temp)) + geom_point(aes(col=Category, shape=Category), size=2.5) + my_theme +
  scale_y_continuous(breaks = c(5,10,15,20,21,22,23,24,25,26,27,28,29,30,35,40)) +
  scale_colour_manual(values=my_colors2) +
  geom_smooth(aes(group=Category),method='lm') +
  scale_shape_manual(values = c(15:18)) +
  theme(panel.grid.major.y = element_line(colour="grey", size=0.5), axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15), legend.key.height = unit(1.5, 'lines')) +
  xlab( expression(atop(paste("Ambient Temperature (", degree,"C)")))) + 
  ylab( expression(atop(paste("Surface Temperature (", degree,"C)")))) +
  guides(colour = guide_legend(override.aes = list(size=4)))


## Plot surface vs ambient temperature
ggplot(out_full, aes(Amb_Temp, Surf_Temp)) + geom_point(aes(col=Category_Traditional, shape=Category_Traditional), size=2.5) + my_theme +
  scale_y_continuous(breaks = c(5,10,15,20,21,22,23,24,25,26,27,28,29,30,35,40)) +
  scale_colour_manual(values=my_colors2) +
  geom_smooth(aes(group=Category_Traditional),method='lm') +
  scale_shape_manual(values = c(15:18)) +
  theme(panel.grid.major.y = element_line(colour="grey", size=0.5), axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15), legend.key.height = unit(1.5, 'lines')) +
  xlab( expression(atop(paste("Ambient Temperature (", degree,"C)")))) + 
  ylab( expression(atop(paste("Surface Temperature (", degree,"C)")))) +
  guides(colour = guide_legend(override.aes = list(size=4)))

## Plotting all max temps of all birds as a histogram
ggplot(m.all_thermal, aes(value)) + geom_histogram(binwidth=1) + my_theme +
  xlab(Temp.lab) #+ ylab("Frequency") #+ geom_point(aes(value, col=variable), alpha=0.8)

## Plotting distribution of max values for all birds, from annotated thermal max file
#thermal_maxes_melted$Category <- revalue(thermal_maxes_melted$Category, c("Shallow"="Shallow Torpor", "Torpor"="Deep Torpor"))
ggplot(thermal_maxes_melted, aes(variable, value)) + my_theme + geom_point(aes(col=Category), size=2, alpha=0.8) +  
  facet_grid(.~Species, scales = "free_x",space = "free_x") +
  ylab(Temp.lab) + xlab("Individual") + 
  #scale_color_manual(values = c('black','deepskyblue2', 'palegreen4', 'red')) +
  scale_color_manual(values=my_colors2) +
  guides(colour = guide_legend(override.aes = list(size=3.5))) +
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), axis.text.y=element_text(size=20),
        legend.key.height = unit(3, 'lines'))

ggplot(datatry, aes(Indiv_pasted, Surf_Temp)) + my_theme + geom_point(aes(col=Category), alpha=0.8) +  
  facet_grid(.~Species, scales = "free_x",space = "free_x") +
  ylab(Temp.lab) + xlab("Individual") + 
  #scale_color_manual(values = c('black','deepskyblue2', 'palegreen4', 'red')) +
  scale_color_manual(values=my_colors2) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5), axis.text.y=element_text(size=15),
        legend.key.height = unit(3, 'lines'))

## package Virides for color scheme
## Try bird vs ambient plot, change color scheme
## Do a predictive plot of surface vs ambient 
ggplot(m.all_thermal, aes(variable, value)) + my_theme + geom_point(aes(col=Category), alpha=0.8) +  
  facet_grid(.~Species, scales = "free_x",space = "free_x") +
  ylab(Temp.lab) + xlab("Individual") + scale_color_manual(values = c('black','deepskyblue2','red', 'palegreen4')) +
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5), axis.text.y=element_text(size=15),
        legend.key.height = unit(3, 'lines'))


## Plotting all bird maxes and amb temps side by side
ggplot(m.all_amb, aes(variable, value)) + my_theme +
  #geom_density2d(data=m.all_thermal, aes(col=variable)) +
  #geom_violin(data=m.all_amb, alpha=0.2, position = position_nudge(x = -0.2)) +
  geom_point(data=m.all_thermal, aes(col=variable), alpha=0.8) +
  geom_point(data=m.all_amb, size=2, alpha=0.2, position = position_nudge(x = -0.2)) +
  ylab(Temp.lab) + xlab("Individual") +
    theme(axis.text.x = element_text(angle=60, size=15, vjust=0.5), axis.text.y=element_text(size=15), 
          legend.position = "none")
    
#### Single-night plots ####

for(i in bird.folders) {
  try <- 
  print(try)
}

library(dplyr)
library(tidyr)
library(broom)
library(purrr)
library(extrafont)

mtcars %>%
  nest(-cyl) %>%
  mutate(Quantiles = map(data, ~ quantile(.$mpg))) %>% 
  unnest(map(Quantiles, tidy))

for(i in single) {
  setwd(paste0(wd, "/", i))
 
  #### Plotting ####
  out<- readRDS(file=paste(i, "_summ.rds", sep=""))
  
  ## Creating a time sequence
  birdTime <- out$Time
  TimeOrder1 <- seq(from = 1900, to = 2459, by = 1)
  TimeOrder2 <- seq(from = 0100, to = 0559, by = 1)
  TimeOrder <- c(TimeOrder1, paste0("0", TimeOrder2))
  TimeOrder <- factor(TimeOrder, as.character(TimeOrder))
  
  Time_unordered<- as.factor(format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 min"),"%H%M", tz="GMT"))
  
  TimeFinal <- droplevels(na.omit(TimeOrder[match(Time_unordered, TimeOrder,nomatch=NA)]))
  
  
  out$Time2 <- TimeOrder[match(birdTime,TimeOrder,nomatch=NA)]
  pdf("Rplot_trial.pdf", useDingbats = F, width = 13.333, height=7.5)
  thermplot <- ggplot(out, aes(Time2, value)) +
    geom_point(aes(shape=variable), size=3) + my_theme +
    theme(axis.text.x = element_text(angle=60, size=20, vjust=0.5), panel.grid.major.y = element_line(colour="grey", size=0.5),
          axis.text.y=element_text(size=20), legend.key.height = unit(3, 'lines'),  plot.title = element_text(hjust = 0.5)) +
    scale_shape_manual(values=c(0,1,2), labels=c("Ambient", "Mean surface", "Max surface"), name="Temperature") + 
    scale_y_continuous(breaks = c(5,10,15,20,21,22,23,24,25,26,27,28,29,30,35)) + 
      #scale_x_discrete(drop=F, levels(out$Time2)[c(T, rep(F, 14))]) +
    ylab(Temp.lab) + xlab("Hour") #+ ggtitle(out$Indiv_ID[1])
  print(thermplot)
  dev.off()
  ggsave("Rplot_trial.pdf")
  print(thermplot)
}

setwd(".//BLHU05_0523")
test <- readRDS("BLHU05_0523_summ.rds")

## Creating a time sequence
birdTime <- test$Time
TimeOrder1 <- seq(from = 1900, to = 2459, by = 1)
TimeOrder2 <- seq(from = 0100, to = 0559, by = 1)
TimeOrder <- c(TimeOrder1, paste0("0", TimeOrder2))
TimeOrder <- factor(TimeOrder, as.character(TimeOrder))

Time_unordered<- as.factor(format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 min"),"%H%M", tz="GMT"))

TimeFinal <- droplevels(na.omit(TimeOrder[match(Time_unordered, TimeOrder,nomatch=NA)]))


test$Time2 <- TimeOrder[match(birdTime,TimeOrder,nomatch=NA)]

thermplot <- ggplot(test, aes(Time2, value)) +
  geom_point(aes(col=variable), size=3) + my_theme +
  theme(axis.text.x = element_text(angle=60, size=15, vjust=0.5), panel.grid.major.y = element_line(colour="grey", size=0.5),
        axis.text.y=element_text(size=15), legend.key.height = unit(3, 'lines'),  plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("black", "violet", "red"), 
                     labels=c("Ambient", "Mean surface", "Max surface"), name="Temperature") + 
  scale_y_continuous(breaks = c(5,10,15,20,21,22,23,24,25,26,27,28,29,30,35)) +
  #scale_x_discrete(drop=F, levels(out$Time2)[c(T, rep(F, 14))]) +
  ylab(Temp.lab) + xlab("Hour") + ggtitle(test$Indiv_ID[1])
thermplot

