## Processing thermal images and video from May-Jun 2018, Southwestern Research Station
## Code Author: Anusha Shankar; started July 2018

#library(here)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyverse)

wd <- file.path("E:", "Google Drive", "IR_2018_csv")
thermal_maxes_melted <- read.csv("E:\\Google Drive\\IR_2018_csv\\Melted_thermal_maxes_all.csv")

thermal_maxes_melted$Category <- factor(thermal_maxes_melted$Category, levels = c("Normothermic", "Shallow", "Transition", "Torpor"))

#bird.folders <- list.dirs(wd, recursive=T)[-1]

## Generic plot theme
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

## Axis labels
Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))

bird.folders <- c("BCHU01_0521", "BCHU02_0526", "BCHU03_0530", "BCHU04_0607", "BCHU05_0607",
                  "BLHU01_0521", "BLHU03_0522", "BLHU04_0523", "BLHU05_0523", "BLHU06_0526", "BLHU07_0529", "BLHU08_0601", 
                  "BLHU09_0603", "BLHU12_0605", "BLHU13_0605", 
                  "MAHU02_0520", "MAHU03_0527", "MAHU05_0529", "MAHU06_0530", "MAHU10_0603", "MAHU12_0606", "MAHU13_0606")

bird.folders.2017 <- c("BC01_0610", "BC02_0612", "BC03_0617",
                  "BL01_0610", "BL02_0612", "BL03_0614", "BL04_0615",
                  "MA02_0611", "MA05_0615", "MA06_0616", "MA07_0617", "MA08_0619")

bird.folders.all <- c("BCHU01_0521", "BCHU02_0526", "BCHU03_0530", "BCHU04_0607", "BCHU05_0607",
                      "BLHU01_0521", "BLHU03_0522", "BLHU04_0523", "BLHU05_0523", "BLHU06_0526", "BLHU07_0529", "BLHU08_0601", 
                      "BLHU09_0603", "BLHU12_0605", "BLHU13_0605", 
                      "MAHU02_0520", "MAHU03_0527", "MAHU05_0529", "MAHU06_0530", "MAHU10_0603", "MAHU12_0606", "MAHU13_0606",
                      "BC01_0610", "BC02_0612", "BC03_0617",
                      "BL01_0610", "BL02_0612", "BL03_0614", "BL04_0615",
                      "MA02_0611", "MA05_0615", "MA06_0616", "MA07_0617", "MA08_0619")


for(i in bird.folders.all) {
setwd(paste0(wd, "/", i))

## nothing

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

## Length of longest column (non-NA)
max(apply(amb, 2, function(x) length(which(!is.na(x)))))

## Stacking all the individual birds' data, keeping all melted columns from earlier (hour, min, max, etc.)
out_all <- data.frame(matrix(ncol = 6, nrow=109*length(bird.folders.all)))
names(out_all) <- names(out)
for(i in bird.folders.all) {
  setwd(paste0(wd, "/", i))
  out<- readRDS(file=paste(i, "_summ.rds", sep=""))
  out_all <- rbind(out,out_all)
}
dim(out_all) ## Check dimensions
out_all <- out_all[complete.cases(out_all),] ## Remove rows with NAs
dim(out_all) ## Check dimensions
out_amb <- out_all[out_all$variable=="Min",] ## Make a separate data frame with just minimum (~= ambient) values
out_max <- out_all[out_all$variable=="Max",] ## Make a separate data frame with just maximum (~= surface) values
out_full <- merge(out_amb,out_max, by = c("Indiv_ID", "Date", "Time", "Hour")) ## Merge the two
out_full <- subset(out_full, select = -c(variable.x, variable.y)) ## Remove unnecessary columns
names(out_full) <- c("Indiv_ID", "Date", "Time", "Hour", "Amb_Temp", "Surf_Temp")
head(out_full)
ggplot(out_full, aes(Amb_Temp, Surf_Temp)) + geom_point(aes(col=Indiv_ID))

m.all_thermal <- melt(all_thermal, na.rm=T)
setwd("E:/Google Drive/IR_2018_csv")
write.csv(m.all_thermal,file = "Melted_thermal_maxes2.csv")
m.all_amb <- melt(all_amb,na.rm=T)

## Plotting all max temps of all birds as a histogram
ggplot(m.all_thermal, aes(value)) + geom_histogram(binwidth=1) + my_theme +
  xlab(Temp.lab) #+ ylab("Frequency") #+ geom_point(aes(value, col=variable), alpha=0.8)

## Plotting distribution of max values for all birds, from annotated thermal max file
ggplot(thermal_maxes_melted, aes(variable, value)) + my_theme + geom_point(aes(col=Category), alpha=0.8) +  
  facet_grid(.~Species, scales = "free_x",space = "free_x") +
  ylab(Temp.lab) + xlab("Individual") + scale_color_manual(values = c('black','deepskyblue2', 'palegreen4', 'red')) +
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
    

for(i in bird.folders) {
  try <- 
  print(try)
}

library(dplyr)
library(tidyr)
library(broom)
library(purrr)

mtcars %>%
  nest(-cyl) %>%
  mutate(Quantiles = map(data, ~ quantile(.$mpg))) %>% 
  unnest(map(Quantiles, tidy))

for(i in bird.folders) {
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
  
  thermplot <- ggplot(out, aes(Time2, value)) +
    geom_point(aes(col=variable), size=3) + my_theme +
    theme(axis.text.x = element_text(angle=60, size=15, vjust=0.5), panel.grid.major.y = element_line(colour="grey", size=0.5),
          axis.text.y=element_text(size=15), legend.key.height = unit(3, 'lines'),  plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values = c("black", "violet", "red"), 
                       labels=c("Ambient", "Mean surface", "Max surface"), name="Temperature") + 
    scale_y_continuous(breaks = c(5,10,15,20,21,22,23,24,25,26,27,28,29,30,35)) +
      #scale_x_discrete(drop=F, levels(out$Time2)[c(T, rep(F, 14))]) +
    ylab(Temp.lab) + xlab("Hour") + ggtitle(out$Indiv_ID[1])
  
  print(thermplot)
}

setwd(".//BC01_0610")
test <- readRDS("BC01_0610_summ.rds")

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
