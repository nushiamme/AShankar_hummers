## Processing thermal images and video from May-Jun 2018, Southwestern Research Station

library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

setwd("E:\\Google Drive\\IR_torpor_2018\\Funny_videos\\BCHU03_0530")

## Generic plot theme
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

## Axis labels
Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))

## Manual way, to try per file- to then incorporate into 'apply' funcs or into a loop
mahu_1 <- read.csv("MAHU08_0531/MAHU08_0531_2231.csv", header=F)
bchu_1 <- read.csv("BCHU03_0530_2323.csv", header=F)
bchu_1_stack <- na.omit(data.frame(stack(bchu_1[1:ncol(bchu_1)])))[,1]
length(bchu_1_stack)
summary(bchu_1_stack)

## Using plyr
paths <- dir(path = "E:\\Google Drive\\IR_torpor_2018\\Funny_videos\\BCHU03_0530\\", pattern = "\\.csv$")
names(paths) <- basename(paths)

ThermFiles <- lapply(paths, read.csv, header=F)

## Creating a summary data frame of 
# Can also create automatic lists of summaries: lapply(ThermFiles_na_omit[[i]], summary)
Thermsumm <- data.frame(matrix(NA, nrow=length(ThermFiles), ncol=4))
names(Thermsumm) <- c("Min", "Mean", "Max", "File")
Thermsumm$File <- noquote(names(ThermFiles))
for(i in 1:length(ThermFiles)) { 
  ThermFiles_na_omit <- vector("list",length(ThermFiles))
  names(ThermFiles_na_omit) <- names(ThermFiles)
  ThermFiles_na_omit[[i]]<- na.omit(data.frame(stack(ThermFiles[[i]][1:ncol(ThermFiles[[i]])])))[1]
  Thermsumm[i,1] <- min(ThermFiles_na_omit[[i]]$values)
  Thermsumm[i,2] <- mean(ThermFiles_na_omit[[i]]$values)
  Thermsumm[i,3] <- max(ThermFiles_na_omit[[i]]$values)
  ## Splitting the file name to get IndivID, date, and time
  Thermsumm$Indiv_ID <- 
    lapply(strsplit(as.character(Thermsumm$File), "\\_"), "[", 1)
  Thermsumm$Date <- 
    lapply(strsplit(as.character(Thermsumm$File), "\\_"), "[", 2)
  Thermsumm$Time <- 
    lapply(strsplit(as.character(Thermsumm$File), "\\_"), "[", 3)
  Thermsumm$Time <- 
    lapply(strsplit(as.character(Thermsumm$Time), "\\."), "[", 1)
}

m.thermsumm <- melt(Thermsumm, id.vars=c("Indiv_ID", "Date", "Time"), 
                    measure.vars = c("Min", "Mean","Max"))

m.thermsumm$Hour <- substr(m.thermsumm$Time,1,2)
m.thermsumm$Hour <- factor(m.thermsumm$Hour, 
                            levels= c("19", "20", "21", "22", "23", "24", "01", "02",
                                      "03", "04", "05", "06"), ordered=T)

ggplot(m.thermsumm, aes(as.character(Time), value)) +
  geom_point(aes(col=variable), size=3) + my_theme +
  theme(axis.text.x = element_text(angle=30, size=15)) +
  ylab(Temp.lab) + xlab("Hour")
