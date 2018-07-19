## Processing thermal images and video from May-Jun 2018, Southwestern Research Station

library(here)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

wd <- D:\\Google Drive\\IR_torpor_2018\\

## Generic plot theme
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

## Axis labels
Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))

bird_id <- "BCHU02_0526"
setwd(paste(wd, bird_id, sep=""))

## Using plyr
paths <- dir(pattern = "\\.csv$")
names(paths) <- basename(paths)

ThermFiles <- lapply(paths, read.csv, header=F)

## Creating a time sequence
TimeOrder <- seq(from = 1900, to = 2459, by = 1,
    length.out = NULL, along.with = NULL, ...)

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
    unlist(lapply(strsplit(as.character(Thermsumm$File), "\\_"), "[", 1))
  Thermsumm$Date <- 
    unlist(lapply(strsplit(as.character(Thermsumm$File), "\\_"), "[", 2))
  Thermsumm$Time <- 
    unlist(lapply(strsplit(as.character(Thermsumm$File), "\\_"), "[", 3))
  Thermsumm$Time <- 
    unlist(lapply(strsplit(as.character(Thermsumm$Time), "\\."), "[", 1))
  m.thermsumm <- melt(Thermsumm, id.vars=c("Indiv_ID", "Date", "Time"), 
                    measure.vars = c("Min", "Mean","Max"))
  m.thermsumm$Hour <- substr(m.thermsumm$Time,1,2)
  m.thermsumm$Hour <- factor(m.thermsumm$Hour, 
                            levels= c("19", "20", "21", "22", "23", "24", "01", "02",
                                      "03", "04", "05", "06"), ordered=T)
  file.name <- paste(m.thermsumm$Indiv_ID[1], "_", m.thermsumm$Date[1], "_summ.rds", sep="")
  saveRDS(m.thermsumm,file.name)
}

out<- readRDS(file=paste(bird_id, "_summ.rds", sep=""))

#out <- readRDS(file = "BCHU01_0521_summ.rds")

#out$Time2 <- reorder(out$Time, out$Hour)

#test_time <- as.POSIXct(out$Time,format='%H%M')

ggplot(out, aes(Time, value)) +
  geom_point(aes(col=variable), size=3) + my_theme +
  theme(axis.text.x = element_text(angle=60, size=15, vjust=0.5)) +
  theme(panel.grid.major.y = element_line(colour="grey", size=0.5),
        axis.text.y=element_text(size=15)) +
  scale_color_manual(values = c("black", "violet", "red")) +
  scale_y_continuous(breaks = c(5,10,15,20,21,22,23,24,25,26,27,28,29,30,35)) +
  ylab(Temp.lab) + xlab("Hour") + ggtitle(out$Indiv_ID[1])
