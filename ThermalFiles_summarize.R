## Processing thermal images and video from May-Jun 2018, Southwestern Research Station
## Code Author: Anusha Shankar; started July 2018

#library(here)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

wd <- file.path("E:", "Google Drive", "IR_2018_csv")

#bird.folders <- list.dirs(wd, recursive=T)[-1]

## Generic plot theme
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

## Axis labels
Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))

bird.folders <- c("BCHU01_0521", "BCHU02_0526", "BCHU03_0530", "BCHU04_0607", "BCHU05_0607",
                  "BLHU01_0521", "BLHU03_0522", "BLHU04_0523", "BLHU05_0523", "BLHU06_0526", "BLHU07_0529", "BLHU08_0601", 
                  "BLHU09_0603", "BLHU11_0604", "BLHU12_0605", "BLHU13_0605", 
                  "MAHU02_0520", "MAHU03_0527", "MAHU05_0529", "MAHU06_0530", "MAHU10_0603", "MAHU12_0606", "MAHU13_0606")

#for(i in bird.folders) {
setwd(paste0(wd, "/", bird_id))

## nothing

#### Compile csv's and process ####
## Using plyr
paths <- dir(pattern = "\\.csv$")
names(paths) <- basename(paths)

# RUN ThermFiles <- lapply(paths, read.csv, header=F)

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


#### Plotting ####
out<- readRDS(file=paste(bird_id, "_summ.rds", sep=""))

## Creating a time sequence
birdTime <- out$Time
TimeOrder1 <- seq(from = 1900, to = 2459, by = 1)
TimeOrder2 <- seq(from = 0100, to = 0559, by = 1)
TimeOrder <- c(TimeOrder1, paste0("0", TimeOrder2))
TimeOrder <- factor(TimeOrder, as.character(TimeOrder))

out$Time2 <- TimeOrder[match(birdTime,TimeOrder,nomatch=NA)]

## Violin plots - don't use
#ggplot(data=out[out$variable=="Max",]) + my_theme +
  #geom_violin(aes(x=Indiv_ID, y=value)) + geom_point(aes(x=Indiv_ID, y=value)) +
  # #geom_linerange(aes(x=Indiv_ID, ymin= min(value), ymax=max(value)), size=1) +
  #geom_point(aes(x=Indiv_ID, y=mean(value)), size=4) + ylab(Temp.lab)  +
  #ylim(0,36)


ggplot(out, aes(Time2, value)) +
  geom_point(aes(col=variable), size=3) + my_theme +
  theme(axis.text.x = element_text(angle=60, size=15, vjust=0.5)) +
  theme(panel.grid.major.y = element_line(colour="grey", size=0.5),
        axis.text.y=element_text(size=15), legend.key.height = unit(3, 'lines')) +
  scale_color_manual(values = c("black", "violet", "red"), 
                     labels=c("Ambient", "Mean surface", "Max surface"), name="Temperature") + 
  scale_y_continuous(breaks = c(5,10,15,20,21,22,23,24,25,26,27,28,29,30,35)) +
  ylab(Temp.lab) + xlab("Hour") + ggtitle(out$Indiv_ID[1])

