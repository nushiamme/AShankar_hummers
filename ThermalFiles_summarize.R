## Processing thermal images and video from May-Jun 2018, Southwestern Research Station

#library(here)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

wd <- file.path("D:", "Google Drive", "IR_torpor_2018", "Analyze")

#bird.folders <- list.dirs(wd, recursive=T)[-1]

## Generic plot theme
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

## Axis labels
Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))

#for(i in bird.folders) {}

bird_id <- "BLHU03_0522"
#bird_id <- "BLHU05_0523"
bird_id <- "MAHU03_0527" ## Didn't work, time parallel thing
#bird_id <- "MAHU05_0529"
#bird_id <- "BLHU07_0529"
#bird_id <- "MAHU06_0530"
bird_id <- "BCHU03_0530"
#bird_id <- "MAHU07_0531"

setwd(paste0(wd, "/", bird_id))

## nothing

#### Compile csv's and process ####
## Using plyr
paths <- dir(pattern = "\\.csv$")
names(paths) <- basename(paths)

ThermFiles <- lapply(paths, read.csv, header=F)

#mag_2202 <- read.csv(file = "MAHU02_0520_2202.csv")
#mag_2202 <- na.omit(data.frame(stack(mag_2202)))


#mag_0450 <- read.csv(file = "MAHU02_0520_0450.csv")
#mag_0450 <- na.omit(data.frame(stack(mag_0450)))


#mag_2111 <- read.csv(file = "MAHU02_0520_2111.csv")
#mag_2111 <- na.omit(data.frame(stack(mag_2111)))

#mag_2331 <- read.csv(file = "MAHU02_0520_2331.csv")
#mag_2331 <- na.omit(data.frame(stack(mag_2331)))

## Creating a summary data frame of 
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

ggplot(data=out[out$variable=="Max",]) + my_theme +
  geom_violin(aes(x=Indiv_ID, y=value)) +
  #geom_linerange(aes(x=Indiv_ID, ymin= min(value), ymax=max(value)), size=1) +
  geom_point(aes(x=Indiv_ID, y=mean(value)), size=4) + ylab(Temp.lab)  +
  ylim(0,36)


ggplot(out, aes(Time2, value)) +
  geom_point(aes(col=variable), size=3) + my_theme +
  theme(axis.text.x = element_text(angle=60, size=15, vjust=0.5)) +
  theme(panel.grid.major.y = element_line(colour="grey", size=0.5),
        axis.text.y=element_text(size=15), legend.key.height = unit(3, 'lines')) +
  scale_color_manual(values = c("black", "violet", "red"), 
                     labels=c("Ambient", "Mean surface", "Max surface"), name="Temperature") + 
  scale_y_continuous(breaks = c(5,10,15,20,21,22,23,24,25,26,27,28,29,30,35)) +
  ylab(Temp.lab) + xlab("Hour") + ggtitle(out$Indiv_ID[1])

