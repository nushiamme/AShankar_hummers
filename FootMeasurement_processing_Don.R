#### Code description ####
## Foot temperature measurements
## For Don Powers
## Code author: A Shankar
## Contact: nushiamme<at>gmail<dot>com
## Started June 15, 2021

## Code layout:
## Load packages
## Generic functions used in the script
## Read in files
## For loops to extract temperature data. First without filtering, then with filtering
## Plots

#### Read in packages ####
library(here)
library(plyr)
library(stringr)
library(reshape2)
library(ggplot2)

## Generic plot theme
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

## Defining a general Amb temp axis
ATemp.lab <- expression(atop(paste("Ambient Temperature (", degree,"C)")))

# my_colors5 <- c("#004e64", "#ffba08", "#73a580", "#f786aa", "#685369")
# my_colors6 <- c("#004e64", "#ffba08", "#f7b2bd", "#c60f7b", "#bbc7a4")
# my_colors7 <- c("#ffe74c", "#508aa8", "#242f40", "#c60f7b", "#bbc7a4")
my_colors8 <- c("#7f7caf", "#fcbf49", "#171738", "#f71735", "#c9e4ca")
# my_colors9 <- c("#5bba6f", "#297373", "#171738", "#9e4770", "#f8c630")
# my_colors10 <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")


## Read in files. Using here() package, so default working directory is the file that the .Rproj file is in.
here <- here::here

## Sets the working directory to a folder called "Data" within the folder that the .Rproj file is in.
##This is where the .csv files should be stored.
here("Data")  


#### Compile csv's and process ####
## Using plyr
paths <- dir(pattern = "\\.csv$")
names(paths) <- basename(paths)

ThermFiles <- lapply(paths, read.csv, header=F)

### Creating a summary data frame of the temperatures; here min, mean, max, sd, and number of values per file
Thermsumm <- data.frame(matrix(NA, nrow=length(ThermFiles), ncol=11))
names(Thermsumm) <- c("File", "Indiv_ID", "Temp", "Run_ID", "Run_no", "BeforeAfter", "Min", "Mean", "Max", "sd", "nVal")
Thermsumm$File <- noquote(names(ThermFiles))
for(i in 1:length(ThermFiles)) { 
  Thermsumm$Indiv_ID[i] <- 
    unlist(lapply(strsplit(as.character(Thermsumm$File[i]), "-"), "[", 1))
  Thermsumm$Temp[i] <- 
    unlist(lapply(strsplit(as.character(Thermsumm$File[i]), "-"), "[", 2))
  Thermsumm$Run_ID[i] <- 
    str_extract(unlist(lapply(strsplit(as.character(Thermsumm$File[i]), "-"), "[", 3)), '.*(?=\\.csv)')
  Thermsumm$Run_no[i] <- as.numeric(str_sub(Thermsumm$Run_ID[i], -3, -2))
  Thermsumm$BeforeAfter[i] <- str_sub(Thermsumm$Run_ID[i], -1, -1)
  
  ThermFiles_na_omit <- vector("list",length(ThermFiles))
  names(ThermFiles_na_omit) <- names(ThermFiles)
  ThermFiles_na_omit[[i]]<- na.omit(data.frame(stack(ThermFiles[[i]][1:ncol(ThermFiles[[i]])])))[1]
  
  Thermsumm$Min[i] <- min(ThermFiles_na_omit[[i]]$values)
  Thermsumm$Mean[i] <- mean(ThermFiles_na_omit[[i]]$values)
  Thermsumm$Max[i] <- max(ThermFiles_na_omit[[i]]$values)
  Thermsumm$sd[i] <- sd(ThermFiles_na_omit[[i]]$values)
  Thermsumm$nVal[i] <- length(ThermFiles_na_omit[[i]]$values)
  ## Splitting the file name to get IndivID, date, and time
  # m.thermsumm <- melt(Thermsumm, id.vars=c("Indiv_ID", "Temp", "Run_ID"), 
  #                     measure.vars = c("nVal", "Min", "Mean","Max", "sd"))
  write.csv(Thermsumm,"..\\Foot_summaries_15June2021.csv")
}

## Using filtered values
## When ambient temp was 22, set threshold to 24.
## When ambient temp was 32, set threshold to 34. 
Filtered_summ <- data.frame(matrix(NA, nrow=length(ThermFiles), ncol=11))
names(Filtered_summ) <- c("File", "Indiv_ID", "Temp", "Run_ID", "Run_no", "BeforeAfter", "Min", "Mean", "Max", "sd", "nVal")
Filtered_summ$File <- noquote(names(ThermFiles))

for(i in 1:length(ThermFiles)) { 
  ThermFiles_na_omit <- vector("list",length(ThermFiles))
  names(ThermFiles_na_omit) <- names(ThermFiles)
  ThermFiles_na_omit[[i]]<- na.omit(data.frame(stack(ThermFiles[[i]][1:ncol(ThermFiles[[i]])])))[1]
  
  # ## Splitting the file name to get IndivID, date, and time
  Filtered_summ$Indiv_ID[i] <-
    unlist(lapply(strsplit(as.character(Filtered_summ$File[i]), "-"), "[", 1))
  Filtered_summ$Temp[i] <-
    unlist(lapply(strsplit(as.character(Filtered_summ$File[i]), "-"), "[", 2))
  Filtered_summ$Run_ID[i] <-
    str_extract(unlist(lapply(strsplit(as.character(Filtered_summ$File[i]), "-"), "[", 3)), '.*(?=\\.csv)')
  Filtered_summ$Run_no[i] <- as.numeric(str_sub(Filtered_summ$Run_ID[i], -3, -2))
  Filtered_summ$BeforeAfter[i] <- str_sub(Filtered_summ$Run_ID[i], -1, -1)
  

  Filtered_summ$Min[i] <- min(ThermFiles_na_omit[[i]]$values[ThermFiles_na_omit[[i]]$values>(2+as.numeric(Filtered_summ$Temp[i]))]) ## These are pretty weird cos of the thresholding, lots of exact repeats
  Filtered_summ$Mean[i] <- mean(ThermFiles_na_omit[[i]]$values[ThermFiles_na_omit[[i]]$values>(2+as.numeric(Filtered_summ$Temp[i]))])
  Filtered_summ$Max[i] <- max(ThermFiles_na_omit[[i]]$values[ThermFiles_na_omit[[i]]$values>(2+as.numeric(Filtered_summ$Temp[i]))])
  Filtered_summ$sd[i] <- sd(ThermFiles_na_omit[[i]]$values[ThermFiles_na_omit[[i]]$values>(2+as.numeric(Filtered_summ$Temp[i]))])
  Filtered_summ$nVal[i] <- length(ThermFiles_na_omit[[i]]$values[ThermFiles_na_omit[[i]]$values>(2+as.numeric(Filtered_summ$Temp[i]))])
}

## i=55 and i=57 are not working cos all temps are <24 at the 22C temp. So run the next few lines (to **) with i=55 and i=57 to refill those without a threshold
i <- 55
ThermFiles_na_omit[[i]]<- na.omit(data.frame(stack(ThermFiles[[i]][1:ncol(ThermFiles[[i]])])))[1]
Filtered_summ$Min[i] <- min(ThermFiles_na_omit[[i]]$values) ## These are pretty weird cos of the thresholding, lots of exact repeats
Filtered_summ$Mean[i] <- mean(ThermFiles_na_omit[[i]]$values)
Filtered_summ$Max[i] <- max(ThermFiles_na_omit[[i]]$values)
Filtered_summ$sd[i] <- sd(ThermFiles_na_omit[[i]]$values)
Filtered_summ$nVal[i] <- length(ThermFiles_na_omit[[i]]$values)

i <- 57
ThermFiles_na_omit[[i]]<- na.omit(data.frame(stack(ThermFiles[[i]][1:ncol(ThermFiles[[i]])])))[1]
Filtered_summ$Min[i] <- min(ThermFiles_na_omit[[i]]$values) ## These are pretty weird cos of the thresholding, lots of exact repeats
Filtered_summ$Mean[i] <- mean(ThermFiles_na_omit[[i]]$values)
Filtered_summ$Max[i] <- max(ThermFiles_na_omit[[i]]$values)
Filtered_summ$sd[i] <- sd(ThermFiles_na_omit[[i]]$values)
Filtered_summ$nVal[i] <- length(ThermFiles_na_omit[[i]]$values)
### **Up to here  


write.csv(Filtered_summ, "..\\Thresholded_Foot_summaries_15June2021.csv")

## Can use this to make a histogram of the values for a run
# hist(ThermFiles_na_omit[[i]]$values)

#### Plots ####

## Define order of Before then After
Filtered_summ$BeforeAfter <- factor(Filtered_summ$BeforeAfter, levels=c("b", "a"))

## Make a new column to uniquely ID runs within individuals, without separating by BeforeAfter
Filtered_summ$Indiv_Run <- paste(Filtered_summ$Indiv_ID, Filtered_summ$Run_no, sep="_")

## Plotting mean values
ggplot(Filtered_summ, aes(BeforeAfter, Mean)) + 
  geom_boxplot(aes(fill=Indiv_ID)) + 
  my_theme + 
  scale_fill_manual(values=my_colors8) +
  facet_grid(.~Temp) +
  theme(legend.key.height = unit(3, "line")) +
  xlab("Before vs. after hovering")

## Mean temps, lines connect Runs before and after
ggplot(Filtered_summ, aes(BeforeAfter, Mean)) + 
  geom_line(aes(group=Indiv_Run)) + 
  geom_point(aes(col=Indiv_ID), size=3) + 
  my_theme + scale_color_manual(values=my_colors8) +
  facet_grid(.~Temp) +
  theme(legend.key.height = unit(3, "line")) +
  xlab("Before vs. after hovering")


## Max temps
ggplot(Filtered_summ, aes(BeforeAfter, Max)) + 
  geom_line(aes(group=Indiv_Run)) + 
  geom_point(aes(col=Indiv_ID), size=3) + 
  my_theme + scale_color_manual(values=my_colors8) +
  facet_grid(.~Temp) +
  theme(legend.key.height = unit(3, "line")) +
  xlab("Before vs. after hovering")


## Number of pixels
ggplot(Filtered_summ, aes(BeforeAfter, nVal)) + 
  geom_line(aes(group=Indiv_Run)) + 
  geom_point(aes(col=Indiv_ID), size=3) + 
  my_theme + scale_color_manual(values=my_colors8) +
  facet_grid(.~Temp) +
  theme(legend.key.height = unit(3, "line")) +
  xlab("Before vs. after hovering") +
  ylab("Number of pixels")


## Mean temps but switching the x-axis and facet so that amb temp is on the x-axis and facets are BeforeAfter
ggplot(Filtered_summ, aes(Temp, Mean)) + 
  geom_line(aes(group=Indiv_Run)) + 
  geom_point(aes(col=Indiv_ID), size=3) + 
  my_theme + scale_color_manual(values=my_colors8) +
  facet_grid(.~BeforeAfter) +
  theme(legend.key.height = unit(3, "line")) +
  xlab(ATemp.lab)
