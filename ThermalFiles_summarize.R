## Processing thermal images and video from May-Jun 2018, Southwestern Research Station

library(plyr)

setwd("E:\\Google Drive\\IR_torpor_2018\\Funny_videos\\BCHU03_0530")

## Manual way, per file- to then incorporate into 'apply' funcs or into a loop
mahu_1 <- read.csv("MAHU08_0531/MAHU08_0531_2231.csv", header=F)

mahu_1_stack <- na.omit(data.frame(stack(mahu_1[1:ncol(mahu_1)])))[,1]
length(mahu_1_stack)
summary(mahu_1_stack)

## Using plyr
paths <- dir(path = "E:\\Google Drive\\IR_torpor_2018\\Funny_videos\\BCHU03_0530\\", pattern = "\\.csv$")
names(paths) <- basename(paths)

ThermFiles <- lapply(paths, read.csv, header=F)


ThermFiles_clean <- sapply(ThermFiles, function(x) na.omit(data.frame(stack(ThermFiles[1:ncol(ThermFiles[i])]))))

## Not tried this yet
combined.df <- do.call(rbind , tables)
#You can then find the mean - find which columns are numeric

s <- sapply(combined.df, is.numeric)
#find the mean of numeric variables

colMeans(combined.df[s])