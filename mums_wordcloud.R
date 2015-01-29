## Load packages
library(tm)
library(wordcloud)

## Set wd
setwd("E:\\Toshiba_Desktop\\Stony Brook\\Teaching courses\\Chordate Spring 2015")

## Read in txt file- required for this wordcloud package as txt
mums <- read.csv("wordfile/MUMS_essays.txt")
head(mums)

## Make a "corpus" of files- uses the tm package
mumwords <- Corpus(DirSource("wordfile/"))
## Check
inspect(mumwords)

## Remove white spaces
mumwords <- tm_map(mumwords, stripWhitespace)

## Make everything lowercase to standardize
mumwords <- tm_map(mumwords, tolower)

## remove common English words like "the", "and", etc.
mumwords <- tm_map(mumwords, removeWords, stopwords('en'))

## Final clean up- reduces to root words- removes -ing, -ed, etc.
mumwords <- tm_map(mumwords, stemDocument)

## Corpus doesn't always automatically save as txt file, so make sure that it's txt
mumwords <- tm_map(mumwords, PlainTextDocument)

## Bam, wordcloud!
wordcloud(mumwords, scale=c(5,0.5), max.words=100, 
          random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))
