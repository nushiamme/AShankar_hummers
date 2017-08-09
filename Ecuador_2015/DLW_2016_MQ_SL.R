dlw_2016 <- read.csv("C:\\Users/ANUSHA/Dropbox/Data 2015/Data 2016 season/DLW_netting_2016/Ecuador_2016_DLW_results.csv")

names(dlw_2016) <- c("Site", "ID", "Species", "Sex", "Mass", "kJ_day", "kJ_day_g")

ggplot(dlw_2016, aes(Species, kJ_day)) + geom_point(aes(size=Mass)) + my_theme + 
  theme(axis.text.x= element_text(angle = 60, hjust=1), legend.key.size = unit(2, 'lines'))
