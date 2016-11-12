require(ggplot2)

dlw <- read.csv("E://Git//Thesis//DLW_2014_data.csv")

dee_data <- ggplot(dlw, aes(x=Sp_sex,y=kJ.day.g)) + facet_grid(.~Site, scales="free_x") + ggtitle("Daily Energy Expenditure") +
  geom_boxplot(col="darkgreen", drop=T) + theme_bw() + xlab("Species") + ylab("kJ/day*g")
dee_data

dlw_eb <- dlw[dlw$Sp_sex=="EB",]

dee_eb_sex <- ggplot(dlw_eb, aes(x=Sex,y=kJ.day.g)) + ggtitle("Daily Energy Expenditure by Sex") +
  geom_boxplot(col="darkgreen", drop=T) + theme_bw() + xlab("Empress Brilliants") + ylab("kJ/day*g")
dee_eb_sex
