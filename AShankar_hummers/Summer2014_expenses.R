library(reshape)

summer2014 <- read.csv("C:\\Users\\ANUSHA\\Desktop\\Ecuador_Expenses2014.csv")
head(summer2014)

m.summ <- melt(summer2014,id.vars = "Paid_by", measure.vars = "Amount_USD", na.rm = T)
AnushaSpent <- (m.summ$value[m.summ$Paid_by=="Anusha"])
sum(AnushaSpent)

CatherineSpent <- sum(m.summ$value[m.summ$Paid_by=="Catherine"])

WolfieMart <- sum(m.summ$value[m.summ$Paid_by=="WolfieMart"])

sum(AnushaSpent,CatherineSpent,WolfieMart)
