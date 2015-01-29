library(reshape)

summer2014 <- read.csv("E:\\Git\\Thesis\\Summer_expenses.csv")
head(summer2014)

m.summ <- melt(summer2014,id.vars = "Paid.by", measure.vars = "Amount..")
AnushaSpent <- sum(m.summ$value[m.summ$Paid.by=="Anusha"])

CatherineSpent <- sum(m.summ$value[m.summ$Paid.by=="Catherine"])

WolfieMart <- sum(m.summ$value[m.summ$Paid.by=="WolfieMart"])

sum(AnushaSpent,CatherineSpent,WolfieMart)
