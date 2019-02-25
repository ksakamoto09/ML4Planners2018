library(readr)

housing <- read_csv("http://www.jaredlander.com/data/housing.csv")

cor(housing$Gross.SqFt, housing$Estimated.Gross.Income)

cor(housing %>% select(Gross.Income.per.SqFt,Gross.SqFt,Total.Units,Expense.per.SqFt))

library(GGally)
ggpairs(housing %>% select(Gross.Income.per.SqFt, Gross.SqFt, Total.Units, Expense.per.SqFt))

cov(housing$Gross.SqFt, housing$Estimated.Gross.Income)

sum((housing$Gross.SqFt - mean(housing$Gross.SqFt)) * 
    (housing$Estimated.Gross.Income - mean(housing$Estimated.Gross.Income)))/nrow(housing)

library(dplyr)
housing %>% select(Gross.SqFt, Estimated.Gross.Income) %>% head()

library(ggplot2)
housing %>% ggplot(aes(x=Gross.SqFt, y=Estimated.Gross.Income)) + geom_point(shape=1) +
    geom_smooth(method = "lm") + theme_minimal() + ylab("Income") + xlab("SqFt")

lmMod <- lm(Estimated.Gross.Income~Gross.SqFt, data = housing)
summary(lmMod)

pred <- predict.lm(lmMod, data.frame(Gross.SqFt = housing$Gross.SqFt))
# Mean Squared Error
meansquareE <- sum(residuals(lmMod)^2) / df.residual(lmMod)
# Root Mean Squared Error
sqrt(meansquareE)
# R^2
1 - sum((housing$Estimated.Gross.Income - pred)^2) / 
    sum((housing$Estimated.Gross.Income - mean(housing$Estimated.Gross.Income, na.rm = TRUE))^2)




