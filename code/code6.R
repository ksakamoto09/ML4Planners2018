library(readr)

housing <- read_csv("http://www.jaredlander.com/data/housing.csv")

names(housing) <- c("Neighborhood", "Class", "Units",
                    "YearBuilt", "SqFt", "Income", "IncomePerSqFt",
                    "Expense", "ExpensePerSqFt", "NetIncome", "Value",
                    "ValuePerSqft", "Boro")
names(housing)

incomeLM <- lm(Income ~ SqFt, data = housing) 

summary(incomeLM)

## outliers

library(car)
outlierTest(incomeLM)


leveragePlots(incomeLM)

cutoff <- 4/((nrow(housing)-length(incomeLM$coefficients)-2))
cutoff
plot(incomeLM, which = 4, cook.levels =cutoff)

influencePlot(incomeLM, id.method="identify", main="Influence Plot")

car::qqPlot(incomeLM)

sresid <- MASS::studres(incomeLM)
data.frame(sresid) %>% ggplot(aes(x=sresid)) +
    geom_histogram(binwidth = 0.5) +theme_minimal()

spreadLevelPlot(incomeLM)


crPlots(incomeLM)


housing %>% ggplot(aes(ValuePerSqft)) + geom_histogram()

housing %>% ggplot(aes(ValuePerSqft, fill=Boro)) + geom_histogram() +
    facet_wrap(~Boro)

housing %>% ggplot(aes(SqFt, ValuePerSqft, color=Boro)) +
    geom_point(shape = 1, alpha = .2) +
    theme_minimal() +
    facet_wrap(~Boro)

housing %>% ggplot(aes(Units, ValuePerSqft, color=Boro)) +
    geom_point(shape = 1, alpha = .2) +
    theme_minimal() +
    facet_wrap(~Boro)
housing %>% ggplot(aes(YearBuilt, ValuePerSqft, color=Boro)) +
    geom_point(shape = 1, alpha = .2) +
    theme_minimal() +
    facet_wrap(~Boro)

condo1 <- lm(ValuePerSqft ~ Units + SqFt + Boro + YearBuilt, data = housing)


summary(condo1)


housing %>% ggplot(aes(Units, ValuePerSqft)) +geom_point() +geom_smooth(method ="lm")

condo1$coefficients

library(coefplot)
coefplot(condo1, sort ="mag", coefficients = condo1$coefficients[-1] %>% names() )
scale(housing$YearBuilt)
condo2 <- lm(ValuePerSqft ~ scale(Units) + scale(SqFt) + Boro + scale(YearBuilt), 
             data = housing)
summary(condo2)

coefplot(condo2, sort = "mag")

multiplot(condo1, condo2)

condo3 <- lm(ValuePerSqft ~ Units + SqFt * Boro + YearBuilt, data = housing)
summary(condo3)

condo4 <- lm(ValuePerSqft ~ Units + SqFt : Boro + YearBuilt, data = housing)
summary(condo4)

condo5 <- lm(ValuePerSqft ~ Units + SqFt*Boro + SqFt*Class + YearBuilt, data =housing)
summary(condo5)

coefplot(condo5, sort ="mag", coefficients = condo5$coefficients[-1] %>% names())

nullModel <- lm(ValuePerSqft ~ 1, data = housing %>% filter(complete.cases(.)))
length(condo4$residuals)
nrow(housing)

step(nullModel,
     scope = list(lower = nullModel, upper = condo5),
     direction = "both"
     )
library(relaimpo)
condoFact <- lm(ValuePerSqft ~ Units + SqFt + as.factor(Boro) + YearBuilt, 
                data = housing)
crlm <- calc.relimp(condoFact,
                    type =c("lmg", "last", "first"),
                    rela =TRUE)
plot(crlm)

