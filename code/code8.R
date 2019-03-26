library(tree)
library(dplyr)
library(readr)
library(ggplot2)

nycSales <- read_csv("Data/nycSales.csv")

nycSales <- nycSales %>% 
    filter(borough %in% c("manhattan", "brooklyn")) %>% 
    select(borough, price, num_beds,
           num_baths, num_sqft, type, 
           built_date, monthly_cost) %>% 
    mutate(borough = as.factor(borough),
           index = 1:n(),
           num_baths = as.numeric(num_baths),
           num_beds = as.numeric(num_beds),
           type = as.factor(type)) %>% 
    filter(complete.cases(.))

str(nycSales)

ggplot(nycSales, aes(price)) + geom_histogram(aes(fill=borough))

nycSalesTree <- tree(formula = borough ~ . -index, data = nycSales)
summary(nycSalesTree)

plot(nycSalesTree)
text(nycSalesTree, pretty = 0)

set.seed(123)
subset <- nycSales %>% group_by(borough) %>% 
    sample_frac(0.5)
set.seed(123)
train <- subset %>% group_by(borough) %>% 
    sample_frac(0.7) 
test <- subset %>% filter(!index %in% train$index)

nycSalesTrain <- tree(borough~.-index, data = train)
plot(nycSalesTrain)
text(nycSalesTrain, pretty = 0)

nycSalesPred <- predict(nycSalesTrain, test, type = "class")
nycSalesPred

library(caret)
confusionMatrix(nycSalesPred, test$borough)

nycSalesCV <- cv.tree(nycSalesTrain, FUN = prune.misclass)
nycSalesCV
plot(nycSalesCV)

nycSalesPrune <- prune.misclass(nycSalesTrain, best = 4)

plot(nycSalesPrune)
text(nycSalesPrune, pretty = 0)

prunePred <- predict(nycSalesPrune, test, type = "class")
confusionMatrix(prunePred, test$borough)


nycSalesReg <- tree(price~.-index, train)
summary(nycSalesReg)

plot(nycSalesReg)
text(nycSalesReg, pretty = 0)


library(randomForest)
nycSalesRF <- randomForest(borough~.-index, train)
nycSalesRF
nycSalesRF$err.rate
str(nycSalesRF)

oobErr <- double(7)
testErr <- double(7)

for(mtry in 1:7){
    fit <- randomForest(borough~.-index, train, mtry = mtry, ntree = 200)
    oobErr[mtry] <- fit$err.rate[200]
    pred <- predict(fit, test)
    testErr[mtry] <- with(test, sum(borough!=pred)/nrow(test))
}

oobErr
testErr

matplot(1:mtry, cbind(oobErr, testErr), pch = 23,
        col = c("red","blue"), type = "b", ylab = "classification Error Rate")
legend("topright", legend = c("oob", "test"), pch = 23, col = c("red", "blue"))


