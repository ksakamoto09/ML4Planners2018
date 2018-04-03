facilities <- read.csv("~/Documents/Spring2017/facilities.csv", stringsAsFactors = F)
df <- facilities[,c("boro", "latitude", "longitude")]

smp_size <- floor(0.75 * nrow(df))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind,2:3 ]
test <- df[-train_ind, 2:3]
train_label <- df[train_ind, 1]
test_label <- df[-train_ind, 1]

library(ggplot2)
ggplot(test, aes(x = longitude, y = latitude)) + geom_point()

library(class)
Boro_Pred <- knn(train, test, train_label, k = 300)

library(gmodels)

CrossTable(x = test_label, y = Boro_Pred, prop.chisq = F)

## k-means
clusterdf <- df[,2:3]
kmeans(clusterdf, 5) -> k5
k5$size
k5$centers
clusterdf$cluster <- as.factor(k5$cluster)

ggplot(clusterdf, aes(x= longitude, y = latitude, colour = cluster)) + geom_point()
