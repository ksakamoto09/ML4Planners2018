filenames <- list.files("~/Documents/Spring2017/PLUTO/", pattern = ".csv",full.names = T)
ldf <- lapply(filenames, read.csv)
pluto <- do.call("rbind", ldf)


BK <- read.csv("~/Documents/Spring2017/PLUTO/BK.csv", header = T, stringsAsFactors = F)

    url <- "https://cran.r-project.org/web/packages/osc/vignettes/paper.pdf"

library(ggvis)
    library(dplyr)
df <- BK[,c("NumFloors","LotArea","ZoneDist1")]
table(df$ZoneDist1)
df[grepl("/", df$ZoneDist1), "Zone"]  <- "Mixed" 
df[grepl("^R", df$ZoneDist1),"Zone"] <- "Res" 
df[grepl("^M", df$ZoneDist1), "Zone"] <- "Ind"
df[grepl("^C", df$ZoneDist1), "Zone"] <- "Com"
df[grepl("^P", df$ZoneDist1),"Zone"] <- "Prk"
table(df$Zone)
df[complete.cases(df),] -> df
normalize <- function(x) {
    num <- x - min(x)
    denom <- max(x) - min(x)
    return (num/denom)
}
dfNorm <- as.data.frame(lapply(df[1:2], normalize))
dfNorm$Zone <- df$Zone
set.seed(1234)
dfNorm %>% filter(Zone == "Com" | Zone == "Ind") %>% sample_frac(size = 0.05) -> dfSample
table(dfSample$Zone)

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

dfSample%>% ggvis(~NumFloors, ~LotArea, fill = ~Zone ) %>% layer_points()

trainDF <- dfSample[ind==1, 1:2]
testDF <-  dfSample[ind==2, 1:2]

trainLabel <- dfSample[ind==1, 3]
testLabel <- dfSample[ind==2, 3]

library(class)
dfPred <- knn(trainDF, testDF, trainLabel, 9)
dfPred

library(gmodels)

CrossTable(testLabel,dfPred,prop.chisq = F)
