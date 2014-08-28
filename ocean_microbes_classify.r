rm(list=ls())

library("caret")
library("ggplot2")
library("rpart")
library("randomForest")
library("e1071")

file <- read.table("/home/paras/work/Courses/Intro_Data_Science/R_classification/seaflow_21min.csv", header = TRUE, sep =",", col.names=c("file_id", "time", "cell_id", "d1", "d2", "fsc_small", "fsc_perp", "fsc_big", "pe", "chl_small", "chl_big", "pop"))

summary(file)

head(file)

# file <- file[!file$file_id == "208", ]

trainindex <- createDataPartition(y=file$pop, times=1, p=0.5, list=FALSE)

traindata <- file[trainindex,]
testdata <- file[-trainindex,]

ggplot(file, aes(x = file$pe, y = file$chl_small, color = file$pop)) + geom_point(size = 2)

fol <- formula(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small)

model <- rpart(fol, method="class", data=traindata)

# model <- randomForest(fol, data=traindata)

# model <- svm(fol, data=traindata)

print(model)

results <- predict(model, testdata, type = "class")

resultscompare <- results == testdata$pop

correctpredictions <- sum(resultscompare)

accuracy <- correctpredictions/length(testdata$pop)

importance(model)

uniqueness = length(unique(file$chl_big))/length(file$chl_big) #relative

table(pred = results, true = testdata$pop)
