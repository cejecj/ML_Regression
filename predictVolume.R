# import libraries
library(caret)
library(ggplot2)
library(corrplot)
library(dplyr)

# load dataset
existingProducts <- read.csv('existingproductattributes2017.csv')
newProducts <- read.csv('newproductattributes2017.csv')

# explore data and convert data types
str(existingProducts)
str(newProducts)
existingProducts$Volume <- as.numeric(existingProducts$Volume)
newProducts$Volume <- as.numeric(newProducts$Volume)
ggplot(existingProducts, aes(x = ProductType, fill = ProductNum)) +
  theme_bw() +
  geom_bar() +
  labs(x = 'Product type',
       y = 'Number',
       title = 'Products Reviewed')

# convert to dummy vars
existingProducts_new <- dummyVars(' ~ .', data = existingProducts)
readyData <- data.frame(predict(existingProducts_new, newdata = existingProducts))
newProducts_new <- dummyVars(' ~ .', data = newProducts)
readyData_new <- data.frame(predict(newProducts_new, newdata = newProducts))

# review new dataframe
str(readyData)
summary(readyData)
readyData$BestSellersRank <- NULL

# correlation matrix
corrplot(cor(readyData))

# feature engineering
corrplot(cor(readyData))

# regression model
set.seed(123)
trainSize <- round(nrow(readyData) * .7)
testSize <- nrow(readyData) - trainSize
trainingIndices <- sample(seq_len(nrow(readyData)), size = trainSize)
trainingSet <- readyData[trainingIndices,]
testingSet <- readyData[-trainingIndices,]
model <- lm(Volume ~ ., trainingSet)
summary(model)

# fit control
fitControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)

# random forest model
rfGrid <- expand.grid(mtry = c(3))
system.time(rfFit <- train(Volume ~ ., data = trainingSet, method = 'rf', trControl = fitControl, tuneGrid = rfGrid))
rfPrediction <- predict(rfFit, newdata = testingSet)
rfFit

# gradient boosted
system.time(gbmFit <- train(Volume ~ ., data = trainingSet, method = 'gbm', trControl = fitControl, verbose = FALSE))
gbmPrediction <- predict(gbmFit, newdata = testingSet)
gbmFit

# support vector machines
system.time(svmFit <- train(Volume ~ ., data = trainingSet, method = 'svmLinear2',
                            trControl = fitControl, tuneLength = 10))
svmPrediction <- predict(svmFit, newdata = testingSet)
svmFit

# make final predictions
rfPredictionsNew <- predict(rfFit, newdata = readyData_new)
rfPredictionsNew
newProducts$Volume <- rfPredictionsNew
newProducts %>% View()
