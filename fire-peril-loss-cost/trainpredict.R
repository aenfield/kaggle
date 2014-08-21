# -- FUNCTIONS
# metric function, per competition
WeightedGini <- function(solution, weights, submission) {
  df = data.frame(solution = solution, weights = weights, submission = submission)
  df <- df[order(df$submission, decreasing = TRUE),]
  df$random = cumsum((df$weights/sum(df$weights)))
  totalPositive <- sum(df$solution * df$weights)
  df$cumPosFound <- cumsum(df$solution * df$weights)
  df$Lorentz <- df$cumPosFound / totalPositive
  n <- nrow(df)
  gini <- sum(df$Lorentz[-1]*df$random[-n]) - sum(df$Lorentz[-n]*df$random[-1])
  return(gini)
}

NormalizedWeightedGini <- function(solution, weights, submission) {
  WeightedGini(solution, weights, submission) / WeightedGini(solution, weights, solution)
}
# var11 <- c(1, 2, 5, 4, 3)
# pred <- c(0.1, 0.4, 0.3, 1.2, 0.0)
# target <- c(0, 0, 1, 0, 1)
# 
# the above scores -0.8214286, when called using (target, var11, pred)

scaleZeroOne = function(x) {
  (x - min(x)) / (max(x) - min(x))
}

convertToFactors = function(x) {
  x$var1 = factor(x$var1, ordered = TRUE)
  x$var3 = factor(x$var3, ordered = TRUE)
  x$var4 = factor(x$var4)
  x$var5 = factor(x$var5)
  x$var7 = factor(x$var7, ordered = TRUE)
  x$var8 = factor(x$var8, ordered = TRUE)
  x$var9 = factor(x$var9) 
  x
}


# train and predict
library(data.table)
library(caret)
library(dplyr)

setwd("/Users/aenfield/work/kaggle/fire-peril-loss-cost/")
trainRaw = tbl_df(fread("./sourcedata/train.csv"))
testRaw = tbl_df(fread("./sourcedata/test.csv"))

# investigation
# > nearZeroVar(trainRaw)
nearZeroVarFeaturesTrainIdx = c(4,8,68,70,72,74,75,79,83,85,88,95,96,105,107,109,111,112,116,120,122,125,127,132,133,134,141,143,145,147,148,150,152,156,158,161,163,165,168,169,177,179,181,183,184,186,191,195,197,198,202,203,204,210,211,213,215,217,218,220,222,226,228,231,233,234,235,238,239,240,246,267,268,276,277,285,286,294,295)
nearZeroVarFeaturesTestIdx = nearZeroVarFeaturesTrainIdx - 1  # test doesn't include column 2 - target - so indexes need to be adjusted down by one
trainTarget = trainRaw$target
trainFeatures = trainRaw[,-nearZeroVarFeaturesTrainIdx]
trainFeatures = trainFeatures[,-18]  # remove 'dummy'
trainFeatures = convertToFactors(trainFeatures)
testFeatures = testRaw[,-nearZeroVarFeaturesTestIdx]
testFeatures = testFeatures[,-17] # remove dummy
testFeatures = convertToFactors(testFeatures)


library(Cubist)
cubistModel = cubist(trainFeatures[,-2], trainTarget)
trainPredsCubist = predict(cubistModel, newdata=trainFeatures)
testPredsCubist = predict(cubistModel, newdata=testFeatures)
preds = testPredsCubist

# playing with pre processing, including creating dummy vars from factors
# dummyVarInfo <- dummyVars(target ~ .-id, data = trainFeatures)
# trainFeaturesWithDummyVars = predict(dummyVarInfo, newdata=trainFeatures)
# trans = preProcess(trainFeaturesWithDummyVars, method = c("BoxCox"))

# playing with manually-created and used cross-validation folds
# set.seed(1)
# cvSplits = createFolds(trainTarget, k=10, returnTrain=TRUE)
# fold1 = cvSplits[[1]]
# cvPredictors1 = trainFeatures[fold1,]
# cvTargets1 = trainTarget[fold1]
  
# playing with a simple model - manual computation of target using two features
# test = data.frame(cbind(testRaw[, var13], testRaw[,var15]))
# colnames(test) = c("var13","var15")
# test$var15[is.na(test$var15)] = 0.0
# target = scaleZeroOne(-test$var13) + scaleZeroOne(test$var15)
  
# generate submission
test.ids = fread("./sourcedata/sampleSubmission.csv")
submit.file.conn = gzfile("./submissions/submit.csv.gz")
write.table(data.frame(id=test.ids$id, target=preds), submit.file.conn, sep=",", row.names=F, quote=F)
close(submit.file.conn)

#NormalizedWeightedGini(trainRaw$target, trainRaw$var11, target)
