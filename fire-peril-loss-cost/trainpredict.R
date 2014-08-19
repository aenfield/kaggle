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


# train and predict
library(data.table)
library(caret)

setwd("/Users/aenfield/work/kaggle/fire-peril-loss-cost/")

testRaw = fread("./sourcedata/test.csv")
#testRaw = fread("./sourcedata/train.csv")

test = data.frame(cbind(testRaw[, var13], testRaw[,var15]))
colnames(test) = c("var13","var15")

test$var15[is.na(test$var15)] = 0.0

target = scaleZeroOne(-test$var13) + scaleZeroOne(test$var15)
  
test.ids = fread("./sourcedata/sampleSubmission.csv")
submit.file.conn = gzfile("./submissions/submit.csv.gz")
write.table(data.frame(id=test.ids$id, target=target), submit.file.conn, sep=",", row.names=F, quote=F)
close(submit.file.conn)

#NormalizedWeightedGini(testRaw$target, testRaw$var11, target)
