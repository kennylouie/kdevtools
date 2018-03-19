#' Leave-one-out-cross-validation with a Support Vector Machine classifier
#'
#' a cross validation method to check the accuracy of a svm classifier on a matrix of features and samples
#' @import e1071
#' @param dat matrix of features in columns and samples in rows
#' @param group vector of classifications for each sample in the same order as the rows in the matrix
#' @param ... all other arguments/parameters for the svm function from the e1071 package
#' @keywords svm leave-one-out-cross-validation
#' @export
#' @examples
#' svmLoocv()
svmLoocv <- function(dat, group, ...) {

  ## loocv svm model

  ## dataframe to store results
  results <- data.frame("prediction"=NA, "actual"=NA)

  ## loocv
  for (ii in 1:dim(dat)[1]) {
    test <- t(as.data.frame(dat[ii,]))
    train <- dat[-ii,]
    trainGroup <- group[-ii]
    svm.model <- svm(train, trainGroup, ...)
    svm.pred <- predict(svm.model, test)
    results[ii, "prediction"] <- as.character(svm.pred)
    results[ii, "actual"] <- as.character(group[ii])
  }

  ## overall accuracy score
  results[,"score"] <- results[,"actual"] == results[,"prediction"]
  overallScore <- sum(results$score)/dim(results)[1]

  ## within group scores

  ## dataframe to store results
  scoring <- data.frame(group=NA, score=NA)

  ## iterative subsetting of loocv results to calculate scores
  numberGroups <- length(levels(group))
  levelGroups <- levels(group)
  for (jj in 1:numberGroups) {
    scoring[jj,"group"] <- levelGroups[jj]
    resultsSubset <- subset(results, actual == levelGroups[jj])
    scoring[jj,"score"] <- sum(resultsSubset[,"score"])/dim(resultsSubset)[1]
  }

  ## compile final list of results
  finalResults <- new.env()
  finalResults$results <- results
  finalResults$accuracy <- overallScore
  finalResults$groupScores <- scoring
  return(finalResults)
}
