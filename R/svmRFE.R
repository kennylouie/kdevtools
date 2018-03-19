#' Support vector machine recursive feature elimination 'svm-rfe'
#'
#' This function allows you to conduct recursive feature elimination using support vector machines
#' @import e1071
#' @param dat a matrix of n samples x m features
#' @param group a class of factor of the coresponding classes for each of the n samples in the matrix
#' @keywords svm rfe machine learning
#' @export
#' @examples
#' svmRFE
svmRFE <- function(dat, group) {
  numberFeatures <- dim(dat)[2]
  survivingFeaturesIndexes <- seq(1:numberFeatures)
  featureRankedList <- vector(length=numberFeatures)
  rankedFeatureIndex <- numberFeatures

  while(length(survivingFeaturesIndexes) > 0) {
    #train the svm model
    svmMod <- svm(as.matrix(dat[,survivingFeaturesIndexes]), group, kernel = "linear")

    #obtain the scores
    rankingScores <- svmMod$w * svmMod$w

    #rank the features by their respective scores
    ranking <- sort(rankingScores)

    #update the feature ranked list
    featureRankedList[rankedFeatureIndex] <- survivingFeaturesIndexes[ranking[1]]
    rankedFeatureIndex <- rankedFeatureIndex - 1

    #elminate the feature with the smallest ranking criterion
    (survivingFeaturesIndexes = survivingFeaturesIndexes[-ranking[1]])
  }
  return(featureRankedList)
}
