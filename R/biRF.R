#' Balanced iterative random forest
#'
#' A random forest embedded method for feature selection
#' @import randomForest
#' @param dat matrix of features in columns and samples in rows
#' @param group vector of classifications for each sample in the same order as the rows in the matrix
#' @param ... all other arguments/parameters for the randomForest function from the randomForest package
#' @keywords random forest feature selection balanced
#' @export
#' @examples
#' biRF()
biRF <- function(dat, group, ...) {
  current.OOB.error <- 1
  previous.OOB.error <- 2
  while(current.OOB.error < previous.OOB.error) {
    rfModel <- randomForest(dat, group, ...)
    imp <- as.data.frame(importance(rfModel))
    dat <- dat[,imp > 0]
    errors <- rfModel$err.rate
    previous.OOB.error <- current.OOB.error
    current.OOB.error <- mean(errors[,1])
  }
  return(dat)
}
