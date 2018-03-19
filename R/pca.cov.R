#' Mining PCA algorithm
#'
#' pca algorithm to iteratively check principal componenets with matched sample covariate data
#' @param dat matrix with desired measure to run pca on
#' @param p dataframe of covariates to test with each pc; covariate data should match with the pca order of samples; covariate data should be categorical
#' @param ignore columns to ignore or not use
#' @param showall TRUE or FALSE; if TRUE, a matrix of all pvalues will be shown
#' @keywords microarray
#' @export
#' @examples
#' pca.cov()
pca.cov <- function(dat, p, ignore, showall) {
  if (!missing(ignore)) {
    p <- p[,-ignore]
  } #remove unused columns for algorithm to check
  for (i in 1:dim(p)[2]) {
    if (!((is.factor(p[,i])) | (is.character(p[,i])))) {
      print("not all columns in p are categorical")
      stop()
    }
  }
  X <- t(sweep(t(dat), 2, colMeans(t(dat)), "-")) #centering the data by subtracting the means
  sv <- svd(t(X)) #svd
  V <-  sv$v
  Z <- t(X)%*%V
  for (i in 1:dim(p)[2]) {
    if (!"results" %in% ls()) {
      results <- data.frame("COV" = NA)
    }
    results[i, "COV"] <- names(p)[i]
    for (j in 1:dim(Z)[2]) {
      results[i, j+1] <- kruskal.test(Z[,j], p[,i])$p.value
    }
  }
  for (i in 2:dim(results)[2]) {
    results[,i] <- p.adjust(results[,i], method = "bonferroni")
  }
  k <- 2
  for (i in 2:dim(results)[2]) {
    if (!"answers" %in% ls()) {
      answers <- data.frame("cov" = results[,1])
    }
    tmp <- results[,i] < 0.05
    if (sum(tmp) > 0) {
      answers <- cbind(answers, results[,i])
      colnames(answers)[k] <- paste0("pc", i-1)
      k <- k + 1
    }
  }
  stored <- results
  rm(results)
  if (dim(answers)[2] > 2) {
    for (i in 1:dim(answers)[1]) {
      tmp <- answers[,-1]  < 0.05
      if (sum(tmp[i,]) > 0) {
        if (!"results" %in% ls()) {
          results <- answers[i,]
        } else {
          results <- rbind(results, answers[i,])
        }
      }
    }
  } else {
    if (dim(answers)[2] == 1) {
      results <- "no significant associations"
    } else {
      for (i in 1:dim(answers)[1]) {
        tmp <- answers[,-1]  < 0.05
        if (sum(tmp[i]) > 0) {
          if (!"results" %in% ls()) {
            results <- answers[i,]
          } else {
            results <- rbind(results, answers[i,])
          }
        }
      }
    }
  }
  if (!exists("showall")) {
    showall <- FALSE
  }
  if (showall == TRUE) {
    return(stored)
  } else {
    return(results)
  }
}