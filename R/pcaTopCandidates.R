#' Determination of top weighted eigengenes for a specific PC
#'
#' function to extract out the top weighted eigengenes of a given matrix and specific PC; top is determined by 1.5* the IQR of the eigenvector
#' @param dat matrix with desired measure to run pca on
#' @param PCnumber the PC of interest
#' @keywords microarray
#' @export
#' @examples
#' pcaTopCandidates()

pcaTopCandidates <- function(dat, PCnumber) {
  X <- t(sweep(t(dat), 2, colMeans(t(dat)), "-")) # centering the data by subtracting the means
  sv <- svd(t(X)) # singular value decomposition
  V <-  sv$v
  vSig <- data.frame(probe = rownames(X), pc = V[,PCnumber])
  vSigQ1 <- quantile(vSig$pc)[2] # 1st quantile of the weights
  vSigQ3 <- quantile(vSig$pc)[4] # 3rd quantile of the weights
  vIqr <- iqr(vSig$pc) # the inter-quartile range
  thres <- vIqr*1.5 # outlier is defined as 1.5xiqr
  upper.thres <- vSigQ3 + thres # calculating the upper threshold to beat
  lower.thres <- vSigQ1 - thres # calculating the lower threshold to beat
  upper.genes <- vSig[vSig$pc > upper.thres,] # filtering the genes above the upper threshold
  lower.genes <- vSig[vSig$pc < lower.thres,] # filtering the genes below the lower threshold
  candGenes <- rbind(upper.genes, lower.genes) # combine both lower and upper into single dataframe
  return(candGenes)
}
