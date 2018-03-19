#' A summary function for limma fits
#'
#' Organizing p-values and methylation differences between groups into a single data frame
#' @importFrom limma topTable
#' @import dplyr
#' @param dat a matrix of the values of interest with the corresponding sample labels and probe labels
#' @param p a dataframe containing the group column matched with the sample labels in dat
#' @param fit a limma fit
#' @param grouping the name of the column in which has the group parameter
#' @param contrast.matrix a model matrix; make sure the levels of the contrasts are the same as the levels in the grouping variable
#' @param adj.method the multiple testings correction method as used in topTable function, i.e. fdr BH etc.
#' @param pval the desired p-value
#' @keywords microarray
#' @export
#' @examples
#' limmaTable()
limmaTable <- function(dat, p, fit, grouping, contrast.matrix, adj.method, pval) {
  grp.naming <- grouping
  contrast <- colnames(contrast.matrix)
  fit <- fit
  for (i in 1:length(contrast)) {
    tmp <- topTable(fit = fit, coef = i, adjust.method = adj.method, number = Inf, p.value = pval)
    if (dim(tmp)[1] > 0) {
      tmp <- tmp %>% mutate(probe = rownames(.)) %>% mutate(comparison = contrast[i]) %>% dplyr::select(probe, adj.P.Val, comparison)
      group1 <- unlist(strsplit(contrast[i], "-"))[1]
      group2 <- unlist(strsplit(contrast[i], "-"))[2]
      p.index <- p[,grp.naming]
      ind.1 <- p.index == group1
      ind.2 <- p.index == group2
      d.tmp <- dat[tmp$probe,]
      if (is.null(dim(d.tmp))) {
        d.grp1 <- d.tmp[ind.1]
        d.grp2 <- d.tmp[ind.2]
        mean_grp1 <- mean(d.grp1)
        sd_grp1 <- sd(d.grp1)
        mean_grp2 <- mean(d.grp2)
        sd_grp2 <- sd(d.grp2)
      } else {
        d.grp1 <- d.tmp[,ind.1]
        d.grp2 <- d.tmp[,ind.2]
        mean_grp1 <- rowMeans(d.grp1)
        sd_grp1 <- apply(d.grp1, 1, sd)
        mean_grp2 <- rowMeans(d.grp2)
        sd_grp2 <- apply(d.grp2, 1, sd)
      }
      tmp[,"Mean1"] <- mean_grp1 * 100
      tmp[,"SD1"] <- sd_grp1 * 100
      tmp[,"Mean2"] <- mean_grp2 * 100
      tmp[,"SD2"] <- sd_grp2 * 100
      tmp[,"diffmean"] <- tmp[,"Mean2"] - tmp[,"Mean1"]
      tmp[,"diffsd"] <- tmp[,"SD1"] - tmp[,"SD2"]
    } else {
      tmp <- data.frame(probe = NA, adj.P.Val = NA, comparison = contrast[i], Mean1 = NA, SD1 = NA, Mean2 = NA, SD2 = NA, diffmean = NA, diffsd = NA)
    }
    if (!"results" %in% ls()) {
      results <- tmp
    } else {
      results <- rbind(results, tmp)
    }
  }
  return(results)
}
