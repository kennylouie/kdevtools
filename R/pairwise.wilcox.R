#' A pairwise testing using wilcox
#'
#' This function allows you to do wilcox pairwise testings of numeric data between members of a population with >2 groups
#' @import dplyr
#' @param dat a dataframe with at least a column with the grouping and another with the numeric data
#' @param var the name of the column of the dependent variable
#' @param grouping the name of the column of the independing variable, i.e. the grouping
#' @param adj.method the multiple testings correction method as used in p.adjust, e.g. "bonferroni" or "BH" or "fdr"
#' @keywords pairwise
#' @export
#' @examples
#' pairwise.wilcox()
pairwise.wilcox <- function(dat, var, grouping, adj.method) {
  if (!is.numeric(dat[,var]) | !is.integer(dat[,var])) {
    print("ensure var is numeric or an integer")
    stop
  }
  if (!is.factor(dat[,grouping])) {
    print("ensure that grouping is a factor")
    stop
  }
  for (i in 1:length(levels(dat[,grouping]))) {
    for (j in 2:length(levels(dat[,grouping]))) {
      if (i != j) {
        if (i < j) {
          group1 <- levels(dat[,grouping])[i]
          group2 <- levels(dat[,grouping])[j]
          tmp.sub <- dat %>% filter(get(grouping) == group1 | get(grouping )== group2)
          tmp <- data.frame("grp1" = group1, "grp2" = group2, "pval" = NA)
          f <- formula(paste0(var, " ~ ", grouping))
          tmp[,"pval"] <- wilcox.test(f, data = tmp.sub)$p.value
          if (!"results" %in% ls()) {
            results <- tmp
          } else {
            results <- rbind(results, tmp)
          }
        }
      }
    }
  }
  results[,"adj.pval"] <- p.adjust(results[,"pval"], method = adj.method)
  return(results)
}
