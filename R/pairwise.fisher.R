#' A pairwise testing using fisher
#'
#' This function allows you to do fisher.test pairwise testings of categorical count data between members of a population with >2 groups
#' @import dplyr
#' @param dat a dataframe containing at least a column with the grouping and another with the categorical value
#' @param var the name of the column of the dependent variable
#' @param grouping the name of the column of the independent variable, i.e. the grouping of interest
#' @param adj.method the multiple testings correction method as used in p.adjust, e.g. "bonferroni" or "BH" or "fdr"
#' @keywords pairwise
#' @export
#' @examples
#' pairwise.fisher()
pairwise.fisher <- function(dat, var, grouping, adj.method) {
  if (!is.factor(dat[,var])) {
    print("ensure var is a factor")
    stop
  }
  if (!is.factor(dat[,grouping])) {
    print("ensure that grouping is a factor")
    stop
  }
  tmp.f <- dat %>% select(var, grouping)
  tmp.f <- apply(tmp.f, 2, function(x) as.factor(as.character(x))) %>% as.data.frame # removes any levels that are not present
  groupings <- levels(tmp.f[,grouping])
  for (i in 1:length(groupings)) {
    tar.tmp <- groupings[i]
    tmp.f.mut <- tmp.f %>% mutate(tmp.col = ifelse(get(grouping) == tar.tmp, as.character(tar.tmp), "all others"))
    dat.tmp <- tmp.f.mut %>% select(var, tmp.col) %>% table
    tmp <- data.frame("target" = tar.tmp, "pval" = NA)
    tmp[,"pval"] <- fisher.test(dat.tmp)$p.value
    if (!"results.tmp" %in% ls()) {
      results.tmp <- tmp
    } else {
      results.tmp <- rbind(results.tmp, tmp)
    }
  }
  ## if there are significant differences, do pairwise
  if (sum(results.tmp$pval < 0.05) > 0) {
    tmp.sig <- results.tmp %>% filter(pval < 0.05) # to get the groups that are significant
    targets <- tmp.sig$target %>% as.character # selecting the groups
    var.targets <- levels(tmp.f[,var]) # the list of variables to loop through
    for (k in 1:length(var.targets)) {
      var.tmp <- var.targets[k] # loop through all the vars
      for (i in 1:length(groupings)) {
        tar.tmp.1 <- groupings[i]
        for (j in 2:length(groupings)) {
          if (i < j) { # to not repeat the comparisons in vice versa order
            tar.tmp.2 <- groupings[j]
            if (!tar.tmp.1 %in% targets && !tar.tmp.2 %in% targets) {
              next # check that the grouping is one of the significant groups, if not, move on
            }
            tmp.dat <- tmp.f %>% filter(get(grouping) == tar.tmp.1 | get(grouping) == tar.tmp.2) # filter out only the 2 groupings of interest
            tmp.dat[,grouping] %<>% as.character %>% as.factor
            tmp.f.mut <- tmp.dat %>% mutate(tmp.tar = ifelse(get(var) == var.tmp, as.character(var.tmp), "all others")) # determine which var is of interest while others will be grouped together
            tmp.tab <- tmp.f.mut %>% select(grouping, tmp.tar) %>% table
            if (dim(tmp.tab)[1] != 2 | dim(tmp.tab)[2] != 2) {
              next
            }
            tmp <- data.frame("target" = paste0(var.tmp, ": ", tar.tmp.1, " vs. ", tar.tmp.2), "pval" = NA)
            tmp[,"pval"] <- fisher.test(tmp.tab)$p.value
            if (!"results" %in% ls()) {
              results <- tmp
            } else {
              results <- rbind(results, tmp)
            }
          }
        }
      }
    }
  }
  results[,"adj.pval"] <- p.adjust(results$pval, method = adj.method)
  results.tmp[,"adj.pval"] <- p.adjust(results.tmp$pval, method = adj.method)
  fini <- rbind(results.tmp, results)
  return(fini)
}
