#' A pairwise testing using tukey HSD after anova modelling
#'
#' This function allows you to do tukey pairwise testings of numeric data between members of a population with >2 groups after modelling with anova
#' @import dplyr
#' @param dat a dataframe with at least a column with the grouping and another with the numeric data
#' @param var the name of the column of the dependent variable
#' @param grouping the name of the column of the independing variable, i.e. the grouping
#' @keywords pairwise
#' @export
#' @examples
#' pairwise.anova()
pairwise.anova <- function(dat, var, grouping) {
  if (!is.numeric(dat$var) | !is.integer(dat$var)) {
    print("ensure var is a numeric or integer class")
    stop
  }
  if (!is.factor(dat$grouping)) {
    print("ensure that grouping is a factor")
    stop
  }
  tmp.f <- dat %>% select(var, grouping)
  f <- formula(paste0(var, " ~ ", grouping))
  a <- aov(f, tmp.f)
  results <- TukeyHSD(a, conf.level = 0.95)
  return(results)
}
