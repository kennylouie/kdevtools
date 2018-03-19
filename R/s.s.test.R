#' Summary statistics by group
#'
#' Summary and statistical comparison of group characteristics
#' @importFrom car leveneTest
#' @param dat dataframe with desired summary statistics to be calculated
#' @param samplelabels the name of the column in which has the group parameter you are interested in
#' @param ignore columns to ignore or not use
#' @keywords microarray
#' @export
#' @examples
#' s.s.test()
s.s.test <- function(dat, samplelabels, ignore) {
  # remove desired columns not needed in stat summary
  if (!missing(ignore)) {
    dat <- dat[,-ignore]
  }
  # making sure no character columns
  for (i in 1:dim(dat)[2]) {
    if (is.character(dat[,i])) {
      dat[,i] <- as.factor(dat[,i])
    }
  }
  # making sure that if data was subsetted, then factor columns are not hiding any levels
  for (i in 1:dim(dat)[2]) {
    if (is.factor(dat[,i])) {
      dat[,i] <- as.factor(as.character(dat[,i]))
    }
  }
  # remove columns with only 1 unique value
  dat <- dat[,sapply(sapply(dat, unique), length) > 1]
  # creating output table
  if (sum(sapply(dat, is.factor)) == 0) {
    maxlvl <- 2
  } else {
    maxlvl <- max(sapply(sapply(dat[,sapply(dat, is.factor)], levels), length))
  }
  ind <- match(samplelabels, names(dat)) #find which column is samplelabels
  for (i in 1:dim(dat)[2]) { #repeat for each column
    output.tmp <- data.frame(factor = NA,
                             grouping = NA,
                             total = NA,
                             pval = NA,
                             test = NA) #generate a temp table to store summary
    output.tmp[1:length(levels(dat[,samplelabels])), "factor"] <- rep(as.character(names(dat)[i]), length(levels(dat[,samplelabels])))
    if (names(dat)[i] != samplelabels) { #you don't want to do this for the sample group column as it would be useles
      if (is.factor(dat[,i])) { #factors are summarised differently than numerics/ints
        tab.tmp <- table(dat[,c(ind, i)]) #summarise the counts for each level of this factor
        output.tmp[1:length(levels(dat[,samplelabels])), "grouping"] <- as.character(rownames(tab.tmp)) #grabbing the samplegroup names and storing them in the temp
        for (j in 1:dim(tab.tmp)[2]) {
          output.tmp[,paste0("stat", j)] <- tab.tmp[,j]
        } #paste columns into the temp table
        if (j+1 <= maxlvl) {
          for (l in (j+1):maxlvl) {
            output.tmp[,paste0("stat", l)] <- NA #generate NA columns to match the max so that cbind will work later
          }
        }
        output.tmp[,"total"] <- rowSums(output.tmp[,c(6:dim(output.tmp)[2])], na.rm = T) #summarize total for each sample group
        f.tmp <- fisher.test(tab.tmp) #fisher test
        output.tmp[,"pval"] <- f.tmp$p.value
        output.tmp[,"test"] <- "fishertest"
      } else {
        dat.subset <- dat[!is.na(dat[,i]),] #filter out NAs
        dat.mean <- aggregate(dat.subset[,i], by = list(dat.subset[,samplelabels]), FUN = mean)
        dat.sd <- aggregate(dat.subset[,i], by = list(dat.subset[,samplelabels]), FUN = sd)
        output.tmp[1:length(levels(dat[,samplelabels])), "grouping"] <- as.character(dat.mean[,1])
        output.tmp[,"stat1"] <- dat.mean[,2]
        output.tmp[,"stat2"] <- dat.sd[,2]
        for (l in (1+2):maxlvl) {
          output.tmp[,paste0("stat", l)] <- NA
        } #generate NA columns to match the max so that cbind will work later
        tab.tmp <- table(dat.subset[,samplelabels])
        output.tmp[,"total"] <- tab.tmp
        n.test <- shapiro.test(dat.subset[,i])
        l.test <- leveneTest(dat.subset[,i] ~ dat.subset[,samplelabels])
        if ((n.test$p.value > 0.05) & (l.test[,"Pr(>F)"][1]) > 0.05) {
          f <- formula(paste0(names(dat.subset)[i], " ~ ", samplelabels))
          output.tmp[,"pval"] <- summary(aov(f, data = dat.subset))[[1]]["Pr(>F)"][1,]
          output.tmp[,"test"] <- "anovatest"
        } else {
          output.tmp[,"pval"] <- kruskal.test(dat.subset[,i], dat.subset[,samplelabels])$p.value
          output.tmp[,"test"] <- "kruskaltest"
        }
      }
    } else {
      tab.tmp <- table(dat[,samplelabels])
      output.tmp[,"grouping"] <- names(tab.tmp)
      for (l in 1:maxlvl) {
        output.tmp[,paste0("stat", l)] <- NA
      } #generate NA columns to match the max so that cbind will work later
    }
    if (!"output" %in% ls()) {
      output <- output.tmp
    } else {
      output <- rbind(output, output.tmp)
    }
  }
  return(output)
}