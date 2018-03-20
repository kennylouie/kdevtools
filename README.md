# kdevtools
R package with functions focused for signal detection of large matrices.

## biRF

Balanced iterative random forest. An embedded machine learning method to iteratively conducteds a random forest while removing column features based on the random forest gini index. This continues until the out-of-box error does not reduce anymore. Code base written off the following publication: Anaissi A, Kennedy PJ, Goyal M, Catchpoole DR. A balanced iterative random forest for gene selection from microarray data. BMC bioinformatics. 2013 Dec;14(1):261.

## limmaTable

A wrapper on top of the already widely used limma package for analysis of microarrays. This function calls on topTable, which provides a table of the significant features from a fitted limma model. On top of this function, given the original beta matrix, the beta diff, sd, and which pairwise group the comparison is significant between is provided. Takes all the arguments from the original topTable function.


## pairwise.anova

Completes an ANOVA and subsequent TukeyHSD test on a dataset.

## pairwise.fisher

Given a variable of interest and column with groupings, this function conducts a fisher.test() on all the data. Significant comparisons will be further tested to identify the pairwise comparison(s) that is/are significant. Includes the p.adjust() function to correct for multiple comparisons.

## pairwise.wilcox

This function conducts a wilcox.test() on all possible pairwise comparisons given a variable of interest and column with groupings. Includes the p.adjust() function to correct for multiple comparisons.

## pca.cov.cor

This function conducts PCA (SVD method) on a data matrix. For each PC that is produced, a correlation (spearman rank correlation)/association (kruskal-wallis) with a provided dataframe with columns as feature covariates. Includes the p.adjust() function to correct for multiple comparisons. The output is a table with significant (showall = F) PC/covariate correlations/associations.

## pca.cov

Similar to pca.cov.cor() but only conducts association test with kruskal.test() on a covariate dataframe of only factors (no numerical data).

## pcaTopCandidates

This function extracts the most weighted eigengenes of a specific principal component. This function conducts PCA (SVD method) on a data matrix. From a specified PC, the most weighted (above 3rd IQR and below 1st) features of the PC will be extracted.

## rfLoocv

This function conducts an iterative randomForest() on a dataframe of features in columns and cases on rows. Of the cases, one will be used as the testing set while the remainder will be used for the training. This will be iteratively conducted until every case has been tested once. The output is an enviromental class.

## s.s.test

This function conducts summary statistics between groups given a dataframe of variables to be summarized as columns and cases in the rows. A column must be specified with the groupings needed.

## svmLoocv

This function conducts an iterative svm() on a dataframe of features in columns and cases on rows. Of the cases, one will be used as the testing set while the remainder will be used for the training. This will be iteratively conducted until every case has been tested once. The output is an enviromental class.
