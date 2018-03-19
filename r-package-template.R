#template for making packages
library("devtools")
library(roxygen2)

setwd("C:/Users/kdev/Desktop/github/repos")

# create a new package
create("kdevtools")

#add .R functions into the R directory
#example cats.R

# to add documentation, add the following lines prior to the function code

#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

cat_function <- function(love=TRUE){
  if(love==TRUE){
    print("I love cats!")
  }
  else {
    print("I am not a cool person.")
  }
}

# creating the documentation
setwd("./kdevtools")
document()

# to install
setwd("..")
install("kdevtools")
