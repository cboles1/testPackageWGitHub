library(usethis)
library(devtools)
use_git()
use_github(protocol = 'https')
use_readme_rmd()       #after running this command, you will need to go 'knit' the readme file,
#                       might need to edit the example

# EXAMPLE FUNCTION - Used so I can have a roxygen skeleton
# Create a function to print squares of numbers in sequence.

#' Title - print squares of numbers in sequence
#'
#' @param a Any number
#'
#' @return Squares of numbers 1 through a in sequence
#' @export
#'
#' @examples
new.function <- function(a) {
  for(i in 1:a) {
    b <- i^2
    print(b)
  }
}
