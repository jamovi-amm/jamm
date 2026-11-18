# Every class in jamm is built with R6::R6Class() when the package is built,
# outside any function, so R CMD check cannot see R6 being used. Importing the
# constructor records the dependency explicitly.
#' @importFrom R6 R6Class
NULL
