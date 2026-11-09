# The bundled RData file is named coopmedmod but contains an object named data.
# Loading through data() also works when tests run against an installed package.
coopmedmod_data <- function(variables) {
    dataset <- new.env(parent = emptyenv())
    utils::data("coopmedmod", package = "jamm", envir = dataset)
    stats::na.omit(dataset$data[, variables, drop = FALSE])
}
