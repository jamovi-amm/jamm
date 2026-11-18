# Loading through data() also works when tests run against an installed package.
coopmedmod_data <- function(variables) {
    dataset <- new.env(parent = emptyenv())
    utils::data("coopmedmod", package = "jamm", envir = dataset)
    stats::na.omit(dataset$coopmedmod[, variables, drop = FALSE])
}

# Path to a file of the source checkout (jamovi/, R/, man/). Under devtools::test()
# that is two levels up from tests/testthat. Under R CMD check the tests run in
# jamm.Rcheck/tests/testthat and the unpacked sources are in
# jamm.Rcheck/00_pkg_src/jamm, so look there as well. Returns "" when neither
# exists (e.g. tests run against an installed package only); callers skip then.
source_file <- function(...) {
    candidates <- c(test_path("..", "..", ...),
                    test_path("..", "..", "00_pkg_src", "jamm", ...))
    found <- candidates[file.exists(candidates)]
    if (length(found) == 0) "" else found[[1]]
}

# Errors raised in .run() are caught by jmvcore when jamovi calls run(noThrow = TRUE)
# and stored on the results root, which is what jamovi displays.
analysis_error <- function(analysis) {
    analysis$results$.__enclos_env__$private$.error
}

# Analysis warnings sent to topic "jamovi" are collected in one native Notice.
notice_content <- function(results) {
    notice <- results$get(".estWarning")
    if (is.null(notice)) "" else notice$content
}
