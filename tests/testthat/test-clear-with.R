clear_with_schema <- function() {
    skip_if_not_installed("yaml")
    resultPath <- test_path("..", "..", "jamovi", "jammglm.r.yaml")
    optionPath <- test_path("..", "..", "jamovi", "jammglm.a.yaml")
    skip_if_not(file.exists(resultPath) && file.exists(optionPath))
    elements <- list()
    walk <- function(element, path) {
        elements[[path]] <<- element
        if (is.list(element$items)) {
            for (child in element$items)
                walk(child, paste(path, child$name, sep = "/"))
        }
        if (is.list(element$template))
            walk(element$template, paste(path, "template", sep = "/"))
    }
    results <- yaml::read_yaml(resultPath)
    for (element in results$items)
        walk(element, element$name)
    options <- yaml::read_yaml(optionPath)
    list(elements = elements, options = vapply(options$options, function(option) option$name, character(1)))
}

test_that("every table and template has explicit valid clearing dependencies", {
    schema <- clear_with_schema()
    tables <- Filter(function(element) identical(element$type, "Table"), schema$elements)
    expect_length(tables, 14)
    for (path in names(tables)) {
        dependencies <- unlist(tables[[path]]$clearWith)
        expect_true(length(dependencies) > 0, info = path)
        expect_true("data" %in% dependencies, info = path)
    }
    for (path in names(schema$elements)) {
        dependencies <- unlist(schema$elements[[path]]$clearWith)
        expect_true(length(setdiff(dependencies, c(schema$options, "data"))) == 0, info = path)
        expect_false(anyDuplicated(dependencies) > 0, info = path)
    }
})

test_that("all fitted-model tables invalidate on estimation and display changes", {
    schema <- clear_with_schema()
    required <- c("data", "dep", "mediators", "factors", "covs", "modelTerms", "mediatorsTerms",
                  "moderatorsTerms", "contrasts", "scaling", "ciType", "ciWidth", "bootN",
                  "simpleScale", "cvalue", "percvalue", "simpleScaleLabels", "tableOptions", "showRealNames")
    paths <- c("infotab", "models/r2", "models/interactions", "models/coefficients",
               "regressions/overall/anova", "regressions/overall/regression",
               "regressions/mediator_regressions/template/anova",
               "regressions/mediator_regressions/template/regression",
               "regressions/full/anova", "regressions/full/regression")
    for (path in paths)
        expect_true(setequal(unlist(schema$elements[[path]]$clearWith), required), info = path)
})

test_that("contrast tables and diagram notes track their underlying output", {
    elements <- clear_with_schema()$elements
    expect_setequal(unlist(elements[["models/contrastCodeTable"]]$clearWith),
                    c("data", "factors", "contrasts", "showRealNames"))
    expect_setequal(unlist(elements[["models/contrastCodeTables/template"]]$clearWith),
                    c("data", "factors", "contrasts", "showContrastCode"))
    expect_identical(elements[["pathgroup/pathnotes"]]$clearWith,
                     elements[["pathgroup/pathmodel"]]$clearWith)
    expect_identical(elements[["pathgroup/notes"]]$clearWith,
                     elements[["pathgroup/diagrams/template"]]$clearWith)
    expect_true(all(c("data", "contrasts", "scaling", "ciWidth", "bootN", "moderatorsTerms",
                      "diag_type", "diag_rotate") %in% unlist(elements[["pathgroup/notes"]]$clearWith)))
})
