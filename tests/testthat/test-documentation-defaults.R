test_that("R defaults match jamovi options for labels and diagrams", {
    options <- jammGLMOptions$new()
    defaults <- formals(jammGLM)
    fields <- c("showRealNames", "simpleScale", "simpleScaleLabels", "model_diagram", "path_diagram",
                "diag_paths", "diag_resid", "diag_labsize", "diag_rotate", "diag_type",
                "diag_shape", "diag_abbrev")
    for (field in fields)
        expect_identical(eval(defaults[[field]]), options[[field]], info = field)
    expect_false(any(c("meddiagram", "diagram") %in% names(defaults)))
    expect_false(any(c("meddiagram", "diagram") %in% options$names))
})

test_that("the R interface prepares the conceptual model diagram by default", {
    dataset <- coopmedmod_data(c("cprime", "EXP", "BEH"))
    result <- jammGLM(
        data = dataset, dep = "BEH", mediators = "EXP", covs = "cprime",
        mediatorsTerms = list(list("cprime")), modelTerms = list("EXP", "cprime")
    )
    expect_true(result$pathgroup$pathmodel$visible)
    expect_type(result$pathgroup$pathmodel$state, "list")
    expect_gt(length(result$pathgroup$pathmodel$state$data$labels), 0)
})

test_that("R help parses and no longer describes unsupported conditioning options", {
    helpPath <- test_path("..", "..", "man", "jammGLM.Rd")
    sourcePath <- test_path("..", "..", "R", "jammglm.f.R")
    skip_if_not(file.exists(helpPath) && file.exists(sourcePath))
    expect_no_warning(tools::parse_Rd(helpPath))
    for (path in c(helpPath, sourcePath)) {
        text <- readLines(path)
        expect_false(any(grepl("custom_percent|Choose the diagram labels|centered to the mean", text)))
        expect_true(any(grepl("minimum abbreviation length", text, fixed = TRUE)))
        expect_true(any(grepl("percentile offsets", text, fixed = TRUE)))
    }
})
