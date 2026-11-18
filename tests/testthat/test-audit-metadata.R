test_that("every result citation has a reference entry", {
    referencePath <- test_path("..", "..", "jamovi", "00refs.yaml")
    resultPath <- test_path("..", "..", "jamovi", "jammglm.r.yaml")
    skip_if_not(file.exists(referencePath) && file.exists(resultPath))
    references <- readLines(referencePath)
    keys <- trimws(references[grepl("^    [[:alnum:]_]+:$", references)])
    keys <- sub(":$", "", keys)
    results <- readLines(resultPath)
    citations <- trimws(sub("^[[:space:]]*refs:", "",
                            results[grepl("^[[:space:]]+refs:", results)]))
    expect_length(setdiff(citations, keys), 0)
    expect_true("semplot" %in% keys)
})

test_that("analysis menu metadata survives manifest regeneration", {
    optionPath <- test_path("..", "..", "jamovi", "jammglm.a.yaml")
    manifestPath <- test_path("..", "..", "jamovi", "0000.yaml")
    skip_if_not(file.exists(optionPath) && file.exists(manifestPath))
    options <- readLines(optionPath)
    manifest <- trimws(readLines(manifestPath))
    for (field in c("menuGroup", "menuSubgroup", "menuTitle")) {
        definition <- options[startsWith(options, paste0(field, ":"))]
        expect_length(definition, 1)
        expect_true(definition %in% manifest)
    }
})
