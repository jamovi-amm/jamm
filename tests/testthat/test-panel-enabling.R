test_that("conditioning and bootstrap controls have matching enable rules", {
    skip_if_not_installed("yaml")
    uiPath <- test_path("..", "..", "jamovi", "jammglm.u.yaml")
    skip_if_not(file.exists(uiPath))
    controls <- list()
    walk <- function(element) {
        if (!is.null(element$name))
            controls[[element$name]] <<- element
        for (child in element$children)
            walk(child)
    }
    walk(yaml::read_yaml(uiPath))
    expect_identical(controls$cvalue$enable, "(simpleScale_mean_sd)")
    expect_identical(controls$percvalue$enable, "(simpleScale_percent)")
    expect_identical(controls$ciWidth$enable, "(!ciType:none)")
    expect_identical(controls$bootN$enable, "(ciType:bca.simple || ciType:perc || ciType:norm)")
})

test_that("unused information widgets and their helpers are absent", {
    options <- jammGLMOptions$new()
    results <- jammGLMResults$new(options)
    resultNames <- vapply(results$items, function(item) item$name, character(1))
    expect_false(any(c("info", "extrainfo") %in% resultNames))
    expect_true(all(c("issues", "infotab") %in% resultNames))
    namespace <- asNamespace("jamm")
    for (name in c("SmartInfo", "sourcifyList", "NOTES", "PLOT_WARNS", "SUB"))
        expect_false(exists(name, envir = namespace, inherits = FALSE), info = name)
    expect_identical(names(DATA_WARNS), "fac_to_cont")
    expect_identical(names(ERRS), "noluck")
})
