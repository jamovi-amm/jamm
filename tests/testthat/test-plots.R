# The two diagrams: their saved state has the format jamovi needs to redraw and
# export them without re-running the analysis, and they actually draw.

test_that("the model diagram state is versioned, plain and small", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                       mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                       modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                       mod_terms = list(list("SVO")))
    state <- results$pathgroup$pathmodel$state
    expect_identical(names(state), c("version", "data", "options"))
    expect_identical(state$version, 1L)
    expect_true(is_plain(state))
    expect_lt(length(serialize(state, NULL)), 20000)
    expect_true(all(c("paths", "pos", "labels", "colors", "moderators", "moderated") %in% names(state$data)))
})

test_that("the model diagram shows every variable once, with its real name", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                       mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                       modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                       mod_terms = list(list("SVO")))
    state <- results$pathgroup$pathmodel$state$data
    expect_setequal(state$labels, c("cprime", "EXP", "BEH"))
    expect_identical(jmvcore::fromB64(unlist(state$moderators)), "SVO")
    # SVO moderates cprime -> EXP, EXP -> BEH and the direct path cprime -> BEH.
    expect_length(state$moderated, 3)
    expect_identical(dim(state$paths), c(3L, 3L))
    expect_identical(nrow(state$pos), 3L)
})

test_that("the model diagram explains what it does not show", {
    moderated <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                         mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                         modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                         mod_terms = list(list("SVO")))
    expect_true(moderated$pathgroup$pathnotes$visible)
    expect_true("Moderators' main effects are not shown." %in% moderated$pathgroup$pathnotes$asDF$info)

    dat <- coopmedmod
    dat$prime <- factor(dat$prime, levels = c(0, 1), labels = c("neutral", "coop"))
    withFactor <- jammGLM(data = dat, dep = "BEH", mediators = "EXP", factors = "prime",
                          mediatorsTerms = list(list("prime")), modelTerms = list("EXP", "prime"))
    expect_true(any(grepl("Categorical independent variables", withFactor$pathgroup$pathnotes$asDF$info)))
})

test_that("the path diagram state holds the fitted paths, not the lavaan model", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                       path_diagram = TRUE)
    state <- results$pathgroup$diagrams$items[[1]]$state
    expect_identical(state$version, 1L)
    expect_true(is_plain(state))
    expect_lt(length(serialize(state, NULL)), 20000)
    expect_identical(names(state$data), c("Pars", "Vars", "Thresholds"))
    regressions <- state$data$Pars[state$data$Pars$edge == "~>", ]
    expect_identical(nrow(regressions), 3L)
    fitted <- lavaan::parameterEstimates(results$model)
    expect_setequal(regressions$est, fitted$est[fitted$op == "~"])
    expect_identical(unname(state$options$nodeLabels), c("EXP", "BEH", "SVO"))
})

test_that("both diagrams draw from their saved state", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                       mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                       modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                       mod_terms = list(list("SVO")), path_diagram = TRUE)
    model <- render_image(results$pathgroup$pathmodel)
    expect_true(model$drawn)
    expect_gt(model$operations, 0)
    path <- render_image(results$pathgroup$diagrams$items[[1]])
    expect_true(path$drawn)
    expect_gt(path$operations, 0)
})

test_that("every path layout draws", {
    for (layout in c("mediation", "circle", "circle2", "tree", "tree2", "spring")) {
        results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                           mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                           path_diagram = TRUE, diag_type = layout)
        expect_true(render_image(results$pathgroup$diagrams$items[[1]])$drawn, info = layout)
    }
})

test_that("diagrams export to image files of the declared size", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                       path_diagram = TRUE)
    png <- tempfile(fileext = ".png")
    svg <- tempfile(fileext = ".svg")
    on.exit(unlink(c(png, svg)))
    results$pathgroup$pathmodel$saveAs(png)
    results$pathgroup$diagrams$items[[1]]$saveAs(svg)
    expect_identical(readBin(png, "raw", 8), as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
    header <- readBin(png, "raw", 24)
    dimension <- function(bytes) sum(as.integer(bytes) * 256^(3:0))
    # jamovi exports PNG at twice the declared size (600 x 500 for the model diagram).
    expect_identical(c(dimension(header[17:20]), dimension(header[21:24])), c(1200, 1000))
    expect_match(paste(readLines(svg, n = 3, warn = FALSE), collapse = ""), "<svg")
})
