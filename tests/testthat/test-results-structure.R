# The shape of what jammGLM() returns: which results exist, their types,
# columns and rows, and which are shown by default.

test_that("jammGLM() returns the documented results objects", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    expect_s3_class(results, "jammGLMResults")
    expect_s4_class(results$model, "lavaan")

    expect_s3_class(results$infotab, "Table")
    expect_s3_class(results$models, "Group")
    expect_s3_class(results$models$r2, "Table")
    expect_s3_class(results$models$interactions, "Table")
    expect_s3_class(results$models$coefficients, "Table")
    expect_s3_class(results$models$contrastCodeTable, "Table")
    expect_s3_class(results$models$contrastCodeTables, "Array")
    expect_s3_class(results$regressions, "Group")
    expect_s3_class(results$regressions$overall$anova, "Table")
    expect_s3_class(results$regressions$overall$regression, "Table")
    expect_s3_class(results$regressions$mediator_regressions, "Array")
    expect_s3_class(results$regressions$full$anova, "Table")
    expect_s3_class(results$regressions$full$regression, "Table")
    expect_s3_class(results$pathgroup$pathmodel, "Image")
    expect_s3_class(results$pathgroup$pathnotes, "Table")
    expect_s3_class(results$pathgroup$diagrams, "Array")
    expect_s3_class(results$pathgroup$notes, "Table")
})

test_that("default output shows the info, R-squared, effects and model diagram only", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    expect_true(results$infotab$visible)
    expect_true(results$models$r2$visible)
    expect_true(results$models$coefficients$visible)
    expect_true(results$pathgroup$pathmodel$visible)

    expect_false(results$models$interactions$visible)
    expect_false(results$models$contrastCodeTable$visible)
    expect_false(results$models$contrastCodeTables$visible)
    expect_false(results$regressions$overall$regression$visible)
    expect_false(results$regressions$full$regression$visible)
    expect_false(results$pathgroup$diagrams$visible)
})

test_that("the info table describes estimation, the models and the indirect effects", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    info <- results$infotab$asDF
    expect_identical(names(info), c("info", "value"))
    value <- function(label) info$value[!is.na(info$info) & info$info == label]
    expect_identical(value("Estimation Method"), "ML")
    expect_identical(value("Number of observations"), "100")
    expect_identical(value("Converged"), "TRUE")
    expect_identical(value("Model"), c("EXP ~ SVO", "BEH ~ EXP + SVO"))
    expect_identical(value("Indirect effects"), "SVO ⇒ EXP ⇒ BEH")
})

test_that("the effects table has one row per effect and the documented columns", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    table <- results$models$coefficients
    values <- table$asDF
    expect_identical(names(values),
                     c("type", "effect", "est", "se", "ci.lower", "ci.upper", "std.all", "z", "pvalue"))
    expect_identical(values$type, c("Indirect", "Component", "Component", "Direct", "Total"))
    expect_false(table$getColumn("condition")$visible)
    expect_false(anyNA(values[, c("est", "se", "ci.lower", "ci.upper", "std.all", "z", "pvalue")]))
})

test_that("the R-squared table has one row per endogenous variable", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    values <- results$models$r2$asDF
    expect_identical(names(values), c("lhs", "r2", "ci.lower", "ci.upper"))
    expect_identical(values$lhs, c("EXP", "BEH"))
    expect_true(all(values$r2 >= 0 & values$r2 <= 1))
})

test_that("observations with missing values are dropped listwise", {
    dat <- coopmedmod
    dat$SVO[1:5] <- NA
    dat$BEH[6] <- NA
    results <- jammGLM(data = dat, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    info <- results$infotab$asDF
    expect_identical(info$value[info$info %in% "Number of observations"], "94")
    complete <- stats::na.omit(dat[, c("SVO", "EXP", "BEH")])
    expect_equal(results$models$r2$asDF$r2[[2]],
                 summary(stats::lm(BEH ~ EXP + SVO, complete))$r.squared, tolerance = 1e-6)
})
