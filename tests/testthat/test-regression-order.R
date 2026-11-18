test_that("interleaved regression terms retain the correct statistics", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")),
                       modelTerms = list("EXP", c("EXP", "SVO"), "SVO"),
                       tableOptions = list("beta", "regression"))
    data <- coopmedmod
    data$EXP <- data$EXP - mean(data$EXP)
    data$SVO <- data$SVO - mean(data$SVO)
    model <- stats::lm(BEH ~ EXP * SVO, data)
    coefficients <- summary(model)$coefficients
    intervals <- stats::confint(model)
    standardized <- stats::coef(stats::lm(BEH ~ EXP * SVO,
                                         as.data.frame(scale(data[c("BEH", "EXP", "SVO")]))))
    table <- results$regressions$full$regression$asDF
    expect_identical(table$rhs, c("EXP", "EXP:SVO", "SVO"))
    expect_equal(table$estimate, unname(coefficients[table$rhs, 1]), tolerance = 1e-6)
    expect_equal(table$se, unname(coefficients[table$rhs, 2]), tolerance = 1e-6)
    expect_equal(table$t, unname(coefficients[table$rhs, 3]), tolerance = 1e-6)
    expect_equal(table$p, unname(coefficients[table$rhs, 4]), tolerance = 1e-6)
    expect_equal(table$ci.lower, unname(intervals[table$rhs, 1]), tolerance = 1e-6)
    expect_equal(table$ci.upper, unname(intervals[table$rhs, 2]), tolerance = 1e-6)
    expect_equal(table$beta, unname(standardized[table$rhs]), tolerance = 1e-6)
    expect_equal(table$df, rep(stats::df.residual(model), nrow(table)))
})

test_that("factor regression statistics follow their terms in every regression table", {
    data <- pathjdata
    data$groups_a <- factor(data$groups_a)
    fit <- function(mediatorTerms, outcomeTerms) {
        jammGLM(data = data, dep = "y1", mediators = "y3", covs = "x1", factors = "groups_a",
                 mediatorsTerms = list(mediatorTerms), modelTerms = outcomeTerms,
                 contrasts = c(groups_a = "repeated"), tableOptions = list("beta", "regression"))
    }
    reference <- fit(list("x1", "groups_a", c("x1", "groups_a")),
                     list("x1", "y3", "groups_a", c("x1", "groups_a")))
    reordered <- fit(list("x1", c("groups_a", "x1"), "groups_a"),
                     list("x1", c("groups_a", "x1"), "y3", "groups_a"))
    tables <- function(results) list(results$regressions$overall$regression$asDF,
                                     results$regressions$mediator_regressions$items[[1]]$regression$asDF,
                                     results$regressions$full$regression$asDF)
    term_keys <- function(names) {
        vapply(strsplit(names, ":", fixed = TRUE), function(term) paste(sort(term), collapse = ":"),
               character(1))
    }
    reference <- tables(reference)
    reordered <- tables(reordered)
    columns <- c("estimate", "se", "t", "p", "df", "ci.lower", "ci.upper", "beta")
    for (index in seq_along(reference)) {
        expected <- reference[[index]]
        actual <- reordered[[index]]
        matched <- match(term_keys(actual$source), term_keys(expected$source))
        expect_false(anyNA(matched))
        for (column in columns)
            expect_equal(actual[[column]], expected[[column]][matched], tolerance = 1e-6,
                         info = paste(index, column))
    }
})

test_that("interactions can appear first, between main effects or last", {
    factorData <- pathjdata
    factorData$groups_a <- factor(factorData$groups_a)
    cases <- list(
        list(data = coopmedmod, dep = "BEH", mediator = "EXP", covs = c("cprime", "SVO"),
             factors = NULL, predictors = c("cprime", "SVO")),
        list(data = factorData, dep = "y1", mediator = "y3", covs = "x1",
             factors = "groups_a", predictors = c("x1", "groups_a")))
    tables <- function(results) list(results$regressions$overall$regression$asDF,
                                     results$regressions$mediator_regressions$items[[1]]$regression$asDF,
                                     results$regressions$full$regression$asDF)
    row_names <- function(table) {
        if (!is.null(table$source)) table$source else if (!is.null(table$rhs)) table$rhs else table$label
    }
    columns <- c("estimate", "se", "t", "p", "df", "ci.lower", "ci.upper", "beta")
    for (case in cases) {
        fit <- function(position) {
            mediatorTerms <- append(as.list(case$predictors), list(case$predictors), after = position)
            outcomeTerms <- append(c(list(case$mediator), as.list(case$predictors)),
                                   list(case$predictors), after = position)
            jammGLM(data = case$data, dep = case$dep, mediators = case$mediator,
                     covs = case$covs, factors = case$factors,
                     mediatorsTerms = list(mediatorTerms), modelTerms = outcomeTerms,
                     tableOptions = list("beta", "component", "regression"))
        }
        reference <- fit(3)
        expectedTables <- tables(reference)
        expectedEffects <- reference$models$coefficients$asDF
        for (position in c(0, 1, 3)) {
            results <- fit(position)
            expect_s4_class(results$model, "lavaan")
            actualTables <- tables(results)
            if (position == 0)
                expect_identical(row_names(actualTables[[3]])[[1]],
                                 paste0(paste(case$predictors, collapse = ":"),
                                        if (length(case$factors) > 0) "1" else ""))
            for (index in seq_along(expectedTables)) {
                expected <- expectedTables[[index]]
                actual <- actualTables[[index]]
                matched <- match(row_names(actual), row_names(expected))
                expect_false(anyNA(matched))
                for (column in columns)
                    expect_equal(actual[[column]], expected[[column]][matched], tolerance = 1e-6,
                                 info = paste(case$dep, position, index, column))
            }
            actualEffects <- results$models$coefficients$asDF
            matched <- match(paste(actualEffects$type, actualEffects$effect),
                             paste(expectedEffects$type, expectedEffects$effect))
            expect_false(anyNA(matched))
            expect_equal(actualEffects$est, expectedEffects$est[matched], tolerance = 1e-6)
        }
    }
})
