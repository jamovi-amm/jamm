# Each option of jammGLM() does what its documentation says.

# ---- confidence intervals -------------------------------------------------

test_that("ciType = 'none' hides every interval column but keeps the estimates", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                       ciType = "none", tableOptions = list("beta", "component", "regression"))
    tables <- list(results$models$r2, results$models$coefficients, results$models$interactions,
                   results$regressions$overall$regression, results$regressions$full$regression,
                   results$regressions$mediator_regressions$items[[1]]$regression)
    for (table in tables) {
        expect_false(table$getColumn("ci.lower")$visible)
        expect_false(table$getColumn("ci.upper")$visible)
    }
    expect_false(anyNA(results$models$coefficients$asDF$est))
})

test_that("every bootstrap type requests the matching lavaan interval and shows it", {
    for (ciType in c("bca", "perc", "norm")) {
        set.seed(1)
        results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                           mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                           ciType = ciType, bootN = 60)
        expect_identical(lavaan::lavInspect(results$model, "options")$se, "bootstrap", info = ciType)
        expect_identical(nrow(lavaan::lavInspect(results$model, "boot")), 60L, info = ciType)
        table <- results$models$coefficients
        expect_true(table$getColumn("ci.lower")$visible, info = ciType)
        values <- table$asDF
        expect_false(anyNA(values$ci.lower), info = ciType)
        expect_true(all(values$ci.lower < values$ci.upper), info = ciType)
    }
})

test_that("bootstrap intervals are reproducible with a seed and differ from the standard ones", {
    set.seed(11)
    first <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                     mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                     ciType = "perc", bootN = 60)
    set.seed(11)
    second <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                      mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                      ciType = "perc", bootN = 60)
    standard <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                        mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    first <- first$models$coefficients$asDF
    standard <- standard$models$coefficients$asDF
    expect_identical(first, second$models$coefficients$asDF)
    expect_equal(first$est, standard$est, tolerance = 1e-6)
    expect_false(isTRUE(all.equal(first$ci.lower, standard$ci.lower)))
})

test_that("bca and percentile bootstrap intervals differ for the same resamples", {
    set.seed(5)
    bca <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                   mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                   ciType = "bca", bootN = 60)
    set.seed(5)
    percentile <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                          mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                          ciType = "perc", bootN = 60)
    expect_false(isTRUE(all.equal(bca$models$coefficients$asDF$ci.lower,
                                  percentile$models$coefficients$asDF$ci.lower)))
})

test_that("ciWidth sets the interval width and its column heading", {
    wide <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                    mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"), ciWidth = 99)
    narrow <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                      mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"), ciWidth = 80)
    width <- function(results) results$models$coefficients$asDF$ci.upper - results$models$coefficients$asDF$ci.lower
    expect_true(all(width(narrow) < width(wide)))
    expect_equal(width(narrow) / width(wide), rep(stats::qnorm(0.9) / stats::qnorm(0.995), 5),
                 tolerance = 1e-6)
    expect_match(narrow$models$coefficients$getColumn("ci.lower")$superTitle, "80%")
})

test_that("invalid option values are rejected", {
    expect_error(jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                         mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                         ciType = "bca.simple"), "ciType")
    expect_error(jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                         mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                         ciWidth = 30), "ciWidth")
    expect_error(jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                         mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                         bootN = 10), "bootN")
    expect_error(jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                         mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                         cvalue = 0), "cvalue")
    expect_error(jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                         mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                         percvalue = 60), "percvalue")
})

# ---- table content ----------------------------------------------------------

test_that("tableOptions 'component' adds the component rows", {
    without <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                       tableOptions = list("beta"))
    with <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                    mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                    tableOptions = list("beta", "component"))
    without <- without$models$coefficients$asDF
    with <- with$models$coefficients$asDF
    expect_false("Component" %in% without$type)
    expect_identical(sum(with$type == "Component"), 2L)
    expect_identical(with$est[with$type != "Component"], without$est)
})

test_that("tableOptions 'beta' shows the beta columns", {
    without <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                       tableOptions = list("component", "regression"))
    with <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                    mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                    tableOptions = list("beta", "regression"))
    expect_false(without$models$coefficients$getColumn("std.all")$visible)
    expect_false(without$regressions$full$regression$getColumn("beta")$visible)
    expect_true(with$models$coefficients$getColumn("std.all")$visible)
    expect_true(with$regressions$full$regression$getColumn("beta")$visible)
})

test_that("tableOptions 'regression' adds ANOVA and coefficient tables that match lm", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                       tableOptions = list("beta", "regression"))
    expect_true(results$regressions$full$regression$visible)

    full <- summary(stats::lm(BEH ~ EXP + SVO, coopmedmod))
    anova <- results$regressions$full$anova$asDF
    expect_equal(anova$rsquared, full$r.squared, tolerance = 1e-6)
    expect_equal(anova$f, full$fstatistic[["value"]], tolerance = 1e-6)
    expect_equal(c(anova$df1, anova$df2), unname(full$fstatistic[c("numdf", "dendf")]))
    regression <- results$regressions$full$regression$asDF
    expect_identical(regression$rhs, c("EXP", "SVO"))
    expect_equal(regression$estimate, unname(full$coefficients[-1, 1]), tolerance = 1e-6)
    expect_equal(regression$se, unname(full$coefficients[-1, 2]), tolerance = 1e-6)
    expect_equal(regression$t, unname(full$coefficients[-1, 3]), tolerance = 1e-6)
    expect_equal(regression$p, unname(full$coefficients[-1, 4]), tolerance = 1e-6)

    mediator <- results$regressions$mediator_regressions$items[[1]]
    expect_equal(mediator$anova$asDF$rsquared,
                 summary(stats::lm(EXP ~ SVO, coopmedmod))$r.squared, tolerance = 1e-6)

    totalFit <- stats::lm(BEH ~ SVO, coopmedmod)
    total <- results$regressions$overall$regression$asDF
    expect_equal(total$estimate, stats::coef(totalFit)[["SVO"]], tolerance = 1e-6)
    expect_equal(c(total$ci.lower, total$ci.upper), unname(stats::confint(totalFit)[2, ]),
                 tolerance = 1e-6)
})

test_that("regression tables show the comparison each factor contrast makes", {
    # pathjdata: groups_a has levels 1, 2, 3; with repeated coding the contrasts
    # compare 1 with 2 and 2 with 3.
    dat <- pathjdata
    dat$groups_a <- factor(dat$groups_a)
    results <- jammGLM(data = dat, dep = "y1", mediators = "y3", covs = "x1", factors = "groups_a",
                       mediatorsTerms = list(list("x1", "groups_a", c("x1", "groups_a"))),
                       modelTerms = list("y3", "x1", "groups_a"),
                       contrasts = c(groups_a = "repeated"), tableOptions = list("regression"))
    total <- results$regressions$overall$regression$asDF
    expect_identical(total$label, c("x1", "1 - 2", "2 - 3"))
    expect_identical(total$source, c("x1", "groups_a1", "groups_a2"))
    mediator <- results$regressions$mediator_regressions$items[[1]]$regression$asDF
    # In interactions each comparison is in parentheses.
    expect_identical(mediator$rhs, c("x1", "1 - 2", "2 - 3", "x1:(1 - 2)", "x1:(2 - 3)"))
    expect_identical(mediator$source, c("x1", "groups_a1", "groups_a2", "x1:groups_a1", "x1:groups_a2"))
    full <- results$regressions$full$regression$asDF
    expect_identical(full$rhs, c("y3", "x1", "1 - 2", "2 - 3"))
    expect_identical(full$source, c("y3", "x1", "groups_a1", "groups_a2"))
    # The comparisons are the ones in the Contrasts Definition table.
    expect_identical(results$models$contrastCodeTable$asDF$clab, c("1 - 2", "2 - 3"))
})

test_that("the effects table shows the comparison of each factor contrast", {
    # pathjdata: groups_a has levels 1, 2, 3; simple coding compares 2 and 3 with 1.
    dat <- pathjdata
    dat$groups_a <- factor(dat$groups_a)
    results <- jammGLM(data = dat, dep = "y1", mediators = "y3", covs = "x1", factors = "groups_a",
                       mediatorsTerms = list(list("x1", "groups_a")), modelTerms = list("y3", "x1", "groups_a"))
    table <- results$models$coefficients
    expect_true(table$getColumn("label")$visible)
    expect_identical(table$getColumn("label")$title, "Contrast")
    contrast <- function(type, effect) {
        value <- effect_row(results, type, effect)$label
        if (is.na(value)) "" else value
    }
    for (type in c("Indirect")) {
        expect_identical(contrast(type, "groups_a1 ⇒ y3 ⇒ y1"), "2 - 1")
        expect_identical(contrast(type, "groups_a2 ⇒ y3 ⇒ y1"), "3 - 1")
        expect_identical(contrast(type, "x1 ⇒ y3 ⇒ y1"), "")
    }
    for (type in c("Direct", "Total")) {
        expect_identical(contrast(type, "groups_a1 ⇒ y1"), "2 - 1")
        expect_identical(contrast(type, "groups_a2 ⇒ y1"), "3 - 1")
        expect_identical(contrast(type, "x1 ⇒ y1"), "")
    }
    # Components: only the path leaving the contrast variable is a comparison.
    expect_identical(contrast("Component", "groups_a1 ⇒ y3"), "2 - 1")
    rows <- table$asDF
    mediatorPaths <- rows$label[rows$type == "Component" & gsub("\\s+", "", rows$effect) == "y3⇒y1"]
    expect_true(all(is.na(mediatorPaths) | mediatorPaths == ""))
})

test_that("the Contrast column follows the coding, showRealNames and the presence of factors", {
    dat <- pathjdata
    dat$groups_a <- factor(dat$groups_a)
    repeated <- jammGLM(data = dat, dep = "y1", mediators = "y3", factors = "groups_a",
                        mediatorsTerms = list(list("groups_a")), modelTerms = list("y3", "groups_a"),
                        contrasts = c(groups_a = "repeated"))
    expect_identical(effect_row(repeated, "Indirect", "groups_a2 ⇒ y3 ⇒ y1")$label, "2 - 3")
    noFactors <- jammGLM(data = pathjdata, dep = "y1", mediators = "y3", covs = "x1",
                         mediatorsTerms = list(list("x1")), modelTerms = list("y3", "x1"))
    expect_false(noFactors$models$coefficients$getColumn("label")$visible)
    # With factors, the column follows showRealNames (TRUE by default).
    expect_true(repeated$models$coefficients$getColumn("label")$visible)
    namesOff <- jammGLM(data = dat, dep = "y1", mediators = "y3", factors = "groups_a",
                        mediatorsTerms = list(list("groups_a")), modelTerms = list("y3", "groups_a"),
                        showRealNames = FALSE)
    expect_false(namesOff$models$coefficients$getColumn("label")$visible)
})

test_that("fixed direct effects and moderated effects show the comparison too", {
    dat <- pathjdata
    dat$groups_a <- factor(dat$groups_a)
    # groups_a is left out of the full model: its direct effect is fixed to 0.
    fixed <- jammGLM(data = dat, dep = "y1", mediators = "y3", covs = "x1", factors = "groups_a",
                     mediatorsTerms = list(list("x1", "groups_a")), modelTerms = list("y3", "x1"))
    expect_identical(effect_row(fixed, "Direct", "groups_a1 ⇒ y1")$label, "2 - 1")
    expect_identical(effect_row(fixed, "Direct", "groups_a1 ⇒ y1")$est, 0)
    # x1 moderates the effects of groups_a.
    moderated <- jammGLM(data = dat, dep = "y1", mediators = "y3", covs = "x1", factors = "groups_a",
                         mediatorsTerms = list(list("x1", "groups_a", c("x1", "groups_a"))),
                         modelTerms = list("y3", "x1", "groups_a", c("y3", "x1")),
                         mod_terms = list(list("x1")))
    for (condition in c("x1 = Mean - 1 SD", "x1 = Mean", "x1 = Mean + 1 SD")) {
        expect_identical(effect_row(moderated, "Indirect", "groups_a2 ⇒ y3 ⇒ y1", condition)$label, "3 - 1")
        expect_identical(effect_row(moderated, "Total", "groups_a1 ⇒ y1", condition)$label, "2 - 1")
    }
})

test_that("showRealNames shows the contrast variable names in every regression table", {
    dat <- pathjdata
    dat$groups_a <- factor(dat$groups_a)
    shown <- jammGLM(data = dat, dep = "y1", mediators = "y3", covs = "x1", factors = "groups_a",
                     mediatorsTerms = list(list("x1", "groups_a")), modelTerms = list("y3", "x1", "groups_a"),
                     tableOptions = list("regression"), showRealNames = TRUE)
    hidden <- jammGLM(data = dat, dep = "y1", mediators = "y3", covs = "x1", factors = "groups_a",
                      mediatorsTerms = list(list("x1", "groups_a")), modelTerms = list("y3", "x1", "groups_a"),
                      tableOptions = list("regression"), showRealNames = FALSE)
    tables <- function(results) list(results$regressions$overall$regression,
                                     results$regressions$mediator_regressions$items[[1]]$regression,
                                     results$regressions$full$regression)
    for (table in tables(shown)) expect_true(table$getColumn("source")$visible)
    for (table in tables(hidden)) expect_false(table$getColumn("source")$visible)
    # Without factors the Names column would repeat the Effect column: it is hidden.
    noFactors <- jammGLM(data = pathjdata, dep = "y1", mediators = "y3", covs = "x1",
                         mediatorsTerms = list(list("x1")), modelTerms = list("y3", "x1"),
                         tableOptions = list("regression"), showRealNames = TRUE)
    for (table in tables(noFactors)) expect_false(table$getColumn("source")$visible)
    # With the names shown, the two columns differ for the contrasts.
    total <- shown$regressions$overall$regression$asDF
    expect_false(identical(total$source, total$label))
})

# ---- factor coding ------------------------------------------------------------
# Uses pathjdata: groups_a has 3 levels (1, 2, 3), groups_b has 2 (1, 2). Both are
# stored as integers and are turned into factors here.

test_that("each contrast type gives the documented codes, labels and estimates", {
    dat <- pathjdata
    dat$groups_a <- factor(dat$groups_a)
    codes <- list(
        simple = stats::contr.treatment(3) - 1 / 3,
        dummy = stats::contr.treatment(3),
        deviation = rbind(-1, diag(2)),
        difference = stats::contr.helmert(3) %*% diag(1 / (2:3)),
        helmert = matrix(c(2 / 3, -1 / 3, -1 / 3, 0, 1 / 2, -1 / 2), 3),
        repeated = matrix(c(2 / 3, -1 / 3, -1 / 3, 1 / 3, 1 / 3, -2 / 3), 3),
        polynomial = stats::contr.poly(3))
    labels <- list(simple = c("2 - 1", "3 - 1"), dummy = c("2 - 1", "3 - 1"),
                   repeated = c("1 - 2", "2 - 3"), polynomial = c("linear", "quadratic"))
    for (type in names(codes)) {
        results <- jammGLM(data = dat, dep = "y1", mediators = "y3", factors = "groups_a",
                           mediatorsTerms = list(list("groups_a")), modelTerms = list("y3", "groups_a"),
                           contrasts = list(list(var = "groups_a", type = type)), showContrastCode = TRUE)
        table <- results$models$contrastCodeTables$get(key = "groups_a")$asDF
        expect_equal(unname(t(as.matrix(table[c("Level=1", "Level=2", "Level=3")]))),
                     unname(codes[[type]]), tolerance = 1e-6, info = type)
        if (!is.null(labels[[type]]))
            expect_identical(results$models$contrastCodeTable$asDF$clab, labels[[type]], info = type)
        a <- stats::coef(stats::lm(y3 ~ groups_a, dat, contrasts = list(groups_a = codes[[type]])))
        expect_equal(effect_row(results, "Component", "groups_a1 ⇒ y3")$est, a[[2]],
                     tolerance = 1e-6, info = type)
    }
})

test_that("showContrastCode shows one coefficient table per factor", {
    dat <- pathjdata
    dat$groups_a <- factor(dat$groups_a)
    dat$groups_b <- factor(dat$groups_b)
    hidden <- jammGLM(data = dat, dep = "y1", mediators = "y3", factors = c("groups_a", "groups_b"),
                      mediatorsTerms = list(list("groups_a", "groups_b")),
                      modelTerms = list("y3", "groups_a", "groups_b"))
    shown <- jammGLM(data = dat, dep = "y1", mediators = "y3", factors = c("groups_a", "groups_b"),
                     mediatorsTerms = list(list("groups_a", "groups_b")),
                     modelTerms = list("y3", "groups_a", "groups_b"), showContrastCode = TRUE)
    expect_false(hidden$models$contrastCodeTables$visible)
    expect_true(shown$models$contrastCodeTables$visible)
    expect_identical(shown$models$contrastCodeTables$itemKeys, list("groups_a", "groups_b"))
})

test_that("contrasts given as a named vector equal the list form", {
    # The named vector is what jamovi's Syntax mode writes.
    dat <- pathjdata
    dat$groups_a <- factor(dat$groups_a)
    dat$groups_b <- factor(dat$groups_b)
    asList <- jammGLM(data = dat, dep = "y1", mediators = "y3", factors = c("groups_a", "groups_b"),
                      mediatorsTerms = list(list("groups_a", "groups_b")),
                      modelTerms = list("y3", "groups_a", "groups_b"),
                      contrasts = list(list(var = "groups_a", type = "dummy"),
                                       list(var = "groups_b", type = "deviation")))
    asVector <- jammGLM(data = dat, dep = "y1", mediators = "y3", factors = c("groups_a", "groups_b"),
                        mediatorsTerms = list(list("groups_a", "groups_b")),
                        modelTerms = list("y3", "groups_a", "groups_b"),
                        contrasts = c(groups_a = "dummy", groups_b = "deviation"))
    expect_equal(asVector$models$coefficients$asDF, asList$models$coefficients$asDF)
})

test_that("contrasts listed for some factors use simple coding for the others", {
    dat <- pathjdata
    dat$groups_a <- factor(dat$groups_a)
    dat$groups_b <- factor(dat$groups_b)
    partial <- jammGLM(data = dat, dep = "y1", mediators = "y3", factors = c("groups_a", "groups_b"),
                       mediatorsTerms = list(list("groups_a", "groups_b")),
                       modelTerms = list("y3", "groups_a", "groups_b"),
                       contrasts = list(list(var = "groups_a", type = "dummy")))
    complete <- jammGLM(data = dat, dep = "y1", mediators = "y3", factors = c("groups_a", "groups_b"),
                        mediatorsTerms = list(list("groups_a", "groups_b")),
                        modelTerms = list("y3", "groups_a", "groups_b"),
                        contrasts = list(list(var = "groups_a", type = "dummy"),
                                         list(var = "groups_b", type = "simple")))
    expect_equal(partial$models$coefficients$asDF, complete$models$coefficients$asDF)
})

# ---- covariate scaling ------------------------------------------------------
# Uses pathjdata, whose variables are not centered, so scaling changes the
# lower-order effects. Model: x1, x2 -> y3 -> y1, with x2 moderating every path.

test_that("scaling changes lower-order effects in a moderated model as documented", {
    fitScaled <- function(type) {
        jammGLM(data = pathjdata, dep = "y1", mediators = "y3", covs = c("x1", "x2"),
                mediatorsTerms = list(list("x1", "x2", c("x1", "x2"))),
                modelTerms = list("y3", "x1", "x2", c("y3", "x2"), c("x1", "x2")),
                mod_terms = list(list("x2")), tableOptions = list("regression"),
                scaling = list(list(var = "x1", type = type), list(var = "x2", type = type),
                               list(var = "y3", type = type)))
    }
    raw <- fitScaled("none")
    centered <- fitScaled("centered")
    standardized <- fitScaled("standardized")
    # The x1 -> y3 coefficient, from the mediator's regression table.
    path <- function(results) {
        table <- results$regressions$mediator_regressions$items[[1]]$regression$asDF
        table$estimate[table$rhs == "x1"]
    }
    expect_equal(path(raw), stats::coef(stats::lm(y3 ~ x1 * x2, pathjdata))[["x1"]], tolerance = 1e-6)
    centeredData <- pathjdata
    centeredData$x1 <- centeredData$x1 - mean(centeredData$x1)
    centeredData$x2 <- centeredData$x2 - mean(centeredData$x2)
    expect_equal(path(centered), stats::coef(stats::lm(y3 ~ x1 * x2, centeredData))[["x1"]],
                 tolerance = 1e-6)
    standardizedData <- pathjdata
    standardizedData$x1 <- c(scale(standardizedData$x1))
    standardizedData$x2 <- c(scale(standardizedData$x2))
    standardizedData$y3 <- c(scale(standardizedData$y3))
    expect_equal(path(standardized),
                 stats::coef(stats::lm(y3 ~ x1 * x2, standardizedData))[["x1"]], tolerance = 1e-6)
    expect_false(isTRUE(all.equal(path(raw), path(centered))))
    # Conditional effects are computed at the same points of x2, whatever the scaling.
    expect_equal(effect_row(raw, "Indirect", "x1 ⇒ y3 ⇒ y1", "x2 = Mean")$est,
                 effect_row(centered, "Indirect", "x1 ⇒ y3 ⇒ y1", "x2 = Mean")$est,
                 tolerance = 1e-6)
})

test_that("uncentered variables in an interaction raise a note", {
    results <- jammGLM(data = pathjdata, dep = "y1", mediators = "y3", covs = c("x1", "x2"),
                       mediatorsTerms = list(list("x1", "x2", c("x1", "x2"))),
                       modelTerms = list("y3", "x1", "x2", c("y3", "x2"), c("x1", "x2")),
                       mod_terms = list(list("x2")),
                       scaling = list(list(var = "x1", type = "none"), list(var = "x2", type = "none"),
                                      list(var = "y3", type = "none")))
    notes <- vapply(results$infotab$notes, function(note) note$note, character(1))
    expect_true(any(grepl("not centered", notes)))
})

test_that("the R default scaling matches jamovi's default (centered)", {
    byDefault <- jammGLM(data = pathjdata, dep = "y1", mediators = "y3", covs = c("x1", "x2"),
                         mediatorsTerms = list(list("x1", "x2", c("x1", "x2"))),
                         modelTerms = list("y3", "x1", "x2", c("y3", "x2"), c("x1", "x2")),
                         mod_terms = list(list("x2")), tableOptions = list("regression"))
    centered <- jammGLM(data = pathjdata, dep = "y1", mediators = "y3", covs = c("x1", "x2"),
                        mediatorsTerms = list(list("x1", "x2", c("x1", "x2"))),
                        modelTerms = list("y3", "x1", "x2", c("y3", "x2"), c("x1", "x2")),
                        mod_terms = list(list("x2")), tableOptions = list("regression"),
                        scaling = list(list(var = "x1", type = "centered"), list(var = "x2", type = "centered"),
                                       list(var = "y3", type = "centered")))
    raw <- jammGLM(data = pathjdata, dep = "y1", mediators = "y3", covs = c("x1", "x2"),
                   mediatorsTerms = list(list("x1", "x2", c("x1", "x2"))),
                   modelTerms = list("y3", "x1", "x2", c("y3", "x2"), c("x1", "x2")),
                   mod_terms = list(list("x2")), tableOptions = list("regression"),
                   scaling = list(list(var = "x1", type = "none"), list(var = "x2", type = "none"),
                                  list(var = "y3", type = "none")))
    estimates <- function(results) results$regressions$full$regression$asDF$estimate
    expect_equal(estimates(byDefault), estimates(centered), tolerance = 1e-6)
    # With this data, scaling does change the estimates.
    expect_false(isTRUE(all.equal(estimates(centered), estimates(raw))))
})

test_that("scaling listed for some variables centers the others", {
    partial <- jammGLM(data = pathjdata, dep = "y1", mediators = "y3", covs = c("x1", "x2"),
                       mediatorsTerms = list(list("x1", "x2")),
                       modelTerms = list("y3", "x1", "x2", c("y3", "x2")),
                       tableOptions = list("regression"),
                       scaling = list(list(var = "x1", type = "standardized")))
    complete <- jammGLM(data = pathjdata, dep = "y1", mediators = "y3", covs = c("x1", "x2"),
                        mediatorsTerms = list(list("x1", "x2")),
                        modelTerms = list("y3", "x1", "x2", c("y3", "x2")),
                        tableOptions = list("regression"),
                        scaling = list(list(var = "x1", type = "standardized"),
                                       list(var = "x2", type = "centered"),
                                       list(var = "y3", type = "centered")))
    expect_equal(partial$regressions$full$regression$asDF$estimate,
                 complete$regressions$full$regression$asDF$estimate, tolerance = 1e-6)
})

test_that("scaling given as a named vector equals the list form", {
    # The named vector is what jamovi's Syntax mode writes.
    asList <- jammGLM(data = pathjdata, dep = "y1", mediators = "y3", covs = c("x1", "x2"),
                      mediatorsTerms = list(list("x1", "x2")), modelTerms = list("y3", "x1", "x2", c("y3", "x2")),
                      tableOptions = list("regression"),
                      scaling = list(list(var = "x1", type = "standardized"), list(var = "x2", type = "none"),
                                     list(var = "y3", type = "centered")))
    asVector <- jammGLM(data = pathjdata, dep = "y1", mediators = "y3", covs = c("x1", "x2"),
                        mediatorsTerms = list(list("x1", "x2")), modelTerms = list("y3", "x1", "x2", c("y3", "x2")),
                        tableOptions = list("regression"),
                        scaling = c(x1 = "standardized", x2 = "none", y3 = "centered"))
    expect_equal(asVector$regressions$full$regression$asDF, asList$regressions$full$regression$asDF)
})

# ---- conditioning of moderators --------------------------------------------------

test_that("cvalue sets the number of standard deviations and values labels show them", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                       mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                       modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                       mod_terms = list(list("SVO")), cvalue = 2, simpleScaleLabels = "values")
    s <- 2 * stats::sd(coopmedmod$SVO)
    expect_identical(unique(results$models$coefficients$asDF$condition),
                     paste("SVO =", format(round(mean(coopmedmod$SVO) + c(-s, 0, s), 3), trim = TRUE)))
})

test_that("values labels show the moderator's own values, not the centered ones", {
    data <- coopmedmod
    data$W <- data$SVO + 10
    formula <- "EXP ~ cprime*W; BEH ~ EXP + cprime*W"
    s <- stats::sd(data$W)
    for (scaling in c("centered", "standardized", "none")) {
        meanSD <- jammGLM(formula = formula, data = data, mod_terms = list(list("W")),
                          scaling = c(W = scaling), simpleScaleLabels = "values")
        expect_identical(unique(meanSD$models$coefficients$asDF$condition),
                         paste("W =", format(round(mean(data$W) + c(-s, 0, s), 3), trim = TRUE)),
                         info = scaling)
        percent <- jammGLM(formula = formula, data = data, mod_terms = list(list("W")),
                           scaling = c(W = scaling), simpleScale = "percent",
                           simpleScaleLabels = "values_labels")
        values <- format(round(stats::quantile(data$W, c(.25, .5, .75)), 3), trim = TRUE)
        expect_identical(unique(percent$models$coefficients$asDF$condition),
                         paste0("W = ", c("25%", "50%", "75%"), " = ", values), info = scaling)
    }
    # Only the labels change: the effects are still computed on the model's scale.
    shifted <- jammGLM(formula = formula, data = data, mod_terms = list(list("W")))
    original <- jammGLM(formula = "EXP ~ cprime*SVO; BEH ~ EXP + cprime*SVO", data = coopmedmod,
                        mod_terms = list(list("SVO")))
    expect_equal(shifted$models$coefficients$asDF$est, original$models$coefficients$asDF$est)
})

test_that("simpleScale 'percent' conditions on percentiles set by percvalue", {
    withValues <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                          mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                          modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                          mod_terms = list(list("SVO")), simpleScale = "percent", percvalue = 10,
                          simpleScaleLabels = "values_labels")
    values <- format(round(stats::quantile(coopmedmod$SVO, c(.4, .5, .6)), 3), trim = TRUE)
    expect_identical(unique(withValues$models$coefficients$asDF$condition),
                     paste0("SVO = ", c("40%", "50%", "60%"), " = ", values))
    labelsOnly <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                          mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                          modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                          mod_terms = list(list("SVO")), simpleScale = "percent", percvalue = 25)
    expect_identical(unique(labelsOnly$models$coefficients$asDF$condition),
                     c("SVO = 25%", "SVO = 50%", "SVO = 75%"))
})

# ---- diagrams ---------------------------------------------------------------

test_that("model_diagram and path_diagram switch the two diagrams on and off", {
    neither <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                       model_diagram = FALSE)
    both <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                    mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                    path_diagram = TRUE)
    expect_false(neither$pathgroup$visible)
    expect_true(both$pathgroup$pathmodel$visible)
    expect_true(both$pathgroup$diagrams$visible)
    expect_length(both$pathgroup$diagrams$items, 1)
})

test_that("pathOptions 'suggested' marks suggested paths in the model diagram", {
    # SVO is in the full model but not in the mediator model: SVO -> EXP is a suggested path.
    suggested <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                         mediatorsTerms = list(list("cprime")), modelTerms = list("EXP", "cprime", "SVO"))
    plain <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                     mediatorsTerms = list(list("cprime")), modelTerms = list("EXP", "cprime", "SVO"),
                     pathOptions = list())
    note <- "Green paths indicate suggested coefficients"
    expect_true("green" %in% suggested$pathgroup$pathmodel$state$data$colors)
    expect_false("green" %in% plain$pathgroup$pathmodel$state$data$colors)
    expect_true(note %in% suggested$pathgroup$pathnotes$asDF$info)
    expect_false(note %in% plain$pathgroup$pathnotes$asDF$info)
})

test_that("diagram options reach the path diagram", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                       mediatorsTerms = list(list("cprime", "SVO")), modelTerms = list("EXP", "cprime", "SVO"),
                       path_diagram = TRUE, diag_paths = "stand", diag_resid = TRUE,
                       diag_shape = "ellipse", diag_type = "tree", diag_rotate = "2", diag_abbrev = "5")
    options <- results$pathgroup$diagrams$items[[1]]$state$options
    expect_identical(options$whatLabels, "stand")
    expect_true(options$residuals)
    expect_identical(options$shapeMan, "ellipse")
    expect_identical(options$layout, "tree")
    expect_identical(options$rotation, 2)
    expect_identical(unname(options$nodeLabels), c("EXP", "BEH", "cprim", "SVO"))
    small <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                     mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                     path_diagram = TRUE, diag_labsize = "small")
    large <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                     mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                     path_diagram = TRUE, diag_labsize = "vlarge")
    expect_lt(small$pathgroup$diagrams$items[[1]]$state$options$sizeMan,
              large$pathgroup$diagrams$items[[1]]$state$options$sizeMan)
})

test_that("a circle layout with a sideways rotation is corrected with a note", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                       path_diagram = TRUE, diag_type = "circle", diag_rotate = "2")
    expect_identical(results$pathgroup$diagrams$items[[1]]$state$options$rotation, 1)
    expect_true(results$pathgroup$notes$visible)
    expect_match(results$pathgroup$notes$asDF$message, "circle layout requires")
})

# ---- deprecated arguments -------------------------------------------------------

test_that("the deprecated moderatorsTerms argument still works, with a warning", {
    current <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                       mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                       modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                       mod_terms = list(list("SVO")))
    expect_warning(
        legacy <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                          mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                          modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                          moderatorsTerms = list(list("SVO"))),
        "'moderatorsTerms' is deprecated")
    expect_equal(legacy$models$coefficients$asDF, current$models$coefficients$asDF)
})
