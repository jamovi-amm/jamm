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

test_that("showRealNames shows the names column of the total-effects regression", {
    shown <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                     mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                     tableOptions = list("regression"), showRealNames = TRUE)
    hidden <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                      mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                      tableOptions = list("regression"), showRealNames = FALSE)
    expect_true(shown$regressions$overall$regression$getColumn("source")$visible)
    expect_false(hidden$regressions$overall$regression$getColumn("source")$visible)
})

# ---- factor coding ------------------------------------------------------------

test_that("each contrast type gives the documented codes, labels and estimates", {
    dat <- coopmedmod
    dat$group <- factor(rep(c("a", "b", "c"), length.out = nrow(dat)))
    codes <- list(
        simple = stats::contr.treatment(3) - 1 / 3,
        dummy = stats::contr.treatment(3),
        deviation = rbind(-1, diag(2)),
        difference = stats::contr.helmert(3) %*% diag(1 / (2:3)),
        helmert = matrix(c(2 / 3, -1 / 3, -1 / 3, 0, 1 / 2, -1 / 2), 3),
        repeated = matrix(c(2 / 3, -1 / 3, -1 / 3, 1 / 3, 1 / 3, -2 / 3), 3),
        polynomial = stats::contr.poly(3))
    labels <- list(simple = c("b - a", "c - a"), dummy = c("b - a", "c - a"),
                   repeated = c("a - b", "b - c"), polynomial = c("linear", "quadratic"))
    for (type in names(codes)) {
        results <- jammGLM(data = dat, dep = "BEH", mediators = "EXP", factors = "group",
                           mediatorsTerms = list(list("group")), modelTerms = list("EXP", "group"),
                           contrasts = list(list(var = "group", type = type)), showContrastCode = TRUE)
        table <- results$models$contrastCodeTables$get(key = "group")$asDF
        expect_equal(unname(t(as.matrix(table[c("Level=a", "Level=b", "Level=c")]))),
                     unname(codes[[type]]), tolerance = 1e-6, info = type)
        if (!is.null(labels[[type]]))
            expect_identical(results$models$contrastCodeTable$asDF$clab, labels[[type]], info = type)
        a <- stats::coef(stats::lm(EXP ~ group, dat, contrasts = list(group = codes[[type]])))
        expect_equal(effect_row(results, "Component", "group1 ⇒ EXP")$est, a[[2]],
                     tolerance = 1e-6, info = type)
    }
})

test_that("showContrastCode shows one coefficient table per factor", {
    dat <- coopmedmod
    dat$prime <- factor(dat$prime, levels = c(0, 1), labels = c("neutral", "coop"))
    dat$group <- factor(rep(c("a", "b", "c"), length.out = nrow(dat)))
    hidden <- jammGLM(data = dat, dep = "BEH", mediators = "EXP", factors = c("prime", "group"),
                      mediatorsTerms = list(list("prime", "group")),
                      modelTerms = list("EXP", "prime", "group"))
    shown <- jammGLM(data = dat, dep = "BEH", mediators = "EXP", factors = c("prime", "group"),
                     mediatorsTerms = list(list("prime", "group")),
                     modelTerms = list("EXP", "prime", "group"), showContrastCode = TRUE)
    expect_false(hidden$models$contrastCodeTables$visible)
    expect_true(shown$models$contrastCodeTables$visible)
    expect_identical(shown$models$contrastCodeTables$itemKeys, list("prime", "group"))
})

# ---- covariate scaling ------------------------------------------------------

test_that("scaling changes lower-order effects in a moderated model as documented", {
    # SVO is shifted so that centering it makes a difference.
    dat <- coopmedmod
    dat$SVO <- dat$SVO + 10
    fitScaled <- function(type) {
        jammGLM(data = dat, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                mod_terms = list(list("SVO")), tableOptions = list("regression"),
                scaling = list(list(var = "cprime", type = type), list(var = "SVO", type = type),
                               list(var = "EXP", type = type)))
    }
    raw <- fitScaled("none")
    centered <- fitScaled("centered")
    standardized <- fitScaled("standardized")
    # The cprime -> EXP coefficient of the fitted lavaan model, found through the
    # regression table so the test does not depend on how jamm names variables
    # internally.
    path <- function(results) {
        table <- results$regressions$mediator_regressions$items[[1]]$regression$asDF
        table$estimate[table$rhs == "cprime"]
    }
    expect_equal(path(raw), stats::coef(stats::lm(EXP ~ cprime * SVO, dat))[["cprime"]],
                 tolerance = 1e-6)
    centeredData <- dat
    centeredData$cprime <- centeredData$cprime - mean(centeredData$cprime)
    centeredData$SVO <- centeredData$SVO - mean(centeredData$SVO)
    expect_equal(path(centered), stats::coef(stats::lm(EXP ~ cprime * SVO, centeredData))[["cprime"]],
                 tolerance = 1e-6)
    standardizedData <- dat
    standardizedData$cprime <- c(scale(standardizedData$cprime))
    standardizedData$SVO <- c(scale(standardizedData$SVO))
    standardizedData$EXP <- c(scale(standardizedData$EXP))
    expect_equal(path(standardized),
                 stats::coef(stats::lm(EXP ~ cprime * SVO, standardizedData))[["cprime"]],
                 tolerance = 1e-6)
    # Conditional effects are computed at the same points of SVO, whatever the scaling.
    expect_equal(effect_row(raw, "Indirect", "cprime ⇒ EXP ⇒ BEH", "SVO = Mean")$est,
                 effect_row(centered, "Indirect", "cprime ⇒ EXP ⇒ BEH", "SVO = Mean")$est,
                 tolerance = 1e-6)
})

test_that("uncentered variables in an interaction raise a note", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                       mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                       modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                       mod_terms = list(list("SVO")),
                       scaling = list(list(var = "cprime", type = "none"), list(var = "SVO", type = "none"),
                                      list(var = "EXP", type = "none")))
    notes <- vapply(results$infotab$notes, function(note) note$note, character(1))
    expect_true(any(grepl("not centered", notes)))
})

test_that("scaling given only for the covariates leaves the mediators at the default", {
    expect_no_error(jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                            mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                            scaling = list(list(var = "SVO", type = "standardized"))))
})

test_that("the R default scaling matches jamovi's default (centered)", {
    byDefault <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                         mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                         modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                         mod_terms = list(list("SVO")))
    explicit <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                        mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                        modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                        mod_terms = list(list("SVO")),
                        scaling = list(list(var = "cprime", type = "centered"),
                                       list(var = "SVO", type = "centered"),
                                       list(var = "EXP", type = "centered")))
    expect_equal(lavaan::parameterEstimates(byDefault$model)$est,
                 lavaan::parameterEstimates(explicit$model)$est, tolerance = 1e-6)
})

# ---- conditioning of moderators --------------------------------------------------

test_that("cvalue sets the number of standard deviations and values labels show them", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                       mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                       modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                       mod_terms = list(list("SVO")), cvalue = 2, simpleScaleLabels = "values")
    s <- 2 * stats::sd(coopmedmod$SVO)
    expect_identical(unique(results$models$coefficients$asDF$condition),
                     paste("SVO =", format(round(c(-s, 0, s), 3), trim = TRUE)))
})

test_that("simpleScale 'percent' conditions on percentiles set by percvalue", {
    withValues <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                          mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                          modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                          mod_terms = list(list("SVO")), simpleScale = "percent", percvalue = 10,
                          simpleScaleLabels = "values_labels")
    centered <- coopmedmod$SVO - mean(coopmedmod$SVO)
    values <- format(round(stats::quantile(centered, c(.4, .5, .6)), 3), trim = TRUE)
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
