# What the user sees while a model is incomplete, and when it cannot be estimated.

test_that("an incomplete setup shows what is missing instead of results", {
    setup <- function(results) {
        info <- results$infotab$asDF
        info$value[info$info %in% "Setup"]
    }
    noDependent <- jammGLM(data = coopmedmod, mediators = "EXP", covs = "SVO",
                           mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    expect_identical(setup(noDependent), "A dependent variable is needed.")
    expect_null(noDependent$model)
    expect_identical(nrow(noDependent$models$coefficients$asDF), 0L)

    noMediator <- jammGLM(data = coopmedmod, dep = "BEH", covs = "SVO",
                          mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    expect_identical(setup(noMediator), "At least one mediator variable is needed.")

    noPredictor <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP",
                           mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    expect_identical(setup(noPredictor), "At least one independent variable is needed.")

    emptyMediatorModel <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                                  mediatorsTerms = list(list()), modelTerms = list("EXP", "SVO"))
    expect_identical(setup(emptyMediatorModel), "Predictors are not specified for 1 mediator variable.")

    noFullModel <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                           mediatorsTerms = list(list("SVO")))
    expect_identical(setup(noFullModel), "The full model is not specified.")

    onlyModerator <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                             mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                             mod_terms = list(list("SVO")))
    expect_identical(setup(onlyModerator),
                     "At least one independent variable must remain outside the moderators.")
})

test_that("a model that cannot be estimated stops with an explanation", {
    # A constant predictor has no variance.
    dat <- coopmedmod
    dat$SVO <- 1
    expect_error(utils::capture.output(
        jammGLM(data = dat, dep = "BEH", mediators = "EXP", covs = "SVO",
                mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))),
        "Model cannot be estimated.*no variance")
})

test_that("variables of the wrong type are rejected", {
    dat <- coopmedmod
    dat$group <- factor(rep(c("a", "b", "c"), length.out = nrow(dat)))
    expect_error(jammGLM(data = dat, dep = "BEH", mediators = "EXP", covs = "group",
                         mediatorsTerms = list(list("group")), modelTerms = list("EXP", "group")),
                 "numeric")
    expect_error(jammGLM(data = coopmedmod, dep = "notAVariable", mediators = "EXP", covs = "SVO",
                         mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO")),
                 "notAVariable")
})

test_that("a model without indirect paths says so and still estimates", {
    # EXP is not in the full model, so no indirect path reaches BEH.
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("SVO"))
    expect_match(results$get(".estWarning")$content, "Indirect effects cannot be computed")
    expect_s4_class(results$model, "lavaan")
    expect_false("Indirect" %in% results$models$coefficients$asDF$type)
})

# Models that do not converge: see test-convergence.R.

test_that("fewer than 5000 bootstrap samples add a note under the info table", {
    fit <- function(ciType, bootN) {
        jammGLM(formula = "EXP ~ SVO; BEH ~ EXP + SVO", data = coopmedmod, ciType = ciType, bootN = bootN)
    }
    notes <- function(results) vapply(results$infotab$notes, function(note) note$note, character(1))
    few <- notes(fit("perc", 100))
    expect_length(grep("based on 100 bootstrap samples", few, fixed = TRUE), 1)
    expect_true(any(grepl("at least 5000", few, fixed = TRUE)))
    # No note without bootstrap, or with enough samples.
    expect_false(any(grepl("bootstrap samples", notes(fit("standard", 100)), fixed = TRUE)))
    # The note is set when the analysis is initialised: check 5000 without estimating.
    enough <- jammGLMClass$new(options = jammGLMOptions$new(dep = "BEH", mediators = "EXP", covs = "SVO",
                                   mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                                   ciType = "perc", bootN = 5000), data = coopmedmod)
    enough$init()
    expect_false(any(grepl("bootstrap samples", notes(enough$results), fixed = TRUE)))
    fewInit <- jammGLMClass$new(options = jammGLMOptions$new(dep = "BEH", mediators = "EXP", covs = "SVO",
                                   mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                                   ciType = "perc", bootN = 4999), data = coopmedmod)
    fewInit$init()
    expect_true(any(grepl("based on 4999 bootstrap samples", notes(fewInit$results), fixed = TRUE)))
})

test_that("bootstrap intervals are footnoted under the indirect and total effects", {
    fit <- function(ciType) {
        jammGLM(formula = "EXP ~ SVO; BEH ~ EXP + SVO", data = coopmedmod, ciType = ciType, bootN = 60)
    }
    notes <- function(results) vapply(results$models$coefficients$notes, function(note) note$note, character(1))
    types <- c(bca = "bias-corrected", perc = "percentile", norm = "normal")
    for (ciType in names(types))
        expect_true(any(grepl(paste0("bootstrap method (", types[[ciType]], ", 60 bootstrap samples)"),
                              notes(fit(ciType)), fixed = TRUE)), info = ciType)
    for (ciType in c("standard", "none"))
        expect_false(any(grepl("bootstrap", notes(fit(ciType)), fixed = TRUE)), info = ciType)
})
