# Variable names in the output. jamm renames every variable internally (to a
# Base64 code) so that any name is safe in lavaan syntax, and translates the
# codes back for the results. These tests check that users always see their own
# variable names, whatever those names look like, and that the estimates do not
# depend on the names.

test_that("a name that starts like another variable's name is shown correctly", {
    # The code of "SVO2" starts with the code of "SVO".
    dat <- coopmedmod
    set.seed(3)
    dat$SVO2 <- stats::rnorm(nrow(dat))
    results <- jammGLM(data = dat, dep = "BEH", mediators = "EXP", covs = c("SVO", "SVO2"),
                       mediatorsTerms = list(list("SVO", "SVO2")),
                       modelTerms = list("EXP", "SVO", "SVO2"))
    effects <- results$models$coefficients$asDF
    expect_setequal(gsub("\\s+", "", effects$effect[effects$type == "Indirect"]),
                    c("SVO⇒EXP⇒BEH", "SVO2⇒EXP⇒BEH"))
    expect_setequal(gsub("\\s+", "", effects$effect[effects$type == "Component"]),
                    c("SVO⇒EXP", "SVO2⇒EXP", "EXP⇒BEH"))
    info <- results$infotab$asDF
    expect_setequal(info$value[info$info %in% "Indirect effects"], c("SVO ⇒ EXP ⇒ BEH", "SVO2 ⇒ EXP ⇒ BEH"))
    expect_identical(info$value[info$info %in% "Model"], c("EXP ~ SVO + SVO2", "BEH ~ EXP + SVO + SVO2"))
    # The order of the variables must not matter.
    reversed <- jammGLM(data = dat, dep = "BEH", mediators = "EXP", covs = c("SVO2", "SVO"),
                        mediatorsTerms = list(list("SVO2", "SVO")),
                        modelTerms = list("EXP", "SVO2", "SVO"))
    expect_equal(effect_row(reversed, "Indirect", "SVO2 ⇒ EXP ⇒ BEH")$est,
                 effect_row(results, "Indirect", "SVO2 ⇒ EXP ⇒ BEH")$est, tolerance = 1e-6)
})

test_that("names with spaces, accents and punctuation appear unchanged in every table", {
    # The same data as the simple mediation, with unusual names.
    dat <- coopmedmod
    names(dat)[names(dat) == "SVO"] <- "orientamento sociale (SVO)"
    names(dat)[names(dat) == "EXP"] <- "aspettativa_età"
    names(dat)[names(dat) == "BEH"] <- "Größe.beh"
    results <- jammGLM(data = dat, dep = "Größe.beh", mediators = "aspettativa_età",
                       covs = "orientamento sociale (SVO)",
                       mediatorsTerms = list(list("orientamento sociale (SVO)")),
                       modelTerms = list("aspettativa_età", "orientamento sociale (SVO)"),
                       tableOptions = list("beta", "component", "regression"), path_diagram = TRUE)
    x <- "orientamento sociale (SVO)"
    m <- "aspettativa_età"
    y <- "Größe.beh"
    effects <- results$models$coefficients$asDF
    expect_identical(gsub("\\s*⇒\\s*", " ⇒ ", effects$effect),
                     c(paste(x, m, y, sep = " ⇒ "), paste(x, m, sep = " ⇒ "), paste(m, y, sep = " ⇒ "),
                       paste(x, y, sep = " ⇒ "), paste(x, y, sep = " ⇒ ")))
    expect_identical(results$models$r2$asDF$lhs, c(m, y))
    expect_identical(results$regressions$full$regression$asDF$rhs, c(m, x))
    expect_identical(results$regressions$overall$regression$asDF$label, x)
    expect_setequal(unname(results$pathgroup$diagrams$items[[1]]$state$options$nodeLabels), c(x, m, y))
    expect_setequal(results$pathgroup$pathmodel$state$data$labels, c(x, m, y))
    # The names change nothing in the estimates.
    plain <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                     mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                     tableOptions = list("beta", "component", "regression"))
    expect_equal(effects$est, plain$models$coefficients$asDF$est, tolerance = 1e-6)
})

test_that("interaction and factor labels use the real names", {
    dat <- coopmedmod
    names(dat)[names(dat) == "SVO"] <- "social value"
    dat$condizione <- factor(dat$prime, levels = c(0, 1), labels = c("neutra", "cooperativa"))
    results <- jammGLM(data = dat, dep = "BEH", mediators = "EXP", covs = c("cprime", "social value"),
                       factors = "condizione",
                       mediatorsTerms = list(list("cprime", "social value", c("cprime", "social value"))),
                       modelTerms = list("EXP", "cprime", "social value", c("EXP", "social value"),
                                         c("cprime", "social value")),
                       mod_terms = list(list("social value")))
    expect_identical(results$models$interactions$asDF$rhs,
                     c("cprime:social value", "EXP:social value", "cprime:social value"))
    expect_identical(unique(results$models$coefficients$asDF$condition),
                     c("social value = Mean - 1 SD", "social value = Mean", "social value = Mean + 1 SD"))

    withFactor <- jammGLM(data = dat, dep = "BEH", mediators = "EXP", factors = "condizione",
                          mediatorsTerms = list(list("condizione")), modelTerms = list("EXP", "condizione"))
    expect_true("condizione1 ⇒ EXP ⇒ BEH" %in% withFactor$models$coefficients$asDF$effect)
    expect_identical(withFactor$models$contrastCodeTable$asDF$rname, "condizione1")
    expect_identical(withFactor$models$contrastCodeTable$asDF$clab, "cooperativa - neutra")
})

test_that("names containing jamm's former internal symbols are not mistaken for codes", {
    # "__XX__XX__" and "._._._." were jamm's interaction and factor symbols before 2.0.1.
    dat <- coopmedmod
    names(dat)[names(dat) == "SVO"] <- "a__XX__XX__b"
    names(dat)[names(dat) == "EXP"] <- "m._._._.1"
    results <- jammGLM(data = dat, dep = "BEH", mediators = "m._._._.1", covs = "a__XX__XX__b",
                       mediatorsTerms = list(list("a__XX__XX__b")),
                       modelTerms = list("m._._._.1", "a__XX__XX__b"))
    expect_true("a__XX__XX__b ⇒ m._._._.1 ⇒ BEH" %in% results$models$coefficients$asDF$effect)
    expect_identical(results$models$r2$asDF$lhs, c("m._._._.1", "BEH"))
})

test_that("a name whose internal code contains a dot is shown correctly", {
    # The code of "score~" is "Xc2NvcmV.": the dot comes from Base64's "+".
    dat <- coopmedmod
    names(dat)[names(dat) == "SVO"] <- "score~"
    results <- jammGLM(data = dat, dep = "BEH", mediators = "EXP", covs = "score~",
                       mediatorsTerms = list(list("score~")), modelTerms = list("EXP", "score~"))
    effects <- gsub("\\s*⇒\\s*", " ⇒ ", results$models$coefficients$asDF$effect)
    expect_identical(effects, c("score~ ⇒ EXP ⇒ BEH", "score~ ⇒ EXP", "EXP ⇒ BEH",
                                "score~ ⇒ BEH", "score~ ⇒ BEH"))
    info <- results$infotab$asDF
    expect_identical(info$value[info$info %in% "Indirect effects"], "score~ ⇒ EXP ⇒ BEH")
    expect_true(all(grepl("score~", info$value[info$info %in% "Model"], fixed = TRUE)))
    plain <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                     mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    expect_equal(results$models$coefficients$asDF$est, plain$models$coefficients$asDF$est,
                 tolerance = 1e-6)
})

test_that("a name whose internal code contains two dots in a row is shown correctly", {
    # The code of "a\uff80z" (a, half-width katakana TA, z) is "XYe..gHo".
    dat <- coopmedmod
    names(dat)[names(dat) == "SVO"] <- "a\uff80z"
    results <- jammGLM(data = dat, dep = "BEH", mediators = "EXP", covs = "a\uff80z",
                       mediatorsTerms = list(list("a\uff80z")), modelTerms = list("EXP", "a\uff80z"))
    effects <- gsub("\\s*⇒\\s*", " ⇒ ", results$models$coefficients$asDF$effect)
    expect_identical(effects, c("a\uff80z ⇒ EXP ⇒ BEH", "a\uff80z ⇒ EXP", "EXP ⇒ BEH",
                                "a\uff80z ⇒ BEH", "a\uff80z ⇒ BEH"))
    expect_identical(results$models$r2$asDF$lhs, c("EXP", "BEH"))
})

test_that("names containing jamm's current internal symbols are shown unchanged", {
    # The markers and symbols jamm uses to build internal names, inside user names.
    dat <- coopmedmod
    names(dat)[names(dat) == "SVO"] <- "a__e_e_e__b"
    names(dat)[names(dat) == "EXP"] <- "m..f_f_f..1"
    names(dat)[names(dat) == "BEH"] <- "y..i_i_i..z..b_b_b.."
    results <- jammGLM(data = dat, dep = "y..i_i_i..z..b_b_b..", mediators = "m..f_f_f..1",
                       covs = "a__e_e_e__b", mediatorsTerms = list(list("a__e_e_e__b")),
                       modelTerms = list("m..f_f_f..1", "a__e_e_e__b"))
    expect_true("a__e_e_e__b ⇒ m..f_f_f..1 ⇒ y..i_i_i..z..b_b_b.." %in%
                results$models$coefficients$asDF$effect)
    expect_identical(results$models$r2$asDF$lhs, c("m..f_f_f..1", "y..i_i_i..z..b_b_b.."))
})

test_that("messages from lavaan name the variables, not their internal codes", {
    # A constant predictor makes lavaan stop with a message about it.
    dat <- coopmedmod
    dat$SVO <- 1
    message <- tryCatch(
        utils::capture.output(jammGLM(data = dat, dep = "BEH", mediators = "EXP", covs = "SVO",
                                      mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))),
        error = function(error) conditionMessage(error))
    expect_match(message, "Model cannot be estimated")
    expect_false(grepl("b_b_b", message, fixed = TRUE))
    expect_false(grepl("XU1ZP", message, fixed = TRUE))
})

test_that("jamovi's syntax works when the dependent variable and mediators have non-syntactic names", {
    data <- coopmedmod
    names(data)[names(data) == "BEH"] <- "my behaviour"
    names(data)[names(data) == "EXP"] <- "exp-1"
    names(data)[names(data) == "SVO"] <- "S V O (é)"
    options <- jammGLMOptions$new(dep = "my behaviour", mediators = "exp-1", covs = c("cprime", "S V O (é)"),
        mediatorsTerms = list(list("cprime", "S V O (é)", c("cprime", "S V O (é)"))),
        modelTerms = list("exp-1", "cprime", "S V O (é)", c("cprime", "S V O (é)")),
        mod_terms = list(list("S V O (é)")), tableOptions = list("regression"),
        scaling = lapply(c("cprime", "S V O (é)", "exp-1"), function(v) list(var = v, type = "centered")))
    analysis <- jammGLMClass$new(options = options, data = data)
    analysis$init()
    analysis$run()
    # The left-hand sides of the generated formulas must keep their backticks.
    fromSyntax <- eval(parse(text = analysis$asSource()), list(data = data))
    expect_equal(fromSyntax$models$coefficients$asDF, analysis$results$models$coefficients$asDF)
    expect_equal(fromSyntax$regressions$full$regression$asDF, analysis$results$regressions$full$regression$asDF)
})
