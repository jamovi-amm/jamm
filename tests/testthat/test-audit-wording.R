test_that("panel labels use full words and hide the inline offset label", {
    uiPath <- test_path("..", "..", "jamovi", "jammglm.u.yaml")
    optionPath <- test_path("..", "..", "jamovi", "jammglm.a.yaml")
    skip_if_not(file.exists(uiPath) && file.exists(optionPath))
    ui <- readLines(uiPath)
    options <- readLines(optionPath)
    titles <- trimws(sub("^[[:space:]]*title:[[:space:]]*", "",
                         options[grepl("^[[:space:]]*title:", options)]))
    expect_true(grepl("name: cvalue\n[[:blank:]]+label: null", paste(ui, collapse = "\n")))
    for (heading in c("Mediator Models", "Factor Coding", "Covariate Scaling",
                      "Covariate Conditioning", "Covariate Labeling", "Mediation Options",
                      "Display in Tables"))
        expect_true(paste0("label: ", heading) %in% trimws(ui), info = heading)
    for (title in c("Bootstrap samples", "Indirect effect components", "Path labels",
                    "Node size", "Exogenous top", "Exogenous bottom", "No abbreviation"))
        expect_true(title %in% titles, info = title)
})

test_that("regression results use consistent titles", {
    dataset <- coopmedmod_data(c("cprime", "EXP", "BEH"))
    result <- jammGLM(
        data = dataset, dep = "BEH", mediators = "EXP", covs = "cprime",
        mediatorsTerms = list(list("cprime")), modelTerms = list("EXP", "cprime"),
        tableOptions = c("beta", "component", "regression")
    )
    expect_identical(result$regressions$title, "Regression Results")
    expect_identical(result$regressions$overall$regression$title, "Total effects predicting: BEH")
    expect_identical(result$regressions$full$regression$title, "Full model predicting: BEH")
    expect_identical(result$models$r2$getColumn("lhs")$title, "Variable")
})

test_that("model diagram notes use grammatical moderator wording", {
    options <- jammGLMOptions$new(
        dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
        mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
        modelTerms = list("EXP", "cprime", "SVO"), mod_terms = list(list("SVO"))
    )
    diagram <- model_diagram_data(options)
    expect_true("Moderators' main effects are not shown." %in% diagram$notes)
    expect_match(DATA_WARNS[["fac_to_cont"]], "Warning: a continuous variable is defined as a factor.",
                 fixed = TRUE)
})
