test_that("formula expansion accepts strings and formula objects", {
    expected <- EXP ~ cprime + SVO + cprime:SVO
    expect_identical(deparse(expand.formula("EXP ~ cprime * SVO")), deparse(expected))
    expect_identical(deparse(expand.formula(EXP ~ cprime * SVO)), deparse(expected))
})

test_that("string model lists match formula objects with multiple mediators", {
    dataset <- coopmedmod_data(c("cprime", "SVO", "EXP", "BEH"))
    equations <- list("EXP ~ cprime", "SVO ~ cprime", "BEH ~ EXP + SVO + cprime")
    formulas <- lapply(equations, stats::as.formula)
    marshal <- jammGLMClass$private_methods$.marshalFormula
    for (field in c("dep", "mediators", "factors", "covs", "mediatorsTerms", "modelTerms")) {
        expect_identical(
            marshal(equations, dataset, field),
            marshal(formulas, dataset, field),
            info = field
        )
    }
    stringsResult <- jammGLM(data = dataset, formula = equations)
    formulasResult <- jammGLM(data = dataset, formula = formulas)
    expect_equal(stringsResult$models$coefficients$asDF, formulasResult$models$coefficients$asDF)
    expect_gt(nrow(stringsResult$models$coefficients$asDF), 0)
})

test_that("text model inputs support portable separators and empty lines", {
    dataset <- coopmedmod_data(c("cprime", "EXP", "BEH"))
    equations <- list("EXP ~ cprime", "BEH ~ EXP + cprime")
    reference <- jammGLM(data = dataset, formula = equations)$models$coefficients$asDF
    for (separator in c(";", "\n", "\r\n", "\r")) {
        text <- paste0(" ;\n ", paste(equations, collapse = separator), " ;\r\n ")
        result <- jammGLM(data = dataset, formula = text)
        expect_equal(result$models$coefficients$asDF, reference)
    }
})

test_that("invalid formula inputs report actionable errors before estimation", {
    dataset <- data.frame(outcome = 1:3, predictor = 3:1)
    expect_error(jammGLM(data = dataset, formula = list("mediator ~ predictor; outcome ~ mediator")),
                 "Invalid formula entry 1: Each list element must contain exactly one equation", fixed = TRUE)
    expect_error(jammGLM(data = dataset, formula = list("outcome ~ predictor", "outcome ~")),
                 "Invalid formula entry 2:", fixed = TRUE)
    expect_error(jammGLM(data = dataset, formula = list(~ predictor)),
                 "dependent variable and predictors", fixed = TRUE)
    expect_error(jammGLM(data = dataset, formula = list(123)),
                 "one equation as a string or formula object", fixed = TRUE)
    expect_error(jammGLM(data = dataset, formula = list(NA_character_)),
                 "one equation as a string or formula object", fixed = TRUE)
    expect_error(jammGLM(data = dataset, formula = list()), "at least one equation", fixed = TRUE)
    expect_error(jammGLM(data = dataset, formula = " ;\r\n "), "at least one equation", fixed = TRUE)
    expect_error(jammGLM(data = dataset, formula = 123), "string of equations or a list", fixed = TRUE)
    expect_error(jammGLM(data = dataset, formula = NA_character_), "single non-missing string", fixed = TRUE)
    expect_error(jammGLM(data = dataset, formula = c("mediator ~ predictor", "outcome ~ mediator")),
                 "single non-missing string", fixed = TRUE)
})
