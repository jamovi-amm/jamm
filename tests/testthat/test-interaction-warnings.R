test_that("uncentered interactions are checked in every mediator and outcome equation", {
    dataset <- coopmedmod_data(c("cprime", "SVO", "EXP", "BEH"))
    dataset$second <- dataset$EXP + dataset$BEH / 10
    for (interactionEquations in list(1L, 2L, 3L, 1:3)) {
        equations <- rep(list(list("cprime", "SVO")), 3)
        equations[[3]] <- list("EXP", "second", "cprime", "SVO")
        for (equation in interactionEquations)
            equations[[equation]] <- c(equations[[equation]], list(c("cprime", "SVO")))
        options <- jammGLMOptions$new(
            dep = "BEH", mediators = c("EXP", "second"), covs = c("cprime", "SVO"),
            mediatorsTerms = equations[1:2], modelTerms = equations[[3]],
            scaling = list(list(var = "cprime", type = "none"),
                           list(var = "SVO", type = "centered"))
        )
        results <- jammGLMResults$new(options)
        analysis <- list(options = options, results = results, data = dataset)
        syntax <- Syntax$new(analysis, Datamatic$new(analysis))
        notes <- vapply(results$infotab$notes, function(note) note$note, character(1))
        expected <- gsub("{vars}", "cprime", WARNS[["nocenterd"]], fixed = TRUE)
        warning <- notes[notes == expected]
        expect_identical(unname(warning), expected)
        expect_false(grepl("{vars}", warning, fixed = TRUE))
    }
})

test_that("centered interactions and unscaled main effects do not trigger the warning", {
    dataset <- coopmedmod_data(c("cprime", "SVO", "EXP", "BEH"))
    for (scaling in c("centered", "standardized", "none")) {
        terms <- list("EXP", "cprime", "SVO")
        if (scaling != "none")
            terms <- c(terms, list(c("cprime", "SVO")))
        options <- jammGLMOptions$new(
            dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
            mediatorsTerms = list(list("cprime", "SVO")), modelTerms = terms,
            scaling = list(list(var = "cprime", type = scaling),
                           list(var = "SVO", type = scaling))
        )
        results <- jammGLMResults$new(options)
        analysis <- list(options = options, results = results, data = dataset)
        syntax <- Syntax$new(analysis, Datamatic$new(analysis))
        notes <- vapply(results$infotab$notes, function(note) note$note, character(1))
        expect_length(notes, 0)
    }
})
