test_that("continuous interactions multiply cleaned columns without parsing names", {
    variables <- c("outcome", "predictor + first", "moderator `second`", "mediator third")
    dataset <- stats::setNames(data.frame(
        outcome = seq_len(6), first = c(1, 3, NA, 7, 2, 5),
        second = c(-2, 0, 4, 1, 3, -1), third = c(2, 5, 1, 4, 3, 8)
    ), variables)
    encoded <- tob64(variables[-1])
    twoWay <- paste(encoded[1:2], collapse = INTERACTION_SYMBOL)
    threeWay <- paste(encoded, collapse = INTERACTION_SYMBOL)
    interactions <- c(twoWay, threeWay)

    for (scaling in c("none", "centered", "standardized")) {
        options <- jammGLMOptions$new(
            dep = variables[1], mediators = variables[4], covs = variables[2:3],
            scaling = lapply(variables[-1], function(variable) list(var = variable, type = scaling))
        )
        analysis <- list(options = options, results = jammGLMResults$new(options), data = dataset)
        datamatic <- Datamatic$new(analysis)
        baseline <- datamatic$cleandata(dataset)
        cleaned <- datamatic$cleandata(dataset, interactions)
        expect_equal(cleaned[[twoWay]], baseline[[encoded[1]]] * baseline[[encoded[2]]])
        expect_equal(cleaned[[threeWay]],
                     baseline[[encoded[1]]] * baseline[[encoded[2]]] * baseline[[encoded[3]]])
        expect_equal(cleaned[names(baseline)], baseline[names(baseline)])
        expect_identical(attr(cleaned, "na.action"), attr(baseline, "na.action"))
        expect_equal(nrow(cleaned), 5L)
    }
})

test_that("factor interactions multiply contrast-coded columns", {
    dataset <- data.frame(
        outcome = seq_len(6), mediator = c(2, 5, 1, 4, 3, 8),
        predictor = c(-2, 0, 4, 1, 3, -1), group = factor(rep(c("a", "b", "c"), 2))
    )
    options <- jammGLMOptions$new(
        dep = "outcome", mediators = "mediator", covs = "predictor", factors = "group",
        contrasts = list(list(var = "group", type = "simple")),
        scaling = list(list(var = "mediator", type = "none"), list(var = "predictor", type = "none"))
    )
    analysis <- list(options = options, results = jammGLMResults$new(options), data = dataset)
    datamatic <- Datamatic$new(analysis)
    dummyColumns <- paste0(tob64("group"), FACTOR_SYMBOL, 1:2)
    twoWay <- paste(dummyColumns[1], tob64("predictor"), sep = INTERACTION_SYMBOL)
    threeWay <- paste(c(dummyColumns[2], tob64("predictor"), tob64("mediator")),
                      collapse = INTERACTION_SYMBOL)
    cleaned <- datamatic$cleandata(dataset, c(twoWay, threeWay))
    coding <- datamatic$contrasts_values[["group"]][as.integer(dataset$group), ]
    expect_equal(cleaned[[twoWay]], coding[, 1] * dataset$predictor)
    expect_equal(cleaned[[threeWay]], coding[, 2] * dataset$predictor * dataset$mediator)
})
