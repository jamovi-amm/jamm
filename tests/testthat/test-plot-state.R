test_that("plot states round-trip data and options without storing render functions", {
    state <- new_plot_state(data.frame(value = 1:3), list(color = "blue"))
    restored <- unserialize(serialize(state, NULL))
    received <- NULL
    expect_true(render_plot_state(restored, function(data, color) {
        received <<- list(data = data, color = color)
    }))
    expect_identical(received$data, state$data)
    expect_identical(received$color, "blue")
    expect_error(new_plot_state(new.env()), "drawing data")
    expect_error(new_plot_state(list(callback = identity)), "drawing data")
    expect_error(new_plot_state(1, list("unnamed")), "uniquely named")
    expect_false(render_plot_state(NULL, identity))
    expect_false(render_plot_state(list(version = 2L, options = list()), identity))
    expect_false(render_plot_state(state, function(...) stop("Drawing failed")))
    expect_false(render_plot_state(state, function(...) FALSE))
})

path_diagram_fixture <- function(...) {
    dataset <- coopmedmod_data(c("cprime", "EXP", "BEH"))
    options <- jammGLMOptions$new(
        dep = "BEH", mediators = "EXP", covs = "cprime",
        mediatorsTerms = list(list("cprime")), modelTerms = list("EXP", "cprime"),
        path_diagram = TRUE, ...)
    analysis <- jammGLMClass$new(options = options, data = dataset)
    analysis$run()
    analysis
}

test_that("path diagrams render restored state without initialization or estimation", {
    skip_if_not_installed("semPlot")
    analysis <- path_diagram_fixture(diag_abbrev = "5", diag_shape = "ellipse",
                                     diag_resid = TRUE, diag_labsize = "large")
    image <- analysis$results$pathgroup$diagrams$get(key = "0")
    state <- unserialize(serialize(image$state, NULL))
    model <- analysis$.__enclos_env__$private$.runner_machine$estimator$model
    expect_identical(names(state$data), c("Pars", "Vars", "Thresholds"))
    expect_lt(length(serialize(state, NULL)), length(serialize(model, NULL)))
    expect_identical(state$options$shapeMan, "ellipse")
    expect_true(state$options$residuals)
    expect_identical(state$options$nodeLabels,
                     abbreviate(fromb64(state$data$Vars$name), minlength = 5, strict = TRUE))
    expect_equal(nrow(state$options$layout), nrow(state$data$Vars))

    fresh <- jammGLMClass$new(options = jammGLMOptions$new(), data = data.frame())
    private <- fresh$.__enclos_env__$private
    expect_null(private$.plotter_machine)
    expect_false(fresh$options$path_diagram)
    output <- tempfile(fileext = ".pdf")
    grDevices::pdf(output)
    on.exit({ grDevices::dev.off(); unlink(output) }, add = TRUE)
    notes <- fresh$results$pathgroup$notes$asDF
    for (redraw in seq_len(2))
        expect_true(private$.showDiagram(list(state = state)))
    expect_identical(fresh$results$pathgroup$notes$asDF, notes)
    state$options$layout <- "not-a-layout"
    expect_false(private$.showDiagram(list(state = state)))
    expect_identical(fresh$results$pathgroup$notes$asDF, notes)
})

test_that("diagram preparation reports rotation notes once and does not open a device", {
    skip_if_not_installed("semPlot")
    device <- grDevices::dev.cur()
    analysis <- path_diagram_fixture(diag_type = "circle", diag_rotate = "2")
    expect_identical(grDevices::dev.cur(), device)
    notes <- analysis$results$pathgroup$notes
    expect_equal(notes$rowCount, 1)
    expect_match(notes$asDF$message, "circle layout")
    expect_true(notes$visible)
    image <- analysis$results$pathgroup$diagrams$get(key = "0")
    expect_equal(image$state$options$rotation, 1)
    analysis$.__enclos_env__$private$.plotter_machine$preparePlots()
    expect_equal(notes$rowCount, 1)
})

test_that("path drawing data matches semPlot without retaining the fitted model", {
    skip_if_not_installed("semPlot")
    dataset <- coopmedmod_data(c("cprime", "EXP", "BEH"))
    model <- lavaan::sem("EXP ~ a*cprime; BEH ~ b*EXP + cprime; indirect := a*b", dataset)
    original <- semPlot::semPlotModel(model)
    drawing <- path_diagram_data(model)
    expect_equal(drawing$Pars, original@Pars, ignore_attr = TRUE)
    expect_equal(drawing$Vars, original@Vars, ignore_attr = TRUE)
    expect_error(new_plot_state(original), "drawing data")

    constrained <- lavaan::sem("EXP ~ a*cprime; BEH ~ b*EXP + cprime; a > 0", dataset)
    before <- lavaan::parTable(constrained)
    state <- new_plot_state(path_diagram_data(constrained), list(layout = "tree"))
    expect_equal(lavaan::parTable(constrained), before)
    output <- tempfile(fileext = ".pdf")
    grDevices::pdf(output)
    on.exit({ grDevices::dev.off(); unlink(output) }, add = TRUE)
    expect_true(render_plot_state(state, draw_path_diagram))
})

test_that("conceptual diagrams use the same state-only rendering lifecycle", {
    analysis <- path_diagram_fixture()
    state <- unserialize(serialize(analysis$results$pathgroup$pathmodel$state, NULL))
    fresh <- jammGLMClass$new(options = jammGLMOptions$new(), data = data.frame())
    expect_null(fresh$.__enclos_env__$private$.plotter_machine)
    output <- tempfile(fileext = ".pdf")
    grDevices::pdf(output)
    on.exit({ grDevices::dev.off(); unlink(output) }, add = TRUE)
    expect_true(fresh$.__enclos_env__$private$.showModelDiagram(list(state = state)))
    expect_true(fresh$.__enclos_env__$private$.showModelDiagram(
        list(state = list(modelDiagram = state$data))))
})

test_that("all path layouts render from drawing primitives alone", {
    skip_if_not_installed("semPlot")
    dataset <- coopmedmod_data(c("cprime", "EXP", "BEH"))
    model <- lavaan::sem("EXP ~ cprime; BEH ~ EXP + cprime", dataset)
    drawing <- path_diagram_data(model)
    expect_true(all(drawing$Pars$label == ""))
    output <- tempfile(fileext = ".pdf")
    grDevices::pdf(output)
    on.exit({ grDevices::dev.off(); unlink(output) }, add = TRUE)
    for (layout in c("circle", "circle2", "tree", "tree2", "spring")) {
        rotations <- if (layout %in% c("circle", "circle2")) c(1, 3) else 1:4
        for (rotation in rotations) {
            state <- new_plot_state(drawing, list(layout = layout, rotation = rotation))
            expect_true(render_plot_state(state, draw_path_diagram),
                        info = paste(layout, rotation))
        }
    }
    analysis <- path_diagram_fixture(diag_type = "circle2", diag_rotate = "4")
    image <- analysis$results$pathgroup$diagrams$get(key = "0")
    expect_equal(image$state$options$rotation, 1)
    expect_equal(analysis$results$pathgroup$notes$rowCount, 1)
})

test_that("failed plot preparation clears old images and reports notes before drawing", {
    analysis <- path_diagram_fixture()
    private <- analysis$.__enclos_env__$private
    private$.runner_machine$estimator$model <- NULL
    private$.plotter_machine$preparePlots()
    expect_length(analysis$results$pathgroup$diagrams$itemKeys, 0)
    expect_equal(analysis$results$pathgroup$notes$rowCount, 1)
    expect_true(analysis$results$pathgroup$notes$visible)
})

test_that("ordered-variable thresholds survive the drawing-data adapter", {
    skip_if_not_installed("semPlot")
    dataset <- coopmedmod_data(c("cprime", "EXP", "BEH"))
    dataset$EXP <- ordered(dataset$EXP > stats::median(dataset$EXP))
    model <- lavaan::sem("EXP ~ cprime; BEH ~ EXP + cprime", dataset, ordered = "EXP")
    drawing <- path_diagram_data(model)
    original <- semPlot::semPlotModel(model)
    expect_equal(drawing$Pars, original@Pars, ignore_attr = TRUE)
    expect_equal(drawing$Thresholds, original@Thresholds, ignore_attr = TRUE)
    expect_gt(nrow(drawing$Thresholds), 0)
    output <- tempfile(fileext = ".pdf")
    grDevices::pdf(output)
    on.exit({ grDevices::dev.off(); unlink(output) }, add = TRUE)
    expect_true(render_plot_state(new_plot_state(drawing, list(layout = "tree")), draw_path_diagram))
})
