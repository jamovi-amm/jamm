path_diagram_data <- function(model) {
    estimates <- lavaan::parameterEstimates(model, standardized = TRUE)
    estimates <- estimates[estimates$op %in% c("~", "~~", "=~", "~1", "~*~", "|"), , drop = FALSE]
    if (is.null(estimates$label))
        estimates$label <- rep("", nrow(estimates))
    parameters <- lavaan::parTable(model)
    grouped <- "group" %in% names(estimates)
    if (!grouped)
        estimates$group <- 1L
    key <- function(table) paste(table$lhs, table$op, table$rhs, table$group, sep = "\r")
    matched <- match(key(estimates), key(parameters))
    edges <- c("~" = "~>", "~~" = "<->", "=~" = "->", "~1" = "int", "~*~" = "<->", "|" = "|")
    reversed <- estimates$op %in% c("~", "~1")
    paths <- data.frame(
        label = estimates$label,
        lhs = ifelse(reversed, estimates$rhs, estimates$lhs),
        edge = unname(edges[estimates$op]),
        rhs = ifelse(reversed, estimates$lhs, estimates$rhs),
        est = estimates$est, std = estimates$std.all,
        group = if (grouped) estimates$group else rep("", nrow(estimates)),
        fixed = parameters$free[matched] == 0, par = parameters$free[matched],
        stringsAsFactors = FALSE)
    observed <- lavaan::lavNames(model, "ov")
    latent <- setdiff(lavaan::lavNames(model, "lv"), observed)
    variables <- c(observed, latent)
    list(
        Pars = paths[paths$edge != "|", , drop = FALSE],
        Vars = data.frame(name = variables, manifest = variables %in% observed,
                          exogenous = NA, stringsAsFactors = FALSE),
        Thresholds = paths[paths$edge == "|", !names(paths) %in% c("edge", "rhs"), drop = FALSE])
}

draw_path_diagram <- function(data, ...) {
    loadNamespace("semPlot")
    model <- methods::new("semPlotModel", Pars = data$Pars, Vars = data$Vars,
                          Thresholds = data$Thresholds, Computed = TRUE)
    semPlot::semPaths(model, ...)
}
