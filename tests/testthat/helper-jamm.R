# Small helpers for reading jammGLM() results. They never call jammGLM() and
# never create or change data: every test does that explicitly, using the
# package data set `coopmedmod`.

# One row of the Indirect and Total Effects table, matched on type and effect
# label (ignoring spaces around the arrows) and, if given, on the moderator
# condition. Fails unless exactly one row matches.
effect_row <- function(results, type, effect, condition = NULL) {
    table <- results$models$coefficients$asDF
    keep <- table$type == type & gsub("\\s+", "", table$effect) == gsub("\\s+", "", effect)
    if (!is.null(condition))
        keep <- keep & table$condition == condition
    row <- table[keep, , drop = FALSE]
    if (nrow(row) != 1)
        stop("Expected one ", type, " row for '", effect, "', found ", nrow(row), call. = FALSE)
    row
}

# Render an image the way jamovi does (through its render function, using only
# the saved state) on an off-screen device. Returns whether the render function
# reported success and how many drawing operations reached the device.
render_image <- function(image) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    grDevices::dev.control("enable")
    drawn <- image$print()
    list(drawn = isTRUE(drawn), operations = length(grDevices::recordPlot()[[1]]))
}

# TRUE when a value contains only plain data (no functions, environments or S4
# objects), so it can be saved in a .omv file and restored without the analysis.
is_plain <- function(value) {
    if (isS4(value) || is.function(value) || is.environment(value))
        return(FALSE)
    if (is.list(value) && !all(vapply(value, is_plain, logical(1))))
        return(FALSE)
    all(vapply(attributes(value), is_plain, logical(1)))
}
