#' Prepare serializable drawing state for a jamovi image
#'
#' Package drawing data and options with a format version for later rendering
#' with \code{render_plot_state()}.
#'
#' @param data Drawing data needed to reproduce the plot, such as coordinates,
#'   summaries, or labels. May contain \code{NULL}, atomic values, lists, data
#'   frames, and matrices. Functions, environments, S4 objects, and other
#'   non-plain values are rejected, including in nested contents or attributes.
#' @param options A list of drawing arguments. A nonempty list must have unique,
#'   nonempty names. The same plain-value restrictions as for \code{data} apply.
#'
#' @details
#' Prepare state during \code{.run()} or in a plot-preparation class, then pass
#' the result to \code{image$setState()}. This function does not draw or save the
#' state itself.
#'
#' Extract only the values needed for drawing from fitted models rather than
#' storing the models themselves. Keep the payload minimal: validation checks
#' value types, not whether a list represents a fitted model, its size, or
#' whether it contains unnecessary raw observations.
#'
#' @return A list with \code{version = 1L}, \code{data}, and \code{options}.
#'   Invalid options or non-plain values cause an error.
#' @seealso \code{render_plot_state()}
#' @export
#' @examples
#' state <- new_plot_state(
#'     list(x = c(1, 2, 3), y = c(2, 4, 3)),
#'     list(col = "steelblue", pch = 19)
#' )
#' state$version
new_plot_state <- function(data, options = list()) {
    plain <- function(value) {
        if (isS4(value) || !(is.null(value) || is.atomic(value) || is.list(value)))
            return(FALSE)
        if (is.list(value) && !all(vapply(value, plain, logical(1))))
            return(FALSE)
        all(vapply(attributes(value), plain, logical(1)))
    }
    if (!is.list(options) || (length(options) > 0 &&
        (is.null(names(options)) || any(!nzchar(names(options))) || anyDuplicated(names(options)))))
        stop("Plot options must be a uniquely named list.", call. = FALSE)
    if (!plain(data) || !plain(options))
        stop("Plot state must contain only drawing data, not fitted models, functions or environments.", call. = FALSE)
    list(version = 1L, data = data, options = options)
}

#' Render a jamovi image from saved drawing state
#'
#' Invoke a drawing function with data and options prepared by
#' \code{new_plot_state()}, reporting whether rendering succeeded.
#'
#' @param state A drawing-state list returned by \code{new_plot_state()},
#'   possibly restored after serialization.
#' @param draw A drawing function defined in the module, not stored in the state.
#'   Its first argument receives \code{state$data}; named arguments from
#'   \code{state$options} follow it. An explicit \code{FALSE} return value
#'   indicates failure; other return values indicate success.
#'
#' @details
#' Call this helper from the image's \code{renderFun} callback, passing
#' \code{image$state} and the module's drawing function.
#'
#' The drawing function must use only its arguments and module code, never an
#' estimator, current analysis options, or a previously initialized Plotter.
#' jamovi can restore image state and render an export without calling
#' \code{.run()}. Test export independence by rendering
#' \code{unserialize(serialize(state, NULL))} without preparation objects.
#'
#' Use jamovi's supplied graphics device. Do not open a device or modify other
#' results, notes, or saved state while drawing. Prepare notes before rendering.
#' This helper checks the state version and options container, but does not
#' repeat the recursive plain-value validation in \code{new_plot_state()}.
#'
#' @return A logical scalar: \code{TRUE} on success; \code{FALSE} for missing or
#'   unsupported state, errors during drawing, or an explicit \code{FALSE}
#'   returned by \code{draw}.
#' @seealso \code{new_plot_state()}
#' @export
#' @examples
#' state <- new_plot_state(
#'     list(x = c(1, 2, 3), y = c(2, 4, 3)),
#'     list(col = "steelblue", pch = 19)
#' )
#' draw_scatter <- function(data, col, pch) {
#'     graphics::plot(data$x, data$y, col = col, pch = pch)
#' }
#' restored_state <- unserialize(serialize(state, NULL))
#' render_plot_state(restored_state, draw_scatter)
render_plot_state <- function(state, draw) {
    if (!is.list(state) || !identical(state$version, 1L) || !is.list(state$options))
        return(FALSE)
    tryCatch({
        !isFALSE(do.call(draw, c(list(state$data), state$options)))
    }, error = function(error) FALSE)
}
