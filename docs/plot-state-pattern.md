# Reusable state-only plotting for jamovi modules

## Lifecycle contract

A jamovi image can be rendered in a fresh analysis instance during export,
without `.run()` executing. A renderer must therefore not depend on estimator
objects, private caches, current analysis data, or options prepared by `.run()`.
It must not modify other results during redraw or export.

`R/PlotState.R` provides two module-independent helpers:

- `new_plot_state(data, options = list())` creates a versioned snapshot using
  only plain drawing data and named drawing options. It rejects S4 objects,
  environments, functions, and such objects nested in lists or attributes.
- `render_plot_state(state, draw)` passes the saved data as the first argument
  of a module-owned drawing function and saved options as named arguments.
  It returns `TRUE` after successful drawing, or `FALSE` for missing/unsupported
  state, a drawing error, or an explicit `FALSE` from the drawing function.

The helpers do not store drawing functions, capture an analysis environment,
open graphics devices during preparation, or modify results during rendering.
They work with base graphics and drawing-library adapters; they do not require
semPlot or lavaan. The drawing function must use only its arguments and code
shipped with the module.

## Using the pattern in another module

Copy the two helpers into the target module, or obtain them from a shared
package once available. Build a small adapter for each drawing library.

During normal analysis execution, explicitly extract the quantities to draw:

```r
image$setState(new_plot_state(
    data = list(x = summary$x, y = summary$y),
    options = list(col = "blue", type = "l")
))
```

Define a stateless adapter in the module:

```r
draw_summary <- function(data, ...) {
    graphics::plot(data$x, data$y, ...)
}
```

The image callback then delegates without consulting runtime objects:

```r
.showSummary = function(image, ggtheme, theme, ...) {
    render_plot_state(image$state, draw_summary)
}
```

Do not pass fitted models, result objects, raw observation data, or closures as
state. The validator prevents unsafe object types, not excessive data volume:
the caller remains responsible for saving only the minimal drawing payload.
If a library needs a rich object, reconstruct that object inside its adapter.
If a plot needs reproducible randomized coordinates, prepare and save those
coordinates rather than relying on the renderer's random state.

Validate settings and populate notes before rendering. Return `FALSE` for
unanticipated drawing errors and let jamovi handle the image failure. Keep
renderers free of result-table writes, warning-dispatch writes, or retries that
mutate analysis options.

## jAMM adapters

- `R/PathDiagram.R` uses lavaan's public parameter-estimate, parameter-table,
  and variable-name APIs to extract path, node, and threshold tables. Constraint
  and defined-parameter rows are not drawing primitives and are excluded.
  The fitted lavaan object is never modified or saved in image state.
- `draw_path_diagram()` reconstructs a `semPlotModel` with an empty `Original`
  slot, then calls `semPaths()` using the saved layout, labels, rotation,
  residual setting, node dimensions, shape, and label settings.
- The conceptual model uses the same state envelope and its existing
  `plot_model_diagram()` adapter. The renderer also accepts the previous
  conceptual diagram state format for compatibility.
- Previous statistical-diagram states containing only `semModel` do not
  contain the missing drawing options. They return `FALSE`; rerun the saved
  analysis once to generate a complete state rather than silently drawing with
  incorrect defaults.

The envelope currently uses `version = 1L`. If its interpretation changes,
introduce a new version and an explicit migration where sufficient information
exists. Do not guess unavailable settings in old state.

## Verification for consumers

1. Serialize and restore the state, then draw it in an analysis that has never
   initialized its workhorse objects or fitted a model.
2. Repeat rendering and check that notes and other results do not change.
3. Check empty/corrupt state and drawing errors return `FALSE`.
4. Inspect serialized payload size and verify no fitted objects or raw data
   were included.
5. Check plot settings and drawing primitives against the original backend.
6. Verify actual jamovi export and save/reopen behavior before release.

The automated tests are in `tests/testthat/test-plot-state.R`.
