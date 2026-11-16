# jAMM: A Suite for Mediation Models

jamovi Advanced Mediation Models 
version 1.*

# Docs and help

Please visit the [jAMM docs page](https://jamovi-amm.github.io/)

# State of the art

* Path model
* Interactions in the path models
* Checks for coherence of the model
* Suggestions towards more coherent models
* Automatically guessing the mediation model more likely to be needed
* Custom models from UI
* It estimates the model coefficients
* Simple mediational effects (estimated at different levels of moderators)
* OLS Regressions for each step of model estimation

# Not implemented

* Interactions among moderators of moderators (not really high priority)
* Rigorous checks for weired and complex models 

# Table architecture

`R/jammglm.b.R` registers the `SmartTable` and `SmartArray` objects defined in
`R/SmartTabs.R`. The shared `R/SmartInit.R` helper initializes registered wrappers;
the analysis invokes their `runTable()` methods after estimation.
These wrappers dispatch to matching `init_*` providers in `R/Initier.R` and
`run_*` providers in `R/Runner.R`, using the result path to select each method.
Providers return row data, not jamovi result objects. `Runner$estimate()` delegates
model estimation to `Estimate`; regression helpers return nested ANOVA and
coefficient data, with each equation fitted only once per estimation.

The generated `R/jammglm.h.R` contains the options, results, and analysis-base R6
classes. `R/jammglm.f.R` maintains the public `jammGLM()` R interface and its
documentation. As in GAMLj, `export: false` in `jamovi/jammglm.a.yaml` prevents
the jamovi compiler from generating a duplicate interface in the header;
the function remains exported through its roxygen declaration and `NAMESPACE`.

## Conditional mediation effects

When moderators are specified, the indirect, direct, total, and optional component
effects are evaluated at each moderator setting. Continuous moderators use the
selected mean ± SD offset or percentile settings; categorical moderators use
their observed levels and the model's contrast coding. Multiple moderators use
all combinations of these settings. The Moderator levels column identifies each
condition, with labels or numeric values according to the labeling option.

Conditioning values are computed on the complete-case, transformed data used to
fit the model. Conditional effects are lavaan-defined parameters, so their standard
errors and confidence intervals use the selected delta-method or bootstrap
procedure. Standardized effects use the predictor-to-outcome SD ratio at each
condition. Models without moderators retain their existing output.

# Tests

Run the R tests from the package directory with `devtools::test()` (requires
`testthat`). Tests use the bundled `coopmedmod` dataset to compare simple and
moderated mediation results against independent regression calculations,
including conditional indirect and total effects at three moderator levels.

# Install

jAMM can be installed from jamovi library within jamovi. 

If you want to install from source, you need to install R developing tools from CRAN and jamovi developing tools.

```r

install.packages("devtools",repos='http://cran.rstudio.com/')

install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))

```

Then you download jAMM module from this repository and install it

```r
devtools::install_github("jamovi-amm/jamm")
pkg<-paste0(.libPaths()[[1]],"jamm")
jmvtools::install(pkg=pkg)

```


