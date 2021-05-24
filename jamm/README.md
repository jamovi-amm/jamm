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


