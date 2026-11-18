# ============================================================================
# LEGACY OPTIONS (jAMM < 2.0)
#
# Everything in this file exists only so that analyses saved with jAMM 1.x, and
# R scripts written for it, keep working with jAMM 2.x. Every piece of legacy
# code elsewhere is tagged with the comment "LEGACY (jAMM < 2.0)":
#
#   jamovi/jammglm.a.yaml    hidden option `moderatorsTerms`
#   jamovi/js/jamm.events.js migrateLegacyModerators() and its call
#   R/jammglm.b.R            call to migrate_legacy_options() in .init(), and
#                            the skip in .sourcifyOption()
#   R/jammglm.f.R            deprecated `moderatorsTerms` argument
#   tests/testthat/test-legacy-options.R
#
# TO REMOVE when the deprecation takes effect: delete this file and the test
# file, delete every block tagged "LEGACY (jAMM < 2.0)" (grep for it), then run
# jmvtools::prepare() to regenerate R/jammglm.h.R.
# ============================================================================

# `moderatorsTerms` (jAMM 1.x) was renamed `mod_terms` in jAMM 2.0, with the
# same structure (an Array of Terms, one block per mediator). jamovi restores
# options by name, so the old name is still declared, as a hidden option, and
# its value is copied into `mod_terms` here, before any code reads the options.
# This runs in .init(), so it also covers analyses that jamovi re-runs without
# the options panel (and its JS migration) ever being opened.
# `mod_terms` wins when both are set, and the legacy value is then emptied so it
# is never used twice. Returns the options invisibly.
migrate_legacy_options <- function(options) {
    if (!is.joption(options, "moderatorsTerms"))
        return(invisible(options))
    legacy <- options$moderatorsTerms
    if (length(unlist(legacy)) == 0)
        return(invisible(options))
    if (length(unlist(options$mod_terms)) == 0) {
        current <- options$option("mod_terms")
        current$value <- legacy
    }
    deprecated <- options$option("moderatorsTerms")
    deprecated$value <- list(list())
    invisible(options)
}
