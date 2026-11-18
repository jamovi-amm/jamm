test_that("R-squared intervals have an approximation footnote only when shown", {
    for (ciType in c("standard", "perc", "none")) {
        set.seed(1)
        results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                           mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                           ciType = ciType, bootN = 50)
        notes <- vapply(results$models$r2$notes, function(note) note$note, character(1))
        expect_equal(sum(notes == "R-squared confidence intervals are approximate (Fisher z)."),
                     if (ciType == "none") 0L else 1L, info = ciType)
    }
})
