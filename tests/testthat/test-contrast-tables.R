test_that("contrast coefficient tables show the coding matrix for every factor", {
    dataset <- coopmedmod_data(c("prime", "SVO", "EXP", "BEH"))
    dataset$prime <- factor(dataset$prime)
    dataset$group <- factor(rep(c("A", "B", "C"), length.out = nrow(dataset)))
    makeResult <- function(showCodes, coding) {
        jammGLM(
            data = dataset, dep = "BEH", mediators = "EXP", covs = "SVO",
            factors = c("prime", "group"), showContrastCode = showCodes,
            contrasts = list(list(var = "prime", type = coding), list(var = "group", type = coding)),
            mediatorsTerms = list(list("SVO", "prime", "group")),
            modelTerms = list("EXP", "SVO", "prime", "group")
        )
    }

    for (coding in c("simple", "dummy", "deviation")) {
        result <- makeResult(TRUE, coding)
        expect_true(result$models$contrastCodeTables$visible)
        for (factor in c("prime", "group")) {
            levels <- levels(dataset[[factor]])
            expected <- stats::contr.treatment(levels)
            if (coding == "simple") expected <- expected - 1 / length(levels)
            if (coding == "deviation") expected[1, ] <- -1
            table <- result$models$contrastCodeTables$get(key = factor)
            values <- table$asDF
            expect_true(table$visible)
            expect_equal(values$cname, paste0(factor, seq_len(length(levels) - 1)))
            expect_equal(unname(as.matrix(values[paste("Level", levels, sep = "=")])),
                         unname(t(expected)))
            expect_true(all(nzchar(values$clab)))
        }
    }

    hidden <- makeResult(FALSE, "simple")
    expect_false(hidden$models$contrastCodeTables$visible)
})
