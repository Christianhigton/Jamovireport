#' Guided chi-square test of independence
#'
#' @param data A data frame.
#' @param row Name of the categorical row variable.
#' @param column Name of the categorical column variable.
#' @param counts Optional name of a numeric frequency variable.
#' @return An `edu_analysis` object.
#' @export
edu_chisq_independence <- function(data, row, column, counts = NULL) {
    variables <- c(row, column, counts)
    .jr_assert_columns(data, variables)
    d <- .jr_complete(data, variables)
    if (!is.null(counts)) {
        .jr_numeric(d[[counts]], counts)
        if (any(d[[counts]] < 0))
            .jr_stop("`counts` must not include negative frequencies.")
        observed <- stats::xtabs(d[[counts]] ~ d[[row]] + d[[column]])
    } else {
        observed <- table(d[[row]], d[[column]])
    }
    observed <- observed[rowSums(observed) > 0, colSums(observed) > 0, drop = FALSE]
    if (nrow(observed) < 2L || ncol(observed) < 2L)
        .jr_stop("A chi-square test of independence requires at least two observed row and column categories.")
    test <- suppressWarnings(stats::chisq.test(observed, correct = FALSE))
    n <- sum(observed)
    cramer_v <- sqrt(as.numeric(test$statistic) / (n * min(nrow(observed) - 1L, ncol(observed) - 1L)))
    cells <- .jr_chisq_cells(observed, test$expected, test$stdres)
    diagnostics <- .jr_chisq_expected_diagnostic(test$expected, identical(dim(observed), c(2L, 2L)))
    relation <- if (test$p.value < .05) "a statistically significant association" else "no statistically significant association"
    apa <- sprintf(
        "A chi-square test of independence found %s between %s and %s, χ²(%s, N = %s) = %s, p %s, Cramer's V = %s.",
        relation, row, column, .jr_num(test$parameter, 0L), .jr_num(n, 0L),
        .jr_num(test$statistic), .jr_p(test$p.value), .jr_num(cramer_v, 2L, TRUE)
    )
    plain <- sprintf(
        "This test compares the observed combinations of %s and %s with the counts expected if the variables were unrelated. Cramer's V describes the strength of the association without claiming that one variable causes the other.",
        row, column
    )
    caution <- if (any(diagnostics$status %in% c("Caution", "Serious")))
        paste("Caution:", .jr_diagnostic_text(diagnostics))
    else ""
    statistics <- data.frame(
        test = "Chi-square test of independence",
        statistic = as.numeric(test$statistic),
        df = as.numeric(test$parameter),
        p = test$p.value,
        effect = cramer_v,
        n = n,
        stringsAsFactors = FALSE
    )
    result <- .new_edu_analysis(
        analysis = "chisq_independence", label = "Chi-Square Test of Independence",
        question = sprintf("Are %s and %s associated?", row, column),
        requirements = "Two categorical variables recorded as independent observations, with sufficiently large expected cell counts.",
        main = statistics, descriptives = cells, effects = statistics,
        diagnostics = diagnostics, interpretation = plain, caution = caution,
        plot_data = cells,
        report_blocks = list(
            rationale = sprintf("A chi-square test of independence assesses whether %s and %s are associated.", row, column),
            descriptives = plain, apa = apa, assumptions = .jr_diagnostic_text(diagnostics), plain = plain
        ),
        statistics = statistics, call = match.call()
    )
    result$cells <- cells
    result$observed <- observed
    result
}

#' Guided chi-square goodness-of-fit test
#'
#' @param data A data frame.
#' @param variable Name of a categorical variable.
#' @param counts Optional name of a numeric frequency variable.
#' @param expected Optional expected proportions or ratios in factor-level order.
#' @return An `edu_analysis` object.
#' @export
edu_chisq_gof <- function(data, variable, counts = NULL, expected = NULL) {
    variables <- c(variable, counts)
    .jr_assert_columns(data, variables)
    d <- .jr_complete(data, variables)
    values <- droplevels(factor(d[[variable]]))
    if (!is.null(counts)) {
        .jr_numeric(d[[counts]], counts)
        if (any(d[[counts]] < 0))
            .jr_stop("`counts` must not include negative frequencies.")
        observed <- tapply(d[[counts]], values, sum)
    } else {
        observed <- table(values)
    }
    observed <- observed[observed > 0]
    if (length(observed) < 2L)
        .jr_stop("A goodness-of-fit test requires at least two observed categories.")
    if (is.null(expected))
        expected <- rep(1, length(observed))
    if (!is.numeric(expected) || length(expected) != length(observed) ||
        any(!is.finite(expected)) || any(expected < 0) || sum(expected) <= 0)
        .jr_stop("`expected` must contain non-negative ratios or proportions for each observed category.")
    proportions <- expected / sum(expected)
    test <- suppressWarnings(stats::chisq.test(as.numeric(observed), p = proportions))
    n <- sum(observed)
    cohen_w <- sqrt(sum(((as.numeric(observed) / n) - proportions)^2 / proportions))
    expected_counts <- n * proportions
    standardised <- (as.numeric(observed) - expected_counts) / sqrt(expected_counts)
    cells <- data.frame(
        category = names(observed),
        observed = as.numeric(observed),
        expected = as.numeric(expected_counts),
        standardised_residual = standardised,
        stringsAsFactors = FALSE
    )
    diagnostics <- .jr_chisq_expected_diagnostic(expected_counts, FALSE)
    distribution <- if (test$p.value < .05) "differed significantly from" else "did not differ significantly from"
    expectation_label <- if (length(unique(proportions)) == 1L) "an equal distribution" else "the specified expected distribution"
    apa <- sprintf(
        "A chi-square goodness-of-fit test indicated that frequencies for %s %s %s, χ²(%s, N = %s) = %s, p %s, Cohen's w = %s.",
        variable, distribution, expectation_label, .jr_num(test$parameter, 0L),
        .jr_num(n, 0L), .jr_num(test$statistic), .jr_p(test$p.value),
        .jr_num(cohen_w, 2L, TRUE)
    )
    plain <- sprintf(
        "This test compares the observed frequencies of %s with the frequencies expected under the stated distribution. Standardised residuals help identify categories contributing most strongly to a significant result.",
        variable
    )
    caution <- if (any(diagnostics$status %in% c("Caution", "Serious")))
        paste("Caution:", .jr_diagnostic_text(diagnostics))
    else ""
    statistics <- data.frame(
        test = "Chi-square goodness-of-fit",
        statistic = as.numeric(test$statistic),
        df = as.numeric(test$parameter),
        p = test$p.value,
        effect = cohen_w,
        n = n,
        stringsAsFactors = FALSE
    )
    result <- .new_edu_analysis(
        analysis = "chisq_gof", label = "Chi-Square Goodness-of-Fit Test",
        question = sprintf("Do observed frequencies for %s match the expected distribution?", variable),
        requirements = "One categorical variable recorded as independent observations, with justified expected proportions and adequate expected counts.",
        main = statistics, descriptives = cells, effects = statistics,
        diagnostics = diagnostics, interpretation = plain, caution = caution,
        plot_data = cells,
        report_blocks = list(
            rationale = sprintf("A chi-square goodness-of-fit test assesses whether %s follows an expected distribution.", variable),
            descriptives = plain, apa = apa, assumptions = .jr_diagnostic_text(diagnostics), plain = plain
        ),
        statistics = statistics, call = match.call()
    )
    result$cells <- cells
    result$expected_proportions <- proportions
    result
}

.jr_chisq_cells <- function(observed, expected, residuals) {
    cells <- expand.grid(
        row = rownames(observed),
        column = colnames(observed),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
    )
    cells$category <- paste(cells$row, cells$column, sep = " / ")
    cells$observed <- as.numeric(observed)
    cells$expected <- as.numeric(expected)
    cells$standardised_residual <- as.numeric(residuals)
    cells[, c("category", "observed", "expected", "standardised_residual")]
}

.jr_chisq_expected_diagnostic <- function(expected, is_two_by_two) {
    values <- as.numeric(expected)
    proportion_below_five <- mean(values < 5)
    serious <- any(values < 1) || proportion_below_five > .20
    data.frame(
        check = "Expected cell counts",
        statistic = min(values),
        p = NA_real_,
        status = if (serious) "Caution" else "Acceptable",
        interpretation = if (serious)
            sprintf("%s%% of expected counts were below 5 and the smallest expected count was %s.", .jr_num(100 * proportion_below_five, 1L), .jr_num(min(values)))
        else
            "Expected counts meet the usual chi-square guideline: none below 1 and no more than 20% below 5.",
        action = if (serious && isTRUE(is_two_by_two))
            "Consider Fisher's exact test and describe why it was preferred."
        else if (serious)
            "Consider an exact or Monte Carlo approach, or combine categories only if substantively justified."
        else
            "Pearson chi-square inference is reasonable for the observed cell-count pattern.",
        stringsAsFactors = FALSE
    )
}
