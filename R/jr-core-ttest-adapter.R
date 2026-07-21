# Adapter for the core jmv Independent Samples T-Test add-on. This file only
# extracts and normalises statistics; report prose belongs in the renderer.

.jr_core_ttest_get <- function(object, name) {
    if (is.null(object))
        return(NULL)
    if (is.list(object) && !is.null(object[[name]]))
        return(object[[name]])
    value <- tryCatch(object[[name]], error = function(e) NULL)
    if (!is.null(value))
        return(value)
    tryCatch(object$get(name), error = function(e) NULL)
}

.jr_core_ttest_option <- function(options, name, default = NULL) {
    value <- .jr_core_ttest_get(options, name)
    if (is.null(value) || !length(value)) default else value
}

.jr_core_ttest_df <- function(item) {
    if (is.null(item))
        return(NULL)
    if (is.data.frame(item))
        return(item)
    value <- tryCatch(item$asDF, error = function(e) NULL)
    if (is.function(value))
        value <- tryCatch(value(), error = function(e) NULL)
    if (is.data.frame(value)) value else NULL
}

.jr_core_ttest_number <- function(row, name) {
    if (is.null(row) || !name %in% names(row))
        return(NA_real_)
    value <- suppressWarnings(as.numeric(row[[name]][1]))
    if (length(value) && is.finite(value)) value else NA_real_
}

.jr_core_ttest_text <- function(row, name, default = "") {
    if (is.null(row) || !name %in% names(row))
        return(default)
    value <- as.character(row[[name]][1])
    if (length(value) && !is.na(value) && nzchar(trimws(value))) value else default
}

.jr_core_ttest_row <- function(table, outcome, variable_columns) {
    if (is.null(table) || !nrow(table))
        return(NULL)
    for (name in variable_columns) {
        if (!name %in% names(table))
            next
        index <- which(as.character(table[[name]]) == outcome)
        if (length(index))
            return(table[index[1], , drop = FALSE])
    }
    NULL
}

.jr_core_ttest_complete_data <- function(data, outcomes, group, missing_mode) {
    if (!is.data.frame(data) || !length(group) || !group %in% names(data))
        return(data)
    if (!identical(missing_mode, "listwise"))
        return(data)
    columns <- unique(c(outcomes, group))
    columns <- columns[columns %in% names(data)]
    data[stats::complete.cases(data[, columns, drop = FALSE]), , drop = FALSE]
}

.jr_core_ttest_supplement <- function(data, outcome, group, test_type,
                                      confidence = .95, alternative = "two.sided") {
    empty <- list(ok = FALSE, warnings = character())
    if (!is.data.frame(data) || !outcome %in% names(data) ||
            !length(group) || !group %in% names(data)) {
        empty$warnings <- "Selected host data were unavailable for optional-field fallback."
        return(empty)
    }
    frame <- data[, c(outcome, group), drop = FALSE]
    frame <- frame[stats::complete.cases(frame), , drop = FALSE]
    frame[[group]] <- droplevels(factor(frame[[group]]))
    if (!is.numeric(frame[[outcome]]) || nlevels(frame[[group]]) != 2L) {
        empty$warnings <- "Optional-field fallback requires a numeric outcome and exactly two usable groups."
        return(empty)
    }
    counts <- table(frame[[group]])
    if (any(counts < 2L) || !all(is.finite(frame[[outcome]]))) {
        empty$warnings <- "Optional-field fallback requires at least two finite observations in each group."
        return(empty)
    }
    test <- tryCatch(stats::t.test(
        stats::reformulate(group, response = outcome), data = frame,
        var.equal = identical(test_type, "student"), conf.level = confidence,
        alternative = alternative
    ), error = function(e) NULL)
    if (is.null(test)) {
        empty$warnings <- "Optional-field fallback could not calculate this comparison."
        return(empty)
    }
    if (any(!is.finite(c(test$statistic, test$parameter, test$p.value)))) {
        empty$warnings <- "Optional-field fallback produced undefined test statistics; check for a constant outcome or insufficient variation."
        return(empty)
    }
    split_values <- split(frame[[outcome]], frame[[group]])
    descriptives <- data.frame(
        group = names(split_values),
        n = vapply(split_values, length, integer(1)),
        mean = vapply(split_values, mean, numeric(1)),
        sd = vapply(split_values, stats::sd, numeric(1)),
        stringsAsFactors = FALSE
    )
    effect <- tryCatch(suppressMessages(effectsize::cohens_d(
        frame[[outcome]], frame[[group]],
        pooled_sd = identical(test_type, "student"), ci = confidence
    )), error = function(e) NULL)
    effect_value <- if (!is.null(effect)) suppressWarnings(as.numeric(effect$Cohens_d[1])) else NA_real_
    effect_low <- if (!is.null(effect)) suppressWarnings(as.numeric(effect$CI_low[1])) else NA_real_
    effect_high <- if (!is.null(effect)) suppressWarnings(as.numeric(effect$CI_high[1])) else NA_real_
    difference <- unname(test$estimate[1] - test$estimate[2])
    if (is.finite(difference) && is.finite(effect_value) && difference != 0 &&
            sign(effect_value) != sign(difference)) {
        effect_value <- -effect_value
        old_low <- effect_low
        effect_low <- -effect_high
        effect_high <- -old_low
    }
    list(
        ok = TRUE,
        statistics = list(
            t = unname(test$statistic), df = unname(test$parameter),
            p = test$p.value, meanDifference = difference
        ),
        confidenceInterval = list(
            level = confidence, lower = unname(test$conf.int[1]),
            upper = unname(test$conf.int[2])
        ),
        effectSize = list(
            type = "Cohen's d", estimate = effect_value,
            lower = effect_low, upper = effect_high, level = confidence
        ),
        descriptives = descriptives,
        warnings = "Optional fields were repeated from host-selected data because the core output did not expose them."
    )
}

.jr_core_ttest_alternative <- function(hypothesis) {
    switch(
        as.character(hypothesis %||% "different")[1],
        oneGreater = "greater",
        oneLess = "less",
        greater = "greater",
        less = "less",
        "two.sided"
    )
}

.jr_core_ttest_adapter <- function(host_options, host_results, data = NULL,
                                   allow_fallback = TRUE) {
    test_table <- .jr_core_ttest_df(.jr_core_ttest_get(host_results, "ttest"))
    desc_table <- .jr_core_ttest_df(.jr_core_ttest_get(host_results, "desc"))
    assum_group <- .jr_core_ttest_get(host_results, "assum")
    levene_table <- .jr_core_ttest_df(.jr_core_ttest_get(assum_group, "eqv"))
    normality_table <- .jr_core_ttest_df(.jr_core_ttest_get(assum_group, "norm"))
    warnings <- character()
    if (is.null(test_table))
        warnings <- c(warnings, "The core t-test result table was not found or had an unsupported structure.")

    outcomes <- as.character(.jr_core_ttest_option(host_options, "vars", character()))
    if (!length(outcomes) && !is.null(test_table)) {
        candidates <- intersect(c("var[stud]", "var[welc]"), names(test_table))
        outcomes <- unique(unlist(test_table[candidates], use.names = FALSE))
    }
    outcomes <- outcomes[!is.na(outcomes) & nzchar(outcomes)]
    group <- as.character(.jr_core_ttest_option(host_options, "group", character()))
    group <- if (length(group)) group[1] else ""
    confidence <- suppressWarnings(as.numeric(
        .jr_core_ttest_option(host_options, "ciWidth", 95)
    )[1] / 100)
    if (!is.finite(confidence) || confidence <= 0 || confidence >= 1)
        confidence <- .95
    missing_mode <- as.character(.jr_core_ttest_option(host_options, "miss", "perAnalysis"))[1]
    data <- .jr_core_ttest_complete_data(data, outcomes, group, missing_mode)
    selected <- c(
        if (isTRUE(.jr_core_ttest_option(host_options, "students", TRUE))) "student",
        if (isTRUE(.jr_core_ttest_option(host_options, "welchs", FALSE))) "welch"
    )
    if (!length(selected))
        warnings <- c(warnings, "Neither Student's nor Welch's t-test is selected in the host analysis.")

    analyses <- list()
    for (outcome in outcomes) {
        desc_row <- .jr_core_ttest_row(desc_table, outcome, "dep")
        descriptives <- if (!is.null(desc_row)) data.frame(
            group = c(
                .jr_core_ttest_text(desc_row, "group[1]"),
                .jr_core_ttest_text(desc_row, "group[2]")
            ),
            n = c(
                .jr_core_ttest_number(desc_row, "num[1]"),
                .jr_core_ttest_number(desc_row, "num[2]")
            ),
            mean = c(
                .jr_core_ttest_number(desc_row, "mean[1]"),
                .jr_core_ttest_number(desc_row, "mean[2]")
            ),
            sd = c(
                .jr_core_ttest_number(desc_row, "sd[1]"),
                .jr_core_ttest_number(desc_row, "sd[2]")
            ),
            stringsAsFactors = FALSE
        ) else data.frame()
        descriptives_available <- nrow(descriptives) == 2L &&
            all(nzchar(descriptives$group)) &&
            all(is.finite(descriptives$n)) &&
            all(is.finite(descriptives$mean)) &&
            all(is.finite(descriptives$sd))
        levene_row <- .jr_core_ttest_row(levene_table, outcome, "name")
        normality_row <- .jr_core_ttest_row(normality_table, outcome, "name")
        assumptions <- list(
            levene = list(
                available = !is.null(levene_row),
                statistic = .jr_core_ttest_number(levene_row, "f"),
                df1 = .jr_core_ttest_number(levene_row, "df"),
                df2 = .jr_core_ttest_number(levene_row, "df2"),
                p = .jr_core_ttest_number(levene_row, "p")
            ),
            normality = list(
                available = !is.null(normality_row),
                statistic = .jr_core_ttest_number(normality_row, "w"),
                p = .jr_core_ttest_number(normality_row, "p")
            )
        )
        for (test_type in selected) {
            suffix <- if (identical(test_type, "student")) "stud" else "welc"
            row <- .jr_core_ttest_row(test_table, outcome, paste0("var[", suffix, "]"))
            if (is.null(row)) {
                warnings <- c(warnings, sprintf(
                    "Core %s result was unavailable for %s.", test_type, outcome
                ))
            }
            statistics <- list(
                t = .jr_core_ttest_number(row, paste0("stat[", suffix, "]")),
                df = .jr_core_ttest_number(row, paste0("df[", suffix, "]")),
                p = .jr_core_ttest_number(row, paste0("p[", suffix, "]")),
                meanDifference = .jr_core_ttest_number(row, paste0("md[", suffix, "]"))
            )
            interval <- list(
                level = confidence,
                lower = .jr_core_ttest_number(row, paste0("cil[", suffix, "]")),
                upper = .jr_core_ttest_number(row, paste0("ciu[", suffix, "]"))
            )
            effect <- list(
                type = .jr_core_ttest_text(row, paste0("esType[", suffix, "]"), "Cohen's d"),
                estimate = .jr_core_ttest_number(row, paste0("es[", suffix, "]")),
                lower = .jr_core_ttest_number(row, paste0("ciles[", suffix, "]")),
                upper = .jr_core_ttest_number(row, paste0("ciues[", suffix, "]")),
                level = suppressWarnings(as.numeric(
                    .jr_core_ttest_option(host_options, "ciWidthES", 95)
                )[1] / 100)
            )
            needs_fallback <- any(!is.finite(c(
                statistics$t, statistics$df, statistics$p,
                statistics$meanDifference, interval$lower, interval$upper,
                effect$estimate, effect$lower, effect$upper
            ))) || !descriptives_available
            fallback <- if (isTRUE(allow_fallback) && needs_fallback) {
                .jr_core_ttest_supplement(
                    data, outcome, group, test_type, confidence,
                    .jr_core_ttest_alternative(
                        .jr_core_ttest_option(host_options, "hypothesis", "different")
                    )
                )
            } else list(ok = FALSE, warnings = character())
            if (isTRUE(fallback$ok)) {
                for (name in names(statistics))
                    if (!is.finite(statistics[[name]])) statistics[[name]] <- fallback$statistics[[name]]
                for (name in c("lower", "upper"))
                    if (!is.finite(interval[[name]])) interval[[name]] <- fallback$confidenceInterval[[name]]
                for (name in c("estimate", "lower", "upper"))
                    if (!is.finite(effect[[name]])) effect[[name]] <- fallback$effectSize[[name]]
                if (!descriptives_available)
                    descriptives <- fallback$descriptives
            }
            analysis_warnings <- unique(c(
                if (is.null(row)) "Primary host row was unavailable." else character(),
                fallback$warnings %||% character()
            ))
            analyses[[length(analyses) + 1L]] <- list(
                analysis = "independentSamplesTTest",
                outcome = outcome,
                group = group,
                testType = test_type,
                statistics = statistics,
                descriptives = descriptives,
                confidenceInterval = interval,
                effectSize = effect,
                assumptions = assumptions,
                warnings = analysis_warnings,
                references = c("RCore", "jReport", "jmvcore", "effectsize", "Cohen1988", "Cumming2014"),
                source = list(
                    primary = if (is.null(row)) "unavailable" else "host-results",
                    repeatedCalculation = isTRUE(fallback$ok)
                )
            )
        }
    }
    structure(list(
        analysis = "independentSamplesTTest",
        analyses = analyses,
        warnings = unique(warnings),
        hostSchema = "jmv::ttestIS"
    ), class = c("jr_core_ttest_adapter", "list"))
}
