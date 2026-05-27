.jr_empty_posthoc_rows <- function() {
    data.frame(
        analysis = character(),
        term = character(),
        comparison = character(),
        mean_difference = numeric(),
        se = numeric(),
        df = numeric(),
        statistic = numeric(),
        p = numeric(),
        adjustment = character(),
        significant = character(),
        stringsAsFactors = FALSE
    )
}

.jr_posthoc_rows <- function(result) {
    rows <- result$posthoc_report
    if (is.null(rows) || !is.data.frame(rows))
        .jr_empty_posthoc_rows()
    else rows
}

.jr_oneway_posthoc <- function(data, outcome, group, method) {
    if (is.null(method) || identical(method, "none"))
        return(.jr_empty_posthoc_rows())
    .jr_assert_columns(data, c(outcome, group))
    d <- .jr_complete(data, c(outcome, group))
    d$y <- d[[outcome]]
    d$g <- droplevels(factor(d[[group]]))
    if (identical(method, "tukey")) {
        fit <- stats::aov(y ~ g, data = d)
        tukey <- as.data.frame(stats::TukeyHSD(fit)$g)
        return(data.frame(
            analysis = "One-Way ANOVA",
            term = group,
            comparison = rownames(tukey),
            mean_difference = tukey$diff,
            se = NA_real_,
            df = stats::df.residual(fit),
            statistic = NA_real_,
            p = tukey[["p adj"]],
            adjustment = "Tukey",
            significant = ifelse(tukey[["p adj"]] < .05, "Yes", "No"),
            stringsAsFactors = FALSE
        ))
    }
    if (!identical(method, "gamesHowell"))
        return(.jr_empty_posthoc_rows())
    split_y <- split(d$y, d$g)
    groups <- names(split_y)
    pairs <- utils::combn(seq_along(groups), 2)
    rows <- lapply(seq_len(ncol(pairs)), function(i) {
        first <- split_y[[pairs[1, i]]]
        second <- split_y[[pairs[2, i]]]
        component_first <- stats::var(first) / length(first)
        component_second <- stats::var(second) / length(second)
        se <- sqrt(component_first + component_second)
        difference <- mean(first) - mean(second)
        statistic <- difference / se
        df <- (component_first + component_second)^2 /
            ((component_first^2 / (length(first) - 1)) +
             (component_second^2 / (length(second) - 1)))
        p <- stats::ptukey(abs(statistic) * sqrt(2), nmeans = length(groups), df = df,
                           lower.tail = FALSE)
        data.frame(
            analysis = "One-Way ANOVA",
            term = group,
            comparison = paste(groups[pairs[1, i]], "-", groups[pairs[2, i]]),
            mean_difference = difference,
            se = se,
            df = df,
            statistic = statistic,
            p = p,
            adjustment = "Games-Howell",
            significant = ifelse(p < .05, "Yes", "No"),
            stringsAsFactors = FALSE
        )
    })
    do.call(rbind, rows)
}

.jr_posthoc_terms <- function(terms) {
    if (is.null(terms) || length(terms) == 0L)
        return(list())
    if (inherits(terms, "formula")) {
        labels <- attr(stats::terms(terms), "term.labels")
        return(lapply(labels, function(label) strsplit(label, ":", fixed = TRUE)[[1]]))
    }
    if (is.character(terms) && !is.list(terms))
        return(list(terms))
    lapply(terms, as.character)
}

.jr_posthoc_corrections <- function(corrections) {
    corrections <- as.character(corrections %||% "tukey")
    corrections[nzchar(corrections)]
}

.jr_emmeans_adjustment <- function(correction) {
    switch(correction,
        bonf = "bonferroni",
        correction
    )
}

.jr_emmeans_label <- function(correction) {
    switch(correction,
        none = "No correction",
        tukey = "Tukey",
        scheffe = "Scheffe",
        bonf = "Bonferroni",
        holm = "Holm",
        correction
    )
}

.jr_model_posthoc <- function(result, terms, corrections, term_map = NULL) {
    if (!requireNamespace("emmeans", quietly = TRUE))
        return(.jr_empty_posthoc_rows())
    terms <- .jr_posthoc_terms(terms)
    corrections <- .jr_posthoc_corrections(corrections)
    if (length(terms) == 0L || length(corrections) == 0L)
        return(.jr_empty_posthoc_rows())
    rows <- list()
    for (term in terms) {
        model_term <- term
        if (!is.null(term_map)) {
            model_term <- vapply(term, function(item) {
                replacement <- term_map[[item]]
                if (is.null(replacement)) item else replacement
            }, character(1))
        }
        estimated <- try(
            suppressMessages(emmeans::emmeans(result$model, specs = model_term)),
            silent = TRUE
        )
        if (inherits(estimated, "try-error"))
            next
        for (correction in corrections) {
            pairwise <- try(
                suppressMessages(emmeans::contrast(
                    estimated, method = "pairwise",
                    adjust = .jr_emmeans_adjustment(correction)
                )),
                silent = TRUE
            )
            if (inherits(pairwise, "try-error"))
                next
            summary <- as.data.frame(suppressMessages(summary(pairwise)))
            if (nrow(summary) == 0L)
                next
            p <- summary[["p.value"]]
            rows[[length(rows) + 1L]] <- data.frame(
                analysis = result$label,
                term = paste(term, collapse = " x "),
                comparison = summary$contrast,
                mean_difference = summary$estimate,
                se = summary$SE,
                df = summary$df,
                statistic = summary$t.ratio,
                p = p,
                adjustment = .jr_emmeans_label(correction),
                significant = ifelse(p < .05, "Yes", "No"),
                stringsAsFactors = FALSE
            )
        }
    }
    if (length(rows) == 0L) .jr_empty_posthoc_rows() else do.call(rbind, rows)
}

.jr_addon_posthoc_rows <- function(results) {
    results <- Filter(function(x) inherits(x, "edu_analysis"), results)
    if (length(results) == 0L)
        return(.jr_empty_posthoc_rows())
    rows <- lapply(results, .jr_posthoc_rows)
    rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
    if (length(rows) == 0L) .jr_empty_posthoc_rows() else do.call(rbind, rows)
}

.jr_addon_posthoc_text <- function(results) {
    rows <- .jr_addon_posthoc_rows(results)
    if (nrow(rows) == 0L)
        return("")
    group <- interaction(rows$analysis, rows$term, rows$adjustment, drop = TRUE)
    sentences <- lapply(split(rows, group), function(table) {
        significant <- table[is.finite(table$p) & table$p < .05, , drop = FALSE]
        header <- sprintf(
            "%s-adjusted post hoc comparisons for %s",
            table$adjustment[1], table$term[1]
        )
        if (nrow(significant) == 0L)
            return(sprintf("%s did not identify statistically significant pairwise differences.", header))
        comparisons <- vapply(seq_len(nrow(significant)), function(i) {
            sprintf(
                "%s (mean difference = %s, p %s)",
                significant$comparison[i],
                .jr_num(significant$mean_difference[i]),
                .jr_p(significant$p[i])
            )
        }, character(1))
        sprintf("%s identified significant differences for %s.", header, paste(comparisons, collapse = "; "))
    })
    paste(unlist(sentences), collapse = " ")
}
