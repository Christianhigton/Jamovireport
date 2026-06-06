.jr_omega_fit <- function(data, keys, correlation) {
    fit <- NULL
    invisible(utils::capture.output(
        fit <- suppressWarnings(suppressMessages(psych::omega(
            data,
            nfactors = 1,
            key = keys,
            flip = FALSE,
            poly = identical(correlation, "polychoric"),
            plot = FALSE
        )))
    ))
    fit
}

.jr_omega_bootstrap_ci <- function(data, keys, correlation, ci, iterations) {
    if (iterations < 20L)
        .jr_stop("`boot_iterations` must be at least 20 when bootstrap confidence intervals are requested.")
    estimates <- replicate(iterations, {
        sample_data <- data[sample.int(nrow(data), replace = TRUE), , drop = FALSE]
        fit <- try(.jr_omega_fit(sample_data, keys, correlation), silent = TRUE)
        if (inherits(fit, "try-error")) NA_real_ else unname(fit$omega.tot)
    })
    estimates <- estimates[is.finite(estimates)]
    if (length(estimates) < max(20L, floor(iterations * .80)))
        return(c(NA_real_, NA_real_))
    limits <- c((1 - ci) / 2, 1 - ((1 - ci) / 2))
    unname(stats::quantile(estimates, probs = limits, names = FALSE, na.rm = TRUE))
}

.jr_omega_strength <- function(omega) {
    if (!is.finite(omega))
        return("could not be estimated reliably")
    if (omega >= .80)
        return("strong internal consistency")
    if (omega >= .70)
        return("moderate internal consistency")
    "limited internal consistency"
}

.jr_cronbach_alpha <- function(scored_data) {
    scored_data <- as.data.frame(scored_data)
    item_variances <- vapply(scored_data, stats::var, numeric(1), na.rm = TRUE)
    total_variance <- stats::var(rowSums(scored_data), na.rm = TRUE)
    item_count <- ncol(scored_data)
    if (item_count < 2L || !is.finite(total_variance) || total_variance <= 0)
        return(NA_real_)
    item_count / (item_count - 1) * (1 - (sum(item_variances, na.rm = TRUE) / total_variance))
}

#' Guided reliability analysis using McDonald's omega
#'
#' Estimates internal consistency using McDonald's omega total and Cronbach's
#' alpha. Item diagnostics are supplied to discourage relying on a single
#' coefficient without inspecting item behaviour.
#'
#' @param data A data frame containing scale items.
#' @param items Character vector containing three or more numeric item names.
#' @param reverse_items Optional item names that require reversed scoring for
#'   this scale.
#' @param correlation Correlation model used for the omega estimate:
#'   `"pearson"` for numeric/approximately continuous items or `"polychoric"`
#'   for ordered categorical response items.
#' @param ci Confidence level for the optional bootstrap interval.
#' @param bootstrap Whether to compute a percentile bootstrap confidence
#'   interval for omega.
#' @param boot_iterations Number of bootstrap resamples when `bootstrap` is
#'   `TRUE`.
#' @return An `edu_analysis` object.
#' @export
edu_reliability_omega <- function(data, items, reverse_items = character(),
                                  correlation = c("pearson", "polychoric"),
                                  ci = .95, bootstrap = TRUE,
                                  boot_iterations = 200L) {
    correlation <- match.arg(correlation)
    items <- unique(items)
    reverse_items <- unique(reverse_items)
    if (length(items) < 3L)
        .jr_stop("Omega reliability analysis requires three or more scale items.")
    if (!all(reverse_items %in% items))
        .jr_stop("Every `reverse_items` variable must also be included in `items`.")
    .jr_assert_columns(data, items)
    for (item in items)
        .jr_numeric(data[[item]], item)

    raw <- data[, items, drop = FALSE]
    n_total <- nrow(raw)
    complete <- stats::complete.cases(raw)
    item_data <- raw[complete, , drop = FALSE]
    n_used <- nrow(item_data)
    if (n_used < 5L)
        .jr_stop("At least five complete responses are required for reliability analysis.")
    keys <- rep(1, length(items))
    keys[items %in% reverse_items] <- -1
    names(keys) <- items
    fit <- .jr_omega_fit(item_data, keys, correlation)
    omega <- unname(fit$omega.tot)
    if (!is.finite(omega))
        .jr_stop("McDonald's omega could not be estimated for these items.")

    interval <- if (isTRUE(bootstrap)) {
        .jr_omega_bootstrap_ci(item_data, keys, correlation, ci, as.integer(boot_iterations))
    } else {
        c(NA_real_, NA_real_)
    }

    scored <- sweep(as.matrix(item_data), 2, keys, `*`)
    alpha <- .jr_cronbach_alpha(scored)
    item_total <- vapply(seq_along(items), function(i) {
        stats::cor(scored[, i], rowMeans(scored[, -i, drop = FALSE]))
    }, numeric(1))
    loadings <- as.numeric(fit$schmid$sl[, "g"])
    descriptives <- data.frame(
        item = items,
        scoring = ifelse(items %in% reverse_items, "Reverse-keyed", "Positive-keyed"),
        n = n_used,
        mean = vapply(item_data, mean, numeric(1)),
        sd = vapply(item_data, stats::sd, numeric(1)),
        item_total_r = item_total,
        loading = loadings,
        stringsAsFactors = FALSE
    )

    missing_n <- n_total - n_used
    weak_items <- items[is.finite(item_total) & item_total < .30]
    negative_items <- items[is.finite(item_total) & item_total < 0]
    diagnostics <- rbind(
        data.frame(
            check = "Complete responses",
            statistic = n_used,
            p = NA_real_,
            status = if (missing_n > 0L) "Caution" else "Acceptable",
            interpretation = if (missing_n > 0L)
                sprintf("Omega was estimated from %s complete of %s total responses.", n_used, n_total)
            else
                sprintf("All %s responses were available for the selected items.", n_used),
            action = if (missing_n > 0L)
                "Describe missing-data handling and consider whether complete cases differ from incomplete cases."
            else
                "No missing-data action is required for this item set.",
            stringsAsFactors = FALSE
        ),
        data.frame(
            check = "Item direction and consistency",
            statistic = if (length(item_total)) min(item_total, na.rm = TRUE) else NA_real_,
            p = NA_real_,
            status = if (length(negative_items) > 0L) "Serious" else if (length(weak_items) > 0L) "Caution" else "Acceptable",
            interpretation = if (length(negative_items) > 0L)
                sprintf("Negative adjusted item-total correlations were found for: %s.", paste(negative_items, collapse = ", "))
            else if (length(weak_items) > 0L)
                sprintf("Low adjusted item-total correlations were found for: %s.", paste(weak_items, collapse = ", "))
            else
                "Selected items show positive corrected item-total relationships.",
            action = if (length(negative_items) > 0L)
                "Check scoring direction and whether the item belongs in this scale before interpreting omega."
            else if (length(weak_items) > 0L)
                "Review weak items substantively; do not remove items solely to increase reliability."
            else
                "Interpret omega alongside the scale's conceptual coverage.",
            stringsAsFactors = FALSE
        ),
        data.frame(
            check = "Interval estimate",
            statistic = as.numeric(boot_iterations),
            p = NA_real_,
            status = if (isTRUE(bootstrap) && all(is.finite(interval))) "Acceptable" else "Caution",
            interpretation = if (isTRUE(bootstrap) && all(is.finite(interval)))
                sprintf("A %s%% percentile bootstrap interval was estimated from %s resamples.", .jr_num(ci * 100, 0L), boot_iterations)
            else if (isTRUE(bootstrap))
                "A stable bootstrap interval could not be obtained for this item set."
            else
                "No bootstrap confidence interval was requested.",
            action = if (isTRUE(bootstrap) && all(is.finite(interval)))
                "Report the interval to communicate precision in the reliability estimate."
            else
                "Enable a bootstrap interval when a precision estimate is needed.",
            stringsAsFactors = FALSE
        )
    )

    interval_text <- if (all(is.finite(interval))) {
        sprintf(", %s%% bootstrap CI %s", .jr_num(ci * 100, 0L), .jr_ci(interval[1], interval[2], 2L, TRUE))
    } else {
        ""
    }
    strength <- .jr_omega_strength(omega)
    apa <- sprintf(
        "Internal consistency for the %s-item scale was estimated using McDonald's omega total, omega = %s%s, and Cronbach's alpha, alpha = %s, based on %s complete responses.",
        length(items), .jr_num(omega, 2L, TRUE), interval_text, .jr_num(alpha, 2L, TRUE), n_used
    )
    plain <- sprintf(
        "The selected items showed %s (omega = %s; Cronbach's alpha = %s). These coefficients indicate how consistently the item set functions as a composite; they do not by themselves establish validity or a single underlying construct.",
        strength, .jr_num(omega, 2L, TRUE), .jr_num(alpha, 2L, TRUE)
    )
    assumption_text <- .jr_diagnostic_text(diagnostics)
    caution <- if (any(diagnostics$status %in% c("Caution", "Serious")))
        paste("Caution:", assumption_text)
    else ""
    statistics <- rbind(
        data.frame(
            coefficient = "McDonald's omega total",
            estimate = omega,
            ci_low = interval[1],
            ci_high = interval[2],
            n = n_used,
            items = length(items),
            stringsAsFactors = FALSE
        ),
        data.frame(
            coefficient = "Cronbach's alpha",
            estimate = alpha,
            ci_low = NA_real_,
            ci_high = NA_real_,
            n = n_used,
            items = length(items),
            stringsAsFactors = FALSE
        )
    )
    omega_note <- paste(
        "We recommend reporting McDonald's omega alongside Cronbach's alpha because omega is less dependent on tau-equivalent item loadings.",
        "See McDonald (1999) and Revelle and Condon (2019)."
    )
    .new_edu_analysis(
        analysis = "reliability_omega", label = "Reliability Analysis: McDonald's Omega and Cronbach's Alpha",
        question = "How consistently do the selected items function together as a scale?",
        requirements = "Three or more numeric scale items scored in the same conceptual direction, with reverse-keyed items identified before estimation.",
        main = statistics, descriptives = descriptives, effects = statistics,
        diagnostics = diagnostics, interpretation = plain, caution = caution,
        plot_data = descriptives[, c("item", "item_total_r")],
        report_blocks = list(
            rationale = "McDonald's omega total estimates internal consistency using common-factor loadings; Cronbach's alpha is included for continuity with common reporting practice.",
            descriptives = sprintf("The estimate used %s items and %s complete responses.", length(items), n_used),
            apa = apa,
            assumptions = assumption_text,
            plain = plain,
            note = omega_note
        ),
        statistics = statistics, call = match.call()
    )
}
