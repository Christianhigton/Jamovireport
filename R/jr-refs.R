#' @importFrom jmvcore .

.jr_reference_entry_text <- function(key) {
    refs <- tryCatch(get(".jmvrefs", envir = asNamespace("jReport")), error = function(e) NULL)
    ref <- refs[[key]]
    if (is.null(ref)) {
        return(switch(
            key,
            Cohen1988 = "Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Lawrence Erlbaum Associates.",
            Cumming2014 = "Cumming, G. (2014). The new statistics: Why and how. Psychological Science, 25(1), 7-29.",
            ""
        ))
    }
    author <- ref$author %||% ""
    year <- ref$year %||% "n.d."
    title <- ref$title %||% key
    publisher <- ref$publisher %||% ""
    url <- ref$url %||% ""
    tail <- paste(c(publisher, url), collapse = ". ")
    tail <- sub("\\. $", "", tail)
    if (nzchar(tail))
        sprintf("%s (%s). %s. %s.", author, year, title, tail)
    else
        sprintf("%s (%s). %s.", author, year, title)
}

.jr_text_reference_keys <- function(result, include_effect_note = TRUE) {
    analysis_keys <- switch(
        result$analysis,
        ttest = c("effectsize", "ggplot2", "BayesFactor"),
        bayes_ttest = c("BayesFactor"),
        anova_oneway = c("afex", "effectsize", "emmeans"),
        anova_between = c("afex", "effectsize", "emmeans"),
        anova_rm = c("afex", "effectsize"),
        anova_mixed = c("afex", "effectsize"),
        ancova = c("car", "effectsize", "emmeans"),
        manova = c("car", "effectsize"),
        correlation = c("effectsize", "ggplot2"),
        regression = c("parameters", "performance", "effectsize", "ggplot2"),
        logistic_regression = c("parameters", "performance", "effectsize", "ggplot2"),
        multinomial_logistic = c("parameters", "performance", "effectsize"),
        chisq_independence = c("effectsize", "ggplot2"),
        chisq_gof = c("effectsize", "ggplot2"),
        reliability_omega = c("psych", "McDonald1999", "RevelleCondon2019", "ggplot2"),
        character(0)
    )
    keys <- c("jmvcore", "RCore", "jReport", analysis_keys)
    if (isTRUE(include_effect_note) && !identical(result$analysis, "reliability_omega"))
        keys <- c(keys, "Cohen1988", "Cumming2014")
    unique(keys)
}

.jr_addon_inject_refs <- function(parent_pkg) {
    if (is.null(parent_pkg) || !nzchar(parent_pkg)) return(invisible(NULL))
    jr_ns <- tryCatch(getNamespace("jReport"), error = function(e) NULL)
    if (is.null(jr_ns) || !(".jmvrefs" %in% names(jr_ns))) return(invisible(NULL))
    jr_refs <- jr_ns[[".jmvrefs"]]

    parent_ns <- tryCatch(getNamespace(parent_pkg), error = function(e) NULL)
    if (is.null(parent_ns) || !(".jmvrefs" %in% names(parent_ns))) return(invisible(NULL))

    parent_refs <- parent_ns[[".jmvrefs"]]
    new_keys <- setdiff(names(jr_refs), names(parent_refs))
    if (length(new_keys) == 0L) return(invisible(NULL))

    for (key in new_keys)
        parent_refs[[key]] <- jr_refs[[key]]

    tryCatch({
        if (bindingIsLocked(".jmvrefs", parent_ns))
            unlockBinding(".jmvrefs", parent_ns)
        parent_ns[[".jmvrefs"]] <- parent_refs
        lockBinding(".jmvrefs", parent_ns)
    }, error = function(e) invisible(NULL))

    invisible(NULL)
}
