#' @importFrom jmvcore .

.jr_reference_entry_text <- function(key) {
    refs <- tryCatch(get(".jmvrefs", envir = asNamespace("jReport")), error = function(e) NULL)
    ref <- refs[[key]]
    if (is.null(ref)) {
        return(switch(
            key,
            Cohen1988 = "Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Lawrence Erlbaum Associates.",
            Cumming2014 = "Cumming, G. (2014). The new statistics: Why and how. Psychological Science, 25(1), 7-29.",
            Holm1979 = "Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6(2), 65-70.",
            Vickerstaff2019 = "Vickerstaff, V., Omar, R. Z., & Ambler, G. (2019). Methods to adjust for multiple comparisons in the analysis and sample size calculation of randomised controlled trials with multiple primary outcomes. BMC Medical Research Methodology, 19, Article 129. https://doi.org/10.1186/s12874-019-0754-4",
            BenjaminiHochberg1995 = "Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery rate: A practical and powerful approach to multiple testing. Journal of the Royal Statistical Society: Series B (Methodological), 57(1), 289-300. https://doi.org/10.1111/j.2517-6161.1995.tb02031.x",
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
