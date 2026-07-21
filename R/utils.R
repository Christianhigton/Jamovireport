.jr_stop <- function(message) {
    stop(message, call. = FALSE)
}

.jr_assert_columns <- function(data, columns) {
    if (!is.data.frame(data))
        .jr_stop("`data` must be a data frame.")
    missing <- setdiff(columns, names(data))
    if (length(missing) > 0L)
        .jr_stop(sprintf("Variables not found in `data`: %s.", paste(missing, collapse = ", ")))
}

.jr_numeric <- function(x, name) {
    if (!is.numeric(x))
        .jr_stop(sprintf("`%s` must be numeric.", name))
    x
}

.jr_p <- function(p) {
    if (is.na(p))
        return("not available")
    if (p < .001)
        return("< .001")
    paste("=", sub("^0", "", formatC(p, format = "f", digits = 3)))
}

.jr_num <- function(x, digits = 2L, omit_zero = FALSE) {
    if (length(x) == 0L || is.na(x))
        return("NA")
    value <- formatC(as.numeric(x), format = "f", digits = digits)
    if (omit_zero) {
        value <- sub("^0\\.", ".", value)
        value <- sub("^-0\\.", "-.", value)
    }
    value
}

.jr_ci <- function(low, high, digits = 2L, omit_zero = FALSE) {
    sprintf("[%s, %s]", .jr_num(low, digits, omit_zero), .jr_num(high, digits, omit_zero))
}

.jr_or <- function(value) {
    if (is.na(value))
        return("NA")
    if (value > 0 && value < .001)
        return("< 0.001")
    if (value > 0 && value < .01)
        return(formatC(value, format = "f", digits = 3))
    .jr_num(value, 2L)
}

.jr_or_ci <- function(low, high) {
    sprintf("[%s, %s]", .jr_or(low), .jr_or(high))
}

.jr_formula_symbol <- function(name) {
    name <- as.character(name)
    if (length(name) != 1L || is.na(name) || !nzchar(name))
        .jr_stop("Formula variable names must be non-empty scalar strings.")
    as.name(name)
}

.jr_formula_term <- function(names, operator = ":") {
    symbols <- lapply(as.character(names), .jr_formula_symbol)
    if (length(symbols) == 0L)
        return(NULL)
    if (length(symbols) == 1L)
        return(symbols[[1]])
    Reduce(function(left, right) call(operator, left, right), symbols)
}

.jr_formula_from_expressions <- function(lhs, expressions = list(), env = parent.frame()) {
    lhs <- as.character(lhs)
    response <- if (length(lhs) == 1L) {
        .jr_formula_symbol(lhs)
    } else {
        as.call(c(list(as.name("cbind")), lapply(lhs, .jr_formula_symbol)))
    }
    expressions <- Filter(Negate(is.null), expressions)
    rhs <- if (length(expressions) == 0L) {
        1
    } else if (length(expressions) == 1L) {
        expressions[[1]]
    } else {
        Reduce(function(left, right) call("+", left, right), expressions)
    }
    stats::as.formula(call("~", response, rhs), env = env)
}

.jr_formula <- function(lhs, rhs = character(), factorial = character(), env = parent.frame()) {
    terms <- if (is.list(rhs)) rhs else as.list(as.character(rhs))
    expressions <- lapply(terms, .jr_formula_term)
    if (length(factorial) > 0L)
        expressions <- c(list(.jr_formula_term(factorial, operator = "*")), expressions)
    .jr_formula_from_expressions(lhs, expressions, env = env)
}

.jr_shapiro <- function(values, label = "Normality") {
    values <- values[is.finite(values)]
    n <- length(values)
    if (n < 3L || n > 5000L) {
        return(data.frame(
            check = label, statistic = NA_real_, p = NA_real_,
            status = "Not assessed",
            interpretation = "A Shapiro-Wilk test is only reported for samples of 3 to 5000 complete observations.",
            action = "Inspect a Q-Q plot and consider the design and sample size.",
            stringsAsFactors = FALSE
        ))
    }
    if (length(unique(values)) < 2L) {
        return(data.frame(
            check = label, statistic = NA_real_, p = NA_real_,
            status = "Not assessed",
            interpretation = "Shapiro-Wilk normality cannot be assessed because all complete values are identical.",
            action = "Review the constant values and interpret the primary analysis alongside the study design and other diagnostics.",
            stringsAsFactors = FALSE
        ))
    }
    result <- stats::shapiro.test(values)
    flagged <- result$p.value < .05
    data.frame(
        check = label,
        statistic = unname(result$statistic),
        p = result$p.value,
        status = if (flagged) "Caution" else "Acceptable",
        interpretation = if (flagged)
            "The distribution departs from normality according to Shapiro-Wilk."
        else
            "No clear departure from normality was detected by Shapiro-Wilk.",
        action = if (flagged)
            "Inspect plots and consider robust or sensitivity analyses; do not change tests from this p value alone."
        else
            "Retain the planned analysis while inspecting plots for influential observations.",
        stringsAsFactors = FALSE
    )
}

.jr_diagnostic_text <- function(diagnostics) {
    diagnostics <- .jr_normalize_diagnostics(diagnostics)
    if (nrow(diagnostics) == 0L)
        return("No automatic diagnostic checks were requested.")
    cautions <- diagnostics$status %in% c("Caution", "Serious")
    if (any(cautions)) {
        checks <- paste(diagnostics$check[cautions], collapse = " and ")
        return(sprintf(
            "Diagnostics raised caution about %s. These checks should inform interpretation and sensitivity analysis rather than automatically choose a different test.",
            checks
        ))
    }
    "The reported diagnostics did not indicate a material assumption concern; study-design assumptions still require justification."
}

.jr_normalize_diagnostics <- function(diagnostics) {
    if (is.null(diagnostics) || nrow(diagnostics) == 0L)
        return(data.frame(
            check = character(), tested = character(), statistic = numeric(), p = numeric(),
            status = character(), interpretation = character(), action = character(),
            stringsAsFactors = FALSE
        ))
    if (!"tested" %in% names(diagnostics)) {
        tested <- ifelse(
            diagnostics$status %in% c("Not assessed", "Not required"),
            "No", "Yes"
        )
        diagnostics$tested <- tested
    }
    diagnostics <- diagnostics[, c(
        "check", "tested", "statistic", "p", "status", "interpretation", "action"
    )]
    diagnostics
}

.jr_assumption_row <- function(check, tested = "No", statistic = NA_real_, p = NA_real_,
                               status = "Not assessed", interpretation, action) {
    data.frame(
        check = check, tested = tested, statistic = statistic, p = p, status = status,
        interpretation = interpretation, action = action, stringsAsFactors = FALSE
    )
}

.jr_default_assumptions <- function(analysis) {
    rows <- list()
    add <- function(check, interpretation, action) {
        rows[[length(rows) + 1L]] <<- .jr_assumption_row(check, interpretation = interpretation, action = action)
    }
    if (analysis %in% c("ttest", "mann_whitney", "bayes_ttest", "anova_oneway",
                        "anova_between", "ancova", "manova", "correlation",
                        "regression", "logistic_regression", "chisq_independence",
                        "chisq_gof")) {
        add(
            "Independence of observations",
            "Independence is a design assumption and is not tested from the data.",
            "Confirm the sampling, grouping, or assignment process; dependence can make p values and intervals too optimistic."
        )
    }
    if (analysis %in% c("ttest", "anova_oneway", "anova_between", "ancova", "manova",
                        "regression", "correlation")) {
        add(
            "Measurement level and scale suitability",
            "The selected variables are checked for numeric/factor type where possible, but the measurement scale must be justified by the researcher.",
            "Confirm that numeric outcomes and covariates are meaningful for the chosen model and that categorical factors represent the intended groups."
        )
    }
    if (analysis %in% c("regression", "logistic_regression", "ancova", "manova")) {
        add(
            "Model specification",
            "Automatic diagnostics cannot confirm that the model contains all important terms, transformations, interactions, or confounders.",
            "Check the research design, theory, and plots before interpreting coefficients or adjusted effects."
        )
    }
    if (analysis %in% c("manova")) {
        add(
            "Multivariate normality",
            "The report checks residual normality for each dependent variable, but it does not provide a full multivariate normality test.",
            "Inspect multivariate outliers, residual plots, and the sensitivity of conclusions to influential cases."
        )
    }
    if (analysis %in% c("chisq_independence", "chisq_gof")) {
        add(
            "Mutually exclusive categories",
            "Category exclusivity is a data-coding assumption and is not tested automatically.",
            "Confirm that each case contributes to one appropriate category unless the analysis explicitly models repeated counts."
        )
    }
    if (analysis %in% c("reliability_omega")) {
        add(
            "Common construct and item direction",
            "Internal consistency assumes the selected items are intended to measure a shared construct.",
            "Review item content, reverse-keying, dimensionality, and item-total diagnostics before reporting omega as scale reliability."
        )
    }
    if (length(rows) == 0L)
        return(.jr_normalize_diagnostics(data.frame()))
    do.call(rbind, rows)
}

.jr_complete <- function(data, columns) {
    out <- data[stats::complete.cases(data[, columns, drop = FALSE]), columns, drop = FALSE]
    if (nrow(out) == 0L)
        .jr_stop("No complete observations remain for this analysis.")
    out
}
