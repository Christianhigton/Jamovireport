.jr_empty_frame <- function() {
    data.frame(stringsAsFactors = FALSE)
}

.jr_frame_or_empty <- function(value) {
    if (is.data.frame(value)) value else .jr_empty_frame()
}

.jr_call_value_text <- function(call, name) {
    value <- tryCatch(call[[name]], error = function(e) NULL)
    if (is.null(value))
        return(character())
    variables <- all.vars(value)
    if (length(variables))
        return(variables)
    text <- as.character(value)
    text[nzchar(text)]
}

.jr_report_warnings <- function(diagnostics) {
    diagnostics <- .jr_frame_or_empty(diagnostics)
    required <- c("check", "status", "interpretation", "action")
    if (!all(required %in% names(diagnostics)))
        return(data.frame(
            code = character(), severity = character(), message = character(),
            action = character(), stringsAsFactors = FALSE
        ))
    flagged <- diagnostics$status %in% c("Caution", "Serious")
    rows <- diagnostics[flagged, required, drop = FALSE]
    if (!nrow(rows))
        return(data.frame(
            code = character(), severity = character(), message = character(),
            action = character(), stringsAsFactors = FALSE
        ))
    data.frame(
        code = make.unique(tolower(gsub("[^A-Za-z0-9]+", "_", rows$check))),
        severity = ifelse(rows$status == "Serious", "severe", "warning"),
        message = rows$interpretation,
        action = rows$action,
        stringsAsFactors = FALSE
    )
}

.jr_nonfinite_issues <- function(frames) {
    issues <- character()
    for (frame_name in names(frames)) {
        frame <- frames[[frame_name]]
        if (!is.data.frame(frame))
            next
        numeric_columns <- names(frame)[vapply(frame, is.numeric, logical(1))]
        for (column in numeric_columns) {
            values <- frame[[column]]
            if (any(is.infinite(values), na.rm = TRUE))
                issues <- c(issues, sprintf("%s.%s contains an infinite value", frame_name, column))
            if (identical(column, "p") && any(is.finite(values) & (values < 0 | values > 1)))
                issues <- c(issues, sprintf("%s.%s contains a value outside [0, 1]", frame_name, column))
        }
    }
    unique(issues)
}

.jr_build_report_model <- function(result) {
    call <- result$call %||% NULL
    frames <- list(
        descriptives = .jr_frame_or_empty(result$descriptives),
        tests = .jr_frame_or_empty(result$statistics),
        effects = .jr_frame_or_empty(result$effects),
        coefficients = .jr_frame_or_empty(result$parameters),
        postHocTests = .jr_frame_or_empty(result$posthoc),
        simpleEffects = .jr_frame_or_empty(result$followups),
        cells = .jr_frame_or_empty(result$cells),
        assumptions = .jr_frame_or_empty(result$diagnostics)
    )
    analysis <- result$analysis %||% "unknown"
    variant <- switch(
        analysis,
        manova = if (length(.jr_call_value_text(call, "covariates"))) "mancova" else "manova",
        reliability_omega = "alpha_and_omega",
        analysis
    )
    group_ns <- frames$descriptives
    if (nrow(group_ns) && all(c("n") %in% names(group_ns))) {
        label <- intersect(c("group", "condition", "variable", "category", "item"), names(group_ns))
        group_ns <- group_ns[, unique(c(label[1], "n")), drop = FALSE]
    } else {
        group_ns <- .jr_empty_frame()
    }
    n_values <- unlist(lapply(frames, function(frame) {
        if (is.data.frame(frame) && "n" %in% names(frame)) frame$n else numeric()
    }), use.names = FALSE)
    n_values <- n_values[is.finite(n_values)]
    units <- result$report_blocks %||% list()
    structure(list(
        analysisType = analysis,
        analysisVariant = variant,
        label = result$label %||% analysis,
        outcome = .jr_call_value_text(call, "outcome"),
        predictors = .jr_call_value_text(call, "formula"),
        factors = .jr_call_value_text(call, "factors"),
        groups = .jr_call_value_text(call, "group"),
        conditions = unique(c(
            .jr_call_value_text(call, "paired_outcome"),
            .jr_call_value_text(call, "measures")
        )),
        n = if (length(n_values)) max(n_values) else NA_real_,
        groupNs = group_ns,
        descriptives = frames$descriptives,
        testStatistic = frames$tests,
        df = intersect(c("df", "df1", "df2"), names(frames$tests)),
        p = if ("p" %in% names(frames$tests)) frames$tests$p else numeric(),
        effectSizes = frames$effects,
        confidenceIntervals = frames$effects[, intersect(c("ci_low", "ci_high", "CI_low", "CI_high"), names(frames$effects)), drop = FALSE],
        corrections = frames$tests[, intersect(c("correction", "epsilon", "method"), names(frames$tests)), drop = FALSE],
        postHocTests = frames$postHocTests,
        simpleEffects = frames$simpleEffects,
        modelFit = frames$tests,
        coefficients = frames$coefficients,
        assumptions = frames$assumptions,
        warnings = .jr_report_warnings(frames$assumptions),
        tableData = list(
            main = frames$tests,
            descriptives = frames$descriptives,
            coefficients = frames$coefficients,
            posthoc = frames$postHocTests,
            followups = frames$simpleEffects,
            cells = frames$cells
        ),
        referenceCategory = result$reference %||% character(),
        eventCategory = result$event %||% character(),
        narrativeUnits = list(
            question = result$question %||% "",
            rationale = units$rationale %||% "",
            descriptives = units$descriptives %||% "",
            inferential = units$apa %||% "",
            assumptions = units$assumptions %||% "",
            explanation = units$plain %||% result$interpretation %||% "",
            caution = result$caution %||% "",
            note = units$note %||% ""
        ),
        validationIssues = .jr_nonfinite_issues(frames)
    ), class = c("jr_report_model", "list"))
}

.jr_finalize_edu_analysis <- function(result) {
    result$report_model <- .jr_build_report_model(result)
    result
}

.jr_report_model <- function(result) {
    if (!inherits(result, "edu_analysis"))
        .jr_stop("`result` must be an educational analysis result.")
    .jr_build_report_model(result)
}

.jr_table_p <- function(value) {
    if (length(value) != 1L || !is.finite(value) || value < 0 || value > 1)
        return("\u2014")
    if (value < .001)
        return("&lt; .001")
    sub("^0", "", formatC(value, format = "f", digits = 3))
}

.jr_table_number <- function(value, column) {
    if (length(value) != 1L || !is.finite(value))
        return("\u2014")
    if (column == "p")
        return(.jr_table_p(value))
    if (column %in% c("n", "items"))
        return(formatC(value, format = "f", digits = 0L))
    if (column %in% c("df", "df1", "df2", "df_error") && abs(value - round(value)) < 1e-8)
        return(formatC(value, format = "f", digits = 0L))
    omit_zero <- column %in% c(
        "effect", "ges", "r2", "adjusted_r2", "beta", "estimate",
        "ci_low", "ci_high", "CI_low", "CI_high", "item_total_r", "loading"
    )
    .jr_num(value, 2L, omit_zero = omit_zero)
}

.jr_table_cell <- function(value, column) {
    if (is.numeric(value))
        return(.jr_table_number(value, column))
    if (length(value) == 0L || is.na(value) || !nzchar(as.character(value)))
        return("\u2014")
    .jr_html_escape(as.character(value))
}

.jr_table_headers <- function() {
    c(
        test = "Test", term = "Effect", group = "Group / condition",
        condition = "Condition", variable = "Variable", category = "Category / comparison",
        outcome = "Outcome", coefficient = "Coefficient", item = "Item",
        n = "<i>n</i>", mean = "<i>M</i>", sd = "<i>SD</i>",
        statistic = "Statistic", df = "df", df1 = "df1", df2 = "df2",
        p = "<i>p</i>", effect = "Effect size", ges = "\u03B7<sub>G</sub><sup>2</sup>",
        ci_low = "CI lower", ci_high = "CI upper", estimate = "Estimate",
        Parameter = "Predictor", term = "Term", Coefficient = "<i>B</i>", B = "<i>B</i>",
        SE = "<i>SE</i>", beta = "\u03B2", t = "<i>t</i>", z = "<i>z</i>",
        OR = "OR", RRR = "RRR", CI_low = "CI lower", CI_high = "CI upper",
        r2 = "<i>R</i><sup>2</sup>", adjusted_r2 = "Adjusted <i>R</i><sup>2</sup>",
        observed = "Observed", expected = "Expected", standardised_residual = "Standardized residual",
        p_holm = "Holm <i>p</i>", scoring = "Scoring", item_total_r = "Corrected item-total <i>r</i>",
        loading = "Loading", items = "Items"
    )
}

.jr_table_html <- function(data, columns, title) {
    data <- .jr_frame_or_empty(data)
    columns <- columns[columns %in% names(data)]
    if (!nrow(data) || !length(columns))
        return("")
    headers <- .jr_table_headers()
    heading <- paste0(
        "<tr>", paste0("<th style='text-align:left;padding:7px 9px;border-bottom:1px solid #536472;'>",
        unname(headers[columns]), "</th>", collapse = ""), "</tr>"
    )
    rows <- vapply(seq_len(nrow(data)), function(i) {
        cells <- vapply(columns, function(column) {
            value <- data[[column]][i]
            align <- if (is.numeric(data[[column]])) "right" else "left"
            sprintf("<td style='text-align:%s;padding:6px 9px;border-bottom:1px solid #dfe6ea;'>%s</td>",
                    align, .jr_table_cell(value, column))
        }, character(1))
        paste0("<tr>", paste(cells, collapse = ""), "</tr>")
    }, character(1))
    paste0(
        "<div style='margin:0 0 16px 0;'>",
        "<div style='font-weight:700;margin:0 0 6px 0;'>", .jr_html_escape(title), "</div>",
        "<table style='width:100%;border-collapse:collapse;border-top:2px solid #18242d;border-bottom:2px solid #18242d;'>",
        "<thead>", heading, "</thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>"
    )
}

.jr_apa_table_sections <- function(model, detail = c("compact", "detailed")) {
    detail <- match.arg(detail)
    main <- model$tableData$main
    desc <- model$tableData$descriptives
    coefficients <- model$tableData$coefficients
    analysis <- model$analysisType
    if (analysis == "manova" && nrow(main))
        main$test <- "Pillai's trace"
    main_columns <- switch(
        analysis,
        ttest = c("test", "statistic", "df", "p", "effect", "ci_low", "ci_high"),
        anova_oneway = c("test", "statistic", "df1", "df2", "p", "effect", "ci_low", "ci_high"),
        anova_between = c("term", "statistic", "df1", "df2", "p", "effect", "ci_low", "ci_high"),
        anova_rm = c("term", "statistic", "df1", "df2", "p", "effect", "ges", "ci_low", "ci_high"),
        anova_mixed = c("term", "statistic", "df1", "df2", "p", "effect", "ges", "ci_low", "ci_high"),
        ancova = c("term", "statistic", "df1", "df2", "p", "effect", "ci_low", "ci_high"),
        manova = c("test", "term", "effect", "statistic", "df1", "df2", "p"),
        correlation = c("test", "statistic", "df", "p", "ci_low", "ci_high"),
        chisq_independence = c("test", "statistic", "df", "p", "effect", "n"),
        chisq_gof = c("test", "statistic", "df", "p", "effect", "n"),
        regression = c("test", "statistic", "df1", "df2", "p", "r2", "adjusted_r2"),
        logistic_regression = c("test", "statistic", "df", "p", "r2"),
        multinomial_logistic = c("test", "statistic", "df", "p", "r2"),
        reliability_omega = c("coefficient", "estimate", "ci_low", "ci_high", "n", "items"),
        names(main)
    )
    sections <- .jr_table_html(main, main_columns, paste(model$label, "results"))
    if (analysis == "regression" && nrow(coefficients)) {
        columns <- c("Parameter", "Coefficient", "SE", "beta", "t", "p")
        if (detail == "detailed") columns <- c(columns, "CI_low", "CI_high")
        sections <- paste0(sections, .jr_table_html(coefficients, columns, "Regression coefficients"))
    } else if (analysis == "logistic_regression" && nrow(coefficients)) {
        columns <- c("Parameter", "Coefficient", "SE", "z", "p", "OR")
        if (detail == "detailed") columns <- c(columns, "CI_low", "CI_high")
        sections <- paste0(sections, .jr_table_html(coefficients, columns, "Logistic regression coefficients"))
    } else if (analysis == "multinomial_logistic" && nrow(coefficients)) {
        columns <- c("category", "term", "B", "SE", "z", "p", "RRR")
        if (detail == "detailed") columns <- c(columns, "CI_low", "CI_high")
        sections <- paste0(sections, .jr_table_html(coefficients, columns, "Outcome-category comparisons"))
    } else if (analysis %in% c("chisq_independence", "chisq_gof") && detail == "detailed") {
        sections <- paste0(sections, .jr_table_html(
            model$tableData$cells,
            c("category", "observed", "expected", "standardised_residual"),
            "Observed and expected frequencies"
        ))
    } else if (analysis == "manova" && detail == "detailed") {
        sections <- paste0(sections, .jr_table_html(
            model$tableData$followups,
            c("term", "outcome", "statistic", "df1", "df2", "p", "p_holm", "effect"),
            "Exploratory univariate follow-ups"
        ))
    } else if (detail == "detailed" && nrow(desc)) {
        desc_columns <- intersect(
            c("group", "condition", "variable", "category", "item", "scoring", "n", "mean", "sd", "item_total_r", "loading"),
            names(desc)
        )
        sections <- paste0(sections, .jr_table_html(desc, desc_columns, "Descriptive statistics"))
    }
    sections
}

.jr_apa_table_html <- function(result, detail = c("compact", "detailed")) {
    detail <- match.arg(detail)
    model <- .jr_report_model(result)
    body <- .jr_apa_table_sections(model, detail)
    if (!nzchar(body))
        return("")
    warnings <- model$warnings
    warning_text <- ""
    if (nrow(warnings)) {
        selected <- warnings$severity == "severe" | detail == "detailed"
        messages <- unique(warnings$message[selected])
        if (length(messages))
            warning_text <- paste0(
                "<div style='margin-top:8px;color:#6f4219;'><strong>Note.</strong> ",
                .jr_html_escape(paste(messages, collapse = " ")), "</div>"
            )
    }
    paste0(
        "<div style='width:100%;box-sizing:border-box;padding:12px 14px;border:1px solid #dfe6ea;background:#fff;'>",
        "<div style='font-size:12px;font-weight:700;text-transform:uppercase;margin-bottom:3px;'>APA-style results table</div>",
        "<div style='font-size:12px;color:#536472;margin-bottom:12px;'>",
        if (detail == "compact") "Compact" else "Detailed", " reporting view</div>",
        body, warning_text, "</div>"
    )
}
