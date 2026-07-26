#' @importFrom jmvcore .

.jr_guided_error_message <- function(error) {
    original <- conditionMessage(error)
    if (grepl("logistic regression requires an outcome with exactly two levels", original, ignore.case = TRUE))
        return("Logistic regression requires a binary outcome variable.")
    if (grepl("3 or more levels", original, ignore.case = TRUE))
        return("Multinomial logistic regression requires an outcome variable with 3 or more categories.")
    if (grepl("factor.*has only one level|only.*one.*level|perfect|singular|computationally singular|aliased", original, ignore.case = TRUE))
        return(paste("The model could not be estimated: one or more variables have only one level, or the model terms are perfectly collinear.", paste0("(", original, ")")))
    if (grepl("not enough.*observations|insufficient.*degrees|residual df", original, ignore.case = TRUE))
        return(paste("Not enough observations to fit this model. Try fewer factors or covariates.", paste0("(", original, ")")))
    paste0(
        "The analysis could not be completed. ",
        "Check that the selected variables have enough complete cases, appropriate measurement levels, and no singular or perfectly collinear model terms."
    )
}

.jr_guided_computation <- function(expr, code = "analysisFailed") {
    tryCatch(
        force(expr),
        error = function(e) jmvcore::reject(.jr_guided_error_message(e), code = code)
    )
}

.jr_diagnostic_row_values <- function(diagnostics, i) {
    list(
        check = diagnostics$check[i],
        tested = diagnostics$tested[i],
        statistic = diagnostics$statistic[i],
        p = diagnostics$p[i],
        status = diagnostics$status[i],
        interpretation = diagnostics$interpretation[i],
        action = diagnostics$action[i]
    )
}

.jr_set_guidance_notes <- function(table, rows, label_column,
                                   interpretation_column = "interpretation",
                                   action_column = "action") {
    if (is.null(rows) || nrow(rows) == 0L)
        return(invisible(NULL))
    for (i in seq_len(nrow(rows))) {
        label <- as.character(rows[[label_column]][i] %||% "Check")
        interpretation <- as.character(rows[[interpretation_column]][i] %||% "")
        action <- as.character(rows[[action_column]][i] %||% "")
        note <- paste(
            sprintf("%s \u2014 What this means: %s", label, interpretation),
            sprintf("Recommended action: %s", action)
        )
        table$setNote(paste0("guidance-", i), note)
    }
    invisible(NULL)
}

.jr_set_effect_notes <- function(table, rows) {
    if (is.null(rows) || nrow(rows) == 0L)
        return(invisible(NULL))
    for (i in seq_len(nrow(rows))) {
        analysis <- as.character(rows$analysis[i] %||% "Analysis")
        test <- as.character(rows$test[i] %||% "Result")
        effect <- as.character(rows$effect[i] %||% "")
        interval <- as.character(rows$ci[i] %||% "")
        table$setNote(
            paste0("effect-", i),
            sprintf("%s \u2014 %s: Effect size %s; confidence interval %s.",
                    analysis, test, effect, interval)
        )
    }
    invisible(NULL)
}

.jr_prefill_diagnostic_rows <- function(table, n) {
    existing <- table$rowKeys
    for (i in seq_len(n)) {
        key <- as.character(i)
        if (!key %in% existing) {
            table$addRow(rowKey = i, values = list(
                check = "",
                tested = "",
                statistic = NA_real_,
                p = NA_real_,
                status = "",
                interpretation = "",
                action = ""
            ))
            existing <- table$rowKeys
        }
    }
}

.jr_populate_diagnostics <- function(table, diagnostics, fixed = FALSE) {
    diagnostics <- .jr_normalize_diagnostics(diagnostics)
    if (isTRUE(fixed))
        .jr_prefill_diagnostic_rows(table, nrow(diagnostics))
    for (i in seq_len(nrow(diagnostics))) {
        values <- .jr_diagnostic_row_values(diagnostics, i)
        if (isTRUE(fixed))
            table$setRow(rowKey = i, values = values)
        else
            table$addRow(rowKey = i, values = values)
    }
    .jr_set_guidance_notes(table, diagnostics, "check")
}
