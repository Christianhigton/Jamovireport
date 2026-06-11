#' Compute rows for a demographic characteristics table
#'
#' @param data A data frame.
#' @param variables Character vector of variable names to summarise.
#' @param custom_rows Optional list of custom row definitions (each a named list
#'   with fields: variable, category, n, percent, note).
#' @param include_custom_in_paragraph Logical; add custom rows to the APA paragraph.
#' @return A list with `rows` (list of named lists), `total_n`, and `paragraph`.
#' @export
edu_demographics <- function(data,
                              variables = character(),
                              custom_rows = NULL,
                              include_custom_in_paragraph = FALSE) {
    if (!is.data.frame(data))
        stop("`data` must be a data frame.")

    rows       <- list()
    total_n    <- nrow(data)
    para_parts <- character()

    for (var in variables) {
        if (!var %in% names(data)) next
        col <- data[[var]]

        if (is.factor(col) || is.character(col)) {
            col     <- droplevels(as.factor(col))
            n_valid <- sum(!is.na(col))
            lvls    <- levels(col)

            rows <- c(rows, list(.dm_row(
                variable = var, category = "",
                n = NA_integer_, percent = NA_real_,
                mean = NA_real_, sd = NA_real_, range = ""
            )))

            level_strs <- character(length(lvls))
            for (i in seq_along(lvls)) {
                lv      <- lvls[[i]]
                n_lv    <- sum(col == lv, na.rm = TRUE)
                pct     <- if (n_valid > 0L) 100 * n_lv / n_valid else NA_real_
                rows    <- c(rows, list(.dm_row(
                    variable = "", category = as.character(lv),
                    n = as.integer(n_lv), percent = pct,
                    mean = NA_real_, sd = NA_real_, range = ""
                )))
                level_strs[[i]] <- sprintf("%s (n = %d, %.1f%%)", lv, n_lv, pct)
            }
            para_parts <- c(para_parts, sprintf(
                "%s: %s.", var, paste(level_strs, collapse = "; ")
            ))

        } else if (is.numeric(col)) {
            n_valid <- sum(!is.na(col))
            m       <- mean(col, na.rm = TRUE)
            s       <- stats::sd(col, na.rm = TRUE)
            mn      <- min(col, na.rm = TRUE)
            mx      <- max(col, na.rm = TRUE)
            rng     <- paste0(sprintf("%.2f", mn), "–", sprintf("%.2f", mx))
            rows <- c(rows, list(.dm_row(
                variable = var, category = "",
                n = as.integer(n_valid), percent = NA_real_,
                mean = m, sd = s, range = rng
            )))
            para_parts <- c(para_parts, sprintf(
                "%s: M = %.2f, SD = %.2f, Range = %s.", var, m, s, rng
            ))
        }
    }

    if (!is.null(custom_rows)) {
        for (cr in custom_rows) {
            var_val  <- trimws(if (is.null(cr$variable)) "" else cr$variable)
            cat_val  <- trimws(if (is.null(cr$category)) "" else cr$category)
            if (!nzchar(var_val) && !nzchar(cat_val)) next

            n_val   <- suppressWarnings(as.numeric(if (is.null(cr$n))       "" else cr$n))
            pct_val <- suppressWarnings(as.numeric(if (is.null(cr$percent)) "" else cr$percent))
            note_val <- trimws(if (is.null(cr$note)) "" else cr$note)

            rows <- c(rows, list(.dm_row(
                variable = var_val,
                category = cat_val,
                n        = if (is.finite(n_val))   as.integer(n_val) else NA_integer_,
                percent  = if (is.finite(pct_val)) pct_val           else NA_real_,
                mean     = NA_real_,
                sd       = NA_real_,
                range    = note_val
            )))

            if (isTRUE(include_custom_in_paragraph)) {
                parts_cr <- c(
                    if (nzchar(var_val)) var_val else character(0),
                    if (nzchar(cat_val)) cat_val else character(0)
                )
                note_str <- if (nzchar(note_val)) paste0(" [", note_val, "]") else ""
                para_parts <- c(para_parts, paste0(
                    paste(parts_cr, collapse = " — "), note_str, "."
                ))
            }
        }
    }

    paragraph <- if (length(para_parts) > 0L) {
        paste(
            sprintf("Descriptive statistics for the sample (N = %d) are presented below.", total_n),
            paste(para_parts, collapse = " ")
        )
    } else {
        sprintf("Descriptive statistics are provided for the sample (N = %d).", total_n)
    }

    list(rows = rows, total_n = total_n, paragraph = paragraph)
}

.dm_row <- function(variable, category, n, percent, mean, sd, range) {
    list(variable = variable, category = category,
         n = n, percent = percent, mean = mean, sd = sd, range = range)
}
