#' Summarise demographic characteristics
#'
#' Produces table rows and narrative text for selected continuous and
#' categorical demographic variables.
#'
#' @param data A data frame.
#' @param table_variables Character vector of variables to include in the table.
#' @param paragraph_variables Character vector of variables to include in the narrative.
#' @param stat_mean,stat_sd,stat_median,stat_iqr Logical values selecting continuous summaries.
#' @param stat_min,stat_max,stat_range Logical values selecting range summaries.
#' @param stat_cont_missing Logical; include continuous-variable missing counts.
#' @param stat_n,stat_pct Logical values selecting categorical counts and percentages.
#' @param stat_cat_missing Logical; include categorical-variable missing counts.
#' @param custom_rows Optional list of additional table rows.
#' @return A list containing table rows, narrative text, sample size, and omitted variables.
#' @export
edu_demographics <- function(
    data,
    table_variables     = character(),
    paragraph_variables = character(),
    stat_mean           = TRUE,
    stat_sd             = TRUE,
    stat_median         = FALSE,
    stat_iqr            = FALSE,
    stat_min            = FALSE,
    stat_max            = FALSE,
    stat_range          = FALSE,
    stat_cont_missing   = FALSE,
    stat_n              = TRUE,
    stat_pct            = TRUE,
    stat_cat_missing    = FALSE,
    custom_rows         = NULL
) {
    all_vars <- unique(c(table_variables, paragraph_variables))
    total_n  <- nrow(data)

    table_rows  <- list()
    para_pieces <- sprintf("The sample consisted of %d participants.", total_n)
    table_omit  <- character(0)
    para_omit   <- character(0)

    for (v in all_vars) {
        col <- data[[v]]
        if (is.null(col)) next

        is_table <- v %in% table_variables
        is_para  <- v %in% paragraph_variables

        vname_attr <- attr(col, "jmv-desc")
        vname <- if (!is.null(vname_attr) && nzchar(vname_attr)) vname_attr else v

        if (all(is.na(col))) {
            if (is_table) table_omit <- c(table_omit, vname)
            if (is_para)  para_omit  <- c(para_omit, vname)
            next
        }

        if (is.numeric(col) && !is.factor(col)) {
            rows <- .dm_cont_rows(col, vname,
                                   stat_mean, stat_sd, stat_median, stat_iqr,
                                   stat_min, stat_max, stat_range, stat_cont_missing)
            if (is_table) table_rows <- c(table_rows, rows)
            if (is_para) {
                s <- .dm_cont_sentence(col, vname,
                                       stat_mean, stat_sd, stat_min, stat_max, stat_range)
                if (!is.null(s)) para_pieces <- c(para_pieces, s)
            }
        } else {
            col_f <- if (is.factor(col)) col else as.factor(col)
            rows  <- .dm_cat_rows(col_f, vname, stat_n, stat_pct, stat_cat_missing)
            if (is_table) table_rows <- c(table_rows, rows)
            if (is_para) {
                s <- .dm_cat_sentence(col_f, vname, stat_n, stat_pct)
                if (!is.null(s)) para_pieces <- c(para_pieces, s)
            }
        }
    }

    if (!is.null(custom_rows)) {
        for (cr in custom_rows) {
            char_val <- trimws(if (!is.null(cr$characteristic)) cr$characteristic else "")
            val_val  <- trimws(if (!is.null(cr$value))          cr$value          else "")
            if (!nzchar(char_val) && !nzchar(val_val)) next

            pct_val  <- trimws(if (!is.null(cr$pct))  cr$pct  else "")
            note_val <- trimws(if (!is.null(cr$note)) cr$note else "")

            cell <- val_val
            if (nzchar(pct_val) && nzchar(cell))
                cell <- paste0(cell, " (", pct_val, "%)")
            else if (nzchar(pct_val))
                cell <- paste0("(", pct_val, "%)")
            if (nzchar(note_val))
                cell <- if (nzchar(cell)) paste0(cell, "; ", note_val) else note_val

            table_rows <- c(table_rows,
                            list(list(characteristic = char_val, value = cell)))
        }
    }

    list(
        table_rows  = table_rows,
        paragraph   = paste(para_pieces, collapse = " "),
        total_n     = total_n,
        table_omit  = table_omit,
        para_omit   = para_omit
    )
}

.dm_cont_rows <- function(col, vname,
                           stat_mean, stat_sd, stat_median, stat_iqr,
                           stat_min, stat_max, stat_range, stat_cont_missing) {
    x    <- col[!is.na(col)]
    mn   <- mean(x)
    sdev <- stats::sd(x)
    med  <- stats::median(x)
    q25  <- as.numeric(stats::quantile(x, 0.25))
    q75  <- as.numeric(stats::quantile(x, 0.75))
    lo   <- min(x)
    hi   <- max(x)

    parts <- character(0)

    if (stat_mean && stat_sd) {
        parts <- c(parts, sprintf("%.2f (%.2f)", mn, sdev))
    } else if (stat_mean) {
        parts <- c(parts, sprintf("M = %.2f", mn))
    } else if (stat_sd) {
        parts <- c(parts, sprintf("SD = %.2f", sdev))
    }

    if (stat_median && stat_iqr) {
        parts <- c(parts, sprintf("Mdn = %.2f [%.2f, %.2f]", med, q25, q75))
    } else if (stat_median) {
        parts <- c(parts, sprintf("Mdn = %.2f", med))
    } else if (stat_iqr) {
        parts <- c(parts,
                   paste0("IQR: ", sprintf("%.2f", q25), "\u2013", sprintf("%.2f", q75)))
    }

    if (stat_range) {
        parts <- c(parts,
                   paste0("range ", sprintf("%.2f", lo), "\u2013", sprintf("%.2f", hi)))
    } else {
        if (stat_min) parts <- c(parts, sprintf("min = %.2f", lo))
        if (stat_max) parts <- c(parts, sprintf("max = %.2f", hi))
    }

    value <- if (length(parts) > 0L) paste(parts, collapse = ", ") else ""
    rows  <- list(list(characteristic = vname, value = value))

    if (stat_cont_missing) {
        n_miss <- sum(is.na(col))
        if (n_miss > 0L)
            rows <- c(rows, list(list(
                characteristic = paste0("  ", vname, " missing"),
                value          = as.character(n_miss)
            )))
    }

    rows
}

.dm_cat_rows <- function(col_f, vname, stat_n, stat_pct, stat_cat_missing) {
    valid_n <- sum(!is.na(col_f))
    rows    <- list(list(characteristic = vname, value = ""))
    levs    <- levels(col_f)

    for (lev in levs) {
        n_lev   <- sum(col_f == lev, na.rm = TRUE)
        pct_lev <- if (valid_n > 0L) 100 * n_lev / valid_n else NA_real_
        val     <- .dm_n_pct_cell(n_lev, pct_lev, stat_n, stat_pct)
        rows    <- c(rows, list(list(characteristic = paste0("  ", lev), value = val)))
    }

    if (stat_cat_missing) {
        n_miss <- sum(is.na(col_f))
        if (n_miss > 0L) {
            pct_miss <- if ((valid_n + n_miss) > 0L)
                100 * n_miss / (valid_n + n_miss) else NA_real_
            val  <- .dm_n_pct_cell(n_miss, pct_miss, stat_n, stat_pct)
            rows <- c(rows, list(list(characteristic = "  Missing", value = val)))
        }
    }

    rows
}

.dm_n_pct_cell <- function(n, pct, stat_n, stat_pct) {
    if (stat_n && stat_pct)  sprintf("%d (%.1f%%)", n, pct)
    else if (stat_n)         as.character(n)
    else if (stat_pct)       sprintf("%.1f%%", pct)
    else                     ""
}

.dm_cont_sentence <- function(col, vname, stat_mean, stat_sd,
                               stat_min, stat_max, stat_range) {
    x    <- col[!is.na(col)]
    mn   <- mean(x)
    sdev <- stats::sd(x)
    lo   <- min(x)
    hi   <- max(x)

    show_range <- stat_range || stat_min || stat_max

    if (show_range && (stat_mean || stat_sd)) {
        if (stat_mean && stat_sd)
            return(sprintf("%s ranged from %.2f to %.2f (M = %.2f, SD = %.2f).",
                           vname, lo, hi, mn, sdev))
        else if (stat_mean)
            return(sprintf("%s ranged from %.2f to %.2f (M = %.2f).", vname, lo, hi, mn))
        else
            return(sprintf("%s ranged from %.2f to %.2f.", vname, lo, hi))
    }

    if (stat_mean && stat_sd) return(sprintf("%s had M = %.2f (SD = %.2f).", vname, mn, sdev))
    if (stat_mean)            return(sprintf("%s had M = %.2f.", vname, mn))

    NULL
}

.dm_cat_sentence <- function(col_f, vname, stat_n, stat_pct) {
    valid_n <- sum(!is.na(col_f))
    levs    <- levels(col_f)
    if (valid_n == 0L || length(levs) == 0L) return(NULL)

    counts <- vapply(levs, function(l) sum(col_f == l, na.rm = TRUE), integer(1))
    pcts   <- 100 * counts / valid_n
    ord    <- order(counts, decreasing = TRUE)
    top    <- utils::head(ord, 3L)

    parts <- vapply(top, function(i) {
        if (stat_n && stat_pct) sprintf("%s (n = %d, %.1f%%)", levs[i], counts[i], pcts[i])
        else if (stat_n)        sprintf("%s (n = %d)", levs[i], counts[i])
        else if (stat_pct)      sprintf("%s (%.1f%%)", levs[i], pcts[i])
        else                    levs[i]
    }, character(1))

    remainder <- length(levs) - length(top)
    if (remainder > 0L)
        parts <- c(parts, sprintf("and %d other %s",
                                  remainder, if (remainder == 1L) "category" else "categories"))

    sprintf("In terms of %s, the distribution was: %s.",
            vname, paste(parts, collapse = "; "))
}

.dm_custom_rows_from_options <- function(options) {
    lapply(1:5, function(i) {
        list(
            characteristic = tryCatch(options[[paste0("customRow", i, "Char")]], error = function(e) ""),
            value          = tryCatch(options[[paste0("customRow", i, "Val")]],  error = function(e) ""),
            pct            = tryCatch(options[[paste0("customRow", i, "Pct")]],  error = function(e) ""),
            note           = tryCatch(options[[paste0("customRow", i, "Note")]], error = function(e) "")
        )
    })
}

.dm_paragraph_html <- function(text) {
    paste0(
        '<div data-jr-copy-section="true" style="font-family:sans-serif;font-size:0.9em;line-height:1.6;padding:8px 0">',
        .jr_copyable_body_html(sprintf("<p>%s</p>", .jr_html_escape(text))),
        '</div>'
    )
}

.dm_omit_note_html <- function(table_omit, para_omit) {
    all_omit <- unique(c(table_omit, para_omit))
    if (length(all_omit) == 0L) return("")
    plural   <- length(all_omit) > 1L
    var_list <- .jr_html_escape(paste(all_omit, collapse = ", "))
    msg <- sprintf(
        "<em>Note.</em> The following selected variable%s contained no usable data and %s omitted: %s.",
        if (plural) "s" else "",
        if (plural) "were" else "was",
        var_list
    )
    sprintf(
        paste0(
            '<div data-jr-copy-section="true" style="font-family:sans-serif;font-size:0.85em;color:#555;padding:4px 0">',
            '%s</div>'
        ),
        .jr_copyable_body_html(sprintf("<p>%s</p>", msg))
    )
}
