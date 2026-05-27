utils::globalVariables(c("group", "occasion", "outcome", "x", "y", "fitted", "residual",
                         "item", "item_total_r", "data"))

#' Plot an educational analysis result
#'
#' @param x An `edu_analysis` object.
#' @param kind Plot type. For regression, choose `"residual"` or `"qq"`.
#' @return A `ggplot2` plot.
#' @export
edu_plot <- function(x, kind = NULL) {
    if (!inherits(x, "edu_analysis"))
        .jr_stop("`x` must be an educational analysis result.")
    if (x$analysis %in% c("ttest", "bayes_ttest", "mann_whitney", "wilcoxon_signed_rank", "anova_oneway")) {
        return(
            ggplot2::ggplot(x$plot_data, ggplot2::aes(x = group, y = outcome)) +
                ggplot2::geom_boxplot() +
                ggplot2::theme_minimal() +
                ggplot2::labs(x = "Group", y = "Outcome", title = x$label)
        )
    }
    if (x$analysis %in% c("anova_between", "ancova")) {
        return(
            ggplot2::ggplot(x$plot_data, ggplot2::aes(x = group, y = outcome)) +
                ggplot2::geom_boxplot() +
                ggplot2::theme_minimal() +
                ggplot2::labs(x = "Design cell", y = "Outcome", title = x$label)
        )
    }
    if (x$analysis %in% c("anova_rm", "anova_mixed")) {
        if ("group" %in% names(x$plot_data)) {
            return(
                ggplot2::ggplot(x$plot_data, ggplot2::aes(x = occasion, y = outcome, fill = group)) +
                    ggplot2::geom_boxplot(position = "dodge") +
                    ggplot2::theme_minimal() +
                    ggplot2::labs(x = "Occasion", y = "Outcome", fill = "Group", title = x$label)
            )
        }
        return(
            ggplot2::ggplot(x$plot_data, ggplot2::aes(x = occasion, y = outcome)) +
                ggplot2::geom_boxplot() +
                ggplot2::theme_minimal() +
                ggplot2::labs(x = "Occasion", y = "Outcome", title = x$label)
        )
    }
    if (x$analysis == "correlation") {
        return(
            ggplot2::ggplot(x$plot_data, ggplot2::aes(x = x, y = y)) +
                ggplot2::geom_point() +
                ggplot2::geom_smooth(method = "lm", se = TRUE) +
                ggplot2::theme_minimal() +
                ggplot2::labs(x = unique(x$plot_data$x_name), y = unique(x$plot_data$y_name),
                              title = "Association plot")
        )
    }
    if (x$analysis == "reliability_omega") {
        return(
            ggplot2::ggplot(
                x$plot_data,
                ggplot2::aes(x = stats::reorder(item, item_total_r), y = item_total_r)
            ) +
                ggplot2::geom_col() +
                ggplot2::geom_hline(yintercept = .30, linetype = "dashed") +
                ggplot2::coord_flip() +
                ggplot2::theme_minimal() +
                ggplot2::labs(
                    x = "Item", y = "Corrected item-total correlation",
                    title = "Item Contribution to Scale Reliability"
                )
        )
    }
    if (x$analysis == "regression") {
        kind <- kind %||% "residual"
        if (kind == "qq") {
            return(
                ggplot2::ggplot(x$plot_data, ggplot2::aes(sample = residual)) +
                    ggplot2::geom_qq() + ggplot2::geom_qq_line() +
                    ggplot2::theme_minimal() +
                    ggplot2::labs(title = "Normal Q-Q Plot of Residuals")
            )
        }
        return(
            ggplot2::ggplot(x$plot_data, ggplot2::aes(x = fitted, y = residual)) +
                ggplot2::geom_point() + ggplot2::geom_hline(yintercept = 0) +
                ggplot2::theme_minimal() +
                ggplot2::labs(x = "Fitted values", y = "Residuals",
                              title = "Residuals Versus Fitted Values")
        )
    }
    .jr_stop("No plot method is defined for this result.")
}

#' @export
plot.edu_analysis <- function(x, ...) {
    edu_plot(x, ...)
}
