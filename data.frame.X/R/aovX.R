#' aov_plot
#'
#' Given a data.frame and valid formula, this plots Mean.Sq vs F.value
#' from an Analysis of Variance (aov) summary.  If the data.frame is not
#' fully numeric, this attempts to make it so in order to run 'aov'.
#'
#' @param    df data.frame object
#' @param    formula valid formula object
#' @export
aov_plot <- function(df, formula) {
    tryCatch({F <- as.formula(formula)},
        error   = function(e) { E <- e }
    )
    if (!is.null(E)) {
        stop(paste("invalid formula: ", E))
    }

    if (!is_numeric_df(df)) {
        warning('Mutating df to numeric')
        df <- as_numeric_df(df)
    }

    unclass(summary(stats::aov(formula, data = df))) |>
        as.data.frame() |>
        rownames_to_column(var = 'Column') |>
        filter(!is.na(F.value)) |>
        mutate(Column = reorder(Column, desc(F.value))) |>
        ggplot(aes(Mean.Sq, F.value, color = Column)) +
        geom_point(cex = 4) +
        scale_x_log10()
}
