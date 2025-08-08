#' corr_plot
#'
#' Wrapper around ggcorrplot, to plot the correlations for the given data.frame.
#' If the provided data.frame is not fully numeric, this attempts to make it so
#' in order to run 'cor'.
#' @param   df data.frame object
#' @param   digits = 2
#' @param   sig.level = 0.001
#' @export
corr_plot <- function(df, digits = 2, sig.level = 0.001) {
    if (!is_numeric_df(df)) {
        warning('Mutating df to numeric')
        df <- as_numeric_df(df)
    }

    p.mat <- ggcorrplot::cor_pmat(df)
    ggcorrplot::ggcorrplot(
        cor(df),
        hc.order    = TRUE,       # reorder layout with hierarchical clustering
        hc.method   = 'median',   # see ?hclust for other methods
        lab         = TRUE,       # adds correlation coefficients
        lab_size    = 3,
        digits      = digits,
        show.diag   = FALSE,      # omits self-correlation diagonal
        p.mat       = p.mat,      # p-values
        sig.level   = sig.level,  # p-value threshold
        insig       = 'blank',    # omits correlations with p-values below threshold
        show.legend = FALSE,
        title       = 'Feature Correlations'
    )
}
