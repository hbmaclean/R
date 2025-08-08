#' nzv_plot
#'
#' This calls nearZeroVar, and blends the results with a variability calculated
#' as number of distinct values per column divided by row count.  If the data.frame
#' is not fully numeric, this attempts to make it so in order to run 'nearZeroVar'.
#'
#' @param    df data.frame object
#' @param    tabular = FALSE
#' @export
nzv_plot <- function(df) {
    if (!is.data.frame(df)) {
        warning('Mutating df to numeric')
        df <- as_numeric_df(df)
    }

    nz_idx  <- nearZeroVar(df)
    nz_cols <- names(df[,nz_idx])

    df |> summarize_all(n_distinct) |>
        pivot_longer(everything(), names_to = 'Column', values_to = 'N_Distinct') |>
        mutate(
           Variability = N_Distinct/nrow(df) * 100,
           NearZeroVar = factor(case_when(
               is.null(nz_cols) ~ NA,
               Column %in% nz_cols ~ TRUE,
               .default = FALSE
           )),
           Column = reorder(Column, desc(Variability))
        ) |>
        ggplot(aes(Column, Variability, color = NearZeroVar)) +
        geom_col(color = '#DC3220', fill = '#DC3220', width = .3) +
        labs(x = element_blank())
}
