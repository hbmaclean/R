#' .pca_barplot
#'
#' Private method to render a barplot from the provided prcomp object.
#'
#' @param   prcomp_obj prcomp object
#' @param   PCs indices of PCs that explain the wanted portion of variance
.pca_barplot <- function(prcomp_obj, PCs) {
    prcomp_obj$rotation[,1:length(PCs), drop = FALSE] |>
        as.data.frame() |>
        rownames_to_column(var = 'Column') |>
        pivot_longer(-Column) |>
        ggplot(aes(y = Column, x = value, fill = factor(name))) +
        geom_col(color = 'black', width = .5) +
        theme(legend.title = element_blank()) +
        labs(
            y     = element_blank(),
            x     = 'Frequency',
            title = 'PCA Barchart'
        )
}

#' .pca_screeplot
#'
#' Private method to render a scree plot from the provided prcomp object.
#'
#' @param   prcomp_obj prcomp object
#' @param   PCs indices of PCs that explain the wanted portion of variance
#' @seealso <https://www.rdocumentation.org/packages/factoextra/versions/1.0.7/topics/eigenvalue>
.pca_screeplot <- function(prcomp_obj, PCs) {
    .summary <- base::summary(prcomp_obj)
    .summary$importance[,1:length(PCs), drop = FALSE] |>
        as.data.frame() |>
        tibble::rownames_to_column(var = 'Column') |>
        dplyr::filter(Column == 'Proportion of Variance') |>
        tidyr::pivot_longer(-Column) |>
        dplyr::select(-Column) |>
        dplyr::mutate(name = reorder(name, desc(value))) |>
        ggplot(aes(name, value, fill = factor(name))) +
        geom_col(color = 'black', show.legend = FALSE) +
        labs(
            x     = element_blank(),
            y     = 'Variance Explained',
            title = 'PCA Scree Plot'
        ) +
        theme(
            axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)
        ) +
        scale_y_continuous(labels = scales::percent)
}

#' .pca_boxplot
#'
#' Private method to render a boxplot from the provided prcomp object.
#'
#' @param   prcomp_obj prcomp object
#' @param   PCs indices of PCs that explain the wanted portion of variance
#' @seealso <https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp>
.pca_boxplot <- function(prcomp_obj, PCs) {
    var_coord_func <- function(loadings, comp.sdev){
        loadings*comp.sdev
    }

    loadings    <- prcomp_obj$rotation
    sdev        <- prcomp_obj$sdev
    var.coord   <- t(apply(loadings, 1, var_coord_func, sdev))
    var.cos2    <- var.coord^2
    comp.cos2   <- apply(var.cos2, 2, sum)
    contrib     <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
    var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))

    var.contrib[,1:length(PCs), drop = FALSE] |>
        as.data.frame() |>
        rownames_to_column() |>
        pivot_longer(-c(1)) |>
        ggplot(aes(factor(name), value)) +
        geom_boxplot(linetype = 'dashed') +
        geom_boxplot(
            aes(ymin = after_stat(lower), ymax = after_stat(upper), fill = name),
            show.legend = FALSE
        ) +
        labs(
            x     = element_blank(),
            y     = 'Variance Contributed',
            title = 'PCA Boxplot'
        )
}

#' pca_plot
#'
#' Arranges up to three plots for princicpal components explaining the wanted
#' percentage of variance for the given data.frame.  If the provided data.frame
#' is not fully numeric, this attempts to make it so in order to run 'prcomp'.
#'
#' The process stops if zero PCs are identified, presumably due to too low a
#' value for pct.
#'
#' @param   df data.frame object
#' @param   pct = 0.90
#' @param   which = c('bar', 'scree', 'box')
#' @export
#' @seealso <https://www.rdocumentation.org/packages/factoextra/versions/1.0.7/topics/eigenvalue>
#' @seealso <https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp>
pca_plot <- function(df, pct = 0.90, which = c('bar', 'scree', 'box')) {
    if (!is_numeric_df(df)) {
        warning('Mutating df to numeric')
        df <- as_numeric_df(df)
    }

    which <- intersect(c('bar', 'scree', 'box'), which)

    if (!length(which)) {
        stop('"which" must be one or more of "bar", "scree", "box"')
    }

    pca <- prcomp(df, scale = TRUE)
    PCs <- which(cumsum(pca$sdev^2)/sum(pca$sdev^2) < pct)

    if (!length(PCs)) {
        stop("No PCs to plot")
    }

    .plots <- list()

    if ('bar' %in% which) {
        .plots[[ 1 + length(.plots) ]] <- .pca_barplot(pca, PCs)
    }
    if ('scree' %in% which) {
        .plots[[ 1 + length(.plots) ]] <- .pca_screeplot(pca, PCs)
    }
    if ('box' %in% which) {
        .plots[[ 1 + length(.plots) ]] <- .pca_boxplot(pca, PCs)
    }

    grid.arrange(
        grobs = .plots,
        top   = textGrob(
            sprintf('Principal Component Analysis, %d%% Variance', pct * 100),
            gp = gpar(fontsize = 16, fontface = "bold")
       )
    )
}
