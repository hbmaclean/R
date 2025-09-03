#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
#    NAME:         online_shoppers.R
#
#    SYNOPSIS:     Rscript online_shoppers.R
#
#    DESCRIPTION:  Downloads data from UCI.
#                  Wrangles the data into a data.frame.
#                  Fits a classification model to it with ranger.
#                  Calculates and reports accuracy and other metrics.
#
#    NOTE:         This intentionally reduced to a minimal working solution.
#
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

library(data.frame.X)
install_packages(c('caret', 'stringr', 'readr', 'dplyr', 'tibble', 'ranger'))

set.seed(234)

fetch_data <- function(verbose = FALSE) {
    options(timeout = 120)
    tmp   <- tempfile()
    zip   <- paste0(
        'https://archive.ics.uci.edu',
        '/static/public/468/online+shoppers+purchasing+intention+dataset.zip'
    )
    download.file(zip, tmp, quiet = !verbose)
    data <- read_csv(unz(tmp, 'online_shoppers_intention.csv'), show_col_types = FALSE)

    # Here I abbreviate some of the lengthy names as given
    colnames(data) <- c(
        'Admin', 'AdminDur', 'Info', 'InfoDur', 'Prod', 'ProdDur',
        'BounceRate', 'ExitRate', 'PageVal', 'SpecialDay', 'Month',
        'OS', 'Browser', 'Region', 'TrafficType', 'VisitorType',
        'Weekend', 'Revenue'
    )
    return(data)
}

shoppers_df <- fetch_data()

shoppers_df <- shoppers_df |>
    mutate(
        Month       = coalesce(match(Month, month.abb), match(Month,month.name)),
        VisitorType = match(VisitorType, as.vector(unique(VisitorType))),
        across(where(is.logical), ~ as.integer(.x)),
        Revenue     = reorder(factor(Revenue), Revenue, decreasing=TRUE)
    )

index    <- createDataPartition(y = shoppers_df$Revenue, times = 1, p = 0.5, list = FALSE)
train_df <- shoppers_df[index,]
test_df  <- shoppers_df[-index,]

fit  <- ranger(
    formula       = Revenue ~ .,
    data          = train_df,
    mtry          = 9,
    min.node.size = 1,
    splitrule     = 'hellinger',
    num.trees     = 1000
)

pred <- predict(fit, test_df)
cm   <- confusionMatrix(pred$predictions, test_df$Revenue)

cm$byClass[c('Prevalence', 'Sensitivity', 'Specificity', 'Precision', 'F1')] |>
    as.data.frame() |>
    rownames_to_column(var = 'Class') |>
    tidyr::pivot_wider(names_from = c(1), values_from = c(2)) |>
    mutate(
        Accuracy = cm$overall[['Accuracy']],
        F_meas   = caret::F_meas(
            data      = train_df$Revenue,
            reference = test_df$Revenue
        ),
        across(everything(), ~ sprintf('%.2f', .x * 100))
    ) |> knitr::kable(digits = 3, caption = 'Classification Metrics')
