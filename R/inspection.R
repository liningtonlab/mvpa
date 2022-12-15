# # Inspection functions
# # Correlation, qq-plots (normal plot), summary statistics


#' QQ-plot to check for normality
#'
#' Builds quantile-quantile plots based on selected variables. Several variables can be selected simultaneously.
#'
#' @param data Dataframe.
#' @param vars_to_plot Selection of variables. If NULL (default) the the first variable will be shown.
#' @param nr_cols Number of QQ-plots per line.
#' @param rel_font_size Relative font size based on default font size 11 (default 1).
#'
#' @importFrom rlang .data
#' @return QQ-plot(s) of variables.
#' @export
#'
#' @examples
#' qq_plot(HOMA_IR, c("PA_100_249", "Age"))
qq_plot <- function(data = NULL,
                    vars_to_plot = NULL,
                    nr_cols = 5,
                    rel_font_size = 1) {

    if (is.null(data)) stop("Please provide data.")

    if (is.null(vars_to_plot)) vars_to_plot <- colnames(data)[1]

    data <- dplyr::select(data, dplyr::all_of(vars_to_plot))
    data_long <- tidyr::pivot_longer(data, cols = dplyr::everything())

    # Create ggplot2 qq-plot
    p <- ggplot2::ggplot(data_long, ggplot2::aes(sample = .data$value))
    p <- p + ggplot2::stat_qq() + ggplot2::stat_qq_line()

    # Modify labels
    p <- p + ggplot2::labs(x = "Theoretical", y = "Sample")

    if (length(vars_to_plot) > 1){
        p <- p + ggplot2::facet_wrap(name ~ ., scales = "free", strip.position = "top", ncol = nr_cols)
    } else {
        p <- p + ggplot2::ggtitle(paste("Shown variable:", vars_to_plot))
    }

    font_size = 11 * rel_font_size
    p <- p + ggplot2::theme(text = ggplot2::element_text(size = font_size))

    fig <- plotly::ggplotly(p)

    fig
}


#' Calculation of a correlation matrix
#'
#' Correlate variables with each other (= correlation matrix) or select a response all variables shall correlated with.
#'
#' @param data Dataframe.
#' @param response Variable all other variables shall be correlated with.
#' @param correlate Correlate variables with a single other variable "vars_w_response" (default). Correlate "variables" or "objects".
#' @param selection Select variables and objects for correlation.
#'
#' @return List with the (square) matrix, containing Pearson's r values and stored parameters.
#' \item{correlation_matrix}{Matrix with Pearson's r values - Square matrix if \code{correlate} equals "variables" or "objects"}
#' \item{correlate}{Identical to input argument \code{correlate}}
#' \item{response}{Identical to input argument \code{response}}
#' @export
#'
#' @examples
#' corr_list <- correlation(HOMA_IR, response = "HOMA_IR",
#'                          selection = c("Sex", "PA_2500_2999", "PA_100_249"))
#'
#' corr_list <- correlation(HOMA_IR, correlate = "variables")
#'
#' # corr_list <- correlation(HOMA_IR, correlate = "objects")
correlation <- function(data = NULL,
                        response = NULL,
                        correlate = "vars_w_response",
                        selection = NULL) {

    if (is.null(data)) stop("Please provide data.")

    if (!is.null(selection) & correlate == "objects") {
        data <- keep_objects(data, selection)
    } else if (!is.null(selection) & correlate == "vars_w_response") {
        data <- keep_variables(data, c(response, selection))
    } else if (!is.null(selection)) {
        data <- keep_variables(data, selection)
    }

    result <- switch(correlate,
                     vars_w_response = stats::cor(data[!(colnames(data) %in% response)], data[[response]]),
                     variables = stats::cor(data),
                     objects = stats::cor(t(data)))



    if (correlate == "vars_w_response") colnames(result) <- response
    if (correlate == "objects") {
        rownames(result) <- selection
        colnames(result) <- selection
    }

    corr_list <- list("correlation_matrix" = as.data.frame(result),
                      "correlate" = correlate,
                      "response" = response)

    return(corr_list)
}


#' Plot correlation results
#'
#' This function uses the correlation results and plots them.
#'
#' @param corr_list Result from the correlation() function.
#' @param rotate In case only one variable was correlated with the remaining, rotate the bar plot.
#' @param rel_font_size Relative font size based on default font size 11 (default 1).
#' @importFrom rlang .data
#' @return Either a squared color-coded correlation matrix or a bar plot.
#' @export
#'
#' @examples
#' corr_list <- correlation(HOMA_IR, response = "HOMA_IR")
#' plot_correlation(corr_list)
#'
#' corr_list <- correlation(HOMA_IR, correlate = "variables")
#' plot_correlation(corr_list)
plot_correlation <- function(corr_list = NULL,
                             rotate = FALSE,
                             rel_font_size = 1) {

    if (is.null(corr_list)) stop("Please provide corr_list.")

    corr_data <- corr_list$correlation_matrix

    if (corr_list$correlate != "vars_w_response"){
        corr_data[upper.tri(corr_data)] <- NA
        corr_data <- as.data.frame.table(as.matrix(corr_data), responseName = "value")

        fig <- plotly::plot_ly(data = corr_data, x = ~.data$Var1, y = ~.data$Var2, z = ~.data$value,
                               type = "heatmap",
                               colorbar = list(title = "Pearson's <i>r</i>"),
                               hoverinfo = "text",
                               hovertext = ~paste0("<b>x: </b>", .data$Var1, "\n",
                                                   "<b>y: </b>", .data$Var2, "\n",
                                                   "<b><i>r: </i></b>", round(.data$value, 3))
                               )

        # plotly options
        text_options <- list(
            # family = "Courier New",
            size = 11 * rel_font_size
        )

        x_axis_options <- list(
            title = "",
            tickangle = 270
        )

        y_axis_options <- list(
            title = ""
        )

        fig <- fig %>%
            plotly::layout(xaxis = x_axis_options,
                           yaxis = y_axis_options,
                           font = text_options
            )
        fig
    }
    else {
        colnames(corr_data) <- "value"
        corr_data <- tibble::rownames_to_column(corr_data, "vars")
        corr_data$vars <- factor(corr_data$vars, levels = corr_data$vars)

        if (rotate) {
            fig <- plotly::plot_ly(data = corr_data, x = ~.data$value, y = ~.data$vars, type = "bar",
                                   color = ~.data$value < 0,
                                   colors = c("#41B6C4", "#FA8128"),
                                   hoverinfo = "text",
                                   hovertext = ~paste0("<b>Variable:</b> ", .data$vars, "\n",
                                                       "<b>Pearson's r:</b> ", round(.data$value, 3))
            )
        } else {
            fig <- plotly::plot_ly(data = corr_data, x = ~.data$vars, y = ~.data$value, type = "bar",
                                   color = ~.data$value < 0,
                                   colors = c("#41B6C4", "#FA8128"),
                                   hoverinfo = "text",
                                   hovertext = ~paste0("<b>Variable:</b> ", .data$vars, "\n",
                                                       "<b>Pearson's r:</b> ", round(.data$value, 3))
            )
        }

    # plotly options
    text_options <- list(
        # family = "Courier New",
        size = 11 * rel_font_size
    )

    x_axis_options <- list(
        title = list(text = ifelse(rotate, paste("Pearson's r with", corr_list$response), "Feature"),
                     standoff = 20L),
        tickangle = ifelse(rotate, 0, 270)
    )

    y_axis_options <- list(
        title = list(text = ifelse(!rotate, paste("Pearson's r with", corr_list$response), "Feature"),
                     standoff  = 20L)
    )

    fig <- fig %>%
        plotly::hide_legend() %>%
        plotly::layout(xaxis = x_axis_options,
                       yaxis = y_axis_options,
                       font = text_options
        )

    fig
    }
}


#' Create descriptive statistics summary
#'
#' Data summary will created for dataframes that only consist of numerical values.
#'
#' @param data Dataframe with only numeric entries.
#'
#' @importFrom rlang .data
#' @return Dataframe with min, max, mean, median, variance and standard deviation as columns and all variables as rows.
#' \item{min}{Minimum}
#' \item{max}{Maximum}
#' \item{mean}{Mean}
#' \item{median}{Median}
#' \item{variance}{Variance}
#' \item{sd}{Standard deviation (N - 1)}
#' @export
#'
#' @examples
#' data_summary(HOMA_IR)
data_summary <- function(data = NULL) {

    if (is.null(data)) stop("Please provide data.")

    correct_order <- colnames(data)
    result <- data %>% tidyr::pivot_longer(dplyr::everything()) %>% dplyr::group_by(.data$name) %>%
          dplyr::summarise(dplyr::across(dplyr::everything(), list(min = min,
                                                                   max = max,
                                                                   mean = mean,
                                                                   median = stats::median,
                                                                   variance = stats::var,
                                                                   sd = stats::sd
                                                                   ), .names = "{fn}")) %>%
          dplyr::arrange(factor(.data$name, levels = correct_order))

    result <- tibble::column_to_rownames(result, "name")

    return(result)
}

