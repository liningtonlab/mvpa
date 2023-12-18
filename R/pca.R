variable_variation <- function(normalized_loadings, scores) {

    squared_loadings <- normalized_loadings^2
    squared_scores <- scores^2

    # Multiply normalized loadings and scores and calculate the sum for each PC.
    nr_vars <- dim(squared_loadings)[2]
    var_variation <- sapply(1:nr_vars, FUN = function(var) apply(sweep(squared_scores, 2,
                                                                       squared_loadings[var,, drop = FALSE], "*"), 2, sum))
    var_variation <- t(var_variation)

    rownames(var_variation) <- rownames(squared_loadings)

    return(var_variation)
}


#' Perform principal component analysis
#'
#'
#' The function performs principal component analysis based on singular value decomposition (SVD).
#' As a consequence, no missing values are allowed. Use the \code{check_dataset(dataset))} function
#' to check if the dataset is in suited.
#'
#' @param data Dataframe.
#' @param standardize Shall data be standardized (default False).
#' @param exclude_variables Variables to exclude from principal component analysis.
#'
#' @return List that contains the following values:
#' \item{scores}{Matrix with PCA scores}
#' \item{loadings}{Matrix with PCA loadings}
#' \item{variances}{Variance of each principal component}
#' \item{explained_variance}{Explained variance by each component}
#' @export
#'
#' @examples
#' # result <- perform_pca(HOMA_IR)
perform_pca <- function(data = NULL,
                        standardize = FALSE,
                        exclude_variables = NULL) {

    if (is.null(data)) {
        stop("Please provide data.")
    }

    if (!is.null(exclude_variables)) {
        X <- dplyr::select(data, -tidyselect::all_of(exclude_variables))
    } else {
        X <- data
    }

    if (standardize){
        X_sd <- apply(X, 2, stats::sd)
        X <- sweep(X, 2, X_sd, '/')
    }

    prcomp_result <- stats::prcomp(X, scale = FALSE)

    var_variation <- variable_variation(normalized_loadings = prcomp_result$rotation,
                                        scores = prcomp_result$x)

    pca_result <- list(scores = prcomp_result$x,
                       loadings = prcomp_result$rotation,
                       variances = prcomp_result$sdev^2,
                       explained_variance = prcomp_result$sdev^2 / sum(prcomp_result$sdev^2),
                       variable_variation = var_variation
    )

    return(pca_result)

}


#' Plot variable variation
#'
#' The function creates a stacked bar plot where each bar represents a variable and
#' each individual section of one bar represents the explained variation
#' by the individual principal component.
#'
#' @param pca_result PCA result obtained from perform_pca() method.
#' @param x_filter Filter for variables / features.
#' @param select_PCs Select principal components that will be shown in detail. Residual PCs will be summarized in a residual value. If NULL, all PCs will be shown (default is 5).
#' @param rotate Rotate plot by 90 degrees to read axis labels more easily.
#' @param rel_font_size Relative font size (default 1, font size 11).
#'
#' @return Variable variation plot
#' @export
#'
#' @examples
#'
#' # pca_result <- perform_pca(HOMA_IR)
#' # plot_variable_variation_pca(var_variation, select_PCs = c("PC1", "PC3"))
plot_variable_variation_pca <- function(pca_result = NULL,
                                        x_filter = NULL,
                                        select_PCs = 5,
                                        rotate = FALSE,
                                        rel_font_size = 1) {

    if (is.null(pca_result)) stop("Please provide pca_result.")

    values <- as.data.frame(pca_result$variable_variation)

    # Summarize variation per PCs into residual variable
    if (dim(values)[2] < select_PCs | is.null(select_PCs)) {
        select_PCs <- dim(values)[2]
    }

    if (dim(values)[2] != select_PCs) {
    values <- values %>%
        dplyr::select((select_PCs + 1):ncol(.)) %>%
        dplyr::mutate(values, Residual = rowSums(.), .before = 1) %>%
        dplyr::select(1:(select_PCs + 1))
    }


    values <- tibble::rownames_to_column(values, var = "feature")

    # Maintain variable order
    values$feature <- factor(values$feature, levels=values$feature)

    values <- tidyr::pivot_longer(values, -.data$feature, names_to = "PC", values_to = "variation")

    # Reorder PCs for plot
    values$PC <- factor(values$PC, levels=unique(values$PC))

    # Optional  variable / feature filtering (string or vector of strings to keep)
    if (!is.null(x_filter)){
        values <- dplyr::filter(values, .data$feature %in% x_filter)
        values <- droplevels(values)
    }

    values <- droplevels(values)

    # Generate a full colormap that will be expanded based on the PC count
    colors_PC = RColorBrewer::brewer.pal(11, "Spectral")

    if (rotate) {
        fig <- plotly::plot_ly(data = values, x = ~.data$variation, y = ~.data$feature, type = "bar",
                               color = ~.data$PC,
                               colors = grDevices::colorRampPalette(colors_PC)(length(unique(values$PC))),
                               hoverinfo = "text",
                               hovertext = ~paste0("<b>", .data$PC, "</b>",
                                                   ": ", round(.data$variation, 3))
        )
    } else {
        fig <- plotly::plot_ly(data = values, x = ~.data$feature, y = ~.data$variation, type = "bar",
                               color = ~.data$PC,
                               colors = grDevices::colorRampPalette(colors_PC)(length(unique(values$PC))),
                               hoverinfo = "text",
                               hovertext = ~paste0("<b>", .data$PC, "</b>",
                                                   ": ", round(.data$variation, 3))
        )
    }

    fig <- fig %>%
        plotly::layout(barmode = "stack")

    # Plotly options
    text_options <- list(
        # family = "Courier New",
        size = 11 * rel_font_size
    )

    legend_options <- list(
        title = "",
        orientation = "h",
        xanchor = "left",
        y = 1.05
    )

    x_axis_options <- list(
        title = list(text = ifelse(rotate, "Variation", "Variable"), standoff = 20L),
        tickangle = ifelse(rotate, 0, 270)
    )

    y_axis_options <- list(
        title = list(text = ifelse(!rotate, "Variation", "Variable"), standoff = 20L)
    )

    fig <- fig %>%
        plotly::layout(xaxis = x_axis_options,
                       yaxis = y_axis_options,
                       font = text_options,
                       legend = legend_options)

    fig

}


#' Plot scree plot
#'
#' The scree plot indicates the explained variance per principal component.
#' An optional line can be added to the plot that indicates the cumulative percentage plot,
#' indicating the increase in explained variance for each principal component.
#'
#' @param pca_result PCA result obtained from perform_pca() method.
#' @param how_many_PCs How many PCs should be shown in the plot.
#' @param add_cumulative_perc Add optional cumulative percentage line (default FALSE).
#' @param rel_font_size Relative font size (default 1, font size 11).
#'
#' @importFrom rlang .data
#' @return ggplot scree plot.
#' @export
#'
#' @examples
#' pca <- perform_pca(HOMA_IR)
#'
#' plot_scree_plot(pca, 4, TRUE, TRUE)
plot_scree_plot <- function(pca_result,
                            how_many_PCs = 10,
                            add_cumulative_perc = FALSE,
                            rel_font_size = 1) {

    if (length(pca_result$explained_variance) < how_many_PCs) {
        how_many_PCs <- length(pca_result$explained_variance)
    }

    PCs <- sprintf("PC%s", 1:how_many_PCs)

    data <- data.frame(PC = PCs, explained_variance = pca_result$explained_variance[1:how_many_PCs])

    data$PC <- factor(data$PC, levels = data$PC)

    # For cumulative percentage
    data <- data %>%
            dplyr::mutate(cumulative_percentage = cumsum(.data$explained_variance) * 100)

    fig <- plotly::plot_ly(data = data, x = ~.data$PC, y = ~.data$explained_variance * 100, type = "bar",
                           colors = "#41b6c4",
                           hoverinfo = "text",
                           hovertext = ~paste0("<b> ", .data$PC, ": </b>",
                                               round(.data$explained_variance * 100, 1), "%")
    )

    # Plot optional line for cumulative percentage
    if (add_cumulative_perc) {
        fig <- fig %>%
               plotly::add_trace(data, x = ~.data$PC, y = ~.data$cumulative_percentage,
                                 type = "scatter", mode = "lines",
                                 hoverinfo = "text",
                                 text = ~paste0("<b> ", .data$PC, ": </b>",
                                                round(.data$cumulative_percentage, 1), "%"),
                                 line = list(
                                     color = "red"
                                 )
               )

    }
    # plotly options
    text_options <- list(
        # family = "Courier New",
        size = 11 * rel_font_size
    )

    x_axis_options <- list(
        title = "Principal component"
    )

    y_axis_options <- list(
        title = "Explained variance (%)",
        range = c(0, 100)
    )

    fig <- fig %>%
           plotly::hide_legend()

    fig <- fig %>%
        plotly::layout(xaxis = x_axis_options,
                       yaxis = y_axis_options,
                       font = text_options)

    fig

}


#' Plot 2D principal component analysis results
#'
#' Choose between PCA "scores" and "loadings" and plot the PCA results.
#'
#' @param pca_result PCA result obtained from perform_pca() method.
#' @param plot Either "loadings" or "scores" (default).
#' @param PCs_to_plot Tuple of PCs to plot (default c(1, 2)).
#' @param rel_point_size Relative point size (default 1, point size 3).
#' @param rel_font_size Relative font size (default 1, font size 11).
#'
#' @importFrom rlang .data
#' @return ggplot2 2D plot with PCA scores or loadings.
#' @export
#'
#' @examples
#'
#' pca <- perform_pca(HOMA_IR)
#'
#' plot_pca_2d(pca, "scores", c(2, 3))
plot_pca_2d <- function(pca_result,
                        plot = "scores",
                        PCs_to_plot = c(1, 2),
                        rel_point_size = 1,
                        rel_font_size = 1) {

    if (plot == "scores") {
        data <- as.data.frame(pca_result$scores)
    } else {
        data <- as.data.frame(pca_result$loadings)
    }

    PCs <- sprintf("PC%s", PCs_to_plot)

    data <- as.data.frame(data) %>%
                 tibble::rownames_to_column(., "object")

    fig <- plotly::plot_ly(data = data, x = ~.data[[PCs[1]]], y = ~.data[[PCs[2]]],
                           type = "scatter", mode = "markers",
                           marker = list(size = 10 * rel_point_size,
                                         color = "#41B6C4",
                                         line = list(color = "#419bc4",
                                                     width = 2 * rel_point_size)
                                        ),
                           hoverinfo = "text",
                           text = ~paste0("<b>", ifelse(plot == "scores", "Object", "Feature"), ": ", .data$object, "</b>\n",
                                          PCs[1], ": ", round(.data[[PCs[1]]], 2),"\n",
                                          PCs[2], ": ", round(.data[[PCs[2]]], 2))
                           )

    # plotly options
    text_options <- list(
        # family = "Courier New",
        size = 11 * rel_font_size
    )

    x_axis_options <- list(
        title = paste0(PCs[1], " (", round(pca_result$explained_variance[PCs_to_plot[1]] * 100, 1),"%)")
    )

    y_axis_options <- list(
        title = paste0(PCs[2], " (", round(pca_result$explained_variance[PCs_to_plot[2]] * 100, 1),"%)")
    )

    legend_options <- list(
        title = ""
    )

    fig <- fig %>%
           plotly::layout(xaxis = x_axis_options,
                          yaxis = y_axis_options,
                          font = text_options)

    fig

}


#' Plot absolute scores or loadings of a specific principal component in form of a bar plot.
#'
#' @param pca_result PCA result obtained from perform_pca() method.
#' @param plot Either "loadings" or "scores" (default).
#' @param PC_to_plot Principal component (PC) to plot (default 1).
#' @param y_filter Minimum and Maximum filter for values. Either single value or vector c(lower, higher).
#' @param x_filter Filter for objects / variables.
#' @param rel_font_size Relative font size (default 1, font size 11).
#' @param rotate Rotate plot by 90 degrees to read axis labels more easily.
#'
#' @importFrom rlang .data
#' @return Bar plot with desired Principal Component Analysis scores or loading values.
#' @export
#'
#' @examples
#'
#' pca <- perform_pca(HOMA_IR)
#'
#' plot_pca_value(pca, "scores", 2)
plot_pca_value <- function(pca_result = NULL,
                           plot = "scores",
                           PC_to_plot = 1,
                           y_filter = NULL,
                           x_filter = NULL,
                           rel_font_size = 1,
                           rotate = FALSE) {

    if (is.null(pca_result)) stop("Please provide pca_result")

    PC = paste0("PC", PC_to_plot)

    if (plot == "scores") {
        values <- as.data.frame(pca_result$scores[,PC])
    } else  if (plot== "loadings") {
        values <- as.data.frame(pca_result$loadings[,PC])
    } else {
        stop("plot can only be 'scores' or 'loadings'")
    }

    name <- ifelse(plot == "scores", "Score value", "Loading value")

    names(values) <- name

    # "feature" means both here, variable or object, depending on scores or loadings
    values <- tibble::rownames_to_column(values, "feature")

    values$feature <- factor(values$feature, levels=values$feature)

    # Optional  variable / feature filtering (string or vector of strings to keep)
    if (!is.null(x_filter)){
        values <- dplyr::filter(values, .data$feature %in% x_filter)
        values <- droplevels(values)
    }

    # y filter is tuple, format c(start, end)
    if (!is.null(y_filter)){
        if (length(y_filter) == 1) {
            values <- dplyr::filter(values, abs(.data[[name]]) >= y_filter) %>%
                      droplevels()
        } else {
            values <- dplyr::filter(values, (abs(.data[[name]]) >= y_filter[1] & abs(.data[[name]]) <= y_filter[2])) %>%
                      droplevels()
        }
    }

    # Plotting
    if (rotate) {
        # Reverse factor order, so that left to right reads like top to bottom when rotated.
        values$feature <- forcats::fct_rev(values$feature)
        fig <- plotly::plot_ly(data = values, x = ~.data[[name]], y = ~.data$feature, type = "bar",
                               color = I("#41b6c4"),
                               hoverinfo = "text",
                               hovertext = ~paste0("<b>", name, ": </b>",
                                                   round(.data[[name]], 2))
                              )

    } else {
        fig <- plotly::plot_ly(data = values, x = ~.data$feature, y = ~.data[[name]], type = "bar",
                               color = I("#41b6c4"),
                               hoverinfo = "text",
                               hovertext = ~paste0("<b>", name, ": </b>",
                                                   round(.data[[name]], 2))
                               )
    }

    # Modify labels and remove legend
    obj_feat_label <- ifelse(plot == "scores",
                             paste0("Object (", PC, ")"),
                             paste0("Variable (", PC, ")"))

    # plotly options
    text_options <- list(
        # family = "Courier New",
        size = 11 * rel_font_size
    )

    x_axis_options <- list(
        title = list(text = ifelse(rotate, name, obj_feat_label),
                     standoff  = 20L),
        tickangle = ifelse(rotate, 0, 270)
    )

    y_axis_options <- list(
        title = list(text = ifelse(!rotate, name, obj_feat_label),
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
