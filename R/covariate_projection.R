# # Covariate projection
# # Uses a dataframe that contains X, y, and Z, whereby Z represents
# # the covariates


#' Remove covariate influence from multivariate data
#'
#' More detailed description
#'
#' @param X_aug Dataframe that contains explanatory (X), response (y) and potential covariate (Z) variables.
#' @param covariates Names of covariate columns.
#' @param standardize Boolean to indicate standardization X_standardized <- X / X_sd (default FALSE).
#' @param reorder_by_covariates Boolean to indicate if order of selected covariates shall be reflected in result dataframe order (default TRUE).
#'
#' @return A List containing (1) a residual variance dataframe and (2) a dataframe with explained variance by the covariates.
#' @export
#'
#' @examples
#' # res <- perform_covariate_projection(X_aug, covariates = c("Gender", "Age"), standardize = TRUE)
perform_covariate_projection <- function(X_aug = NULL,
                                         covariates = NULL,
                                         standardize = FALSE,
                                         reorder_by_covariates = TRUE) {

    if (is.null(X_aug)) stop("Please provide X_aug.")
    if (is.null(covariates)) stop("Please provide covariate(s).")

    # Optional standardization
    if (standardize){
        X_aug_sd <- apply(X_aug, 2, stats::sd)
        X_aug <- sweep(X_aug, 2, X_aug_sd, '/')
    }

    # Center data
    X_aug <- mean_center(X_aug)

    X_aug_mat <- as.matrix(X_aug)

    covariate_var_list <- list()
    counter = 1
    for (i in covariates) {
        # Prepare w vector --> All variables become zero, but only covariate becomes 1
        w <- rep(0, dim(X_aug)[2])
        w[which(colnames(X_aug) %in% i)] <- 1

        # Calculate covariate score
        t_covariate <- X_aug_mat %*% w

        # Calculate covariate loading
        p_covariate <- (t(X_aug_mat) %*% t_covariate) / c(t(t_covariate) %*% t_covariate)

        # Calculate residual matrix
        E_aug <- X_aug_mat - (t_covariate %*% t(p_covariate))

        # Explained variance by covariate
        covariate_specific_variance <- apply((t_covariate %*% t(p_covariate)), 2, stats::var)

        covariate_var_list[[i]] <- covariate_specific_variance
        counter = counter + 1

        # Substitute X_aug with E_aug
        X_aug_mat <- E_aug
    }

    # Add residual variance
    total_variance = apply(X_aug, 2, stats::var)
    covariate_var_list[["Residual_variance"]] <- total_variance - apply(do.call(rbind, covariate_var_list),2 ,sum)

    # Reformat dataframe
    covariate_var_df <- as.data.frame(do.call(rbind, covariate_var_list))

    # Make a dataframe with column names
    X_aug <- as.data.frame(X_aug_mat,
                           col.names = colnames(X_aug),
                           row.names = rownames(X_aug))

    if (reorder_by_covariates) {
        column_names <- colnames(X_aug)
        new_order <- column_names[!column_names %in% covariates]
        new_order <- append(new_order, covariates, after = 1)

        X_aug <- X_aug[new_order]
        covariate_var_df <- covariate_var_df[new_order]
    }

    # Add covariates as variable
    covariate_var_df <- tibble::rownames_to_column(covariate_var_df, 'covariate')


    # Save results in list
    result <- list('residual_variance_df' = X_aug,
                   'explained_variance_by_covariates' = covariate_var_df)

    return(result)
}


# # Plotting of explained variance by covariates

#' Plot explained variance by covariate
#'
#' @param covariate_analysis_result Result list, obtained from perform_covariate_projection().
#' @param x_filter Filter for variables / features.
#' @param rel_font_size Relative font size based on default font size 11 (default 1).
#' @param rotate Rotate plot by 90 degrees to read axis labels more easily.
#'
#'
#' @importFrom rlang .data
#' @return Stacked bar plot showing the residual variance and explained variance by covariates.
#' @export
#'
#' @examples
#' # # Perform covariate projection
#' # res <- perform_covariate_projection(X_aug, covariates = c("Gender", "Age"), standardize = TRUE)
#' # plot_explained_var_by_covariates(res$explained_variance_by_covariates)
plot_explained_var_by_covariates <- function(covariate_analysis_result = NULL,
                                             x_filter = NULL,
                                             rel_font_size = 1,
                                             rotate = FALSE) {

    if (is.null(covariate_analysis_result)) stop("Please provide covariate_analysis_result")

    X_aug <- covariate_analysis_result$explained_variance_by_covariates

    # Make long table out of covariate variance dataframe
    X_aug_long <- X_aug %>% tidyr::pivot_longer(!(.data$covariate), names_to = 'variable', values_to = 'variance')

    # Wrap text of features so that no more than 15 spaces are used
    X_aug_long$variable <- stringr::str_wrap(X_aug_long$variable, width = 15)

    # Create factors out of covariates and feature names, reverse order of covariates, so that
    # residual variance comes first
    X_aug_long$covariate <- factor(X_aug_long$covariate, levels=X_aug$covariate)

    X_aug_long$variable <- factor(X_aug_long$variable, levels = colnames(X_aug)[-1])

    # Optional  variable / feature filtering (string or vector of strings to keep)
    if (!is.null(x_filter)){
        X_aug_long <- dplyr::filter(X_aug_long, .data$variable %in% x_filter)
    }

    # Generate a full colormap that will be expanded based on the covariate count
    colors_conf = RColorBrewer::brewer.pal(11, "Spectral")

    if (rotate) {
        x <- "variance"
        y <- "variable"
        X_aug_long$variable <- factor(X_aug_long$variable, levels = rev(levels(X_aug_long$variable)))
    } else {
        x <- "variable"
        y <- "variance"
    }

    fig <- plotly::plot_ly(data = X_aug_long, x = ~.data[[x]], y = ~.data[[y]], type = "bar",
                           color = ~.data$covariate,
                           colors = grDevices::colorRampPalette(colors_conf)(dim(X_aug)[1]),
                           hoverinfo = "text",
                           hovertext = ~paste0("Variance explained by ",
                                               .data$covariate,
                                               ": ", round(.data$variance, 2))
    )

    fig <- fig %>%
           plotly::layout(barmode = "stack")

    # Plotly options
    text_options <- list(
        # family = "Courier New",
        size = 11 * rel_font_size
    )

    x_axis_options <- list(
        title = list(text = ifelse(rotate, "Variance", "Variable"), standoff = 20L),
        tickangle = ifelse(rotate, 0, 270)
    )

    y_axis_options <- list(
        title = list(text = ifelse(!rotate, "Variance", "Variable"), standoff = 20L)
    )

    fig <- fig %>%
           plotly::layout(xaxis = x_axis_options,
                          yaxis = y_axis_options,
                          font = text_options
           )

    fig
}
