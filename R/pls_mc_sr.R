# # Partial least squares regression, validated by Monte Carlo resampling
# # Uses a dataframe that contains X and y


explained_y_var <- function(X, y, b_LVR) {
    X_c <- mean_center(X)
    y_c <- mean_center(y)

    R2 <- 1 - ((l2_norm(y_c - X_c %*% b_LVR))^2 / (l2_norm(y_c))^2)

    return(R2)
}


explained_y_var_each_component <- function(X, y, a_opt, verbose=FALSE) {

    R2_before <- 0
    R2_list <- c()
    for (i in 1:a_opt) {
        pls_model <- chemometrics::pls1_nipals(X, y, i)
        b <- pls_model$b
        X_c <- mean_center(X)
        y_c <- mean_center(y)

        R2_new <- 1 - ((l2_norm(y_c - X_c %*% b))^2 / (l2_norm(y_c))^2)

        R2_list <- c(R2_list, R2_new - R2_before)

        R2_before <- R2_new
    }

    names(R2_list) <- sprintf("%s_component_model", 1:a_opt)

    if (verbose){
        cat("The explained variance in y for each LV is:", sprintf("%s%% ", round(R2_list * 100, 2)),
            sprintf("(total explained variance in y: %s%%)", round(sum(R2_list), 2)))
    }

    return(invisible(R2_list))
}


explained_X_var = function(X, pls_model, verbose = FALSE){
    magnitude_component <- apply(pls_model$T^2, 2, sum) * apply(pls_model$P^2, 2, sum)

    total_variance <- sum(mean_center(X)^2)

    expl_var <- magnitude_component / total_variance
    names(expl_var) <- sprintf("%s_component_model", seq(length(expl_var)))

    if (verbose){
        cat("The explained variance in X for each LV is:", sprintf("%s%% ", round(expl_var * 100, 2)),
            sprintf("(total explained variance in X: %s%%)", round(sum(expl_var * 100), 2)))
    }

    return(invisible(expl_var))
}


#' Target projection on latent variable regression model
#'
#' This function calculates all target projection values, including all selectivity ratios and derivatives.
#' A detailed algebraic description of the target projection can be found in
#' Kvalheim, e3211, 2020, Journal of Chemometrics (\href{doi.org/10.1002/cem.3211}{link to publication}).
#'
#'
#' @param X Matrix object with explanatory variables (n x m).
#' @param y Matrix object with response variables (n x 1).
#' @param b_LVR Matrix object with regression coefficients (m x 1).
#' @param sr_only Boolean to indicate if only selectivity ratios and derived values shall be returned (default FALSE).
#' @param X_sd Standard deviation of X - Used for calculation of MCovC
#' @param y_sd Standard deviation of y - Used for calculation of MCovC
#'
#' @return List that contains the following values:
#' \item{wTP}{Target projection weights}
#' \item{tTP}{Target projection scores}
#' \item{pTP}{Target projection loadings}
#' \item{Etp}{Target projection residual matrix}
#' \item{explained_variance}{Explained variance - derived from \code{tTP} and \code{pTP}}
#' \item{residual_variance}{Residual variance - derived from \code{Etp}}
#' \item{explained_variance_tTP}{Variance explained by the target score tTP = \eqn{\frac{explained\:variance}{(exaplained\:variance\:+\:residual\:variance)}}}
#' \item{selectivity_ratio}{Selectivity ratio (SR) = \eqn{\frac{explained\:variance}{residual\:variance}}}
#' \item{selectivity_fraction}{Selectivity fraction (SF) = \eqn{\frac{explained\:variance}{var(X)}}}
#' \item{MCorrC}{Multivariate correlation coefficient - MCorrC = \eqn{\frac{pTP}{|pTP|} * \sqrt{|SF\:R2|}}}
#' \item{MCovC}{Multivariate covariance coefficient - MCovC = \eqn{\frac{MCorrC}{sd(X)}}}
#' @export
#'
#' @examples
#' # target_projection(X, y, b_LVR, TRUE, X_sd, y_sd)
target_projection <- function(X = NULL,
                              y = NULL,
                              X_sd = NULL,
                              y_sd = NULL,
                              b_LVR = NULL,
                              sr_only = FALSE) {

    if (is.null(X)) stop("Please provide X.")
    if (is.null(y)) stop("Please provide y.")
    if (is.null(X_sd)) stop("Please provide the standard deviation of X.")
    if (is.null(y_sd)) stop("Please provide the standard deviation of y.")
    if (is.null(b_LVR)) stop("Please provide b_LVR (latent variable regression model coefficients).")

    # Equations mentioned refer to doi.org/10.1002/cem.3211
    # Center X data
    X_c <- mean_center(X)

    # # Calculate the normalized regression coefficients (wTP) (eq. 5)
    wTP <- b_LVR/l2_norm(b_LVR)

    # # Calculate the target-projected score vector by projecting wTP on X (eq. 6)
    tTP <- X_c %*% wTP

    # # Obtain the corresponding target-projected loadings (eq. 7)
    pTP <- t(X_c) %*% tTP / as.numeric(t(tTP) %*% tTP)

    # # Calculate the residual matrix Etp from X, the score vector tTP and the corresponding loading vector pTP (eq. 8)
    Etp <- X_c - tTP %*% t(pTP)

    # # Calculate the SR for each predictor variable individually (eq. 9)
    variance_explained <- apply(tTP %*% t(pTP), 2, l2_norm)^2 / (nrow(X) - 1)
    variance_residual <- apply(Etp^2, 2, sum) / (nrow(X) - 1)

    SR <- variance_explained / variance_residual

    # # Multiply the normalized SR with the sign of the corresponding loading vector pTP to enable the SR plot visualization
    sign <- pTP/abs(pTP)
    SR <- SR * sign

    # Calculate SF (also known as SR2) - explained variance of each predictor divided by total variance in X
    total_variance <- apply(X_c, 2, stats::var)

    SF <- variance_explained / total_variance
    SF <- SF * sign

    # Calculate SF * explained variance in y aka SR3
    R2 <- explained_y_var(X, y, b_LVR)
    SR3 <- SF * R2

    # Calculate multivariate correlation coefficient (MCorrC)
    # Square-root of absolute values of SR3
    MCorrC <- sqrt(abs(SR3)) * sign

    # Calculate multivariate covariance coefficient (MCovC) by division
    # of the standard deviation of the X predictors
    MCovC <- MCorrC / X_sd

    # Substitute NaN SRs with 0 values.
    # This happens when 0 division occurs.
    SR[is.nan(SR)] <- 0
    SF[is.nan(SF)] <- 0
    SR3[is.nan(SR3)] <- 0
    MCorrC[is.nan(MCorrC)] <- 0
    MCovC[is.nan(MCovC)] <- 0

    # Build list with all selectivity ratios
    if (sr_only) {
        tp_list <- list("selectivity_ratio" = SR,
                        "selectivity_fraction" = SF,
                        "sr3" = SR3,
                        "MCorrC" = MCorrC,
                        "MCovC" = MCovC)
    } else {
        tp_list <- list("wTP" = as.data.frame(matrix(wTP, dimnames = list(rownames(wTP), "wTP"))),
                        "tTP" = as.data.frame(matrix(tTP, dimnames = list(rownames(tTP), "tTP"))),
                        "pTP" = as.data.frame(matrix(pTP, dimnames = list(rownames(pTP), "pTP"))),
                        "explained_variance" = as.data.frame(matrix(variance_explained, dimnames = list(names(variance_explained), "variance_explained"))),
                        "residual_variance" = as.data.frame(matrix(variance_residual, dimnames = list(names(variance_residual), "variance_residual"))),
                        "explained_variance_tTP" = sum(variance_explained) / (sum(variance_explained) + sum(variance_residual)),
                        "Etp" = Etp,
                        "selectivity_ratio" = as.data.frame(matrix(SR, dimnames = list(rownames(SR), "SR"))),
                        "selectivity_fraction" = as.data.frame(matrix(SF, dimnames = list(rownames(SF), "SF"))),
                        "sr3" = as.data.frame(matrix(SR3, dimnames = list(rownames(SR3), "SR3"))),
                        "MCorrC" = as.data.frame(matrix(MCorrC, dimnames = list(rownames(MCorrC), "MCorrC"))),
                        "MCovC" = as.data.frame(matrix(MCovC, dimnames = list(rownames(MCovC), "MCovC"))))
    }
    return(tp_list)
}


#' PLS-R model building and Target Projection
#'
#' This function allows PLS model building, using a combined dataframe with explanatory and a single response variable.
#' It is recommended to use Monte Carlo resampling to determine an optimal number of PLS components used for the model.
#' During Monte Carlo resampling, a fixed number of random objects is picked to build PLS models with varying numbers of
#' PLS components. A loss function (RMSEP by default) calculates the goodness of the built models and the objects left out
#' for validation. The model with the most robust and best performance (= lowest cost function value) indicates the number of components
#' to use for the full dataset.
#'
#' @param data Dataframe that contains explanatory variables (X) and a response variable (y).
#' @param response Name of the response variable.
#' @param nr_components Number of components to test (default 8).
#' @param mc_resampling Boolean to indicate usage of Monte Carlo resampling (default TRUE). If FALSE, nr_of components will be used directly to build the PLS-R model.
#' @param cal_ratio Ratio of calibration data to total available data (eg. 0.8 -> 80\% of the data is used for calibration).
#' @param nr_repetitions Number of Monte Carlo repetitions (default 100).
#' @param standardize Boolean to indicate standardization X_standardized <- X / X_sd (default FALSE).
#' @param use_sd_full_dataset Boolean to indicate if the standard deviation (n -1) of the full dataset shall be used. Default FALSE. Recommended/Necessary when Monte Carlo resampling leads to samples with variables that exhibit zero variance.
#' @param validation_threshold Threshold fraction upon a component number is selected. Based on Monte Carlo resampling. Too high values lead to overfitting (default 0.5).
#' @param cost_function_type Cost function to use to measure model performance. Root mean squared error ("rmsep", default) and mean absolute error ("mae") available.
#'
#' @return A list that contains the following results:
#' \item{errors}{\code{cost_function} error value matrix with dimensions \code{nr_repetitions} * \code{nr_components}}
#' \item{cost_function}{Indicates with cost function was used (RMSEP, MAE)}
#' \item{A_optimal}{Optimal number of components. Component count was used to perform target projection on full dataset}
#' \item{fractions}{Ratio of cost function values (higher component) above and below the median cost function value of the lesser component. The selected \code{validation_threshold} determines \code{A_optimal}}
#' \item{tested_components}{List with target projection values of tested components 1 to \code{nr_components} for each repetition}
#' \item{target_projection_A_opt}{List with target projection values of the full dataset and based on \code{A_optimal} retained components}
#' \item{explained_variance_in_X}{Explained variance in X for each component - Multiply by 100 to obtain \%}
#' \item{explained_variance_in_y}{Explained variance in y for each component - Multiply by 100 to obtain \%}
#' \item{explained_variance_tTP}{Explained variance of target score tTP - Multiply by 100 to obtain \%}
#' @export
#'
#' @examples
#' # perform_mc_pls_tp(data = HOMA_IR, response = "HOMA_IR", nr_components = 8,
#' #                   mc_resampling = TRUE, cal_ratio = 0.5, repetitions = 100, standardize = TRUE)
perform_mc_pls_tp <- function(data = NULL,
                              response = NULL,
                              nr_components = 8,
                              mc_resampling = TRUE,
                              cal_ratio = 0.5,
                              nr_repetitions = 100,
                              standardize = FALSE,
                              use_sd_full_dataset = FALSE,
                              validation_threshold = 0.5,
                              cost_function_type = "rmsep") {

    # Target projection  values to save
    tp_vals <- c("wTP",
                 "tTP",
                 "pTP",
                 "selectivity_ratio",
                 "selectivity_fraction",
                 "MCorrC",
                 "MCovC")


    if (is.null(data)) stop("Please provide data.")
    if (is.null(response)) stop("Please provide response.")

    if (nr_components > dim(data)[2]) {
        nr_components <- dim(data)[2] - 1
        warning(paste("Chosen number of components greater than number of variables in the dataset. Argument will be reduced to number of variables:", nr_components))
    }
    # Convert data.frame into X and y matrix
    X <- as.matrix(dplyr::select(data, -tidyselect::all_of(response)))
    y <- as.matrix(dplyr::select(data, tidyselect::all_of(response)))

    # Calculate standard deviation from full data
    X_sd_full_data_set <- apply(X, 2, stats::sd)
    y_sd_full_data_set <- stats::sd(y)

    # Check that cal_ratio leads to at least two retained calibration/validation samples.
    # If 1 or 0, the method will fail.
    if (mc_resampling & any(lapply(split_dataset(data, NULL, cal_ratio), nrow) <= 2)) {
        stop("Chosen cal_ratio results in less than 2 calibration / validation samples. Please adjust cal_ratio.")
    }

    result_list <- list()
    # Monte Carlo Resampling
    if (mc_resampling) {

        mc_list <- list()
        for (component in seq(0, nr_components)) {
            mc_list[[paste0(component, "_component_model")]] <- list()
        }

        # Repeat the PLSR model creation for the components nr_repetitions times
        for (repetition in seq(nr_repetitions)) {

            # Split data set
            data_list <- split_dataset(X, y, cal_ratio = cal_ratio)

            # Get standard deviation for X and y sample selected by the resampling procedure
            if (use_sd_full_dataset) {

                # Use sd of full dataset
                X_cal_sd <- X_sd_full_data_set
                y_cal_sd <- y_sd_full_data_set

            } else {

                X_cal_sd <- apply(data_list$X_cal, 2, stats::sd)
                y_cal_sd <- stats::sd(y)

                if (any(X_cal_sd == 0)) {
                    stop("Monte Carlo resampling resulted in the creation of datasets where zero variance is encountered in variables.
                         Please redo the analysis with 'use_full_dataset_sd' set to TRUE.")
                } else if (y_cal_sd == 0) {
                    stop("Monte Carlo resampling resulted in the creation of datasets where zero variance is encountered in the response.
                         Please redo the analysis with 'use_full_dataset_sd' set to TRUE.")
                }
            }

            # Optional standardization (X / sd(X))
            if (standardize) {

                # Standardize
                X_cal <- sweep(data_list$X_cal, 2, X_cal_sd, "/")
                X_val <- sweep(data_list$X_val, 2, X_cal_sd, "/")

                # Get mean of standardized calibration X
                X_cal_mean <- apply(X_cal, 2, mean)

                # Center
                X_cal <- sweep(X_cal, 2, X_cal_mean, "-")
                X_val <- sweep(X_val, 2, X_cal_mean, "-")

            } else {
                # Only center data
                X_cal_mean <- apply(data_list$X_cal, 2, mean)
                X_cal <- sweep(data_list$X_cal, 2, X_cal_mean, "-")
                X_val <- sweep(data_list$X_val, 2, X_cal_mean, "-")
            }

            # Zero component model
            y_hat <- mean(data_list$y_cal)

            cost_func_val <- cost_function(data_list$y_val, y_hat, type = cost_function_type)
            mc_list[["0_component_model"]][[cost_function_type]] <- append(mc_list[["0_component_model"]][[cost_function_type]], cost_func_val)

            # Test components 1 up to nr_components
            for (component in seq(nr_components)) {

                # Build PLSR model
                # Suppress error:Error in solve.default(t(P) %*% W) : system is computationally singular: reciprocal condition number
                pls_model <- tryCatch(
                    expr = {
                        chemometrics::pls1_nipals(X = X_cal,
                                                  y = data_list$y_cal,
                                                  a = component)
                    },

                    error = function(e) {
                        print(e)
                        return(NULL)
                    }
                )

                # Test if coefficients calculated by pls1_nipals contain NAs, indicating problems with the
                # input data (eg only 0s in X)
                if (is.null(pls_model)) {

                    stop("PLS-R model could not be built due to poor X or y data.
                          Possible source of error: Centered X variable values are all 0.")
                }

                # Target projection
                tp_list <- target_projection(X = X_cal,
                                             y = data_list$y_cal,
                                             b_LVR = pls_model$b,
                                             X_sd = X_cal_sd,
                                             y_sd = y_cal_sd,
                                             sr_only = FALSE)

                # Predict y_hat using the PLS model and account for the
                # y_calibration offset, using the median and calculate the RMSEP
                y_hat <- mean(data_list$y_cal) + X_val %*% pls_model$b
                cost_func_val <- cost_function(data_list$y_val, y_hat, type = cost_function_type)

                # Add selectivity ratios and target projection values to mc_list
                current_compound <- paste0(component, "_component_model")
                mc_list[[current_compound]][[cost_function_type]] <- append(mc_list[[current_compound]][[cost_function_type]], cost_func_val)
                mc_list[[current_compound]][["tp_list"]] <- append(mc_list[[current_compound]][["tp_list"]], list(tp_list))
            }
        }

        # result_list <- list("error" = retrieve_cost_function_vals(mc_list, cost_function_type),
        #                     "cost_function" = cost_function_type)
        result_list[["error"]] <- retrieve_cost_function_vals(mc_list, cost_function_type)
        result_list[["cost_function"]] <- cost_function_type

        # Calculate fractions (former known as p-value) and determine optimal number of components
        A_opt_list <- optimal_nr_components(result_list$error, validation_threshold = validation_threshold)

        result_list[["A_optimal"]] <- A_opt_list$A_optimal
        result_list[["fractions"]] <- A_opt_list$fractions
        # Name fractions by comparison (1 vs 0, 2 vs 1, etc)
        names(result_list[["fractions"]]) <- paste(seq(length(A_opt_list$fractions)), 'vs', seq(length(A_opt_list$fractions)) -1)

        # Reformat selectivity ratio data, so that it is a matrix object
        result_list[["tested_components"]] <- list()
        for (component in seq(nr_components)) {
            current_component <- paste0(component, "_component_model")
            result_list[["tested_components"]][[current_component]] <- list()

            for (tp_val in tp_vals) {
                result_list[["tested_components"]][[current_component]][[tp_val]] <- retrieve_tp_val(mc_list,
                                                                                                     tp_val_to_retrieve = tp_val,
                                                                                                     component = current_component,
                                                                                                     cost_function = cost_function_type)
            }
        }
    } else {
        # If Monte Carlo resampling is neglected, the optimal nr of components will be set to
        # the chosen nr_of components
        result_list[["A_optimal"]] <- nr_components
    }

    # Perform Target Projection on PLS model with optimal number of components
    # If 0 component is best, skip this step

    if (result_list$A_optimal != 0) {

        if (standardize) {

            X <- sweep(X, 2, X_sd_full_data_set, "/")
            # Get mean of standardized calibration X
            X_mean <- apply(X, 2, mean)
            # Center
            X <- sweep(X, 2, X_mean, "-")

        } else {

            # Only center data
            X_mean <- apply(X, 2, mean)
            X <- sweep(X, 2, X_mean, "-")

        }

        # Build PLSR model
        # Suppress error:Error in solve.default(t(P) %*% W) : system is computationally singular: reciprocal condition number
        tryCatch(
            expr = {
                pls_model <- chemometrics::pls1_nipals(X = X,
                                                       y = y,
                                                       a = result_list$A_optimal)
            },

            error = function(e) {
                result_list <- NULL
                return(result_list)
            }
        )

        if (is.null(pls_model)) {

            stop("PLS-R model could not be built due to poor X or y data.
                  Possible source of error: Centered X variable values are all 0.")
        }

        tp_list <- target_projection(X = X,
                                     y = y,
                                     X_sd = X_sd_full_data_set,
                                     y_sd = y_sd_full_data_set,
                                     b_LVR = pls_model$b,
                                     sr_only = FALSE)

        result_list[["target_projection_A_opt"]] <- tp_list

        # add explained variance in X and y to results
        result_list[["explained_variance_in_X"]] <- explained_X_var(X, pls_model)
        result_list[["explained_variance_in_y"]] <- explained_y_var_each_component(X, y, result_list$A_optimal)
        result_list[["explained_variance_tTP"]] <- tp_list$explained_variance_tTP

    } else {
        result_list[["target_projection_A_opt"]] <- NULL
    }

    return(result_list)
}


retrieve_tp_val <- function(mc_list,
                            tp_val_to_retrieve,
                            component,
                            cost_function) {

    tp_list <- c()
    nr_repetitions <- length(mc_list[["0_component_model"]][[cost_function]])
    for (i in seq(nr_repetitions)){
        tp_list <- c(tp_list, mc_list[[component]][["tp_list"]][[i]][tp_val_to_retrieve])
    }

    tp_mat <- t(do.call(cbind, tp_list))
    rownames(tp_mat) <- NULL

    return(tp_mat)
}


retrieve_cost_function_vals <- function(mc_list,
                                        cost_function) {

    # Retrieve the cost function values for each component
    cost_func_list <- list()
    for (component in seq(0, length(mc_list) - 1)){
        comp <- paste0(component, "_component_model")
        cost_func_list <- append(cost_func_list, list(mc_list[[comp]][[cost_function]]))
    }

    # Build a matrix from the values (repetitions x tested components)
    cost_func_mat <- do.call(cbind, cost_func_list)
    rownames(cost_func_mat) <- NULL
    colnames(cost_func_mat) <- paste0(seq(0, length(mc_list) - 1), "_component_model")

    return(cost_func_mat)
}


optimal_nr_components <- function(cost_func_vals = NULL,
                                  validation_threshold = 0.5) {

    median_cost_func_component <- apply(cost_func_vals, 2, stats::median)

    nr_repetitions <- dim(cost_func_vals)[1]

    # Find component with smallest RMSE value
    Amin = which(median_cost_func_component == min(median_cost_func_component)) - 1

    # In case several components have the same lowest median, use the lesser component
    if (length(Amin) > 1) Amin = Amin[1]

    # Determine the fraction of the m a+1 values that are smaller or equal to the median RMSE value of component a
    fractions = c()
    for (component in 2:length(median_cost_func_component)) {
        fraction = sum(median_cost_func_component[component-1] <= cost_func_vals[,component]) / nr_repetitions
        fractions = c(fractions, (fraction))
    }

    # The expression checks the p values from 1 to Amin and looks in the backwards direction for the first occasion of a p value that is smaller than p upper
    Aopt = seq(Amin, 1)[which(rev(fractions[1:Amin]) <= validation_threshold)[1]]
    if (is.na(Aopt)) Aopt = 0

    result <- list("A_optimal" = Aopt, "fractions" = fractions)

    return(result)
}


#' Plot cost function values
#'
#' This function uses the results from \code{perform_mc_pls_tp()} and plots the cost function values (eg. RMSEP) on the y-axis
#' and the tested PLS components on the x-axis.
#'
#' @param result_list Result list obtained from \code{perform_mc_pls_tp()}.
#' @param validation_threshold Threshold fraction upon a component number is selected. Based on Monte Carlo resampling. Too high values lead to overfitting (default 0.5).
#' @param show_confidence_limits Show confidence limits (default FALSE).
#' @param confidence_limits Confidence limits. Default 5\% and 95\% (-> c(.05, .95)).
#' @param rel_font_size Relative font size based on default font size 11 (default 1).
#'
#' @importFrom rlang .data
#'
#' @return Bar plot that shows cost function values (default RMSEP) for each tested component. Error bars indicate 5% and 95% confidence limit.
#' @export
#'
#' @examples
#' # # result_list obtained from perform_mc_pls_tp()
#' # cost_function_plot(result_list, validation_threshold = 0.5)
plot_cost_function <- function(result_list = NULL,
                               validation_threshold = 0.5,
                               show_confidence_limits = FALSE,
                               confidence_limits = c(.05, .95),
                               rel_font_size = 1) {

    if (is.null(result_list)) stop("Please provide result_list")

    cost_func_vals <- result_list$error
    med_cl <- median_cl(cost_func_vals, confidence_limits = confidence_limits)
    A_opt_list <- optimal_nr_components(cost_func_vals, validation_threshold = validation_threshold)

    optimum_vec <- rep("Not optimal", nrow(med_cl))
    optimum_vec[A_opt_list[[1]] + 1] <- "Optimal"
    med_cl["optimum"] <- optimum_vec

    # Do not make plotly change the order of plotted features
    med_cl$value_to_plot <- factor(med_cl$value_to_plot, levels=med_cl$value_to_plot)

    fig <- plotly::plot_ly(data = med_cl, x = ~as.numeric(.data$value_to_plot), y = ~.data$median, type = "bar",
                           color = ~.data$optimum, colors = c("#0096FF", "#40E0D0"),
                           hoverinfo = "text",
                           hovertext = ~paste0("<b>", toupper(result_list$cost_function), ": </b>", round(.data$median, 2))
    )

    if (show_confidence_limits) {
    fig <- fig %>%
           plotly::add_markers(
               x = ~as.numeric(.data$value_to_plot),
               y = ~.data$median,
               error_y = ~list(symmetric = FALSE,
                               arrayminus = .data$median - .data$cl_lower,
                               array = .data$cl_upper - .data$median),
               showlegend = FALSE
           )
    }

    # plotly options
    text_options <- list(
        # family = "Courier New",
        size = 11 * rel_font_size
    )

    x_axis_options <- list(
        title = "Number of components in model",
        ticktext = seq(length(med_cl$median)) - 1,
        tickvals = seq(length(med_cl$median))
    )

    y_axis_options <- list(
        title = list(text = toupper(result_list$cost_function),
                     standoff = 20L)
    )

    legend_options <- list(
        title = "",
        orientation = "h",
        xanchor = "left",
        y = 1.05
    )

    fig <- fig %>%
           plotly::layout(xaxis = x_axis_options,
                          yaxis = y_axis_options,
                          font = text_options,
                          legend = legend_options)

    fig
}


#' Plot cost function values distribution
#'
#' Plots the median of the cost function values (eg. RMSEP) of the lower component n
#' next to the respective cost function values of the next higher component n+1. The ratio of values falling
#' below the median versus the total count of performed repetitions result in the validation threshold.
#' This plot complements the \code{plot_cost_function()} function.
#'
#' @param result_list Result list obtained from \code{perform_mc_pls_tp()}.
#' @param show_title Do not show title
#' @param rel_font_size Relative font size based on default font size 11 (default 1).
#'
#' @return Points plot with cost function values and segments indicating the median cost function value of the lesser component.
#' @export
#'
#' @examples
#' # # result_list obtained from perform_mc_pls_tp()
#' # plot_cost_function_vals_distribution(result_list)
plot_cost_function_values_distribution <- function(result_list = NULL,
                                                   show_title = TRUE,
                                                   rel_font_size = 1) {

    if (is.null(result_list)) stop("Please provide result_list")

    # get all cost_function_vals values from component 0 to n tested
    cost_function_vals <- as.data.frame(result_list$error)
    rep_nr <- nrow(cost_function_vals)
    p_vals <- result_list$fractions
    cost_function <- toupper(result_list$cost_function)

    # get medians from 0 to n-1 components, last is not necessary
    medians <- utils::head(apply(cost_function_vals, 2, stats::median), -1)
    count_comparisons <- length(medians)

    # initiate a line shape object
    line <- list(
        type = "line",
        line = list(color = "black"),
        xref = "x",
        yref = "y"
    )

    lines <- list()
    for (i in seq(count_comparisons)) {
        line[["x0"]] <- i - 0.25
        line[["x1"]] <- i + 0.25
        line[c("y0", "y1")] <- medians[i]
        lines <- c(lines, list(line))
    }

    # Prepare dataframe for cost function value long table
    points <- cost_function_vals[-1] %>%
              tidyr::gather(., value = "cost_function_vals", key = "components")

    points["median"] <- rep(medians, each = rep_nr)
    points$components <- factor(points$components, levels = unique(points$components))

    points <- points %>%
              dplyr::mutate(comparison_to_median = ifelse(.data$cost_function_vals < .data$median,
                                                          "< median", "> median"))

    # Annotations for plot: global minimum cost function median, optimal number of components
    minimum <- points[which.min(points$median), ]

    fig <- plotly::plot_ly(data = points,
                           x = ~jitter(as.numeric(.data$components)),
                           y = ~.data$cost_function_vals,
                           color = ~.data$comparison_to_median,
                           colors = c("#3CCC0C", "#CC190C"),
                           mode = "markers",
                           type = "scatter",
                           hoverinfo = "text",
                           text = ~paste(cost_function, ":", round(.data$cost_function_vals, 2))
    )


    # plotly options
    text_options <- list(
        # family = "Courier New",
        size = 11 * rel_font_size
    )

    x_axis_options <- list(
        title = list(text = "Component comparisons (fraction)",
                     standoff = 20L),
        ticktext = sprintf("%s vs %s\n(%s)",
                           seq(count_comparisons),
                           seq(0, count_comparisons - 1),
                           round(p_vals, 2)),
        tickvals = seq(count_comparisons)
    )

    y_axis_options <- list(
        title = list(text = cost_function,
                     standoff = 20L)
    )

    legend_options <- list(
        title = list(text = paste("<b>", cost_function, "Values\n N =", nrow(cost_function_vals), "\n Optimal nr:", result_list$A_optimal, "</b>"))
    )

    fig <- fig %>%
           plotly::layout(title = paste("Median", cost_function, "of lesser component shown as bars. Dots show", cost_function, "values of higher component.",
                                        "\nValues in () indicate fractions."),
                          xaxis = x_axis_options,
                          yaxis = y_axis_options,
                          font = text_options,
                          shapes = lines,
                          legend = legend_options)

    if (!show_title) fig <- fig %>% plotly::layout(title = '')

    fig

}


#' Plot sampled target projection values
#'
#' Plot Target Projection variables, such as scores, loadings, weights, selectivity ratio, selectivity fraction,
#' multivariate correlation coefficients (MCorrC) and unstandardized multivariate correlation coefficients (MCovC).
#' In addition, confidence limits are provided which are derived from the Monte Carlo resampling.
#'
#' @param result_list Result list obtained from \code{perform_mc_pls_tp()}.
#' @param tp_value_to_plot Target projection value: "wTP", "tTP", "pTP", "selectivity_ratio", "selectivity_fraction", "MCorrC" (multivariate correlation coefficient) and "MCovC" (multivariate covariance coefficient). See examples for details.
#' @param component Component number used for PLS regression and subsequent Target Projection.
#' @param confidence_limits Confidence limits. Default 2.5\% and 97.5\% (-> c(.025, .975)).
#' @param y_filter Minimum and Maximum filter for values. Either single value or vector c(lower, higher).
#' @param x_filter Filter for variables / features.
#' @param rel_font_size Relative font size based on default font size 11 (default 1).
#' @param rotate Rotate plot by 90 degrees to read axis labels more easily.
#'
#' @importFrom rlang .data
#' @return Bar plot with Target projection value for each feature. Error bars indicate confidence limits.
#' @export
#'
#' @examples
#' # # result_list obtained from perform_mc_pls_tp()
#' # plot_tp_value_mc(result_list, tp_value_to_plot = "selectivity_fraction", component = 3)
plot_tp_value_mc <- function(result_list = NULL,
                             tp_value_to_plot = "selectivity_ratio",
                             component = NULL,
                             confidence_limits = c(.025, .975),
                             x_filter = NULL,
                             y_filter = NULL,
                             rel_font_size = 1,
                             rotate = FALSE) {

    if (is.null(result_list)) stop("Please provide result_list")


    if (is.null(component)) component <- result_list$A_optimal


    if (component == 0) stop("No target projection was performed, since 0 component model showed best performance.
                             No target projection values can be plotted.")


    if (is.numeric(component)) component <- paste0(component, "_component_model")

    tp_vals <- result_list[["tested_components"]][[component]][[tp_value_to_plot]]

    name <- switch(tp_value_to_plot,
                   "wTP" = "wTP - Target projection weights",
                   "pTP" = "pTP - Target projection loadings",
                   "tTP" = stop("Target projection scores are not supported by this function."),
                   "selectivity_ratio" = "Selectivity ratio",
                   "selectivity_fraction" = "Selectivity fraction",
                   "MCorrC" = "Multivariate correlation coefficient",
                   "MCovC" = "Multivariate covariance coefficient"
    )

    med_cl <- median_cl(tp_vals, confidence_limits = confidence_limits)

    # Do not make plotly change the order of plotted features
    med_cl$value_to_plot <- factor(med_cl$value_to_plot, levels = med_cl$value_to_plot)

    # Optional  variable / feature filtering (string or vector of strings to keep)
    if (!is.null(x_filter)){
        med_cl <- dplyr::filter(med_cl, .data$value_to_plot %in% x_filter)
        med_cl <- droplevels(med_cl)
    }

    # y filter is tuple, format c(start, end) - Filter filters absolute values, so a filter between 0 and 1 would include also values between 0 and -1
    # This is intended, since absolute strength is more important than sign.
    if (!is.null(y_filter)){
        if (length(y_filter) == 1) {
            med_cl <- dplyr::filter(med_cl, abs(.data$median) >= y_filter) %>%
                      droplevels()
        } else {
            med_cl <- dplyr::filter(med_cl, (abs(.data$median) >= y_filter[1] & abs(.data$median) <= y_filter[2]))%>%
                      droplevels()
        }
    }

    # Plotting
    if (rotate) {
        # Reverse factor order, so that left to right reads like top to bottom when rotated.
        med_cl$value_to_plot <- forcats::fct_rev(med_cl$value_to_plot)
        fig <- plotly::plot_ly(data = med_cl, x = ~.data$median, y = ~.data$value_to_plot, type = "bar",
                               color = I("#41b6c4"),
                               hoverinfo = "text",
                               hovertext = ~paste0("<b>", name, ": </b>",
                                                   round(.data$median, 2))
                               )
        fig <- fig %>%
            plotly::add_markers(
                x = ~.data$median,
                y = ~.data$value_to_plot,
                error_x = ~list(symmetric = FALSE,
                                arrayminus = .data$median - .data$cl_lower,
                                array = .data$cl_upper - .data$median)
            )
    } else {
        fig <- plotly::plot_ly(data = med_cl, x = ~.data$value_to_plot, y = ~.data$median, type = "bar",
                               color = I("#41b6c4"),
                               hoverinfo = "text",
                               hovertext = ~paste0("<b>", name, ": </b>",
                                                   round(.data$median, 2)),
                               error_x = ~list(symmetric = FALSE,
                                               arrayminus = .data$median - .data$cl_lower,
                                               array = .data$cl_upper - .data$median)
                               )
        fig <- fig %>%
            plotly::add_markers(
                x = ~.data$value_to_plot,
                y = ~.data$median,
                error_y = ~list(symmetric = FALSE,
                                arrayminus = .data$median - .data$cl_lower,
                                array = .data$cl_upper - .data$median)
            )

    }

    # plotly options
    text_options <- list(
        # family = "Courier New",
        size = 11 * rel_font_size
    )

    x_axis_options <- list(
        title = list(text = ifelse(rotate, name, "Variable"),
                     standoff = 20L),
        tickangle = ifelse(rotate, 0, 270)
    )

    y_axis_options <- list(
        title = list(text = ifelse(!rotate, name, "Variable"),
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

#' Plot target projection values (full dataset)
#'
#' Plot Target Projection variables, such as scores, loadings, weights, selectivity ratio, selectivity fraction,
#' multivariate correlation coefficients (MCorrC) and unstandardized multivariate correlation coefficients (MCovC)
#'
#' @param result_list Result list obtained from \code{perform_mc_pls_tp()}.
#' @param tp_value_to_plot Target projection value: "wTP", "tTP", "pTP", "selectivity_ratio", "selectivity_fraction", "MCorrC" (multivariate correlation coefficient) and "MCovC" (multivariate covariance coefficient). See examples for details.
#' @param y_filter Minimum and Maximum filter for values. Either single value or vector c(lower, higher).
#' @param x_filter Filter for variables / features.
#' @param rel_font_size Relative font size based on default font size 11 (default 1).
#' @param rotate Rotate plot by 90 degrees to read axis labels more easily.
#'
#' @importFrom rlang .data
#' @return Bar plot with desired Target Projection variable.
#' @export
#'
#' @examples
#' # # Obtain optimal PLS-R model and perform Target Projection on full dataset,
#' # # using the optimal number of components.
#' # result_list <- perform_mc_pls_tp(data = HOMA_IR, response = "HOMA_IR", nr_components = 8,
#' #                                 cal_ratio = 0.5, repetitions = 100, standardize = TRUE)
#'
#' # plot_tp_value(result_list)
plot_tp_value <- function(result_list = NULL,
                          tp_value_to_plot = "selectivity_ratio",
                          y_filter = NULL,
                          x_filter = NULL,
                          rel_font_size = 1,
                          rotate = FALSE) {

    if (is.null(result_list)) stop("Please provide result_list")

    if (result_list$A_optimal == 0) stop("No target projection was performed, since 0 component model showed best performance.
                                         No target projection values can be plotted.")

    # Build dataframe
    values <- as.data.frame(result_list[["target_projection_A_opt"]][[tp_value_to_plot]])

    name <- switch(tp_value_to_plot,
                   "wTP" = "wTP - Target projection weights",
                   "pTP" = "pTP - Target projection loadings",
                   "tTP" = "tPT - Target projection scores",
                   "selectivity_ratio" = "Selectivity ratio",
                   "selectivity_fraction" = "Selectivity fraction",
                   "MCorrC" = "Multivariate correlation coefficient",
                   "MCovC" = "Multivariate covariance coefficient"
    )

    names(values) <- name
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
    obj_feat_label <- ifelse(tp_value_to_plot == "tTP", "Object", "Variable")

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


#' Plot variable variance distribution based
#'
#' This stacked bar plot depicts explained and residual variance for each variable.
#' Explained and residual variance are obtained through target projection.
#'
#' @param result_list Result list obtained from perform_mc_pls_tp().
#' @param x_filter Filter for variables / features.
#' @param rel_font_size Relative font size based on default font size 11 (default 1).
#' @param rotate Rotate plot by 90 degrees to read axis labels more easily.
#'
#' @return Stacked bar plot with explained and residual variance per feature based on target projection.
#' @export
#'
#' @examples
#' # # Obtain optimal PLS-R model and perform Target Projection on full dataset,
#' # # using the optimal number of components.
#' # result_list <- perform_mc_pls_tp(data = HOMA_IR, response = "HOMA_IR", nr_components = 8,
#' #                                 cal_ratio = 0.5, repetitions = 100, standardize = TRUE)
#'
#' # plot_variable_variance_distribution(result_list)
plot_variable_variance_distribution <- function(result_list = NULL,
                                                x_filter = NULL,
                                                rel_font_size = 1,
                                                rotate = FALSE) {

    if (is.null(result_list)) stop("Please provide result_list")

    if (result_list$A_optimal == 0) stop("No target projection was performed, since 0 component model showed best performance.
                                          No target variable variance distribution can be plotted.")

    values <- data.frame(result_list$target_projection_A_opt$explained_variance,
                         result_list$target_projection_A_opt$residual_variance)

    values <- tibble::rownames_to_column(values, var = "feature")

    # Maintain variable order
    values$feature <- factor(values$feature, levels=values$feature)

    values <- tidyr::pivot_longer(values, -.data$feature, names_to = "variance_type", values_to = "variance")

    values <- values %>%
            # It is important to keep the 4 spaces, since plotly in shiny tends to cut off the text
            dplyr::mutate(variance_type_pretty = ifelse(.data$variance_type == "variance_explained", "Explained variance    ", "Residual variance    "))


    # Optional  variable / feature filtering (string or vector of strings to keep)
    if (!is.null(x_filter)){
        values <- dplyr::filter(values, .data$feature %in% x_filter)
        values <- droplevels(values)
    }

    if (rotate) {
        fig <- plotly::plot_ly(data = values, x = ~.data$variance, y = ~.data$feature, type = "bar",
                               color = ~.data$variance_type_pretty, colors = c("#FB9B50", "#5B84C4"),
                               hoverinfo = "text",
                               hovertext = ~paste0("<b>", .data$variance_type_pretty, "</b>",
                                                   ": ", round(.data$variance, 4))
        )
    } else {
        fig <- plotly::plot_ly(data = values, x = ~.data$feature, y = ~.data$variance, type = "bar",
                               color = ~.data$variance_type_pretty, colors = c("#FB9B50", "#5B84C4"),
                               hoverinfo = "text",
                               hovertext = ~paste0("<b>", .data$variance_type_pretty, "</b>",
                                                   ": ", round(.data$variance, 4))
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
        title = list(text = ifelse(rotate, "Variance", "Variable"), standoff = 20L),
        tickangle = ifelse(rotate, 0, 270)
    )

    y_axis_options <- list(
        title = list(text = ifelse(!rotate, "Variance", "Variable"), standoff = 20L)
    )

    fig <- fig %>%
        plotly::layout(xaxis = x_axis_options,
                       yaxis = y_axis_options,
                       legend = legend_options,
                       font = text_options
        )

    fig

}
