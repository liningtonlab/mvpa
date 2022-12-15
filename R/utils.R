# # Base functions

mean_center <- function(X) {
    return(X - rep(colMeans(X), rep.int(nrow(X), ncol(X))))
}


rmsep <- function(y,
                  y_hat) {
    return(sqrt(mean((y - y_hat)^2)))
}


mae <- function(y,
                y_hat) {
    return(mean(abs(y - y_hat)))
}


cost_function <- function(y,
                          y_hat,
                          type = "rmsep") {
    switch(type,
           rmsep = rmsep(y, y_hat),
           mae = mae(y, y_hat))
}


l2_norm <- function(a) {
    return(as.numeric(sqrt(t(a) %*% a)))
}


median_cl <- function(data,
                      confidence_limits = c(0.025, 0.975),
                      pretty_column_names = FALSE) {

    helper <- function(data,
                       confidence_limits = confidence_limits){
        med <- stats::median(data)

        # Get nth percentile values (nearest data value),
        # ordered by lower and then upper value
        cl <- as.numeric(stats::quantile(data, sort(confidence_limits), type = 3))

        return(c(med, cl))
    }

    df <- as.data.frame(t(apply(data, 2 , helper, confidence_limits = confidence_limits)))
    colnames(df) <- c('median', 'cl_lower', 'cl_upper')
    df <- tibble::rownames_to_column(df, var = "value_to_plot")

    if (pretty_column_names) {
        colnames(df) <- c('Variable', 'Median', 'CL_lower', 'CL_upper')
    }

    return(df)
}


split_dataset <- function(X = NULL,
                          y = NULL,
                          cal_ratio) {

    # Creation of the random split data set, dependent on the cal ratio.
    rows_cal = sort(sample(nrow(X),size = abs(nrow(X) * cal_ratio), replace = FALSE))

    rows_val = setdiff(1:nrow(X), rows_cal)

    split_data <- list('X_cal' = X[rows_cal, ,drop = FALSE],
                       'X_val' = X[rows_val, ,drop = FALSE])

    if (!is.null(y)){
        split_data[['y_cal']] <- y[rows_cal, ,drop = FALSE]
        split_data[['y_val']] <- y[rows_val, ,drop = FALSE]
    }

    return(split_data)
}
