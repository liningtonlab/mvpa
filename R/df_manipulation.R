# # Dataframe manipulation

#' Check if dataset is compatible with mvpa's tools
#'
#' Test dataframe (objects x variables) for missing, zero, invariant negative and categorical string values (eg. male / female).
#'
#' @param data Dataframe.
#' @param verbose Shall informative messages about dataset integrity be printed (default FALSE).
#' @importFrom rlang .data
#' @return List highlighting the problematic valuesthat  have been found and informs about dataset structure. Tells if dataset is ready for downstream analysis.
#' @export
#'
#' @examples
#' data <- data.frame(sex = c("m", "w", "w", "m", "m"),
#'  age = c(24, 12, 14, 12, 0),
#'  marital_status = c("m", "s", "s", "d", "d"),
#'  status = c("sick", "healthy", "healthy", "sick", "healthy"),
#'  na_vals = c(1, 3, NA, NA, NA),
#'  na_vals2 = c(2, "B", 2 , "C", 3)
#'  )
#'  check_dataset(data)
check_dataset <- function(data = NULL,
                          verbose = FALSE) {

    if (is.null(data)) stop("Please provide data.")

    dimension <- "The dataframe contains %s object(s) and %s variable(s) / features and %s element(s) in total.\n"

    empty <- "Dataset is empty."
    missing <- "Missing values detected - Consider an imputation method to fill gaps, or remove affected variables or objects.\n"
    missing_all_vars <- "Variables detected where all values are missing. Removal of those variables is necessary.\n"
    missing_all_objs <- "Objects detected where all values are missing. Removal of those objects is necessary.\n"
    invariant_vars <- "Invariant variables detected (variance = 0) - Those variables need to be removed. \n"
    invariant_objs <- "Invariant objects detected (variance = 0) - Those objects should to be removed. \n"
    zero <- "Zero values detected - Consider an offset, if log transformation is desired.\n"
    zero_all_vars <- "Variables detected where all values are zero. Removal of those variables is necessary.\n"
    zero_all_objs <- "Objects detected where all values are zero. Removal of those objects is necessary.\n"
    negative <- "Negative values detected - Consider an offset, if log transformation or an even root transformation is desired.\n"
    categorical <- "Categorical string variable(s) detected - Consider the introduction of dummy variables or 1/0 encoding for binary variables.\n"
    pass <- "The dataset seems to be in a good condition and is ready for analysis."

    columns <- "The following variables are (also) affected: %s \n"
    columns_all <- "The following variables are entirely affected: %s \n"
    rows <- "The following objects are (also) affected: %s \n"
    rows_all <- "The following objects are entirely affected: %s \n"

    for_msg <- dim(data)
    if (verbose) {
        cat(sprintf(dimension, for_msg[1], for_msg[2], prod(for_msg)))
    }

    checklist <- list()
    # Empty dataset
    check_empty <- nrow(data) == 0 | ncol(data) == 0

    if (check_empty) {
        checklist["empty"] <- TRUE
        checklist["missing_vals"] <- FALSE
        checklist["missing_vals_all_vars"] <- FALSE
        checklist["missing_vals_all_objs"] <- FALSE
        checklist["invariant_vars"] <- FALSE
        checklist["invariant_objs"] <- FALSE
        checklist["zero_vals"] <- FALSE
        checklist["zero_vals_all_vars"] <- FALSE
        checklist["zero_vals_all_objs"] <- FALSE
        checklist["negative_vals"] <- FALSE
        checklist["categorical_vals"] <- FALSE

        if (verbose){
            cat("\n")
            cat(empty)
        }

        return(invisible(checklist))
    } else {
        checklist["empty"] <- FALSE
    }

    # Missing values
    for_msg_any <- data %>%
                   dplyr::select_if(function(x) any(is.na(x)))
    for_msg_all_vars <- data %>%
                        dplyr::select_if(function(x) all(is.na(x)))
    for_msg_all_objs <- dplyr::filter_all(data, dplyr::all_vars(is.na(.)))

    if (ncol(for_msg_any) > 0 | ncol(for_msg_all_vars) > 0 | nrow(for_msg_all_objs) > 0) {

        checklist["missing_vals"] <- TRUE
        if (ncol(for_msg_all_vars) > 0 & nrow(for_msg_all_objs) > 0) {
            checklist["missing_vals_all_vars"] <- TRUE
            checklist["missing_vals_all_objs"] <- TRUE
        } else if (ncol(for_msg_all_vars) > 0) {
            checklist["missing_vals_all_vars"] <- TRUE
            checklist["missing_vals_all_objs"] <- FALSE
        } else if (nrow(for_msg_all_objs) > 0) {
            checklist["missing_vals_all_vars"] <- FALSE
            checklist["missing_vals_all_objs"] <- TRUE
        } else {
            checklist["missing_vals_all_vars"] <- FALSE
            checklist["missing_vals_all_objs"] <- FALSE
        }

        if (verbose){
            cat("\n")
            cat(missing)

            if (checklist[["missing_vals_all_vars"]] & checklist[["missing_vals_all_objs"]]) {
                cat(missing_all_vars)
                cat(missing_all_objs)
                col_names_all <- colnames(data)[apply(data, 2, FUN = function(x) all(is.na(x)))]
                row_names_all <- rownames(data)[apply(data, 1, FUN = function(x) all(is.na(x)))]

                subset_df <- data[!(rownames(data) %in% row_names_all), !(colnames(data) %in% col_names_all)]

                cat(sprintf(columns_all, paste(col_names_all, collapse = ", ")))
                cat(sprintf(rows_all, paste(row_names_all, collapse = ", ")))

                if (is.data.frame(subset_df)) {
                    col_names_any <- colnames(subset_df)[apply(subset_df, 2, FUN = function(x) any(is.na(x)))]
                    row_names_any <- rownames(subset_df)[apply(subset_df, 1, FUN = function(x) any(is.na(x)))]
                    if (length(col_names_any) != 0) {
                        cat(sprintf(columns, paste(col_names_any, collapse = ", ")))
                        cat(sprintf(rows, paste(row_names_any, collapse = ", ")))
                    }
                }
            } else if (checklist[["missing_vals_all_vars"]]) {
                cat(missing_all_vars)
                col_names_all <- colnames(data)[apply(data, 2, FUN = function(x) all(is.na(x)))]

                subset_df <- data[, !(colnames(data) %in% col_names_all)]

                cat(sprintf(columns_all, paste(col_names_all, collapse = ", ")))

                if (is.data.frame(subset_df)) {
                    col_names_any <- colnames(subset_df)[apply(subset_df, 2, FUN = function(x) any(is.na(x)))]
                    row_names_any <- rownames(subset_df)[apply(subset_df, 1, FUN = function(x) any(is.na(x)))]
                    if (length(row_names_any) != 0) {
                        cat(sprintf(rows, paste(row_names_any, collapse = ", ")))
                    }
                }
            } else if (checklist[["missing_vals_all_objs"]]) {
                cat(missing_all_objs)
                row_names_all <- rownames(data)[apply(data, 1, FUN = function(x) all(is.na(x)))]

                subset_df <- data[!(rownames(data) %in% row_names_all), ]

                cat(sprintf(rows_all, paste(row_names_all, collapse = ", ")))
                cat(sprintf(rows, paste(row_names_any, collapse = ", ")))

                if (is.data.frame(subset_df)) {
                    col_names_any <- colnames(subset_df)[apply(subset_df, 2, FUN = function(x) any(is.na(x)))]
                    row_names_any <- rownames(subset_df)[apply(subset_df, 1, FUN = function(x) any(is.na(x)))]
                    if (length(col_names_any) != 0) {
                        cat(sprintf(columns, paste(col_names_any, collapse = ", ")))
                    }
                }
            } else {
                col_names <- colnames(data)[apply(data, 2, FUN = function(x) any(is.na(x)))]
                row_names <- rownames(data)[apply(data, 1, FUN = function(x) any(is.na(x)))]
                cat(sprintf(columns, paste(col_names, collapse = ", ")))
                cat(sprintf(rows, paste(row_names, collapse = ", ")))
            }
        }
    } else {
        checklist["missing_vals"] <- FALSE
        checklist["missing_vals_all_vars"] <- FALSE
        checklist["missing_vals_all_objs"] <- FALSE
    }

    # Invariant variables and objects
    for_msg_vars <- data %>%
                    dplyr::select_if(is.numeric) %>%
                    dplyr::select_if(function(x) all(stats::var(x, na.rm = TRUE) == 0))
    for_msg_objs <- data %>%
                    dplyr::select_if(is.numeric) %>%
                    { if (ncol(.) > 0) dplyr::rowwise(.) %>%
                                       dplyr::filter(stats::var(dplyr::c_across(dplyr::everything()), na.rm = TRUE) == 0)
                      else . }

    if (is.null(unlist(for_msg_objs))) {
        for_msg_objs <- data.frame()
    }

    if (ncol(for_msg_vars) > 0 | nrow(for_msg_objs) > 0) {

        if (ncol(for_msg_vars) > 0 & nrow(for_msg_objs) > 0) {
            checklist["invariant_vars"] <- TRUE
            checklist["invariant_objs"] <- TRUE
        } else if (ncol(for_msg_vars) > 0) {
            checklist["invariant_vars"] <- TRUE
            checklist["invariant_objs"] <- FALSE
        } else if (nrow(for_msg_objs) > 0) {
            checklist["invariant_vars"] <- FALSE
            checklist["invariant_objs"] <- TRUE
        }

        if (verbose){
            cat("\n")

            if (checklist[["invariant_vars"]] & checklist[["invariant_objs"]]) {
                cat(invariant_vars)
                cat(invariant_objs)
                col_names <- colnames(for_msg_vars)[apply(for_msg_vars, 2, FUN = function(x) all(stats::var(x, na.rm = TRUE) == 0))]
                row_names <- rownames(for_msg_objs)[apply(for_msg_objs, 1, FUN = function(x) all(stats::var(x, na.rm = TRUE) == 0))]

                cat(sprintf(columns_all, paste(col_names, collapse = ", ")))
                cat(sprintf(rows_all, paste(row_names, collapse = ", ")))
            } else if (checklist[["invariant_vars"]]) {
                cat(invariant_vars)
                col_names <- colnames(for_msg_vars)[apply(for_msg_vars, 2, FUN = function(x) all(stats::var(x, na.rm = TRUE) == 0))]

                cat(sprintf(columns_all, paste(col_names, collapse = ", ")))
            } else if (checklist[["invariant_objs"]]){
                cat(invariant_objs)
                row_names <- rownames(for_msg_objs)[apply(for_msg_objs, 1, FUN = function(x) all(stats::var(x, na.rm = TRUE) == 0))]

                cat(sprintf(rows_all, paste(row_names, collapse = ", ")))
            }
        }
    } else {
        checklist["invariant_vars"] <- FALSE
        checklist["invariant_objs"] <- FALSE
    }

    # Zero values
    for_msg_any <- data %>% dplyr::select_if(function(x) is.numeric(x) & any(x == 0, na.rm = TRUE))
    for_msg_all_vars <- data %>% dplyr::select_if(function(x) is.numeric(x) & all(x == 0, na.rm = TRUE))
    for_msg_all_objs <- dplyr::filter_all(data, dplyr::all_vars(is.numeric(.) & . == 0))
    if (ncol(for_msg_any) > 0){

        checklist["zero_vals"] <- TRUE
        if (ncol(for_msg_all_vars) > 0 & nrow(for_msg_all_objs) > 0) {
            checklist["zero_vals_all_vars"] <- TRUE
            checklist["zero_vals_all_objs"] <- TRUE
        } else if (ncol(for_msg_all_vars) > 0) {
            checklist["zero_vals_all_vars"] <- TRUE
            checklist["zero_vals_all_objs"] <- FALSE
        } else if (nrow(for_msg_all_objs) > 0) {
            checklist["zero_vals_all_vars"] <- FALSE
            checklist["zero_vals_all_objs"] <- TRUE
        } else {
            checklist["zero_vals_all_vars"] <- FALSE
            checklist["zero_vals_all_objs"] <- FALSE
        }

        if (verbose){
            cat("\n")
            cat(zero)

            if (checklist[["zero_vals_all_vars"]] & checklist[["zero_vals_all_objs"]]) {
                cat(zero_all_vars)
                cat(zero_all_objs)
                col_names_all <- colnames(for_msg_all_vars)[apply(for_msg_all_vars, 2, FUN = function(x) all(x == 0, na.rm = TRUE))]
                row_names_all <- rownames(for_msg_all_objs)[apply(for_msg_all_objs, 1, FUN = function(x) all(x == 0, na.rm = TRUE))]

                subset_df <- data[!(rownames(data) %in% row_names_all), !(colnames(data) %in% col_names_all)] %>% is.numeric

                cat(sprintf(columns_all, paste(col_names_all, collapse = ", ")))
                cat(sprintf(rows_all, paste(row_names_all, collapse = ", ")))

                if (is.data.frame(subset_df)) {
                    col_names_any <- colnames(subset_df)[apply(subset_df, 2, FUN = function(x) any(x == 0, na.rm = TRUE))]
                    row_names_any <- rownames(subset_df)[apply(subset_df, 1, FUN = function(x) any(x == 0, na.rm = TRUE))]
                    if (length(col_names_any) != 0) {
                        cat(sprintf(columns, paste(col_names_any, collapse = ", ")))
                        cat(sprintf(rows, paste(row_names_any, collapse = ", ")))
                    }
                }
            } else if (checklist[["zero_vals_all_vars"]]) {
                cat(zero_all_vars)
                col_names_all <- colnames(for_msg_all_vars)

                subset_df <- data %>% dplyr::select(!dplyr::all_of(col_names_all)) %>% dplyr::select_if(is.numeric)

                cat(sprintf(columns_all, paste(col_names_all, collapse = ", ")))

                if (is.data.frame(subset_df)) {
                    col_names_any <- colnames(subset_df)[apply(subset_df, 2, FUN = function(x) any(x == 0, na.rm = TRUE))]
                    row_names_any <- rownames(subset_df)[apply(subset_df, 1, FUN = function(x) any(x == 0, na.rm = TRUE))]
                    if (length(row_names_any) != 0) {
                        cat(sprintf(columns, paste(col_names_any, collapse = ", ")))
                        cat(sprintf(rows, paste(row_names_any, collapse = ", ")))
                    }
                }
            } else if (checklist[["zero_vals_all_objs"]]){
                cat(zero_all_objs)
                row_names_all <- rownames(for_msg_all_objs)[apply(for_msg_all_objs, 1, FUN = function(x) all(x == 0, na.rm = TRUE))]

                subset_df <- data[!(rownames(data) %in% row_names_all), ] %>% is.numeric

                cat(sprintf(rows_all, paste(row_names_all, collapse = ", ")))

                if (is.data.frame(subset_df)) {
                    col_names_any <- colnames(subset_df)[apply(subset_df, 2, FUN = function(x) any(x == 0, na.rm = TRUE))]
                    row_names_any <- rownames(subset_df)[apply(subset_df, 1, FUN = function(x) any(x == 0, na.rm = TRUE))]
                    if (length(col_names_any) != 0) {
                        cat(sprintf(columns, paste(col_names_any, collapse = ", ")))
                        cat(sprintf(rows, paste(row_names_any, collapse = ", ")))
                    }
                }
            } else {
                col_names <- colnames(for_msg_any)[apply(for_msg_any, 2, FUN = function(x) any(x == 0, na.rm = TRUE))]
                row_names <- rownames(for_msg_any)[apply(for_msg_any, 1, FUN = function(x) any(x == 0, na.rm = TRUE))]
                cat(sprintf(columns, paste(col_names, collapse = ", ")))
                cat(sprintf(rows, paste(row_names, collapse = ", ")))
            }
        }
    } else {
        checklist["zero_vals"] <- FALSE
        checklist["zero_vars_all_vars"] <- FALSE
        checklist["zero_vals_all_objs"] <- FALSE
    }

    # Negative values
    for_msg <- data %>% dplyr::select_if(is.numeric) %>% dplyr::select_if(function(x) any(x < 0, na.rm = TRUE))
    if (ncol(for_msg) > 0){
        checklist["negative_vals"] <- TRUE

        if (verbose){
            cat("\n")
            cat(negative)

            row_names <- rownames(for_msg)[apply(for_msg, 1, FUN = function(x) any(x < 0, na.rm = TRUE))]

            cat(sprintf(columns, paste(colnames(for_msg), collapse = ", ")))
            cat(sprintf(rows, paste(row_names, collapse = ", ")))
        }
    } else checklist["negative_vals"] <- FALSE

    # Categorical values
    for_msg <- data %>% dplyr::select_if(function(x) is.character(x) | is.factor(x))
    if (ncol(for_msg) > 0){
        checklist["categorical_vals"] <- TRUE

        if (verbose){
            cat("\n")
            cat(categorical)

            cat(sprintf(columns, paste(colnames(for_msg), collapse = ", ")))
        }
    } else checklist["categorical_vals"] <- FALSE

    if (!any(sapply(checklist, function(x) x)) & verbose) cat(pass)

    return(invisible(checklist))
}


#' Keep selected variables
#'
#' Selected variables are kept while the others are removed.
#'
#' @param data Dataframe.
#' @param vars_to_keep Variable name(s) that shall be kept in the dataframe. The rest will be removed.
#'
#' @return Dataframe with kept variables only.
#' @export
#'
#' @examples
#' # df <- keep_variables(df, vars_to_keep = c("Age", "Skinfold"))
#' # df <- keep_variables(df, "Gender")
keep_variables <- function(data = NULL,
                           vars_to_keep = NULL) {

    if (is.null(data)) stop("Please provide data.")

    if (is.null(vars_to_keep)) {
        return(data)
    }

    return(data %>% dplyr::select(dplyr::any_of(vars_to_keep)))
}


#' Keep selected objects
#'
#' Selected objectss are kept while the others are removed.
#'
#' @param data Dataframe.
#' @param objs_to_keep Object name(s) that shall be kept in the dataframe. The rest will be removed.
#'
#' @return Dataframe with kept objects only.
#' @export
#'
#' @examples
#' # df <- add_unique_ids(df)
#' # df <- keep_objects(df, c("0011", "0231", "2357"))
#' # df <- keep_object(df, "0042")
keep_objects <- function(data = NULL,
                         objs_to_keep = NULL) {

    if (is.null(data)) stop("Please provide data.")

    if (is.null(objs_to_keep)) {
        return(data)
    }

    indices <- rownames(data)
    data <- dplyr::filter(data, indices %in% objs_to_keep)

    return(data)
}


#' Remove variables / features from dataframe
#'
#' @param data Dataframe.
#' @param vars_to_remove Variable name(s) that shall be removed from the dataframe.
#' @param remove_na Remove all objects that contain NAs (default FALSE).
#' @param remove_invariants Remove variables that are invariant (variance = 0).
#'
#' @return Dataframe with removed variables.
#' @export
#'
#' @examples
#' # df <- remove_variables(df, vars_to_remove = c("Age", "Skinfold"))
#' # df <- remove_variables(df, "Gender")
remove_variables <- function(data = NULL,
                             vars_to_remove = NULL,
                             remove_na = FALSE,
                             remove_invariants = FALSE) {

    if (is.null(data)) stop("Please provide data.")

    if (is.null(vars_to_remove)) {
        vars_to_remove <- c()
    }

    if (remove_na){
        var_names <- data %>%
                     dplyr::select_if(function(x) any(is.na(x))) %>%
                     colnames()

        vars_to_remove <- unique(c(vars_to_remove, var_names))
    }

    if (remove_invariants) {
        var_names <- data %>%
                     dplyr::select_if(is.numeric) %>%
                     dplyr::select_if(function(x) all(stats::var(x) == 0)) %>%
                     colnames()

        vars_to_remove <- unique(c(vars_to_remove, var_names))
    }

    return(data %>% dplyr::select(-dplyr::any_of(vars_to_remove)))
}


#' Remove objects / samples from dataframe.
#' Requires column with character-based identifier.
#'
#' @param data Dataframe
#' @param objs_to_remove Objects that shall be removed from the dataframe. Is ignored, if remove_na is TRUE.
#' @param remove_na Remove all objects that contain NAs (default FALSE).
#'
#' @return Dataframe with removed objects.
#' @export
#'
#' @examples
#' # # Add unique id to dataframe that is based on row indices
#' # df <- add_unique_id(df)
#' # df <- remove_objects(df, c("0011", "0231", "2357"))
#' # df <- remove_objects(df, "0042")
remove_objects <- function(data = NULL,
                           objs_to_remove = NULL,
                           remove_na = FALSE) {

    if (is.null(data)) stop("Please provide data.")

    if (remove_na){
        data <- stats::na.omit(data)
    } else {
        indices <- rownames(data)
        data <- dplyr::filter(data, !(indices %in% objs_to_remove))
    }
    return(data)
}


#' Join two dataframes based on rownames
#'
#' This function facilitates joining variables obtained downstream (principal component or PLS scores, etc.)
#' with the original dataframe. Ideal if original variables shall be substituted with latent variables for
#' the purpose of dimension reduction.
#'
#' @param data Dataframe.
#' @param data_to_add Dataframe with one or more columns that shall be added.
#'
#' @return Enhanced dataframe.
#' @export
#'
#' @examples
#'
#' # enhanced_dataset <- enhance_dataset(data = old_dataset, data_to_add = new_variables)
enhance_dataset <- function(data = NULL,
                            data_to_add = NULL) {

    if (is.null(data)) stop("Please provide data.")

    if (is.null(data_to_add)) warning("No further data provided.")

    # Test if the objects of both datasets match
    if (length(rownames(data)) != length(rownames(data_to_add))) {
        stop("The number of objects between the PCs and the current dataset differs. Please revise current dataset!")
    }

    if (!all(rownames(data_to_add) %in% rownames(data))) {
        stop("The object names between the PCs and the current data do not match. Please revise current dataset!")
    }

    enhanced_dataset <- tibble::add_column(data, data_to_add, .after = 1)

    return(enhanced_dataset)
}


#' Construct unique IDs for rownames.
#'
#' Substitutes current row indices with unique IDs based on row index value and leading zeros.
#'
#' @param data Dataframe.
#' @return Dataframe with new row names or a new id column.
#' @export
#'
#' @examples
#' # df <- add_unique_id(data = df)
add_unique_ids <- function(data = NULL) {

    if (is.null(data)) stop("Please provide data.")

    ids <- stringr::str_pad(1:nrow(data), nchar(nrow(data)), pad = "0")
    rownames(data) <- ids

    return(data)
}


#' Impute missing values
#'
#' Missing values are calculated based on the selected method ('mean' or 'median') or are selected manually
#' by choosing a custom value.
#'
#' @param data Dataframe with missing values (NAs).
#' @param method Imputation method: "mean" (default), "median", "custom". Mean and median values will be calculated by variable.
#' @param custom_value Value to substitute NAs in dataframe with.
#'
#' @return Dataframe with missing value substituted by defined method.
#' @export
#'
#' @examples
#'
#' data <- data.frame(owns = c(-24, 125, 250, -150, 134),
#'                    na_vals = c(3, 5, 5, 3, NA),
#'                    na_vals2 = c(3, 3, 5 , NA, NA))
#' mean_imp_data <- impute_missing_values(data) # default method mean
#'
#' # Dataframe-wide imputation with small value
#' custom_imp_data <- impute_missing_values(data, method = "custom", custom_value = 1e-6)
impute_missing_values <- function(data = NULL,
                                  method = "mean",
                                  custom_value = NULL) {

    if (is.null(data)) stop("Please provide data.")

    if (method == "custom" & is.null(custom_value)) {
        stop("Please choose a custom_value for missing value imputation.")
    }

    if (any(apply(is.na(data), 2, all)) & method != "custom") {
        stop("Please choose 'custom' method since variables exist where all values are missing.")
    }

    impute_by_column <- function(data, imputation_method = mean){
        for (col in seq(ncol(data))){
            if (any(is.na(data[col]))){
                value_for_imputation <- imputation_method(data[[col]], na.rm = TRUE)
                data[col][is.na(data[col]), ] <- value_for_imputation
            }
        }
        return(data)
    }

    data <- switch(method,
                   custom = {data %>% dplyr::mutate_all(~replace(., is.na(.), custom_value))},
                   mean = impute_by_column(data, mean),
                   median = impute_by_column(data, stats::median)
    )

    return(data)
}


#' Introduce dummy variables
#'
#' Introduces binary dummy variables for columns that contain characters / factors.
#' In case a variable only contains two levels, only one column is retained to avoid redundancy.
#' The respective columns containing characters / factors are removed.
#'
#' @param data Dataframe.
#' @param col_names (Optional) If only specific columns shall be converted to dummy columns.
#' @param add_indicator Add level name to variable name, separated by an underscore,  indicating 1's.
#' Only affects binary variables (default FALSE). Variables with more than two levels will get an indicator regardless.
#'
#' @return Dataframe with new dummy columns. Former character / factor-containing columns are removed.
#' @export
#'
#' @examples
#' # df <- introduce_dummies(df)
#' # df <- introduce_dummies(df, col_names = c("Gender", "marital_status"))
introduce_dummies <- function(data = NULL,
                              col_names = NULL,
                              add_indicator = FALSE) {

    if (is.null(data)) stop("Please provide data.")

    binarize <- function(data, binary_cols, add_indicator) {
        representative_var <- data[1, binary_cols]

        binarized_columns <- apply(data[binary_cols], 1, FUN = function(x) x %in% representative_var * 1)

        if (length(binary_cols) > 1) binarized_columns <- t(binarized_columns)
        data[binary_cols] <- binarized_columns

        if (add_indicator){
            names(data)[names(data) %in% binary_cols] <- paste(binary_cols, representative_var , sep = "_")
        }

        return(data)
    }

    # Find columns with string variables
    if (is.null(col_names)) {
        col_names <- colnames(data)
    }
    # All column names, containing strings
    col_names <- data[col_names] %>%
                 dplyr::select_if(function(x) is.factor(x) | is.character(x)) %>%
                 colnames()
    # Columns containing binary string variables
    binary_cols <- data[col_names] %>%
                   dplyr::select_if(function(x) length(unique(x)) == 2) %>%
                   colnames()
    # Columns containing more than two different string variables
    non_binary_cat_cols <- setdiff(col_names, binary_cols)

    # Convert binary string variables
    if (!identical(binary_cols, character(0))) {
        data <- binarize(data, binary_cols, add_indicator)
    }

    # Convert non-binary string variables
    if (!identical(non_binary_cat_cols, character(0))) {
        data <- fastDummies::dummy_columns(data,
                                           ignore_na = TRUE,
                                           select_columns = non_binary_cat_cols,
                                           remove_selected_columns = TRUE)
    }

    return(data)
}


#' Introduce offset value
#'
#' Set offset to avoid negative and zero variable values. Useful when log or square root methods shall be applied.
#'
#' @param data Dataframe.
#' @param col_names Names of specific columns to be changed.
#' @param new_min New (positive) minimum value (default = 1)
#'
#' @return Dataframe ready for scaling / transformation (eg. mvpa::scale_data).
#' @export
#'
#' @examples
#' data <- data.frame(status = c("sick", "healthy", "healthy", "sick", "healthy"),
#'                    height = c(124, 142, 152, 163, 146),
#'                    owns = c(-24, 125, 250, -150, 134))
#'
#' introduce_offset(data, new_min = 1)
introduce_offset <- function(data = NULL,
                             new_min = 1,
                             col_names = NULL) {

    if (is.null(data)) stop("Please provide data.")

    if (new_min <= 0) stop("Please provide minimum greater than 0.")

    if (!is.null(col_names)){
        data_orig <- data
        data <- data %>% dplyr::select(dplyr::all_of(col_names))
    }

    # If no new_value is chosen, offset will be chosen so that new min is 1
    offset_vals <- data %>% dplyr::select_if(is.numeric) %>%
        dplyr::mutate_if(~(min(., na.rm = TRUE) <= 0), ~(. + (-1 * min(.)) + abs(new_min)))

    data[colnames(offset_vals)] <- offset_vals

    if (!is.null(col_names)){
        data_orig[colnames(data)] <- data
        data <- data_orig
    }

    return(data)
}


#' Scale / Normalize dataset
#'
#' Requires that dataset is cleaned from negative values, in case log or nth-root scaling is selected.
#'
#' @param data Dataframe.
#' @param col_names (Optional) Names with numeric columns that shall be scaled / normalized
#' @param method Methods to apply to data: "log10", "log2", "ln", "sqrt", "nth_root", "normalize_to_max", "min_max", "sd" (standardization), (default "log10").
#' @param n_for_root Integer to select nth-root (eg. 3 -> cubic root).
#'
#' @return Scaled dataframe.
#' @export
#'
#' @examples
#' data <- data.frame(status = c("sick", "healthy", "healthy", "sick", "healthy"),
#'                    height = c(124, 142, 152, 163, 146),
#'                    owns = c(-24, 125, 250, -150, 134))
#'
#' data <- introduce_offset(data)
#'
#' scale_data(data, method = "nth_root", n_for_root = 4)
#'
#' scale_data(data, col_names = "height", method = "min_max")
#'
#' scale_data(data, col_names = c("height", "owns"), "normalize_to_max")
scale_data <- function(data = NULL,
                       col_names = NULL,
                       method = "log10",
                       n_for_root = 3) {

    if (is.null(data)) stop("Please provide data.")

    if (!is.null(col_names)) {
        data_orig <- data
        data <- data %>% dplyr::select(dplyr::all_of(col_names))
    }

    if (method == "sd") {
        X_sd <- apply(data, 2, stats::sd)
    }

    data <- switch(method,
                   log10 = {data %>% dplyr::mutate_if((~ !any(is.na(.)) & is.numeric(.)), log10)},
                   log2 = {data %>% dplyr::mutate_if((~ !any(is.na(.)) & is.numeric(.)), log2)},
                   ln = {data %>% dplyr::mutate_if((~ !any(is.na(.)) & is.numeric(.)), log)},
                   sqrt = {data %>% dplyr::mutate_if((~ !any(is.na(.)) & is.numeric(.)), sqrt)},
                   sd = {sweep(data, 2, X_sd, "/") %>% tibble::as_tibble()},
                   nth_root = {data %>% dplyr::rowwise() %>% dplyr::mutate_if((~ !any(is.na(.)) & is.numeric(.)), ~ {if (.x > 0) .^(1 / n_for_root) else - (-.)^(1 / n_for_root)})},
                   normalize_to_max = {apply(data, 2, FUN = function(x) x / max(x)) %>% tibble::as_tibble()},
                   min_max = {apply(data, 2, FUN = function(x) (x - min(x)) / (max(x) - min(x))) %>% tibble::as_tibble()}

                   # # They return NAs in very specific cases, even though apply works perfectly fine in those situations
                   # normalize_to_max = {data %>% dplyr::mutate_if((~ !any(is.na(.)) & is.numeric(.)), ~ (.x / max(.x)))},
                   # min_max = {data %>% dplyr::mutate_if((~ !any(is.na(.)) & is.numeric(.)), ~ ((.x - min(.x)) / (max(.x) - min(.x))))}
    )

    if (!is.null(col_names)){
        data_orig[colnames(data)] <- data
        data <- data_orig
    }

    return(as.data.frame(data))
}


#' Introduce unique cross terms
#'
#' Based on the selected method (default is multiplication - "*"), unique variable combinations are formed and are added
#' to the dataframe.
#'
#' @param data Dataframe with numeric entries.
#' @param method Operation to combine columns. Default is "*" to form products of column values.
#' @param ignore_variables Mention variables that all shall be ignored when forming combinations. Default is NULL.

#' @return Enhanced dataframe that contains unique column combinations.
#' @export
#'
#' @examples
#' enhanced_data <- introduce_unique_combinations(HOMA_IR[1:3, 1:8])
#'
introduce_unique_combinations <- function(data = NULL,
                                          method = "*",
                                          ignore_variables = NULL) {

    if (is.null(data)) stop("Please provide data.")

    if (is.null(method)) stop("Please provide method.")

    if (!(method %in% c("*", "-", "+", "/"))) stop("Please provide a valid method ( \"*\" , \"-\" , \"+\" , \"/\" )")

    if (!is.null(ignore_variables)) {
        crossterms <- dplyr::select(data, -dplyr::one_of(ignore_variables)) %>%
                                    utils::combn(., 2, FUN = Reduce, f = method)

        column_names <- dplyr::select(data, -dplyr::one_of(ignore_variables)) %>%
                        colnames

        colnames(crossterms) <- utils::combn(column_names, 2, FUN = paste, collapse = "_x_")

    } else {
        crossterms <- utils::combn(data, 2, FUN = Reduce, f = method)

        colnames(crossterms) <- utils::combn(colnames(data), 2, FUN = paste, collapse = "_x_")
    }

    # Check for possible duplicates and remove them based on column name
    # This is a safety precaution, in case the function is triggered several times in a row
    enhanced_cols_to_keep <- setdiff(colnames(crossterms), colnames(data))
    if (length(enhanced_cols_to_keep) != 0) {
        crossterms <- crossterms[, enhanced_cols_to_keep, drop = FALSE]
    }

    enhanced_data <- cbind(data, crossterms)

    return(enhanced_data)
}
