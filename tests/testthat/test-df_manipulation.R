test_that("remove variables", {

    dataset <- data.frame(a = c(1:11), b = c(12:22), c = c(23:33), d = rep(1, 11), e = rep(NA, 11))

    result_inv <- remove_variables(data = dataset,
                                   vars_to_remove = NULL,
                                   remove_na = FALSE,
                                   remove_invariants = TRUE)

    result_na <- remove_variables(data = dataset,
                                  vars_to_remove = NULL,
                                  remove_na = TRUE,
                                  remove_invariants = FALSE)

    result_b <- remove_variables(data = dataset,
                                 vars_to_remove = "b",
                                 remove_na = FALSE,
                                 remove_invariants = FALSE)

    result_b_na <- remove_variables(data = dataset,
                                    vars_to_remove = "b",
                                    remove_na = TRUE,
                                    remove_invariants = FALSE)

    result_b_na_inv <- remove_variables(data = dataset,
                                        vars_to_remove = "b",
                                        remove_na = TRUE,
                                        remove_invariants = TRUE)

    expect_setequal(colnames(result_inv), c("a", "b", "c", "e"))
    expect_setequal(colnames(result_na), c("a", "b", "c", "d"))
    expect_setequal(colnames(result_b), c("a", "c", "d", "e"))
    expect_setequal(colnames(result_b_na), c("a", "c", "d"))
    expect_setequal(colnames(result_b_na_inv), c("a", "c"))
})

test_that("keep variables", {

    dataset <- data.frame(a = c(1:11), b = c(12:22), c = c(23:33), d = rep(1, 11), e = rep(NA, 11))

    result <- keep_variables(data = dataset,
                             vars_to_keep = c("b", "d"))

    expect_setequal(colnames(result), c("b", "d"))
})

test_that("keep objects", {

    dataset <- data.frame(a = c(1:11), b = c(12:22), c = c(23:33), d = rep(1, 11), e = rep(NA, 11))

    result <- keep_objects(data = dataset,
                           objs_to_keep = c(1, "2"))

    expect_setequal(rownames(result), c("1", "2"))
})

test_that("enhance dataset", {

    dataset <- data.frame(a = c(1:11), b = c(12:22), c = c(23:33))

    # Wrong number of objects
    expect_error(
        enhance_dataset(data = dataset,
                        data_to_add = rep(1, 12))
        )

    rownames(dataset) <- paste0("object_", 1:11)

    # Wrong object names
    expect_error(
        enhance_dataset(data = dataset,
                        data_to_add = data.frame(rep(1, 12)))
                 )

    new_data <- data.frame(new = 1:11)
    rownames(new_data) <- paste0("object_", 1:11)

    expect_snapshot(
        enhance_dataset(data = dataset,
                        data_to_add = new_data)
    )
})

test_that("add unique IDs (leading zeros)", {
    dataset <- data.frame(a = c(1:11), b = c(12:22), c = c(23:33))

    expect_snapshot(
        add_unique_ids(data = dataset)
    )
})


test_that("impute missing values", {

    dataset <- data.frame(a = c(1:10), b = c(11:20), c = c(21:30), d = rep(c(1,NA), 5))

    # Custom value
    expect_snapshot(
        impute_missing_values(data = dataset,
                              method = "custom",
                              custom_value = 4)
    )

    # Mean
    expect_snapshot(
        impute_missing_values(data = dataset,
                              method = "mean",
                              custom_value = 4)
    )

    # Median
    expect_snapshot(
        impute_missing_values(data = dataset,
                              method = "median",
                              custom_value = 4)
    )
})

test_that("introduce dummies", {

    dataset <- data.frame(a = c(1:10), b = c(11:20), c = rep(c("a", "b", "c", "d", "e"), 2))

    dataset_binary <- data.frame(a = c(1:10), b = c(11:20), c = rep(c("a", "b"), 5))

    expect_snapshot(
        introduce_dummies(data = dataset,
                          col_names = NULL,
                          add_indicator = FALSE)
    )

    expect_snapshot(
        introduce_dummies(data = dataset,
                          col_names = NULL,
                          add_indicator = FALSE)
    )

    expect_snapshot(
        introduce_dummies(data = dataset,
                          col_names = "c",
                          add_indicator = TRUE)
    )

    expect_snapshot(
        introduce_dummies(data = dataset_binary,
                          col_names = "c",
                          add_indicator = FALSE)
    )
})

test_that("introduce offset", {

    dataset <- data.frame(a = c(0, -3, 2), b = c(0, 1, 3), c = c(1, 2, 3))

    expect_snapshot(
        introduce_offset(data = dataset,
                         new_min = 5,
                         col_names = NULL)
    )

    expect_snapshot(
        introduce_offset(data = dataset,
                         new_min = 5,
                         col_names = c("a"))
    )

})

test_that("scale / transformations", {

    dataset <- data.frame(a = c(0, -3, 2), b = c(0, 1, 3), c = c(1, 2, 3))

    expect_warning(
        scale_data(data = dataset,
                   col_names = NULL,
                   method = "log10",
                   n_for_root = 3)
    )

    expect_snapshot(
        scale_data(data = dataset,
                   col_names = c("b", "c"),
                   method = "sqrt",
                   n_for_root = 3)
    )

    expect_snapshot(
        scale_data(data = dataset,
                   col_names = c("b", "c"),
                   method = "nth_root",
                   n_for_root = 3)
    )

    expect_snapshot(
        scale_data(data = dataset,
                   col_names = c("b", "c"),
                   method = "normalize_to_max",
                   n_for_root = 3)
    )

    expect_snapshot(
        scale_data(data = dataset,
                   col_names = c("b", "c"),
                   method = "min_max",
                   n_for_root = 3)
    )

})


test_that("unique combinations", {

    dataset <- data.frame(a = c(0, -3, 2), b = c(0, 1, 3), c = c(1, 2, 3))

    expect_snapshot(
        introduce_unique_combinations(data = dataset,
                                      method = "*",
                                      ignore_variables = NULL)
    )

    expect_snapshot(
        introduce_unique_combinations(data = dataset,
                                      method = "/",
                                      ignore_variables = NULL)
    )
    expect_snapshot(
        introduce_unique_combinations(data = dataset,
                                      method = "*",
                                      ignore_variables = "b")
    )

})
