test_that("mean center dataset", {
    expect_snapshot(
        mean_center(mtcars)
    )
})

# test_that("root mean square error", {
#     expect_equal(
#         rmsep(c(0.909375, -70.721875, 0.3034375, -1.38875, 0.59375),
#               c(-0.1875, -36.6875, -0.59725, -0.4375, 0.312)),
#         15.2403236
#     )
# })

test_that("median - confidence limits", {
    df <- data.frame(sequence = seq(1, 1000))

    median_and_confidence_limits <- median_cl(data = df,
                                              confidence_limits = c(0.025, 0.975),
                                              pretty_column_names = FALSE)

    expect_equal(
        median_and_confidence_limits$median, 500.5
    )

    expect_equal(
        median_and_confidence_limits$cl_lower, 25
    )

    expect_equal(
        median_and_confidence_limits$cl_upper, 975
    )

    # confidence_limits are always ordered in increasing order
    median_and_confidence_limits <- median_cl(data = df,
                                              confidence_limits = c(0.700, 0.350),
                                              pretty_column_names = FALSE)

    expect_equal(
        median_and_confidence_limits$cl_lower, 350
    )

    expect_equal(
        median_and_confidence_limits$cl_upper, 700
    )


})

test_that("split dataset - even sample size", {

    dataset <- data.frame(a = c(1:10), b = c(11:20), c = c(21:30))

    result <- split_dataset(X = dataset,
                            y = dataset[, 2, drop = FALSE],
                            cal_ratio = 0.5)

    # X and y objects should be identical
    expect_equal(
        rownames(result$X_cal), rownames(result$y_cal)
    )

    expect_equal(
        rownames(result$X_val), rownames(result$y_val)
    )

    # Validation and testdata objects should equal all objects in whole dataset
    expect_setequal(
        rownames(dataset), c(rownames(result$X_cal), rownames(result$X_val))
    )
})

test_that("split dataset - uneven sample size", {

    dataset <- data.frame(a = c(1:11), b = c(12:22), c = c(23:33))

    result <- split_dataset(X = dataset,
                            y = dataset[, 2, drop = FALSE],
                            cal_ratio = 0.5)

    # X and y objects should be identical
    expect_equal(
        rownames(result$X_cal), rownames(result$y_cal)
    )

    expect_equal(
        rownames(result$X_val), rownames(result$y_val)
    )

    # Validation and test data objects should equal all objects in the whole dataset
    expect_setequal(
        rownames(dataset), c(rownames(result$X_cal), rownames(result$X_val))
    )
})
