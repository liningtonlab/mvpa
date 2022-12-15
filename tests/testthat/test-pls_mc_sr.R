test_that("optimal number of components", {

    expect_snapshot(
        optimal_nr_components(cost_func_vals = data.frame(a = c(12:22), b = c(23:33), c = c(1:11)),
                            validation_threshold = 0.5)
    )

    expect_snapshot(
        optimal_nr_components(cost_func_vals = data.frame(a = c(12:21), b = c(rep(4,4), rep(4, 6)), b = c(rep(3,6), rep(4, 4))),
                              validation_threshold = 0.5)
    )

})
