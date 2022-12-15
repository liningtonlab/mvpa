test_that("covariate projection - no standardization", {
    expect_snapshot(
        perform_covariate_projection(X_aug = mtcars,
                                      standardize = FALSE,
                                      covariates = c("hp", "mpg"))
        )
})

test_that("covariate projection - no standardization", {
    expect_snapshot(
        perform_covariate_projection(X_aug = mtcars,
                                      standardize = TRUE,
                                      covariates = c("hp", "mpg"))
    )
})

test_that("covariate projection plot", {

    result <- perform_covariate_projection(X_aug = mtcars,
                                            standardize = TRUE,
                                            covariates = c("hp", "mpg"))

    expect_snapshot(
        plot_explained_var_by_covariates(covariate_analysis_result = result,
                                          rel_font_size = 2,
                                          rotate = TRUE,
                                          x_filter = "hp")
        )
})
