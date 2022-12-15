test_that("principal component analysis - no standardization", {
    expect_snapshot(
        perform_pca(data = mtcars,
                    standardize = FALSE,
                    exclude_variables = "hp")
        )
})

test_that("principal component analysis - standardization", {
    expect_snapshot(
        perform_pca(data = mtcars,
                    standardize = TRUE,
                    exclude_variables = "hp")
    )
})

test_that("principal component analysis - plot scree plot", {

    result <- perform_pca(data = mtcars,
                          standardize = TRUE,
                          exclude_variables = "hp")

    expect_snapshot(
        plot_scree_plot(pca_result = result,
                        how_many_PCs = 3,
                        add_cumulative_perc = TRUE,
                        rel_font_size = 2)

    )
})

test_that("principal component analysis - plot scores", {

    result <- perform_pca(data = mtcars,
                          standardize = TRUE,
                          exclude_variables = "hp")

    expect_snapshot(
        plot_pca_2d(pca_result = result,
                    plot = "scores",
                    PCs_to_plot = c(1, 2),
                    rel_point_size = 1,
                    rel_font_size = 1)

    )
})

test_that("principal component analysis - plot loadings", {

    result <- perform_pca(data = mtcars,
                          standardize = TRUE,
                          exclude_variables = "hp")

    expect_snapshot(
        plot_pca_2d(pca_result = result,
                    plot = "loadings",
                    PCs_to_plot = c(1, 2),
                    rel_point_size = 1,
                    rel_font_size = 1)

    )
})


