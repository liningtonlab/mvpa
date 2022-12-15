test_that("inspection - qq plot", {
    expect_snapshot(
        qq_plot(data = mtcars,
                vars_to_plot = c("hp", "mpg"),
                nr_cols = 2,
                rel_font_size = 1)
    )
})

test_that("inspection - correlation variables with response", {
    expect_snapshot(
        correlation(data = mtcars,
                    response = "hp",
                    correlate = "vars_w_response",
                    selection = c("mpg", "disp", "qsec"))
    )
})

test_that("inspection - variable correlation matrix", {
    expect_snapshot(
        correlation(data = mtcars,
                    response = "hp",
                    correlate = "variables",
                    selection = c("mpg", "disp", "qsec"))
    )
})

test_that("inspection - object correlation matrix", {
    expect_snapshot(
        correlation(data = mtcars,
                    correlate = "objects")
    )
})

test_that("inspection - plot correlation variables with response", {

    result <- correlation(data = mtcars,
                          response = "hp",
                          correlate = "vars_w_response",
                          selection = c("mpg", "disp", "qsec"))

    expect_snapshot(
        plot_correlation(corr_list = result,
                         rotate = TRUE,
                         rel_font_size = 1.4)
    )
})

test_that("inspection - plot variable correlation matrix", {

    result <- correlation(data = mtcars,
                          response = "hp",
                          correlate = "variables",
                          selection = c("mpg", "disp", "qsec"))

    expect_snapshot(
        plot_correlation(corr_list = result,
                         rotate = TRUE,
                         rel_font_size = 1.4)
    )
})

test_that("inspection - plot object correlation matrix", {

    result <- correlation(data = mtcars,
                          correlate = "objects")

    expect_snapshot(
        plot_correlation(corr_list = result,
                         rotate = TRUE,
                         rel_font_size = 1.4)
    )
})

test_that("inspection - data summary", {
    expect_snapshot(
        data_summary(data = mtcars)
    )
})
