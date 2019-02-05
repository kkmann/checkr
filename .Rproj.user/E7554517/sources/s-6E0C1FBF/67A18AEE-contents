context("greater equal")

test_that("ge accepts single input", {

    some_variable <- 3
    expect_true(
        is.null(evaluate(ge(0), some_variable))
    )

})



test_that("ge accepts vector input", {

    some_variable <- c(1, 3)
    expect_true(
        is.null(evaluate(ge(0), some_variable))
    )

})



test_that("ge error message", {

    some_variable <- -1
    expect_true(
        evaluate(ge(0), some_variable) == "some_variable = -1.000e+00 < 0.000e+00"
    )

})



test_that("ge throws error at correct location", {

    some_variable <- c(0, -1, -2)
    expect_equal(
        evaluate(ge(0), some_variable),
        "some_variable[2] = -1.000e+00 < 0.000e+00\n\rsome_variable[3] = -2.000e+00 < 0.000e+00"
    )

})