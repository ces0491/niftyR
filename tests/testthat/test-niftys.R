test_that("not in", {

  test <- c(1,2,3,4) %nin% c(1,2,5)

  expected <- c(FALSE, FALSE, TRUE, TRUE)

  testthat::expect_equal(test, expected)

})

test_that("remove NA from list", {

  x <- list(c(1:3), letters[1:4], NA, c(1, NA), list(c(5,6, NA), NA, "A"))

  test_rec_t <- rm_na_list(x, recursive = TRUE)
  test_rec_f <- rm_na_list(x, recursive = FALSE)

  exp_rec_t <- list(c(1:3), letters[1:4], c(1), list(c(5,6), "A"))
  exp_rec_f <- list(c(1:3), letters[1:4], c(1, NA), list(c(5,6, NA), NA, "A"))

  testthat::expect_equal(test_rec_t, exp_rec_t)
  testthat::expect_equal(test_rec_f, exp_rec_f)

})

test_that("complex numbers to NA", {

  test <- complexn_to_na(c(NaN, 1, 2, 3, Inf))

  expected <- c(NA, 1:3, NA)

  testthat::expect_equal(test, expected)

})

test_that("cleaning strings", {

  test <- string_clean("91 Cesairé T & co")
  expected <- "CesaireT&co"

  test2 <- string_clean("91 Cesairé T & co", non_alnum_replace = "_")
  expected2 <- "CesaireT_co"

  test3 <- string_clean("91 Cesairé T & co", keep_spaces = TRUE)
  expected3 <- "Cesaire_T_&_co"

  test4 <- string_clean("91 Cesairé T & co", keep_leading_num = TRUE, to_lower = TRUE)
  expected4 <- "cesairet&co_91"

  testthat::expect_equal(test, expected)
  testthat::expect_equal(test2, expected2)
  testthat::expect_equal(test3, expected3)
  testthat::expect_equal(test4, expected4)

})
