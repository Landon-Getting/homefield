test_that("queries successfully", {
  skip("api key does not translate to github actions")

  homefield_conquest <- get0("cfb_conquest_s1999_w4", envir = asNamespace("homefield"))

  cfb_conquest_s1999_w4 <- cfb_conquest(season = 1999, week = 4)

  expect_equal(head(cfb_conquest_s1999_w4), homefield_conquest, ignore_attr = TRUE)

})
