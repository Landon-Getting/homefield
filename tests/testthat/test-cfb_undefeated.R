test_that("queries successfully", {
  skip("api key does not translate to github actions")

  homefield_undefeated <- get0("cfb_undefeated_s1999_w4", envir = asNamespace("homefield"))

  cfb_undefeated_s1999_w4 <- cfb_undefeated(season = 1999, week = 4)

  expect_equal(head(cfb_undefeated_s1999_w4), homefield_undefeated, ignore_attr = TRUE)

})
