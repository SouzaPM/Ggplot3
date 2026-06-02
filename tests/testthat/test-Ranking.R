test_that("rank_table returns a data frame", {
  result <- rank_table("co2_per_capita", 2019)
  
  expect_s3_class(result, "data.frame")
})

test_that("invalid variable throws an error", {
  expect_error(
    rank_table("not_a_variable", 2019),
    "not a valid variable"
  )
})

test_that("output contains expected columns", {
  result <- rank_table("co2_per_capita", 2019)
  
  expect_equal(names(result), c("Country", "Value", "Rank"))
})

test_that("ranks are sorted ascending", {
  result <- rank_table("co2_per_capita", 2019)
  
  expect_equal(
    result$Rank,
    sort(result$Rank)
  )
})