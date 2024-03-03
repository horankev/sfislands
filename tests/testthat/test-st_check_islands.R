test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

checkdf <- st_bridges(uk_election, "constituency_name", remove_islands = FALSE) |>
  st_check_islands()

test_that("indices match names", {
  skip_on_cran()
  skip_on_ci()

  expect_equal(checkdf[,1], uk_election$constituency_name[as.numeric(checkdf[,2])])
})
