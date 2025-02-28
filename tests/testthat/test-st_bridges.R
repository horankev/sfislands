test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# list
nbsf_list <- st_bridges(uk_election, row_identifier = "constituency_name", remove_islands = FALSE)
nbsf_list_noislands <- st_bridges(uk_election, row_identifier = "constituency_name", remove_islands = TRUE)
nbsf_list_nodf <- st_bridges(uk_election, row_identifier = "constituency_name", remove_islands = FALSE, add_to_dataframe = FALSE)
nbsf_list_noislands_nodf <- st_bridges(uk_election, row_identifier = "constituency_name", remove_islands = TRUE, add_to_dataframe = FALSE)

# matrix
nbsf_matrix <- st_bridges(uk_election, row_identifier = "constituency_name", remove_islands = FALSE, nb_structure = "matrix")
nbsf_matrix_noislands <- st_bridges(uk_election, row_identifier = "constituency_name", remove_islands = TRUE, nb_structure = "matrix")
nbsf_matrix_nodf <- st_bridges(uk_election, row_identifier = "constituency_name", remove_islands = FALSE, add_to_dataframe = FALSE, nb_structure = "matrix")
nbsf_matrix_noislands_nodf <- st_bridges(uk_election, row_identifier = "constituency_name", remove_islands = TRUE, add_to_dataframe = FALSE, nb_structure = "matrix")

test_that("neighbors are of appropriate length for lists", {
  skip_on_cran()
  skip_on_ci()

  # list
  expect_length(nbsf_list, 10)
  expect_length(nbsf_list$nb, 632)
  expect_equal(nrow(nbsf_list), 632)

  expect_length(nbsf_list_noislands, 10)
  expect_length(nbsf_list_noislands$nb, 628)
  expect_equal(nrow(nbsf_list_noislands), 628)

  expect_length(nbsf_list_nodf, 632)
  expect_equal(nrow(nbsf_list_nodf), NULL)

  expect_length(nbsf_list_noislands_nodf, 628)
  expect_equal(nrow(nbsf_list_noislands_nodf), NULL)

  expect_equal(names(nbsf_list$nb),uk_election$constituency_name)

})

test_that("neighbors are of appropriate length for matrices", {
  skip_on_cran()
  skip_on_ci()

  # matrix
  expect_length(nbsf_matrix, 10)
  expect_length(nbsf_matrix$nb, 399424)
  expect_equal(nrow(nbsf_matrix), 632)

  expect_length(nbsf_matrix_noislands, 10)
  expect_length(nbsf_matrix_noislands$nb, 394384)
  expect_equal(nrow(nbsf_matrix_noislands), 628)

  expect_length(nbsf_matrix_nodf, 399424)
  expect_equal(nrow(nbsf_matrix_nodf), 632)

  expect_length(nbsf_matrix_noislands_nodf, 394384)
  expect_equal(nrow(nbsf_matrix_noislands_nodf), 628)

  expect_equal(rownames(nbsf_matrix$nb),uk_election$constituency_name)

})
