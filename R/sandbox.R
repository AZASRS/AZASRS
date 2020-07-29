AZASRS_TEST_DATA_DIRECTORY <- "./tests/testthat/data/"

#' @export
saveRDS_test <- function(data) {
  # Insert check to see if
  # print location and success/failure
  new_file_name <- paste0(AZASRS_TEST_DATA_DIRECTORY, deparse(substitute(data)), ".rds")
  print("Saving to:")
  print(new_file_name)
  saveRDS(data, new_file_name)
}

#' @export
readRDS_test <- function(data) {
  # Insert check to see if
  # print location and success/failure
  test_file_name <- paste0("./data/", deparse(substitute(data)), ".rds")
  print("Checking file:")
  print(test_file_name)
  readRDS(test_file_name)
}
