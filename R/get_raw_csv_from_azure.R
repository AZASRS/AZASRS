#' Get CSV Data from Azure
#'
#' @description Gets RAW csv for IRR comparison from Azure
#' @param blob_directory is the folder within the blob
#' @param filename is the filename within the blob's folder
#' @param download_to_directory is the relative path where the data will be downloaded and stored to
#' @param read_csv if the file is a CSV and you want it to be read, it will be returned as a tibble
#' @export
get_raw_data_from_azure <- function(blob_directory = "source-data", filename = "pm_fund_info.csv", download_to_directory = "./", read_csv = TRUE) {
  cont <- AzureStor::blob_container(paste0(Sys.getenv("ASRS_BLOB"), blob_directory),
    key = Sys.getenv("ASRS_BLOB_KEY")
  )
  download_data <- AzureStor::download_blob(cont, src = filename, dest = paste0(download_to_directory, filename), overwrite = TRUE)

  if (read_csv) {
    return(download_data %>% readr::read_csv(download_data))
  }
  print("Data has been downloaded successfully!")
}
