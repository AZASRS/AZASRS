#' Upload Data to Azure
#'
#' @description UPLOADS RAW data to Azure
#' @param data object to be converted to .rds and uploaded
#' @param blob_directory is the folder within the blob
#' @param filename is the filename within the blob's folder
#' @export
send_raw_data_to_azure <- function(data, blob_directory = "calculated-data", filename) {

  cont <- AzureStor::blob_container(paste0(Sys.getenv("ASRS_BLOB"), blob_directory),
                                    key = Sys.getenv("ASRS_BLOB_KEY"))

  saveRDS(data, filename)

  upload_data <- AzureStor::upload_blob(cont,
                                        src = filename,
                                        dest = filename)

  print("Data has been uploaded successfully!")
}
