# Global variables - to be used for extremely basic functions interacting with low level connections to the database

#' Opening Remarks
#' @description Prints statements as the package loads, not really a function
AZASRS__welcome__message__ <- print("Loading AZASRS package, please ensure this is up-to-date by using devtools::install_github('AZASRS/AZASRS') and that your .Renviron is up-to-date and in the right location.")


#' Opens a database connection
#' @description Uses AZASRS_DATABASE_LOCATION and should only be used with tbl_ functions. Allows for chaining of tbl_ functions to optimize SQL queries. Must close connection after usage. You must have .Renviron on your computer in the directory found by executing normalizePath('~/') in your console. Environment variables in there will allow you to connect to the database.
#' @param development signifies whether to use development or productions database (production = 0, development = 1) -- default is production
#' @return MS SQL database connection (Azure SQL)
#' @examples
#' con <- AZASRS_DATABASE_CONNECTION()
#' data <- tbl_pm_fund_nav_daily(con) %>%
#'   left_join(tbl_pm_fund_info(con), by = "pm_fund_id") %>%
#'   as_tibble()
#' @export
AZASRS_DATABASE_CONNECTION <- function(development = 0) {
  # Development will utilize a different database in order to be replicated
  if (development) {
    print("[FYI] - You are utilizing the DEVELOPMENT database.")
    database <- Sys.getenv("DATABASE_DEVELOPMENT")
  } else {
    database <- Sys.getenv("DATABASE")
  }

  # Detect OS & Set Driver (important for Windows, Shiny/Linux, Mac)
  os <- Sys.info()[1]
  username <- Sys.info()[6]
  if (os == "Darwin" | os == "mac" | os == "Windows") {
    driverName <- "ODBC Driver 17 for SQL Server"
  } else if(os == "Linux"){
    driverName <- "ODBC Driver 13 for SQL Server"
  }
  else {
    driverName <- "SQLServer"
  }

  if(username == 'asrsadmin'){
    # For virtual machine only.
    driverName <- "ODBC Driver 17 for SQL Server"
  }

  tryCatch(
    {
      connection <- DBI::dbConnect(odbc::odbc(),
        Driver = driverName,
        Server = Sys.getenv("SERVER"),
        Database = database,
        UID = Sys.getenv("UID"),
        PWD = Sys.getenv("PWD"),
        Port = Sys.getenv("PORT")
      )
    },
    error = function(e) {
      print("Database will start up momentarily (sleeps when there is inactivity over 24 hours).
              Following error was thrown:")
      print(e)

      connection <- DBI::dbConnect(odbc::odbc(),
        Driver = driverName,
        Server = Sys.getenv("SERVER"),
        Database = database,
        UID = Sys.getenv("UID"),
        PWD = Sys.getenv("PWD"),
        Port = Sys.getenv("PORT")
      )
    }
  )
  return(connection)
}


#' Updates database with AZURE files
#' @description To be used with individual files. Note: the first connection takes a long time to spin up the server, then should work quickly if more files are necessary.
#' @examples
#' UPDATE_DATABASE("pm_fund_info.csv")
#' @export
UPDATE_DATABASE <- function(filename, development = 0, local_azure_functions = FALSE) {

  # Force user to decide if they want to modify things in production or development
  if (!development) {
    x <- readline(
      "[WARNING] - You are attempting to modify the PRODUCTION database.\n
    Modifying the DEVELOPMENT database is preferred to avoid data integrity issues.\n
    Do you wish to proceed to modify the PRODUCTION database? (y / n)"
    )

    if (x == "y") {
      development <- 0
    } else if (x == "n") {
      development <- 1
    }
    else {
      stop("You must choose y or n!")
    }
  }

  # Allow for local func host run --python to populate DB
  if (local_azure_functions) {
    print("[FYI] - Utilizing local Azure Functions Server.")
    uri <- "http://localhost:7071/api/PopulateDB?code="
  } else {
    uri <- "https://azasrs-populate-database.azurewebsites.net/api/PopulateDB?code="
  }

  request_url <- paste0(
    uri,
    Sys.getenv("ASRS_FUNCTIONS_CODE"),
    "&username=", Sys.getenv("UID"),
    "&password=", Sys.getenv("PWD"),
    "&account_name=asrs",
    "&account_key=", Sys.getenv("ASRS_BLOB_KEY"),
    "&filename=", filename,
    "&development=", development
  )

  print("Attempting to GET URL: ")
  print(request_url)
  r <- httr::GET(request_url)
  if (r$status_code != 200) {
    print(paste("ERROR Status Returned: ", r$status_code, "\n FROM GET REQUEST on: ", r$url))
  } else {
    print(paste("SUCCESS Status Returned", r$status_code, "\n FROM GET REQUEST on: ", r$url))
  }
  return(r)
}


#' Initially populates ALL tables via .csv files
#' @description To be used in the event the database needs a total refresh. Note: the benchmark data will fail to upload the first time simply due to its size, upon running again it will work.
#' @examples
#' UPDATE_DATABASE("pm_fund_info.csv")
#' @export
INITIAL_DATABASE_POPULATION <- function(development = 0, local_azure_functions = FALSE) {
  files <- c(
    "constants.csv",
    "pm_fund_info.csv",
    "pm_fund_cash_flow_daily.csv",
    "pm_fund_nav_daily.csv",
    "benchmark_info.csv",
    "benchmark_symbol_lookup.csv",
    "benchmark_index.csv",
    "account_info.csv",
    "account_info_benchmark_info.csv",
    "pm_fund_info_benchmark_info.csv",
    "ssbt_composite_info.csv",
    "ssbt_composite_info_account_info.csv",
    "composite_book_of_record_daily.csv",
    "composite_book_of_record_monthly.csv",
    "account_book_of_record_daily.csv",
    "account_book_of_record_monthly.csv",
    "create_views"
  )

  n_succeed <- c()
  n_fail <- c()

  for (f in files) {
    r <- UPDATE_DATABASE(f, development = development, local_azure_functions = local_azure_functions)

    if (r$status_code == 200) {
      n_succeed <- c(n_succeed, f)
    } else {
      n_fail <- c(n_fail, f)
    }
    print("=======================================")
    print("Successes: ")
    if (length(n_succeed) > 0) {
      print(paste0(n_succeed))
    } else {
      print("No successes have occurred.")
    }
    print("=======================================")
    print("Failures: ")
    if (length(n_fail) > 0) {
      print(paste0(n_fail))
    } else {
      print("No failures have occurred.")
    }
    print("=======================================")
  }
  return(list(
    success = n_succeed,
    fail = n_fail
  ))
}


#' Disconnect from database
#' @description Disconnect from database if using AZASRS_DATABASE_CONNECTION
#' @param con object from AZASRS_DATABASE_CONNECTION()
#' @return boolean for DBI disconnected or not
#' @examples
#' con <- AZASRS_DATABASE_CONNECTION()
#' data <- tbl_pm_fund_nav_daily(con) %>%
#'   left_join(tbl_pm_fund_info(con), by = "pm_fund_id") %>%
#'   as_tibble()
#' AZASRS_DATABASE_DISCONNECT(con)
#' @export
AZASRS_DATABASE_DISCONNECT <- function(con) {
  DBI::dbDisconnect(conn = con)
}


#' Get value date
#'
#' @description Finds the value date based off of the constants table from most recent database population
#' @param con object from AZASRS_DATABASE_CONNECTION()
#' @return string of YYYY-mm-dd of value date as set by ASRS
#' @export
get_value_date <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dat <- tbl_constants(con) %>% tibble::as_tibble()
  dat <- dat$value_date
  return(dat)
}


#' Get next quarter date
#'
#' @description Finds the next quarter date based off of constants table from most recent database population
#' @param con object from AZASRS_DATABASE_CONNECTION()
#' @return string of YYYY-mm-dd of value date as set by ASRS + 1 qtr
#' @export
get_next_quarter <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dat <- tbl_constants(con) %>% tibble::as_tibble()
  dat <- dat$next_quarter
  return(dat)
}


# tbl_ allows access to optimize queries from database while using tidyverse style. These ONLY refer to raw tables and not views.

#' @export
tbl_account_asset_class <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "account_asset_class")
}

#' @export
tbl_account_book_of_record_daily <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "account_book_of_record_daily")
}

#' @export
tbl_account_book_of_record_monthly <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "account_book_of_record_monthly")
}

#' @export
tbl_account_category <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "account_category")
}

#' @export
tbl_account_info <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "account_info")
}

#' @export
tbl_account_info_benchmark_info <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "account_info_benchmark_info")
}

#' @export
tbl_account_info_pm_fund_info <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "account_info_pm_fund_info")
}

#' @export
tbl_account_portfolio <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "account_portfolio")
}

#' @export
tbl_account_sponsor <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "account_sponsor")
}

#' @export
tbl_account_sub_portfolio <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "account_sub_portfolio")
}

#' @export
tbl_benchmark_daily_index <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "benchmark_daily_index")
}

#' @export
tbl_benchmark_daily_return <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "benchmark_daily_return")
}

#' @export
tbl_benchmark_info <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "benchmark_info")
}

#' @export
tbl_benchmark_monthly_return <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "benchmark_monthly_return")
}

#' @export
tbl_benchmark_type <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "benchmark_type")
}

#' @export
tbl_constants <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "constants")
}

#' @export
tbl_pm_fund_cash_flow_daily <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "pm_fund_cash_flow_daily") %>% dplyr::mutate(cash_flow = round(cash_flow, 2))
}

#' @export
tbl_pm_fund_category <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "pm_fund_category")
}

#' @export
tbl_pm_fund_category_description <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "pm_fund_category_description")
}

#' @export
tbl_pm_fund_city <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "pm_fund_city")
}

#' @export
tbl_pm_fund_info <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "pm_fund_info")
}

#' @export
tbl_pm_fund_info_benchmark_info <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "pm_fund_info_benchmark_info")
}

#' @export
tbl_pm_fund_nav_daily <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "pm_fund_nav_daily")
}

#' @export
tbl_pm_fund_portfolio <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "pm_fund_portfolio")
}

#' @export
tbl_pm_fund_sector <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "pm_fund_sector")
}

#' @export
tbl_pm_fund_sponsor <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "pm_fund_sponsor")
}

#' @export
tbl_ssbt_composite_book_of_record_daily <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "ssbt_composite_book_of_record_daily")
}

#' @export
tbl_ssbt_composite_book_of_record_monthly <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "ssbt_composite_book_of_record_monthly")
}

#' @export
tbl_ssbt_composite_info <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "ssbt_composite_info")
}

#' @export
tbl_ssbt_composite_info_account_info <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "ssbt_composite_info_account_info")
}

#' @export
tbl_ssbt_composite_info_benchmark_info <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "ssbt_composite_info_benchmark_info")
}

# Views go below (we will still refer to these as tbl_view_ to stay consistent with naming)

#' @export
tbl_view_all_pm_fund_info <- function(con = AZASRS_DATABASE_CONNECTION()) {
  dplyr::tbl(con, "all_pm_fund_info")
}
