#' Get relationships between funds and benchmarks get_benchmark_fund_relationship
#'
#' @description Finds all data from benchmark_info_id matched with pm_fund_info_id
#' @param con is a database connection object from AZASRS::AZASRS_DATABASE_CONNECTION()
#' @param bench_type is the type of benchmark. The default is 'PVT' which is private.
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble or database object
#' @examples
#' get_benchmark_fund_relationship(return_tibble = TRUE)
#' # A tibble: 282 x 3
#' # pm_fund_info_id benchmark_info_id benchmark_type
#' # <int>                <int>               <chr>
#' #  1                    53                 PVT
#' #  2                    53                 PVT
#' #  3                    53                 PVT
#' # ... with 279 more rows
#' @export
get_benchmark_fund_relationship <- function(con = AZASRS_DATABASE_CONNECTION(), bench_type = "PVT", get_all_benchmark_types = FALSE, return_tibble = FALSE) {
  dat <- tbl_pm_fund_info_benchmark_info(con) %>%
    dplyr::left_join(tbl_benchmark_type(con), by = "benchmark_type_id") %>%
    dplyr::select(pm_fund_info_id, benchmark_info_id, benchmark_type)

  if(!get_all_benchmark_types){
    dat = dat %>% dplyr::filter(benchmark_type == bench_type)
  }


  if (return_tibble) {
    return(dat %>% tibble::as_tibble())
  } else {
    return(dat)
  }
}
