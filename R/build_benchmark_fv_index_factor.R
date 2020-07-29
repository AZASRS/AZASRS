#' @description Build a tibble of benchmark index future value factor
#' @param con is a database connection object from AZASRS::AZASRS_DATABASE_CONNECTION()
#' @param start_date is the start date of analysis
#' @param value_date is the object of get_value_date()
#' @param benchmark_daily is data from get_benchmark_daily_index() and can be previously loaded and filtered
#' @param all_bench_types is a boolean that determines whether all bench type are included
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble with grouping variables and all of their respective metrics
#' @export
build_benchmark_fv_index_factor <- function(con = AZASRS_DATABASE_CONNECTION(),
                                            start_date = "2004-09-30",
                                            value_date = get_value_date(con = con),
                                            benchmark_daily = get_benchmark_daily_index(
                                              con = con,
                                              all_bench_types = TRUE,
                                              return_tibble = FALSE
                                            )) {

  # Filter data and require tibble to allow for distinct + keep_all
  tmp <- benchmark_daily %>%
    dplyr::filter(
      effective_date >= start_date,
      effective_date <= value_date
    ) %>%
    dplyr::as_tibble() %>%
    dplyr::distinct(benchmark_info_id, effective_date, .keep_all = TRUE)

  # Create a "future value" based off of index
  bench_daily_filtered <- tmp %>%
    dplyr::group_by(benchmark_info_id) %>%
    dplyr::arrange(effective_date) %>%
    dplyr::mutate(index_fv = dplyr::last(index_value) / index_value) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(benchmark_info_id, effective_date)

  return(bench_daily_filtered)
}
