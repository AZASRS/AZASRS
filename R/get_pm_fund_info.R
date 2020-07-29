#' Get all pm_fund_info
#'
#' @description A view to get all private market fund info, can be filtered. By default, SQL --> SELECT * FROM all_pm_fund_info;
#' @param con is a database connection object from AZASRS::AZASRS_DATABASE_CONNECTION()
#' @param add_benckmark is a boolean that appends multiple benchmarks as their own columns. i.e. PVT_Benchmark, SAA_Benchmark, etc.
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble or SQL result with all pm_fund_info metadata.
#' @examples
#' get_pm_fund_info()
#' # A tibble: 282 x 26
#' # pm_fund_info_id pm_fund_id pm_fund_descrip… pm_fund_common_… vintage commit unfunded legacy specialist
#' # <int> <chr>      <chr>                   <chr>              <int>   <int>    <int>    <chr>  <chr>
#' # 1      Hgh19     AP Mezzanine Pa…      HPS Mezz 2019         2019    6.00e8  3.95e8     A    " "
#' # 2      HghBr     AP Mezzanine Pa…      HPS Mezz 2            2013    2.00e8  1.30e7     A    " "
#' # 3      HghBr3    AP Mezzanine Pa…      HPS Mezz 3            2016    5.00e8  9.85e7     A    " "
#' # … with 279 more rows, and 17 more variables: invest_end <date>, term_end <date>, extension <dbl>,
#' #   ext_time <dbl>, ext_used <dbl>, fee_cat <chr>, consultant <chr>, adv_board <lgl>, obsvr <lgl>,
#' #   fund_size_m <dbl>, closed <chr>, pm_fund_category <chr>, pm_fund_category_description <chr>,
#' #   pm_fund_portfolio <chr>, pm_fund_sponsor <chr>, pm_fund_city <chr>, pm_fund_sector <chr>
#' @export
get_pm_fund_info <- function(con = AZASRS_DATABASE_CONNECTION(), add_benchmark = FALSE, return_tibble = TRUE) {
  dat <- tbl_view_all_pm_fund_info(con = con)

  if (add_benchmark) {
    print("NOTE: Bench addon requires a tibble to be returned, and will override the input argument!")

    dat <- dat %>%
      tibble::as_tibble()

    benchmark_fund_relationship <- get_benchmark_fund_relationship(con = con, get_all_benchmark_types = TRUE,  return_tibble = TRUE) %>%
      tibble::as_tibble() %>%
      tidyr::pivot_wider(names_from = benchmark_type, values_from = benchmark_info_id)

    benchmark_info <- tbl_benchmark_info(con = con) %>%
      dplyr::select(benchmark_info_id, benchmark_id) %>%
      tibble::as_tibble()

    benchmark_joined <- benchmark_fund_relationship %>%
      dplyr::left_join(benchmark_info, by = c("PVT" = "benchmark_info_id")) %>%
      dplyr::left_join(benchmark_info, by = c("SAA" = "benchmark_info_id")) %>%
      dplyr::left_join(benchmark_info, by = c("IMP" = "benchmark_info_id")) %>%
      dplyr::left_join(benchmark_info, by = c("Sector" = "benchmark_info_id")) %>%
      dplyr::rename(PVT_Benchmark = benchmark_id.x, SAA_Benchmark = benchmark_id.y, IMP_Benchmark = benchmark_id.x.x, Sector_Benchmark = benchmark_id.y.y) %>%
      dplyr::select(-PVT, -SAA, -IMP, -Sector)

    dat <- dat %>%
      dplyr::left_join(benchmark_joined, by = "pm_fund_info_id")
    return(dat)
  }

  if (return_tibble) {
    return(dat %>% tibble::as_tibble())
  } else {
    return(dat)
  }
}
