# File is reserved for basic financial calculations that should be standardized across ASRS

#' IRR - JD replaced Karl's asrsMethods
#'
#' @description Calculates IRR based off of asrsMethods library. Can be used with tidyverse style.
#' @param cash_flow is a c() of cash_flows with the first being the earliest NAV (negative) and the last being NAV (positive)
#' @param dates is a c() of dates that corresponds with the cash_flow
#' @export
calc_irr <- function(cash_flow, dates, gips = TRUE, cumulative = FALSE) {
  cf_zoo <- zoo::zoo(as.numeric(cash_flow), dates)

  npv.znoadjust <- function(i, cf.z, freq, tdiff) {
    d <- (1 + (i / freq))^tdiff
    sum(cf.z / d)
  }

  irr_freq <- 1
  if (any(is.na(cf_zoo))) {
    return(NA)
  }
  if (sum(cf_zoo) == 0) {
    return(0)
  }
  if (length(cf_zoo) <= 1) {
    return(NA)
  }
  if (all(cf_zoo <= 0)) {
    return(NA)
  }
  if (all(cf_zoo >= 0)) {
    return(NA)
  }
  if (!zoo::is.zoo(cf_zoo)) {
    time_diff <- 1:length(cf_zoo) - 1
  } else {
    time_line <- time(cf_zoo)
    time_diff <- as.numeric(time_line - time_line[1])
    if (class(time_line) == "Date") {
      irr_freq <- 365
    }
  }
  fail_IRR <- FALSE
  same_sign <- TRUE
  if (sum(cf_zoo) < 0) {
    range_hi <- .01
    range_lo <- 0
    i <- 0
    # low range on search for negative IRR is -100%
    while (i < 1002 & same_sign & !fail_IRR) {
      range_hi <- range_lo
      range_lo <- max(-10, range_lo - .01)
      hi_npv <- npv.znoadjust(range_hi, cf_zoo, irr_freq, time_diff)
      lo_npv <- npv.znoadjust(range_lo, cf_zoo, irr_freq, time_diff)
      same_sign <- sign(hi_npv) == sign(lo_npv)
      fail_IRR <- (is.nan(hi_npv) | is.nan(lo_npv) | is.na(same_sign))
      i <- i + 1
    }
  } else {
    range_hi <- 0
    range_lo <- -.01
    i <- 0
    # while hi range on search for positive IRR is 100,000%
    while (i < 100000 & same_sign & !fail_IRR) {
      range_lo <- range_hi
      range_hi <- range_hi + .01
      hi_npv <- npv.znoadjust(range_hi, cf_zoo, irr_freq, time_diff)
      lo_npv <- npv.znoadjust(range_lo, cf_zoo, irr_freq, time_diff)
      same_sign <- sign(hi_npv) == sign(lo_npv)
      fail_IRR <- (is.nan(hi_npv) | is.nan(lo_npv) | is.na(same_sign))
      i <- i + 1
    }
  }
  if (fail_IRR) {
    return(NA)
  }
  npv_one <- lo_npv
  npv_two <- hi_npv
  if (sign(npv_one) == sign(npv_two)) {
    return(NA)
  }
  cf_vec <- as.numeric(cf_zoo)

  # calculate with uniroot if cash flow starts negative and ends positive otherwise do your own search
  if ((cf_vec[1] < 0) & (cf_vec[length(cf_vec)] > 0)) {
    ans <- stats::uniroot(npv.znoadjust, c(range_lo, range_hi), cf = cf_zoo, freq = irr_freq, tdiff = time_diff)
    apr <- ans$root
  } else {
    int_one <- range_lo
    int_two <- range_hi
    for (i in 1:40) {
      int_a <- mean(c(int_one, int_two))
      npv_a <- npv.znoadjust(int_a, cf_zoo, irr_freq, time_diff)
      if (sign(npv_a) == sign(npv_one)) {
        int_one <- int_a
        npv_one <- npv_a
      } else {
        int_two <- int_a
        npv_two <- npv_a
      }
    }
    apr <- mean(int_one, int_two)
  }
  # convert IRR to compounding at irr_freq interval
  ans <- ((1 + (apr / irr_freq))^irr_freq) - 1
  ans_cumlative <- ((1 + (apr / irr_freq))^xts::last(time_diff)) - 1
  # convert IRR to GIPS compliant if requested
  if (gips) {
    if (cf_zoo[1] == 0) {
      cf_zoo <- cf_zoo[-1]
    }
    dur <- zoo::index(cf_zoo)[length(cf_zoo)] - zoo::index(cf_zoo)[1]
    if (dur < irr_freq) {
      ans <- (1 + ans)^((as.numeric(dur)) / irr_freq) - 1
    }
  }
  if (cumulative == TRUE) {
    ans <- ans_cumlative
  }
  return(ans)
}
# Kar'ls method below
# calc_irr = function(cash_flow, dates){
#   dat <- asrsMethods::irr.z(zoo::zoo(cash_flow, dates), gips = TRUE)
#   return(dat)
# }


#' IRR from dataframe
#'
#' @description Abstraction of calc_irr to use with dataframe
#' @param df is a dataframe with columns named cash_flow and dates
#' @export
calc_irr_from_df <- function(df) {
  dat <- calc_irr(df$cash_flow, df$dates)
  return(dat)
}



#' TVPI
#'
#' @description Calculates TVPI that is NOT based off of asrsMethods library. asrsMethods needs to be updated because it does not reflect TVPI as Kerry calculates.
#' @param contributions are negative cash flow
#' @param distributions are positive cash flow
#' @param nav is NAV at the beginning and end with zeros in between. Both NAV are positive.
#' @export
calc_tvpi <- function(distributions, contributions, nav) {
  dat <- sum(distributions + nav) / sum(abs(contributions))
  return(dat)
}


#' DPI
#'
#' @description Calculates DPI that is NOT based off of asrsMethods library. asrsMethods needs to be updated because it does not reflect TVPI as Kerry calculates.
#' @param distributions are positive cash flow
#' @param contributions are negative cash flow
#' @export
calc_dpi <- function(distributions, contributions) {
  dat <- sum(abs(distributions)) / sum(abs(contributions))
  return(dat)
}

#' Appreciation
#'
#' @description Calculates Appreciation that is NOT based off of asrsMethods library. asrsMethods needs to be updated because it does not reflect TVPI as Kerry calculates.
#' @param cash_flow is a c() of cash flows
#' @param nav is NAV at the beginning and end with zeros in between. Both NAV are positive.
#' @export
calc_appreciation <- function(cash_flow, nav) {
  dat <- sum(nav) + sum(cash_flow)
  return(dat)
}

#' PME
#'
#' @description Calculates PME that is NOT based off of asrsMethods library. asrsMethods needs to be updated because it does not reflect TVPI as Kerry calculates.
#' @param distributions are negative cash flow
#' @param contributions are positive cash flow
#' @param nav is NAV at the beginning and end with zeros in between. Both NAV are positive.
#' @param fv_index_factors are a c() of future value data points based off of index value divided by first index value (i.e. 1.18, 1.11, 1.05, 1.00)
#' @export
calc_pme <- function(distributions, contributions, nav, fv_index_factors) {
  total_fv_distributions <- sum((distributions * fv_index_factors) + nav)
  total_fv_contributions <- abs(sum(contributions * fv_index_factors))
  pme <- total_fv_distributions / total_fv_contributions
  return(pme)
}


#' DVA
#'
#' @description Calculates DVA that is NOT based off of asrsMethods library.
#' @param cash_flow is the cash flow tibble
#' @param fv_index_factors are a c() of future value data points based off of index value divided by first index value (i.e. 1.18, 1.11, 1.05, 1.00)
#' @export
calc_dva <- function(cash_flow, fv_index_factors) {
  dva <- sum(cash_flow * fv_index_factors)

  return(dva)
}
