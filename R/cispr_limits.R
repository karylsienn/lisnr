
#' EN55011 limits
#'
#' @importFrom tibble tribble
#' @export
#'
en55011 <- list(
  ClassB = list(
    QP = tribble(
      ~start_frequency, ~stop_frequency, ~start_limit, ~stop_limit, ~type,
      150e3, 500e3, 66, 56, "log",
      500e3, 5e6, 56, 56, "const",
      5e6, 30e6, 60, 60, "const"
    ),

    Avg = tribble(
      ~start_frequency, ~stop_frequency, ~start_limit, ~stop_limit, ~type,
      150e3, 500e3, 56, 46, "log",
      500e3, 5e6, 46, 46, "const",
      5e6, 30e6, 50, 50, "const"
    )
  )
)



#' Get the standardised limits according to EN55011
#'
#' @param type "Avg" (Average) or "QP" (Quasi-Peak)
#' @param class "A" or "B"
#'
#' @return
#' @export
get_limits <- function(type = "Avg", class = "B") {

  types <- c("AVERAGE" = "AVG", "AVG" = "AVG", "QP" = "QP", "QUASI-PEAK" = "QP")
  type <- stringr::str_to_upper(type)
  if (!(type %in% names(types)) )
    stop("Specify correct type.")

  type <- types[type]

  class <- stringr::str_to_upper(class)
  if (!(class %in% c("B")) )
    stop("Specify correct class.")

  if(class == "B") {
    if(type == "AVG") {
      return(en55011$ClassB$Avg)
    } else if(type == "QP") {
      return(en55011$ClassB$QP)
    }
  }
}



#' Compute the limits for given frequencies
#'
#'
#' Compute the limits in dBuV as prescribed by the standards EN55011, for a given
#' class of device (A or B) and type of spectrum (Average or Quasi-Peak).
#'
#'
#' @param frequency numeric vector of frequencies
#' @param type character, type of the detector: average or Quasi-Peak
#' @param class character, class of the device "A" or "B"
#'
#' @return numeric vector, limits for
#' @export
#' @details For the frequencies outside the limits, this function returns NA_real_.
#'
#'
#' @importFrom dplyr rowwise mutate lst ungroup row_number slice left_join select rename arrange bind_rows
#' @importFrom tibble tibble
#' @importFrom purrr map_dbl
#'
compute_en55011 <- function(frequency, type = "AVG", class = "B") {

  # I am getting rid of %>% in case this is used in a loop
  T1 <- get_limits(type, class)
  T1 <- rowwise(T1)
  T1 <- mutate(T1, fn_col = lst(
    select_fn(type, start_frequency, stop_frequency, start_limit, stop_limit))) # Select log or const limit functions
  T1 <- ungroup(T1)
  T1 <- mutate(T1, intervals = row_number())

  # Find in which buckets the frequency vector is.
  ints <- findInterval(frequency, sort(union(T1$start_frequency, T1$stop_frequency)))

  # Tibble of the new frequencies
  T2 <- tibble(freq = frequency)
  T2 <- mutate(T2, row = row_number())

  # Cut into two sections: where the frequencies are inside the limits prescribed
  # by the standard and those outside it.
  pos <- which(ints %in% T1$intervals) # inside the limits
  T2pos <- slice(T2, pos)
  T2pos <- mutate(T2pos, intervals = ints[pos])
  T2pos <- left_join(T2pos, select(T1, intervals, fn_col), by = 'intervals')
  T2pos <- rowwise(T2pos)
  T2pos <- mutate(T2pos, limit = map_dbl(freq, fn_col))  # Apply the limit functions to the frequencies
  T2pos <- select(T2pos, freq, row, limit)

  # outside the limits
  npos <- setdiff(seq_along(ints), pos) # positions
  T2npos <- slice(T2, npos)
  T2npos <- mutate(T2npos, limit = NA_real_)   # There is no prescribed limits. Return NA
  T2npos <- select(T2npos, freq, row, limit)


  # Bind them and arrange into the original order.
  T2 <- bind_rows(
    select(T2npos, freq, row, limit), # Outside limits
    select(T2pos, freq, row, limit)  # within limits
  )
  T2 <- arrange(T2, row)

  return(pull(T2, limit))

}






#' Compute limits
#'
#' \code{log_limits_fn} creates the logarithmic limits, \code{const_limits_fn} computes
#' constant limits, \code{select_fn} select appropriate function based on the \code{type}
#' argument.
#'
#' @param start_freq numeric, start frequency
#' @param end_freq numeric, end frequency
#' @param start_val numeric, starting limit
#' @param end_val numeric, ending limit
#' @param type character, the type of the limit ("log" or "const")
#'
#' @return function to compute the limits given the frequency
#' @name limits_fun
#'
NULL



#' @rdname limits_fun
#' @keywords internal
log_limits_fn <- function(start_freq, end_freq, start_val, end_val) {
  N <- 10000
  x <- 10^seq(log10(start_freq), log10(end_freq), length.out = N)
  y <- seq(start_val, end_val, length.out = N)
  return(approxfun(x, y, method = "linear"))
}


#' @rdname limits_fun
#' @keywords internal
const_limits_fn <- function(start_freq, end_freq, start_val, end_val=start_val) {
  return(approxfun(c(start_freq, end_freq), c(start_val, start_val), method = "constant"))
}

#' @rdname limits_fun
#' @keywords internal
select_fn <- function(type, start_freq, end_freq, start_val, end_val) {
  if(type == "log") {
    return(log_limits_fn(start_freq, end_freq, start_val, end_val))
  } else {
    return(const_limits_fn(start_freq, end_freq, start_val, end_val))
  }
}


