#' Interpolate categorical variables to switch at midpoint
#'
#' Categorical variables like `DECAYCD` can't be linearly interpolated between
#' inventory years.  Instead, we assume they switch values at the midpoint
#' (rounded down) between non-missing values.  Trailing `NA`s are replaced with
#' the last non-`NA` value and leading `NA`s are returned as-is.
#'
#' @param x a vector
#'
#' @returns a vector with no `NA`s
#' @export
#' @returns a vector
#' @examples
#' step_interp(c(NA, NA, "A", NA, NA, NA, "B", NA, NA, NA, NA, "C", NA))
step_interp <- function(x) {
  if (all(is.na(x))) {
    return(x)
  }
  x_non_nas <- x[!is.na(x)]
  i <- seq_along(x)

  # index at which each non-missing value is
  i_non_nas <- i[!is.na(x)]

  # count leading NAs
  leading_NAs <- i_non_nas[1] - 1

  #number of rep()s to the left, including the non-missing value
  left <- floor((i_non_nas - dplyr::lag(i_non_nas)) / 2)
  left[is.na(left)] <- 0

  #this is the number of rep()s on the right side (not including the non-missing value)
  right <- ceiling((dplyr::lead(i_non_nas) - i_non_nas) / 2)
  right[is.na(right)] <- 1

  times <- left + right

  #extrapolate trailing NAs
  #reps of last value gets adjusted so sum(times) adds up to the total length of the input
  times[length(times)] <- length(x) -
    (sum(times) - times[length(times)] + leading_NAs)

  x_interp <- purrr::map2(x_non_nas, times, rep) |> purrr::list_c()

  #add back leading NAs
  c(rep(NA, leading_NAs), x_interp)
}
