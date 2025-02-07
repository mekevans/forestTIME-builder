#' Linear intrpolation and extrapolation
#'
#' Performs linear interpolation and, optionally, extrapolation.  Interpolation
#' is used for annualizing tree measurements such as `DIA`, `HT`, and
#' `ACTUALHT`.  Extrapolation is necessary for trees that have `NA`s recorded in
#' their first inventory marked dead because we want to extrapolate their growth
#' from the last time they were inventoried a live to an estimated mortality
#' year.
#' 
#' @param x numeric; an x variable, usually `YEAR` in forestTIME-builder
#' @param y numeric; the variable to be interpolated/extrapolated
#' @param extrapolate logical; perform extrapolation if possible?
#' @author Eric R.
#' @returns numeric vector
#' @export
#'
#' @examples
#' x <- 1:7
#' y <- c(2, NA, 5, 6, NA, NA, NA)
#' inter_extra_polate(x = x, y = y)
inter_extra_polate <- function(x, y, extrapolate = TRUE) {
  if (sum(!is.na(y)) < 2){
    return(y)
  } else {
    #first interpolate
    interpolated <- approx(x, y, xout = x)$y
  }
  #then extrapolate trailing NAs if needed
  if (isFALSE(extrapolate)) {
    return(interpolatedd)
  } else {
    if (all(!is.na(interpolated))) {
      return(interpolated)
    } else {
      extrapolated <- 
        Hmisc::approxExtrap(
          x = x[!is.na(interpolated)],
          y = interpolated[!is.na(interpolated)],
          xout = x[is.na(interpolated)]
        )$y
      return(c(interpolated[!is.na(interpolated)], extrapolated))
    }
  }
}

