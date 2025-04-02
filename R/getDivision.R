# Virginia Tech CRM2.0 application funs
getDivision <- function(x, prov = FALSE) {
  x <- trimws(x)
  
  # first pass
  y <- ifelse(nchar(x) == 0, "", ifelse(
    nchar(x) == 4,
    substr(x, 1, 3),
    ifelse(nchar(x) == 7, substr(x, 1, nchar(x) - 3), substr(x, 1, nchar(x) - 2))
  ))
  
  if (prov) {
    return(y)
  } 
  
  # finish trimming
  z <- ifelse(nchar(y) != 0, paste0(substr(y, 1, nchar(y) - 1), "0"), "")
  
  return(z)
}