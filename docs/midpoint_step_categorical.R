#How to "interpolate" categorical variables so they change at the midpoint between non-missing values?
#given:
x <- c("A", NA,  "B", NA,  NA,  "C", NA,  NA,  NA,  "D", NA,  NA,  NA,  NA,  "E", NA,  NA,  "C", NA)
#desired outcome:
y <- c("A", "A", "B", "B", "C", "C", "C", "C", "D", "D", "D", "D", "E", "E", "E", "E", "C", "C", "C")

#y is equivalent to:
c(
  rep(NA, 1),
  rep("A", 2),
  rep("B", 2),
  rep("C", 4),
  rep("D", 4),
  rep("E", 4),
  rep("C", 3)
  
)
#how do we make this?
step_interp <- function(x) {
  
  x_non_nas <- x[!is.na(x)]
  i <- seq_along(x)
  
  # index at which each non-missing value is
  i_non_nas <- i[!is.na(x)]
  
  # count leading NAs
  leading_NAs <- i_non_nas[1] - 1
  
  #this is the number of rep()s on the right side (not including the non-missing value)
  right <- floor((lead(i_non_nas) - i_non_nas) / 2)
  right[is.na(right)] <- 0
  
  #number of rep()s to the left, including the non-missing value
  left <- ceiling((i_non_nas - lag(i_non_nas)) / 2)
  left[is.na(left)] <- 1
  
  times <- left + right
  
  #extrapolate trailing NAs
  #reps of last value gets adjusted so sum(times) adds up to the total length of the input
  times[length(times)] <- length(x) - (sum(times) - times[length(times)] + leading_NAs)
  
  x_interp <- map2(x_non_nas, times, rep) |> list_c()
  
  #add back leading NAs
  c(rep(NA, leading_NAs), x_interp)
}

x_interp <- step_interp(x)
all.equal(y, x_interp)

#does it work with leading NAs?

x_interp_leading <- step_interp(c(NA, x))
is.na(x_interp_leading[1])
all.equal(x_interp_leading, c(NA, y)) #nope!

#BOO YAH!

