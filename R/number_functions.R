#' @name round_nearest
#' @rdname round_nearest
#'
#' @title Round numbers to the nearest base
#'
#' @description Functions to round a number to to the nearest base using either nearest value, ceiling or floor methods
#'
#' @param x A number/vector to round, can be integer or float
#' @param base A base to round to, can be integer or float
#' @details Round a number to the nearest base, e.g, round a number to the nearest 500 or 45
#' @return A numeric value or a vector of round numeric values.
#' @examples
#' round_nearest(350, 500)
#' [1] 500
#' round_nearest(19.4, 10.1)
#' [1] 20.2
#' round_up(350, 500)
#' [1] 500
#' round_down(350, 500)
#' [1] 0
round_nearest <- function(x, base){
  base * round(x / base)
}
#' @describeIn round_nearest Round a number up to the nearest specified base
round_up <- function(x, base) {
x + (base - x %% base)
}
#' @describeIn round_nearest Round a number down to the nearest specified base
round_down <-function(x, base){
  x - (x %% base)
}


#' @name integer_reverse
#' @rdname integer_reverse
#'
#' @title integer_reverse
#'
#' @description Reverse an integer in place
#'
#' @param x A integer or numeric that can be coerced to an integer
#' @details Reverses an integer x in place, for both positive and negative values. Does not work for vectors or non-integer data such as floats
#' @return A rounded integer value
#' @examples
#'integer_reverse(123)
#' [1] 321
#' integer_reverse(-123)
#' [1] -321
integer_reverse = function(x){
  if(all(x%%1 == 0 & length(x) == 1))
    if(sign(x)==1)
      as.integer(collapse(rev(strtok(x))))
    else
      -as.integer(collapse(rev(strtok(abs(x)))))
  else
    stop("Input is not a single integer")
}


