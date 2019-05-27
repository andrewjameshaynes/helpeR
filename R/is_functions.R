#' @name mod0
#' @rdname mod0
#'
#' @title Special case modulus functions
#'
#' @description Alternative mod function which returns the original value if base is 0, passed to is_decimal & is_natural functions
#'
#' @param x A numeric value
#' @param y A base for modulus function
#' @details A work around where you need modulus function to return the input value, not NaN
#' @return The remainder between x and y, x mod y
#' @examples
#' mod0(5, 2)
#' [1] 1
#' mod0(5, 0)
#' [1] 5
mod0<-function(x,y){
  if(is_zero(y, strict = T)){
    return(x)
  } else {
    return(x %% y)
  }
}


#' @name is_
#' @rdname is_
#'
#' @title Functions for boolean checks
#'
#' @description Functions to check for numerous different conditions
#'
#' @param x A number/vector to pass to a boolean check
#' @param strict Whether a value is equal to the actual value, or also equal to natural coercion value
#' @details A list of is_* checks to be passed to other functions.
#' @return A boolean, TRUE/FALSE
#' @examples
#' is_zero(0, strict=T)
#' [1] TRUE
#' is_natural(4)
#' [1] TRUE
#' is_decimal(-8.17)
#' [1] TRUE
#' is_zero(FALSE, strict=T)
#' [1] FALSE
#' is_natural(4.4)
#' [1] FALSE
#' is_decimal(-8)
#' [1] FALSE
is_zero<-function(x, strict=F){
  ifelse(strict,
    ifelse(is.numeric(x) & x==0, T, F),
    ifelse(x==0,T,F))
}

#' @describeIn is_ Check whether a number is a positive integer
is_natural<-function(x){
  if(is.numeric(x) == F | is_zero(x)){return(F)}
  ifelse(is.numeric(x) == T & x > 0, is_zero(mod0(x, floor(x))), FALSE)
}

#' @describeIn is_  Check to see if a number is a decimal, both positive and negative
is_decimal<-function(x){
  tmp = abs(mod0(x,floor(x)))
  ifelse(is.natural(tmp) | is_zero(tmp) ,F,T)
}


#
##' @describeIn is_ Check to see if a single value is char or factor
#is_discrete<-function(x){is.factor(x) | is.character(x)}

##' @describeIn is_ Check to see if a value is continuous (not discrete)
#is_continuous<-function(x){!is_discrete(x)}

