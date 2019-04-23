#' @name not_in
#' @rdname not_in
#'
#' @title Value Matching
#'
#' @description Functions to match (or not match) data.
#'
#' @details %not in% is the human readable version of !(a %in% b).
#' @return TRUE or FALSE value whether an element is in the object or not.
#' @examples
#'  1 %not in% c(2,3,4)
#' [1] TRUE
`%not in%` = Negate(`%in%`)

#' @name like
#' @rdname like
#'
#' @title String Matching
#'
#' @description Functions to match easily match strings, based on SQL wildcards.
#'
#' @details %like% is an infix version of the like operator in SQL. Provides an easier, more readable wrapper for the grepl function with a single pattern. %not like% provides the opposite functionionality.
#' @return TRUE or FALSE value whether an element is like and element in an object or not.
#' @examples
#' c("bear", "beat", "boat") %like% "bea"
#' [1]  TRUE  TRUE FALSE
`%like%` = function(string, pattern){
  grepl(pattern = pattern, x = string)
}

`%not like%` = Negate(`%like%`)
