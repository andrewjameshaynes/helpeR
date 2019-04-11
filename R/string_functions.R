#' Combine unquoted items into a vector of character strings
#'
#' @param ... Unquoted single word strings.
#' @details Works the same way that c() does for all types of data types. cs() allows you to quickly create vectors of single word strings without having to use quotes.
#' @return A vector of character strings.
#' @examples
#' > cs(a,b,a)
#' [1] "a" "b" "a"
#' > cs(Hello, green, 99)
#' [1] "Hello" "green" "99"

cs=function(...){
  as.character(substitute(list(...)))[-1]
}
