#' Combine unquoted items into a vector of character strings
#'
#' @param ... Unquoted single word strings.
#' @details Works the same way that c() does for all types of data types. cs() allows you to quickly create vectors of single word strings without having to use quotes.
#' @return A vector of character strings.
#' @examples
#' cs(a,b,a)
#' [1] "a" "b" "a"
#' cs(Hello, green, 99)
#' [1] "Hello" "green" "99"

cs=function(...){
  as.character(substitute(list(...)))[-1]
}

#' Concatenate a series of characters
#'
#' @param ... A vector, or vectors of strings
#' @param sep A delimeter on which to join strings
#' @details A wrapper for paste(..., sep), called concat() to avoid confusion with collapse().
#' @return A vector of character strings.
#' @examples
#' concat("species", "yellow")
#' [1] "speciesyellow"
#' concat(c("species", "yellow"), c("species", "yellow"), sep = ' ')
#' [1] "species species" "yellow yellow"
concat =  function(..., sep = ''){
  paste(..., sep = sep)
}

#' Collapse a series of strings
#'
#' @param ... A vector, or vectors of strings
#' @param sep A delimeter on which two collapse strings
#' @details A wrapper for paste0(..., collapse = <option>). Built to be more clear than paste0() when collapsing strings, and to avoid the issue of no partial string match in the paste() functions for the colllapse option.
#' @return A single string of collapsed input
#' @examples
#' collapse(c("species", "yellow"), c("species", "yellow"), sep = ' ')
#' [1] "speciesspecies yellowyellow"
#' collapse(c("species", "yellow"))
#' [1] "speciesyellow"
collapse<-function(..., sep =''){
  paste0(..., collapse=sep)
}

#' Split strings to tokens
#'
#' @param x A string or vector of strings
#' @param split A delimeter on which to split strings, defaults to an empty string.
#' @details An alternative to strsplit() that returns a vector if a single string, or a list if multiple. Named from c++ function to turn strings into tokens.
#' @return Either a single vector of characters, or a list of split strings.
#' @examples
#'strtok("hello")
#' [1] "h" "e" "l" "l" "o"
#' strtok(c("hello","loko"))
#' [[1]]
#' [1] "h" "e" "l" "l" "o"
#'
#' [[2]]
#' [1] "l" "o" "k" "o"
strtok<-function(x, split = ''){
  split_ = strsplit(as.character(x), split = split)
  if(length(split_) == 1)
    return_ = el(split_)
  else
    return_ = split_
  return(return_)
}


