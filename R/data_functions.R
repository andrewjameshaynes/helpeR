#' Return the first N rows and columns of a data.frame or matrix
#'
#' @param data A data.frame, matrix or data type that has both rows and columns
#' @param rows The number of rows to be returned, similar to head(). Default value is 6.
#' @param cols The number of columns to be returned. Defaults to the same value as rows to maintain a square format.
#' @details A two dimensional version of head() that is useful for working with wide datasets. Unlike head() and tail() this is not a generic function and cannot be extended to other classes.
#' @return An object (usually) like data but generally smaller.
#' @examples
#' box(iris)
#' box(mtcars, rows = 10, cols = 3)
#'
#' box(matrix(0, nrow=3, ncol=3), 2)
box = function(data, rows = 6, cols = rows) {
  if(class(data) %in% c('data.frame', 'matrix'))
    head(data[, 1:min(cols, ncol(data))], rows)
  else
    stop(paste("Input of class", class(data), "is not supported. See ?box for details."))
}

#' Return the first N rows and columns of a data.frame or matrix
#'
#' @param data A data.frame, matrix or data type that has both rows and columns
#' @param rows The number of rows to be returned, similar to head(). Default value is 6.
#' @param cols The number of columns to be returned. Defaults to the same value as rows to maintain a square format.
#' @details A two dimensional version of head() that is useful for working with wide datasets. Unlike head() and tail() this is not a generic function and cannot be extended to other classes.
#' @return An object (usually) like data but generally smaller.
#' @examples
#' box(iris)
#' box(mtcars, rows = 10, cols = 3)
#'
#' box(matrix(0, nrow=3, ncol=3), 2)
box = function(data, rows = 6, cols = rows) {
  if(class(data) %in% c('data.frame', 'matrix'))
    head(data[, 1:min(cols, ncol(data))], rows)
  else
    stop(paste("Input of class", class(data), "is not supported. See ?box for details."))
}


#' Quickly check which columns of a dataset have NA values.
#'
#' @param data A data.frame, or other data that could contain NA values.
#' @details A function that gives a quick check of data to see where NA's are. Most useful for data.frames where the funciton will supply the number if NA's in each column. The output can be passed to the sum() function to check for total NA's.
#' @return Number of NA's in position of data.
#' @examples
#' check_na_cols(iris)
check_na_cols = function(data) sapply(data, function(x) sum(is.na(x)))

#' Frequency table as data.frame
#'
#' @param ... One or more objects that can be passed to table()
#' @param sort How the data should be sorted on frequency - ascending, descending or none.
#' @details Data.frame output of table with better column names and ability to sort on Freq column. Essentially creates crosstab table in long data format.
#' @return Sorted data.frame based either on first object, or by sort option object.
#' @examples
#' freq(iris$Species)
#' freq(c(1,2,3,1,1,1,1))
freq = function(..., sort = "none"){
  df = data.frame(table(...))
  colnames(df) = gsub(".*\\$", "", c(as.character(substitute(list(...)))[-1], "Freq"))

  if(tolower(sort) %in% c("asc", 'ascending'))
    df = df[order(df$Freq),]
  else if(tolower(sort) %in% c("desc", 'descending'))
    df = df[order(-df$Freq),]
  else if(tolower(sort) == "none")
    df = df
  else
    stop("Incorrect sort option, specify 'ascending', 'descending' or 'none'.")
  return(df)
}

#' Proportion table as data.frame
#'
#' @param ... One or more objects that can be passed to table()
#' @param sort How the data should be sorted on proportion count - ascending, descending or none.
#' @details Data.frame output of table with better column names and ability to sort on Prop column. Essentially creates crosstab proportions in long data format.
#' @return Sorted data.frame based either on first object, or by sort option object.
#' @examples
#' prop(iris$Species)
#' prop(iris$Species, iris$Petal.Length)
prop = function(..., sort = 'none'){
  df = data.frame(prop.table(table(...)))
  colnames(df) = gsub(".*\\$", "", c(as.character(substitute(list(...)))[-1], "Prop"))

  if(tolower(sort) %in% c("asc", 'ascending'))
    df = df[order(df$Prop),]
  else if(tolower(sort) %in% c("desc", 'descending'))
    df = df[order(-df$Prop),]
  else if(tolower(sort) == "none")
    df = df
  else
    stop("Incorrect sort option, specify 'ascending', 'descending' or 'none'.")
  return(df)
}

#' Contingency table creation
#'
#' @param var1 First object that can be passed to table()
#' @param var2 Second object that can be passed to table()
#' @param margin Margin, as passed to prop.table. See ?prop.table for more details.
#' @details Two variable cross tabulation of data. Output as a percentage to 1 decimal place.
#' @return Table object of the form form prop.table, but formatted to be a percentage to 1 decimal place.
#' @examples
#' cross_tab(mtcars$disp, mtcars$mpg)
#' cross_tab_df(mtcars$disp, mtcars$mpg)
cross_tab = function(var1, var2, margin = NULL){
  100*round(prop.table(table(var1, var2), margin), 3)
}
cross_tab_df = function(var1, var2, margin = NULL){
  data.frame(cross_tab(var1, var2, margin))
}

#' @name lunique
#' @rdname lunique
#'
#' @title The number of unique elements in an object
#'
#' @description Function to calculate the length of the numebr of unique elements in a data object.
#'
#' @details Just a super convenient wrapper for the length(unique(x)) combination. See ?unique for more details.
#' @return The number of unique elements
#' @examples
#' lunique(iris$Species)
#' [1] 3
#'
#' sapply(iris, lunique)
#' Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species
#' 35           23           43           22            3
lunique = function(x){
  length(unique(x))
}
