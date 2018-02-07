############################################
# Miscellaneous functions for Andrew
# Code golf, useful, etc
#
#
############################################

##: cs() - works like c() but for doesnt require character vectors, embed with colnames
cs=function(...){
  as.character(substitute(list(...)))[-1]
}

##: box() - works like head() but also limits columns, useful for wide data
box = function(data, rows = 6, cols = rows) {
  head(data[, 1:min(cols, ncol(data))], rows)
}

##: intToDigits() - separates an integer into its individuals digits as a vector
intToDigits<-function(x){
  return(as.integer(el(strsplit(as.character(x),''))))
}

##: charToElements() - separates a string into individual elements in a vector
charToElements<-function(x){
  return(el(strsplit(x,'')))
}

##: collaspe() - wrapper function for paste0(...,collapse='')
collapse<-function(x, sep =''){
  paste0(x,collapse=sep)
}

##: intReverse() - function to reverse an integer
intReverse<-function(x){
  return(strtoi(collapse(rev(intToDigits(x)))))
}

##: charReverse() - like rev() but on a single element
charReverse<-function(x){
  collapse(rev(charToElements(x)))
}

##is.zero() - checks if a numeric is zero
isZero<-function(x){
  ifelse(x==0,T,F)
}

##: uncollapse() - wrapper for charToElements and numericToDigits/replaces el(strsplit(x,''))
uncollapse<-function(x){
  if(is.character(x)){return(charToElements(x))}
  if(is.numeric(x) & is.natural(x)){intToDigits(x)}
}

##: tableDF - wrapper function that turns the table you asked for into a dataframe
tableDF<-function(...,data){
  data.frame(table(iris$Sepal.Length))
  table(paste0(collapse(paste0('iris$',f(e,j,h,y),',')),'data'))
}

##: convertClass - convert data of class 1 to class 2, error if doesn't follow R coercion rules
convertClass <- function(object1, object2){
  logic=c("logical", "integer", "numeric", "complex", "character", "list")

  ifelse(match(class(object1), logic) < match(class(object2), logic),
         eval(parse(text=paste0('as.',class(object2),"(",object1,")"))),
         warning("convertClass() cannot convert type ", class(object1), " to ", class(object2)))
}

##: find - similar to SAS find() function, finds target substring within string. Target can be a single value or a vector, returning all occurences or zero if not found
find<-function(string,target){
  position<-which(uncollapse(string) %in% target)
  ifelse(length(position)>0,return(position),0)
}

##: findFirst - the first occurence of the find() function
findFirst<-function(string,target){
  min(find(string,target))
}

##: findNth - find the nth occurence of the find() function, n may be scalar or vector
findNth<-function(string, target, n){
  find(string, target)[n]
}

##: which_() - like which() but just reduced syntax
which_<-function(expr){
  elements = as.character(substitute(expr))
  func=elements[1]
  call = as.character(elements[2])
  data = substr(call,1,find(call,"$")-1)
  value = as.character(elements[3])

  eval(parse(text=paste0(data,"[",call,func,"'",value,"'",",]")))
}

##: upcaseFirstLetter() - for a single word, will capitalise first letter. Stop words are to be excluded

upcaseFirstLetter = function(word, stopWords = c()){
  word = tolower(word)
  ifelse(!word %in% stopWords, paste0(toupper(substring(word,1,1)),substring(word,2)), word)
}

##: stringToWords() - split a string into vector of words based on a delimiter

stringToWords<-function(x, sep=' '){
  return(el(strsplit(x, split = sep)))
}

##: goodColumnNames() - Capitalises first letter in each word, except stop words. Use delim to separate word list

goodColumnNames=function(string, stopWords=c(), sep= ' '){
  upper = sapply(stringToWords(string, sep = sep), upcaseFirstLetter, stopWords = stopWords, USE.NAMES = FALSE)
  collapse(upper, sep=' ')
}

##: goodLabelNames() - Capitalises first letter in each word unless word is 3 or less letters in size, then leaves as is
goodLabelNames=function(string, stopWords=c(), sep= '_'){
  words<-stringToWords(string, sep = sep)
  for(i in 1:length(words)){
    index<-which(nchar(words)>3)
    if(i %in% index){
      words[i]<-upcaseFirstLetter(words[i],stopWords = stopWords)
    }
  }

  collapse(words, sep=' ')
}

##: read.some() - works just like read.table,
##: but allows you to remove unwanted columns
##: Also used comma separator and header as default

read.some<-function(file,removeCols, sep=",", header = T,...){
  temp<-read.table(file,nrow=20,sep,header=T)
  cols = rep(NA, length(colnames(temp)))
  cols[match(removeCols,colnames(a))] = "NULL"
  read.table(file, colClasses = cols, sep, header = T, ...)
}

##: Alternative mod function which returns the original value if base is 0, passed to is.decimal & is.natural functions
mod0<-function(x,y){
  if(isZero(y)){
    return(x)
  } else {
    return(x %% y)
  }
}

##is.natural() - checks to see if a number is both whole, and > 0
is.natural<-function(x){
  if(is.numeric(x) == F | isZero(x)){return(F)}
  ifelse(is.numeric(x) == T & x > 0, isZero(mod0(x, floor(x))), FALSE)
}

##: is.decimal() - checks to see if a number is a decimal, both positive and negative
is.decimal<-function(x){
  tmp = abs(mod0(x,floor(x)))
  ifelse(is.natural(tmp) | isZero(tmp) ,F,T)
}
