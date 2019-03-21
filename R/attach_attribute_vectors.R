#' Reattach tabulate attribute vectors to columns
#'
#' Should we lose tabulate attributes (i.e. reading in a file) we can reattach the parameters
#' using this function.
#'
#' @param x tibble or data frame
#' @param labels labels to reattach
#' @param tabulateParams tabulate parameters to reattach
#'
#' @return tibble with tabulate attributes
 attach_attribute_vectors <- function(x,labels,tabulateParams){
   for(i in 1:ncol(x)){
     attr(x[[i]],'label') <- labels[i]
     attr(x[[i]],'tabulate') <- tabulateParams[i]
   }
   return(x)
 }

#
