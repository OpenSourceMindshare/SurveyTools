#' Place name of list as attribute within the element
#'
#' Where we want the name of the list to be accessible within a loop or function, we can
#' use this function to place the name within the list element as an attribute.
#'
#' @param x list
#'
#' @return list with names as attributes
name_element <- function(x){
  for (i in 1:length(x)) {
    attr(x[[i]],'element_name') <- names(x)[i]
  }
  return(x)
}

