#' Reattach tabulate attributes to columns
#'
#' Should we lose tabulate attributes (i.e. reading in a file) we can reattach the parameters
#' using this function.
#'
#' @param x tibble or data frame
#' @param attributes tibble with labels and tabulate columns
#'
#' @return tibble with tabulate attributes
attach_attributes <- function(x,attributes){
  new <- suppressWarnings(
    tibble('column'=colnames(x))%>%
      left_join(attributes,by='column')%>%
      mutate_at(c('labels','tabulate'),function(x){ifelse(is.na(x),'',x)})
  )

  for(i in 1:ncol(x)){
    attr(x[[i]],'label') <- attributes[['labels']][i]
    attr(x[[i]],'tabulate') <- attributes[['tabulate']][i]
  }
  return(x)
}

