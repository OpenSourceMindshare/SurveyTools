#' Append rows so they retain tabulate attributes
#'
#' Since tabulate utilises attributes assigned to columns, traditional methods of combining
#' will lose these attributes. This function will combine two structures together and reapply
#' the labels and tabulate attributes.
#'
#' @param x tibble or data frame
#' @param y tibble or data frame
#'
#' @return tibble with tabulate attributes
append_rows <- function(x,y){
  labels <- unlist(lapply(lapply(x,attr,'label'),function(z){z[is.null(z)]='';z})) %>% as.character
  tabulateParams <- unlist(lapply(lapply(x,attr,'tabulate'),function(z){z[is.null(z)]='';z})) %>% as.character

  out <- attach_attribute_vectors(bind_rows(x,y),labels,tabulateParams)
  return(out)
}

