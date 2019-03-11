#' Append columns so they retain tabulate attributes
#'
#' Since tabulate utilises attributes assigned to columns, traditional methods of combining
#' may lose these attributes. This function will combine two structures together and reapply
#' the labels and tabulate attributes.
#'
#' @param x tibble or data frame
#' @param y tibble or data frame
#'
#' @return tibble with tabulate attributes intact
append_columns <- function(x,y){
  #bind_cols does this as default
  labels.x <- unlist(lapply(lapply(x,attr,'label'),function(z){z[is.null(z)]='';z})) %>% as.character
  labels.y <- unlist(lapply(lapply(y,attr,'label'),function(z){z[is.null(z)]='';z})) %>% as.character
  labels <- as.character(c(labels.x,labels.y))

  tabulateParam.x <- unlist(lapply(lapply(x,attr,'tabulate'),function(z){z[is.null(z)]='';z})) %>% as.character
  tabulateParam.y <- unlist(lapply(lapply(y,attr,'tabulate'),function(z){z[is.null(z)]='';z})) %>% as.character
  tabulateParam <- as.character(c(tabulateParam.x,tabulateParam.y))

  out <- attach_attribute_vectors(bind_cols(x,y),labels,tabulateParam)

  return(out)
}

