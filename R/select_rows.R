#' Select rows so they retain tabulate attributes
#'
#' Since tabulate utilises attributes assigned to columns, traditional methods of selecting rows
#' will lose these attributes. This function will returns the selected rows with tabulate attributes intact.
#'
#' @param x tibble or data frame
#' @param index rows we wish to select
#'
#' @return tibble with tabulate attributes
select_rows <- function(x,index){
  labels <- unlist(lapply(lapply(x,attr,'label'),function(z){z[is.null(z)]='';z})) %>% as.character
  tabulateParams <- unlist(lapply(lapply(x,attr,'tabulate'),function(z){z[is.null(z)]='';z})) %>% as.character

  out <- attach_attribute_vectors(x[index,],labels,tabulateParams)
  return(out)
}

