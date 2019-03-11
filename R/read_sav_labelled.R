#' Read in .sav file with labelled columns
#'
#' @param file file location
#'
#' @return tibble
read_sav_labelled <- function(file){
  out <- read_sav(file)
  labels <- unlist(lapply(out,attr,'label'))
  message('Read in')
  for(i in 1:ncol(out)){
    if(is.labelled(out[[i]])){
      out[[i]] <- haven::as_factor(out[[i]])
    }
    attr(out[[i]],'label') <- labels[i]
  }
  return(out)
}
