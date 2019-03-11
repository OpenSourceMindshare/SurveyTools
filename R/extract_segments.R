#' Extract segment columns from tabulate data structure
#'
#' @param x tabulate data structure with tabulate parameters
#'
#' @return tibble with columns labelled as "segment" in tabulate parameters
extract_segments <- function(x){
  params <- get_attributes(x)%>%
    filter(tabulate=='segment')

  if(nrow(params)>0){
    x %>%
      .[params$column]
  }else{
    stop('No segment columns identified')
  }
}
