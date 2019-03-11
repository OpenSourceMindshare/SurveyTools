#' Import tabulate attributes from a csv file
#'
#' If we have exported tabulate attributes for editing in a .csv file then we can use this function to import them and also append them to a specified data structure.
#'
#' @param fileLocation string indicating location of tabulate attributes file
#' @param data data structure to apply the parameters to
#'
#' @return tibble containing tabulate attributes
import_attributes <- function(data,fileLocation){
  parameters <- read_csv(fileLocation) %>%
    mutate(Section=gsub('\\|',':',Section),
           Group=gsub('\\|',':',Group),
           Question=gsub('\\|',':',Question),
           tabulate=ifelse(is.na(tabulate),'',tabulate)) %>%
    unite('labels',Section:Question,sep='|')

  out <- data %>%
    attach_attributes(parameters)
  return(out)
}


