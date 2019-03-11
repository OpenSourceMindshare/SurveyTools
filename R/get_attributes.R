#' Obtain tabulate attributes
#'
#' Should we want to view the tabulate attributes as a tibble, in case of editing or
#' checking accuracy, we can use this function to extract them and store within a tibble.
#'
#' @param x list
#' @param export logical indicating whether we want to export the parameters into a csv file within the current working directory
#' @param filename string defaulting to "Survey Tools Parameters Sys.Date().csv"
#'
#' @return tibble of parameters
#'
#' @examples library(surveySimulate)
#' example <- simulateSurvey(100)
#' parameters <- get_attributes(example)
get_attributes <- function(x,export=FALSE,filename=sprintf('Survey Tools Parameters %s.csv',Sys.Date())){
  label <- do.call(rbind,lapply(x,attr,'label'))
  params <- do.call(rbind,lapply(x,attr,'tabulate'))
  if(is.null(label)){
    label <- data.frame('labels'=rep('',ncol(x)),'column'=colnames(x))
  }else{
    label <- label %>% as_tibble %>% set_colnames('labels') %>% mutate('column'=rownames(label))
  }

  if(is.null(params)){
    params <- data.frame('tabulate'=rep('',ncol(x)),'column'=colnames(x))
  }else{
    params <- params %>% as_tibble %>% set_colnames('tabulate') %>% mutate('column'=rownames(params))
  }
  out <- data.frame('column'=colnames(x))%>%
    left_join(label)%>%
    left_join(params)%>%
    dplyr::select('column','labels','tabulate')%>%
    mutate(labels=as.character(labels),
           tabulate=as.character(tabulate))

  if(export){
    out <- out %>%
      separate(labels,c('Section','Group','Question'),sep='\\|')

    if(filename %in% list.files(getwd())){
      filename <- gsub('\\.csv',paste('-',sum(grepl(filename,list.files(getwd())))+1,'.csv',sep=''),filename)
    }
    write_csv(out,filename)
  }else{
    return(as_tibble(out))
  }
}

