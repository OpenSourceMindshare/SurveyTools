#' Construct tables from a tabulate data structure.
#'
#' Taking a data structure with labels and parameters this function will return a tidy tibble that is a breakdown of responses to each question within the structure. These tables can be weighted or segmented by providing the columns and can also contain a Base column which is useful for indexing.
#'
#' @param x tabulate data structure with labels and tabulate parameters
#' @param weights weights column
#' @param segments segments column(s)
#' @param filterNA boolean indcating whether NA's are filtered prior to percentage calculation
#' @param addBase boolean indicating whether a column "Base" should be added
#'
#' @return Tidy tibble containing the response to questions
tabulate_data <- function(x,weights=NULL,segments=NULL,filterNA=TRUE,addBase=FALSE){

  parameters <- get_attributes(x) %>%
    separate(labels,c('Section','Group','Question'),sep='\\|',remove = FALSE)%>%
    mutate('col'=1:n())%>%
    filter(tabulate%notin%c('','segment','weight'))%>%
    filter(!is.na(tabulate))%>%
    unite('Split',c(Section,Group,tabulate),sep='_',remove=FALSE)

  # Add segment size.
  sizes <- NULL

  if(!is.null(segments)){
    colnames(segments) <- get_attributes(segments)$labels

    # If there are 2 unique elements (0 and 1) then treat as logical
    if(addBase){
      segments <- lapply(segments%>%mutate('Base'=TRUE),function(x){
        if(length(unique(x))==2&sum(c(0,1)%in%x)){
          x <- as.logical(x)
        }
        return(x)
      }) %>%
        bind_cols %>%
        attach_attribute_vectors(c(get_attributes(segments)$labels,'Base'),c(get_attributes(segments)$tabulate,'segment'))
    }else{
      segments <- lapply(segments,function(x){
        if(length(unique(x))==2&sum(c(0,1)%in%x)){
          x <- as.logical(x)
        }
        return(x)
      }) %>%
        bind_cols %>%
        attach_attribute_vectors(get_attributes(segments)$labels,get_attributes(segments)$tabulate)
    }
    if(is.null(weights))
      weights <- tibble('weights'=rep(1,nrow(x)))
    segments <- mutate_all(segments,as.character)
    temp <- bind_cols(weights,segments)
    y <- gather(temp,'Segment','Membership',-1) %>%
      # filter(Membership==1) %>%# Because TRUE==1
      filter(!is.na(Membership))%>% # Removing missing segments JIC
      filter(Membership!='FALSE')%>%
      unite('Segment',c("Segment",'Membership'),sep =': ')%>%
      mutate('Segment'=gsub(': TRUE','',Segment))%>%
      suppressWarnings()
    colnames(y)[1] <- 'Weights'
    sampleSize <- sum(weights)
    sizes <- y %>% group_by(Segment)%>%
      mutate(N=1)%>%
      summarise('Count'=sum(Weights),
                'N'=sum(N))%>%
      ungroup%>%
      mutate(
        'Percentage'=Count/sampleSize*100,
        'Label'='Information|-|Segments',
        'Response'='Size')
  }

  tables <- suppressMessages(lapply(split(parameters,parameters$Split),
                                    function(y,x,weights,segments,filterNA){
                                      tabulate_columns(x[y$col],weights = weights,segments = segments,filterNA=filterNA,binary=attr(x[[y$col[1]]],'tabulate')=='binary',multipleChoice=attr(x[[y$col[1]]],'tabulate')=='multiple_choice')
                                    },x=x,weights=weights,segments=segments,filterNA=filterNA) %>%
                               bind_rows)

  if(!is.null(sizes)){
    tables <- bind_rows(sizes %>% select_(.dots=colnames(tables)),tables)
  }

  return(tables)

}

