#' Construct indexed tables from a tabulate generated table
#'
#' DESCRIPTION
#'
#' @param x Table generated from tabulate
#' @param baseSegment Segment name to treat as base for indexing
#' @param order Do we want to order the indexed data by segment in order of index extremity
#'
#' @return tidy tibble with indexed and classified responses
index_table <- function(x,baseSegment,order=TRUE){
  formatted <- gather(x,'Type','Value',c('Count','N','Percentage'))%>%
    spread(Segment,Value,fill=0)%>%
    gather('Segment','Value',colnames(.)[!colnames(.)%in%colnames(formatted)])
  perc0 <- formatted %>%
    filter(Type=='Percentage')%>%
    dplyr::select(-Type)%>%
    spread(Segment,Value)

  count0 <- formatted %>%
    filter(Type=='Count')%>%
    dplyr::select(-Type)%>%
    spread(Segment,Value)

  n0 <- formatted %>%
    filter(Type=='N')%>%
    dplyr::select(-Type)%>%
    spread(Segment,Value)

  index0 <- bind_cols(perc0[c('Label','Response')],
                      perc0 %>%
                        dplyr::select(-Response,-Label)%>%
                        mutate_all(.funs=function(x) x/.[[baseSegment]])*100
  )

  perc <- perc0 %>%
    gather('Segment','Percentage',-(1:2))
  count <- count0 %>%
    gather('Segment','Count',-(1:2))
  index <- index0 %>%
    gather('Segment','Index',-(1:2))
  n <- n0 %>%
    gather('Segment','N',-(1:2))


  classified <- index %>%
    mutate('Classified'=ifelse(Index<=80,'Under',ifelse(Index>=120,'Over','Neither')))

  distinct <- classified %>%
    group_by(Label,Response,Classified)%>%
    summarise(Distinct0=n())%>%
    mutate('Distinct0'=ifelse(Distinct0==1,'Unique','Non-Unique'))%>%
    unite('Distinct',c(Distinct0,Classified),sep=': ',remove = FALSE)%>%
    mutate('Distinct'=ifelse(!is.na(Classified),ifelse(grepl('Neither',Distinct),'Neither',Distinct),NA))%>%
    dplyr::select(-Distinct0)

  indexDistinct <- left_join(classified,distinct,by=c('Label','Response','Classified'))

  base <- perc0 %>%
    dplyr::select('Label',"Response",baseSegment)
  colnames(base)[3] <- 'Base Percentage'

  merged <- perc %>%
    left_join(count,by=c("Label","Response","Segment"))%>%
    left_join(n,by=c("Label","Response","Segment"))%>%
    left_join(indexDistinct,by=c("Label","Response","Segment"))%>%
    left_join(base,by=c("Label","Response"))

  if(order){
    out <- merged %>%
      mutate('IndexAbs'=ifelse(abs(Index-100)==100,0,abs(Index-100)))%>%
      arrange(Segment,desc(IndexAbs),desc(Distinct),desc(Index))
  }else{
    out <- merged
  }
  return(out)
}

