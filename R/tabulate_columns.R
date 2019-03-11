#' Return table of responses to ordinal/nominal response questions
#'
#' This function returns a tidy tibble containing percentage tables
#'
#' @param x tibble of ordinal/nominal responses
#' @param weights (Optional) vector of weights
#' @param segments (Optional) vector or data frame of segment membership
#' @param filterNA Whether we remove NAs before calculating percentage
#' @param binary Are the columns binary responses (1 or TRUE)
#' @param multipleChoice Are the columns multiple choice and therefore should be grouped together.
#'
#' @return tidy tibble containing percentage table of responses
tabulate_columns <- function(x,weights=NULL,segments=NULL,filterNA=TRUE,binary=FALSE,multipleChoice=FALSE){

  if(sum(is.na(x))!=prod(dim(x))){

    if(multipleChoice){
      # The way in which we calculate mutliple choice percentages means we need to correct our percentage to take into account that users can make up to ncol(x) choices
      MCA <- ncol(x)
    }
    if(!is.null(do.call(rbind,lapply(x,function(y)attr(y,'label')))[,1])){
      labels <- tibble('Label'=do.call(rbind,lapply(x,function(y)attr(y,'label')))[,1],'Column'=colnames(x))
    }else{
      labels <- NULL
    }

    if(is.null(weights)){
      x$weights <- 1
      weightsLocation <- ncol(x)
    }else{
      if(!is.null(dim(weights))){
        weights <- weights[[1]]
      }
      x$weights <- weights
      weightsLocation <- ncol(x)
    }
    colnames(x)[weightsLocation] <- 'Weights'

    noSegment=FALSE
    if(is.null(segments)){
      x$segment <- ''
      segmentLocation <- ncol(x)
      colnames(x)[segmentLocation] <- 'Segment'
      noSegment=TRUE
    }else{
      segments <- mutate_all(segments,as.character)
      temp <- bind_cols(x,segments)
      segmentLocation <- (ncol(x)+1):ncol(temp)
      x <- gather(temp,'Segment','Membership',segmentLocation) %>%
        # filter(Membership==1) %>%# Because TRUE==1
        filter(!is.na(Membership))%>% # Removing missing segments JIC
        filter(Membership!='FALSE')%>%
        unite('Segment',c("Segment",'Membership'),sep =': ')%>%
        mutate('Segment'=gsub(': TRUE','',Segment))%>%
        suppressWarnings()
      segmentLocation <- grep('Segment',colnames(x))
    }

    if(multipleChoice){
      if(filterNA){
        count <- x %>%
          mutate(Weights=1)%>%
          gather('Column','Response',-Weights,-Segment) %>%
          filter(!is.na(Response))%>%
          group_by(Column,Segment,Response) %>%
          summarise('Count'=sum(Weights)) %>%
          split(.['Column']) %>%
          lapply(function(y) y %>%
                   spread(Response,Count,fill=0)%>%
                   gather('Response','Count',-(1:2)))%>%
          bind_rows %>%
          group_by(Segment,Response)%>%
          summarise(Count=sum(Count))%>%
          mutate(Column=sprintf('%s',colnames(x)[1]))%>%
          rename('N'='Count')%>%
          .[c('Segment','Column','Response','N')]

        out <- x %>%
          gather('Column','Response',-Weights,-Segment) %>%
          filter(!is.na(Response))%>%
          group_by(Column,Segment,Response) %>%
          summarise('Count'=sum(Weights)) %>%
          split(.['Column']) %>%
          lapply(function(y) y %>%
                   spread(Response,Count,fill=0)%>%
                   gather('Response','Count',-(1:2)))%>%
          bind_rows %>%
          group_by(Segment,Response)%>%
          summarise(Count=sum(Count))%>%
          mutate(Column=sprintf('%s',colnames(x)[1]),
                 Percentage=Count/sum(Count)*MCA*100)%>%# Because multiple choice
          .[c('Segment','Column','Response','Count','Percentage')]%>%
          left_join(count)

      }else{
        count <- x %>%
          mutate(Weights=1)%>%
          gather('Column','Response',-Weights,-Segment)%>%
          group_by(Column,Segment,Response) %>%
          summarise('Count'=sum(Weights)) %>%
          split(.['Column']) %>%
          lapply(function(y) y %>%
                   spread(Response,Count,fill=0)%>%
                   gather('Response','Count',-(1:2)))%>%
          bind_rows %>%
          group_by(Segment,Response)%>%
          summarise(Count=sum(Count))%>%
          rename('N'='Count')%>%
          mutate(Column=sprintf('%s',colnames(x)[1]))%>%
          .[c('Segment','Column','Response','N')]

        out <- x %>%
          gather('Column','Response',-Weights,-Segment)%>%
          group_by(Column,Segment,Response) %>%
          summarise('Count'=sum(Weights)) %>%
          split(.['Column']) %>%
          lapply(function(y) y %>%
                   spread(Response,Count,fill=0)%>%
                   gather('Response','Count',-(1:2)))%>%
          bind_rows %>%
          group_by(Segment,Response)%>%
          summarise(Count=sum(Count))%>%
          mutate(Column=sprintf('%s',colnames(x)[1]),
                 Percentage=Count/sum(Count)*MCA*100)%>%# Because multiple choice
          .[c('Segment','Column','Response','Count','Percentage')]%>%
          left_join(count)
      }
    }else{
      if(filterNA){
        count <- x %>%
          mutate(Weights=1)%>%
          gather('Column','Response',-Weights,-Segment) %>%
          filter(!is.na(Response))%>%
          group_by(Column,Segment,Response) %>%
          summarise('Count'=sum(Weights)) %>%
          split(.['Column']) %>%
          lapply(function(y) y %>%
                   spread(Response,Count,fill=0)%>%
                   gather('Response','Count',-(1:2)))%>%
          bind_rows %>%
          group_by(Segment,Column)%>%
          rename('N'='Count')%>%
          .[c('Segment','Column','Response','N')]

        out <- x %>%
          gather('Column','Response',-Weights,-Segment) %>%
          filter(!is.na(Response))%>%
          group_by(Column,Segment,Response) %>%
          summarise('Count'=sum(Weights)) %>%
          split(.['Column']) %>%
          lapply(function(y) y %>%
                   spread(Response,Count,fill=0)%>%
                   gather('Response','Count',-(1:2)))%>%
          bind_rows %>%
          group_by(Segment,Column)%>%
          mutate(Percentage=Count/sum(Count)*100)%>%
          .[c('Segment','Column','Response','Count','Percentage')]%>%
          left_join(count)
      }else{

        count <- x %>%
          mutate(Weights=1)%>%
          gather('Column','Response',-Weights,-Segment)%>%
          group_by(Column,Segment,Response) %>%
          summarise('Count'=sum(Weights)) %>%
          split(.['Column']) %>%
          lapply(function(y) y %>%
                   spread(Response,Count,fill=0)%>%
                   gather('Response','Count',-(1:2)))%>%
          bind_rows %>%
          group_by(Segment,Column)%>%
          rename('N'='Count')%>%
          .[c('Segment','Column','Response','N')]

        out <- x %>%
          gather('Column','Response',-Weights,-Segment)%>%
          group_by(Column,Segment,Response) %>%
          summarise('Count'=sum(Weights)) %>%
          split(.['Column']) %>%
          lapply(function(y) y %>%
                   spread(Response,Count,fill=0)%>%
                   gather('Response','Count',-(1:2)))%>%
          bind_rows %>%
          group_by(Segment,Column)%>%
          mutate(Percentage=Count/sum(Count)*100)%>%
          .[c('Segment','Column','Response','Count','Percentage')]%>%
          left_join(count)
      }
    }

    if(!is.null(labels)){
      out <- left_join(out,labels)%>%
        .[c('Segment','Label','Response','Count','Percentage','N')]
    }

    if(noSegment)
      out <- out %>%
      ungroup %>%
      dplyr::select(-Segment)

    if(binary){
      if('Yes' %in% out$Response){
        out <- out %>%
          filter(Response=='Yes')
      }else{
        out <- out %>%
          filter(Response==1|Response==TRUE) %>%
          mutate(Response='Yes')
      }
    }

    return(out)
  }
}

