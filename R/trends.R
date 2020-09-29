#' gets changepoints from sequence  trend
#'
#' @param df
#' @param Q The maximum number of changepoints to search for using the "BinSeg" method, see changepoint::cpt.mean
#' @param minseg minimum lenth of segment
#' @param method  Choice of "AMOC", "PELT", "SegNeigh" or "BinSeg", see changepoint::cpt.mean
#'
#' @return
#' @export
#'
#' @examples
get_breaks <- function(df , Q = 16, minseg = 12, method="BinSeg"){

  my.inds <- which(!is.na(df$trend.grad))
  my.grad <- df$trend.grad[!is.na(df$trend.grad)]
  my.cpt  <- changepoint::cpt.mean( my.grad ,penalty="Manual",
                                    pen.value=0.025,
                                    method="BinSeg",
                                    Q=Q,
                                    test.stat="Normal",
                                    class=TRUE,
                     param.estimates=TRUE,
                     minseglen=minseg)
  #my.cpt
  my.cpts1 <- sort(unique(as.numeric(my.cpt@cpts.full))) #extratc and sort indexes of changepoints

  my.dates <- df$date[my.inds[my.cpts1[ !is.na(my.cpts1)]]]

  my.dates}



# add_groups<-function(df, groups, col="date"){
#
#   groups<- groups[order(groups)]
#
#   my.col<- which(names(df)==col)
#   my.vec<- df[ ,my.col]
#
#   df$group <- 1
#   for (i in 2:(length(groups)+1)){
#     df$group[my.vec > groups[i-1]] <- i
#   }
#   df$group<- factor(df$group )
#   df
# }


#' Title
#'
#' @param groups
#' @param ...
#' @param df
#'
#' @return
#' @export
#'
#' @examples
add_groups<-function(df,  groups, ...){
  groups<-   groups[order(groups)]
  my.vec <-  eval(substitute(...), df, parent.frame())
  if  (class(my.vec) !=class(groups)) {
    print( paste0("NOTE: class of groups (",class(groups),") and ", substitute(...), " (", class(my.vec),") differ"))
    print("      only one `group` returned")
    df$group <- "1"
  } else{

    df$group <- "1"
    for (i in 2:(length(groups)+1))
      df$group[my.vec > groups[i-1]] <-as.character(i)

  }
  return(df)
}

#' Title
#'
#' @param df
#' @param index
#' @param order
#' @param centre
#'
#' @return
#' @export
#'
#' @examples
add_trends <- function(df, index=3, order=12, centre=T) {
  if (is.character(index)) index = which(names(df)==index)
  df$trend <- forecast::ma(df[,index], order = order, centre = centre) # moving average of trend
  #central differencing
  df$trend.grad <- c(NA,NA, (diff(df$trend)/tail(df$trend,-1) + diff(df$trend)/head(df$trend,-1))/2 )# gradient of trend

  return(df)
}


get_data_trends <- function(my.data.source ){
  read_data(my.data.source, data=T) %>%
    mutate(  date=decimal_date(cdate),source="flask" ) %>%
    add_trends() ->
    my.x
  return(my.x)
}

#####



#' Title
#'
#' @param my.data
#' @param ...
#' @param nq
#' @param minseg
#' @param top
#'
#' @return
#' @export
#'
#' @examples
get_changepoints<- function(my.data, ..., nq = 40 ,   minseg = 12,top=12){
  e <- eval(substitute(...), my.data, parent.frame())
  # e.narm <- e #[!is.na(e)] #remove NA's
  my.cpt <-  changepoint::cpt.mean( e  , penalty="Manual",
                                    pen.value=0.025,
                                    method="BinSeg",
                                    Q=nq,
                                    test.stat="Normal",
                                    class=TRUE,
                                    param.estimates=TRUE,
                                    minseglen=minseg)
  #print(my.cpt@cpts.full)
  my.cpts.inds <- (unique(as.numeric(my.cpt@cpts.full)) %>% subset(!is.na(.)))[1:top]
  #my.cpts.inds.1 <- e[e==e.narm[my.cpts.inds]]
  return(my.cpts.inds)
}

#' Title
#'
#' @param my.df
#' @param my.dates
#' @param my.ranges
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
group_summary <- function(my.df=my.x, my.dates, my.ranges, ... ){

  add_groups(as.data.frame(my.df), my.dates,  ...) %>%
    group_by(group) %>%
    summarise(date=mean(date),
              cdate=mean(cdate),
              value=mean(trend, na.rm=T),
              trend.grad=annual.percentage.factor*mean(trend.grad, na.rm=T) ) %>%
    arrange(cdate) %>%
    mutate(range= my.ranges ,
           labels=paste0(round(trend.grad,2), "% p.a."),
           nlabels=paste0(round(trend.grad,2),"%" ))
}

plot_cpts_group <-function(my.data, title=c("Mace head", "US gas"),my.date=m.date, se=T){

  p.cpt<- ggplot(my.data , aes(trend.grad.x, trend.grad.y, colour=grouping,label=range, fill=grouping))+
    geom_smooth(method="lm", alpha=.1, fullrange=T, size=.2, se=se)+
    ggrepel::geom_text_repel(nudge_y=1, size=2.5, segment.size=.25 ,  family=ffamily)+
    geom_point(size=4, colour="white")+
    geom_point(size=3.3)+
    scale_colour_manual(values =c("royalblue4","firebrick2"))+
    scale_fill_manual(values =c(  "royalblue4","firebrick2"))+
    theme(legend.position="none")+
    labs(x=paste(title[1], "ann. growth rate"),
         y=paste(title[2], "ann. growth rate"))

  cpt.models <- my.data %>%
    split(.$grouping) %>%
    map(~lm(.$trend.grad.x ~ .$trend.grad.y, data=. ))

  cptfits <-cpt.models %>% map(summary) %>%  map_dbl(~.$adj.r.squared)
  return (p.cpt +
            geom_text( data=my.data[1,], x=ggplot_normx(p.cpt,.05), y=ggplot_normy(p.cpt,.9),
                       label= paste0("adj. r-squared = ",  round(cptfits[[2]],3),  ", > ",my.date) ,
                       colour="firebrick2",
                       size=5,
                       hjust=0 , family=ffamily)+
            geom_text( data=my.data[1,], x=ggplot_normx(p.cpt,.05), y=ggplot_normy(p.cpt,.83),
                       label= paste0("adj. r-squared = ",  round(cptfits[[1]],2), ", < ",my.date) ,
                       colour="royalblue4",
                       size=5,
                       hjust=0 , family=ffamily)
  )
}


#' Title
#'
#' @param my.lag
#' @param df
#' @param r
#'
#' @return
#' @export
#'
#' @examples
lag_x <- function(my.lag=0,  df=my.x.y.spread,r=T){
  names(df) <-c("date", "data1", "data2","grouping")
  # print(names(df))
  if (my.lag>0) df$data2<- dplyr::lead(df$data2, abs(my.lag))
  if (my.lag<0)  df$data2 <- dplyr::lag(df$data2, abs(my.lag))
  models <- df %>%
    split(.$grouping) %>%
    map(~lm(.$data1 ~ .$data2, data=. ))

  if (r==T) { my.return <- models %>% map(summary) %>%  map_dbl(~.$adj.r.squared)  } else {
    my.return <-  models %>%  map(summary) }
  return(my.return)
}


#' Title
#'
#' @param my.lags
#' @param df
#' @param gather
#'
#' @return
#' @export
#'
#' @examples
lag_x_i <- function(my.lags, df=my.x.y.spread, gather=T){
  lagged.df <-   map(my.lags,df=df, lag_x, r=T) %>%
    reduce(rbind) %>%
    as.data.frame()  %>%
    mutate(lag=my.lags)
  if (gather) return(gather(lagged.df, period, `adj.r.squared`,-lag)) else
    return(lagged.df)
}



