
#' copied directly from smapr
#'
#' @param destination_directory
#' @param folder
#'
#' @return
#' @export
#'
#' @examples
validate_directory <- function(destination_directory, folder="aemo") {
  if (is.null(destination_directory)) {
    destination_directory <- rappdirs::user_cache_dir(folder)
  }
  if (!dir.exists(destination_directory)) {
    dir.create(destination_directory, recursive = TRUE)
  }
  destination_directory
}


#' reorder_dmy
#' changes character vector with dates section order dmy to ymd.
#'
#'
#' @param dates character vector assuem to be in ymd version, can include addtional info such as dmy hms
#'
#' @return charcater vector in ymd order
#' @export
#'
#' @examples
reorder_dmy <-function(dates){
  my.locs<-stringr::str_locate(dates, c("/" ))
  inds<-which(my.locs[,1]<4)
  #print(inds)
  my.splits<-stringr::str_split(dates[inds]," ")
  my.u.d<-sapply(my.splits, "[[", 1)
  my.u.d.2<-sapply(my.splits, "[[", 2)
  my.u.d.1 <-stringr::str_split(my.u.d, "/") %>%
    sapply("rev") %>%
    t() %>%
    as.data.frame() %>%
    apply(  1, paste, collapse="/")%>%
    as.data.frame() %>%
    dplyr::mutate(V4=paste0(" ", my.u.d.2))  %>%
    apply(  1, paste0, collapse="")
  dates[inds]<-my.u.d.1
  dates[stringr::str_length(dates)==16] <-  stringr::str_c(dates[stringr::str_length(dates)==16], ":00")
  return(dates)
}

#' sets  day of month in date to day
#'
#' @param date date or datetime
#' @param day numeric
#'
#' @return
#' @export
#'
#' @examples
#' reproscir::set_month_day(lubridate::ymd("2012-01-01"),15)
#' #"2012-01-15"
set_month_day <-function(date, day=15) {
  date+lubridate::days(day-lubridate::mday(date))
}


#' Title
#'
#' @param name
#' @param collapse
#' @param drop
#' @param remove
#'
#' @return
#' @export
#'
#' @examples
collapse_to_lower<- function(name, collapse=".", drop=1, remove="-"){
  stringr::str_to_lower(name) %>% stringr::str_remove(remove) %>%
    stringr::str_split("\\s+") %>% unlist() %>% head(-drop) %>% paste(collapse=collapse)
}




#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
cagr<- function(df){
  #message('calculated annual growth rate')
  last.year<-tail(df$year,1)
  first.year<-df$year[1]
  n<-last.year-first.year
  last.value<-tail(df$value,1)
  first.value<-df$value[1]

  cagr<- 100*(((last.value/first.value)^(1/n))-1)

  # cagr<- 100*(LastYearV-FirstYearV)/FirstYearV/(n)
  message("cagr ", signif(cagr,3), "% applies from ", first.year," to ", last.year)
  return(cagr)
}

#' Title
#'
#' @param data.df
#' @param gr
#' @param n
#'
#' @return
#' @export
#'
#' @examples
project_cagr<- function(df, gr=NA, n=40){
  if (is.na(gr)) gr<- cagr(df)

  gr.i<- purrr::imap_dbl(rep(1,n), ~.x*((1.+ gr/100)^.y))

   data.frame(year=seq(1:n) + tail(df$year,1),
                region=tail(df$region,1),
                value=gr.i*tail(df$value,1)
  )

}

