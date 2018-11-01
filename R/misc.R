
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

