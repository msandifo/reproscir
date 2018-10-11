# copied directly from smapr
#' @importFrom rappdirs user_cache_dir
#'
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
