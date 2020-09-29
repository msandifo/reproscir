#' conversion from terajoules per day to megawatts
#'
#' @param n
#' terajoules per day
#' a common "unit" of  gas production in Australia
#' @return numeric
#' @export
#'
#' @examples
#' tjday_to_mw(30)
tjd_to_mw <-function(input)  {
  input*1e12/1e6/24/60/60
}

#' conversion from lng tonnes per month to TJ per day
#'
#' @param n
#' @param date
#'
#' @return
#' @export
#'
#' @examples
lng_tm_to_tjd <-function(n,date) {
  n*8.97/172/lubridate::days_in_month(date)
}


lngtm2tjd <-function(n,date) {
  n*8.97/172/lubridate::days_in_month(date)
}


#' @export
mtoe2j <- 4.1868e16

#' @export
j2tw <- 1/3.156e7/1e12

#' @export
tjd2mw <- 1e12/1e6/24/60/60

#' @export
tjday_to_mw <- function(x) {x*tjd2mw}

#' @export
cf2j <- 1055055.8526

#' @export
tj2cf <- 947817.120313



#' @export
cm2bbl <-  6.2898

#' @export
syd.harbour <- 562e6 # cubic metres


