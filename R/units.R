#' conversion of terajoules per day to megawatts
#'
#' @param n
#' terajoules per day
#' a common "unit" of  gas production in Australia
#' @return numeric
#' @export
#'
#' @examples
#' tjday_to_mw(30)
tjday_to_mw <-function(input)  {
  input*1e12/1e6/24/60/60
}
