#' Title
#'
#' @param year
#' @param month
#' @param fuel
#' @param country
#'
#' @return
#' @export
#'
#' @examples
read_gladstone_ports<- function(year=NULL,
                                month=NULL,
                                fuel="Liquefied Natural Gas",
                                country="Total"){
  args <- as.list(match.call()) # print(exists("args$year"))
  if (is.null(year)) year <- lubridate::year(Sys.Date())
  if (is.null(month)) {
    current.month <- lubridate::month(Sys.Date())
    #current month not posted (usually not posted until second week of current month)

    if (current.month>1 ) month <- current.month- 1  else  {
      month=12
      year<-year-1
    }
  }
  if (month>= 10) yearmonth= paste(year,month, sep="") else
    yearmonth= paste(year,month, sep="0")
  message(paste(fuel,  month.abb[month], year))
  url<-paste0("http://content1.gpcl.com.au/viewcontent/CargoComparisonsSelection/CargoOriginDestination.aspx?View=C&Durat=M&Key=",yearmonth)
  wg <- rvest::html_session(url )
  batches <- read_html(wg) %>%
    rvest::html_nodes("#MainContent_pnlResults")   #class(batches)
  table <- batches %>%
    rvest::html_nodes("td") %>%
    rvest::html_text()
  lng.ind <- which( table==fuel)
  t.ind <-which(stringr::str_detect(table,country))
  t.lng.ind  <- t.ind[t.ind>lng.ind][1]
  value <- table[t.lng.ind+1] %>% stringr::str_replace_all(",","") %>%as.numeric()
  mdays <- lubridate::days_in_month(lubridate::ymd(paste(year,month, "01", sep="-")))
  if(country=="Total"){
    ships <- table[t.lng.ind+2] %>% stringr::str_replace_all(",","") %>%as.numeric()
    return(data.frame(year=year,
                      month=month,
                      date=lubridate::ymd(paste(year,month, "15", sep="-")),
                      tonnes=value,
                      shipments=ships,
                      mdays=mdays))
  } else
    return(data.frame(year=year,
                      month=month,
                      date=lubridate::ymd(paste(year,month, "15", sep="-")),
                      tonnes=value,
                      mdays=mdays))
}

# requires purrr
#reads a sequence of gladstone port authority tables
#' Title
#'
#' @param years
#' @param fuel
#' @param country
#'
#' @return
#' @export
#'
#' @examples
read_gladstone_year <- function(years=2015:lubridate::year(Sys.Date()),fuel="Liquefied Natural Gas", country="Total"){
  y= rep(years, each=12)
  m= rep(1:12,  length(years))   #l  <- list(y=2016:2017,m=1:2 )
  my_fun <- function(y,m, fuel="Liquefied Natural Gas", country="Total") read_gladstone_ports(year=y, month=m, fuel=fuel,country = country )
  purrr::map2_df(y,m, my_fun,  fuel=fuel, country=country)
}


#' Title
#'
#' @param local.path
#' @param years
#'
#' @return
#' @export
#'
#' @examples
update_gladstone <- function(local.path=NULL, years=2015:lubridate::year(Sys.Date())){
  local.path=validate_directory(local.path, folder="gladstone")
  gladstone.file = paste0(local.path, "/", "lng.Rdata")
  if (!file.exists(gladstone.file)) {
    read_gladstone_year(years=years ) -> lng
    save(lng, file=gladstone.file )} else load(gladstone.file)
  lng
}