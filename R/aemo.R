#' Title
#'
#' @param states
#' @param remote.path
#' @param local.path
#'
#' @return
#' @export
#'
#' @examples
download_aemo_current <- function(states = c("NSW", "QLD", "SA", "TAS", "VIC"),
                                  remote.path="http://www.nemweb.com.au/mms.GRAPHS/DATA/",
                                  local.path= NULL
) {
  local.path=validate_directory(local.path)
  for (state in states) {
    file.name <- paste0("DATACURRENTMONTH_", state, "1.csv")
    remote.url <- paste0(remote.path, file.name)
    local.file <- paste0(local.path, file.name)
    utils::download.file(remote.url, local.file)
  }
}

#' Title
#'
#' @param states
#' @param remote.path
#' @param local.path
#' @param months
#' @param verbose
#' @param years
#'
#' @return
#' @export
#'
#' @examples
download_aemo_aggregated <- function(states = c("NSW","QLD","SA", "TAS", "VIC"),
                                     remote.path="http://www.nemweb.com.au/mms.GRAPHS/data/",
                                     local.path=NULL,
                                     months=NA,
                                     verbose=F,
                                     years=NA) {
  file.names=NULL
  local.path=validate_directory(local.path)
  if (is.na(months[1])) months <- lubridate::month(Sys.Date())-1 #defaults to last month
  if (is.na(years[1])) years <- lubridate::year(Sys.Date())  # defaults to this year
  if (months[1]==0) {months[1]<-12; years[1]<- years[1]-1}
  for (this.year in years){
    for (this.month in months){
      for (this.state in states){

        month.name <-format(ISOdate(this.year,this.month,1),"%B") #previous month
        if (verbose)  message(paste(month.name, this.year, this.state))

        # make  this.month string 2 chatacter's long by adding "0"
        if (this.month < 10) file.name<- paste0("DATA", this.year,"0", this.month,"_", this.state,"1.csv") else
          file.name <- paste0("DATA", this.year, this.month,"_", this.state,"1.csv")

        remote.url <- paste0(remote.path, file.name)


        local.file <-paste0(local.path,"/", file.name)
        if (verbose)   message(local.file)
        if ((this.year< lubridate::year(Sys.Date()) | this.month< (lubridate::month(Sys.Date())) )){
          if (!file.exists(local.file))   utils::download.file(remote.url, local.file) else
            message(remote.url," already downloaded")}
        file.names <- c(file.names, file.name)
      }
    }
  }
  return( file.names)
}

#' Title
#'
#' @param local.path
#' @param state
#' @param files
#'
#' @return
#' @export
#'
#' @examples
get_aemo_data<- function(local.path=NULL, state="NSW", files=NULL) {
  local.path <- validate_directory(local.path)
  my.files<- list.files(local.path)
  if (!is.null(files)) my.files<- my.files[stringr::str_detect(my.files, files)]
  my.files<-paste0(local.path, "/",my.files[stringr::str_detect(my.files,state) ] )
  dt<-data.table::rbindlist(lapply( my.files, data.table::fread, colClasses= c("character", "character", "numeric","numeric","character"), drop="PERIODTYPE"))
  dt$SETTLEMENTDATE<- reorder_dmy(dt$SETTLEMENTDATE) %>% fasttime::fastPOSIXct(  tz="GMT",required.components=3)
  dt$month <- lubridate::month(dt$SETTLEMENTDATE)
  dt$year<- lubridate::year(dt$SETTLEMENTDATE)
  dt %>% subset(year<=lubridate::year(Sys.Date()) & year >= 2007 ) #nb. checks for errors with POSIXCt translation
}




download_gasbb <- function(local.path=NULL){
  local.path=validate_directory(local.path, folder="gasbb")
  local.file <- paste0(local.path,"/ActualFlows.zip")
  if (!file.exists(local.file))
  utils::download.file("https://www.aemo.com.au/-/media/Files/Gas/Natural_Gas_Services_Bulletin_Board/2018/GBB-2018-Docs/Archived-Data/ActualFlows.zip",
  local.file)
  return(local.file)
}

read_gasbb <- function(file.name){
  readr::read_csv(file.name ) %>%
    janitor::clean_names(case="snake") %>%
    dplyr::mutate(gasdate= lubridate::dmy(gasdate), lastchanged= lubridate::dmy_hms(lastchanged) )

}

group_gasbb <-function(df, zone="Roma"){
   df %>%
    subset(stringr::str_detect(zonename, zone) & flowdirection=="DELIVERY") %>%
    dplyr::group_by(gasdate)%>%
    dplyr::summarise(actualquantity= sum(actualquantity) )

}

