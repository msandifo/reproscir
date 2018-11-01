
#' writes out download as bash shell script
#'
#' @param data.set
#' @param script
#' @param yaml
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
write_script <- function(
                         data.set="cdiac",
                         script = paste0(data.set, "_get.sh"),
                         yaml=Sys.getenv("R_DATAYAML"),
                         verbose=T ){
  my.yaml<- yaml::yaml.load_file(yaml)

  my.data.yaml <- rlist::list.first(my.yaml, name==data.set)
  # if (exists("my.data.yaml$comment"))
  print(script)
  cat("#!/bin/bash\n#script for downloading data for reproduction \n#", my.data.yaml$comment,    file=script)

  cat("#", my.data.yaml$download,   file=script, append=T)
  cat("\nOUT_DIR=",my.data.yaml$directory," \n", file=script, append=T, sep="")
  cat('if [ ! -d "$OUT_DIR" ]; then\n  mkdir "$OUT_DIR"\nfi\n',   file=script, append=T)
  curl.line<- paste0("curl -s -L -o ", "$OUT_DIR/",my.data.yaml$file, " ", my.data.yaml$link ,"\n")

  cat(curl.line,   file=script, append=T)
    return(paste0( "bash ",pwd(), "/",script))
}

#' updates data ste by downloading latest version
#'
#' @param data.set
#' @param yaml
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
update_data<- function(data.set="cdiac",
                       yaml=Sys.getenv("R_DATAYAML"), verbose=T ){
  my.yaml<- yaml::yaml.load_file(yaml)

  my.data.yaml <- rlist::list.first(my.yaml, name==data.set)
 # if (exists("my.data.yaml$comment"))

 if (verbose==T) { print(my.data.yaml$comment)
   print(my.data.yaml$download) }
  if (!exists("my.data.yaml$download")) my.data.yaml$download<- TRUE
        if  (my.data.yaml$download !="FALSE"){
  if (!dir.exists(my.data.yaml$directory))  dir.create(my.data.yaml$directory, recursive=T)
          if (verbose==T) print(paste0(my.data.yaml$directory, "/", my.data.yaml$file))
  if (!is.null(my.data.yaml$wget)) {
    get.file<- system(my.data.yaml$wget)
    }else {
      get.file<-download.file(my.data.yaml$link,

                              paste0(my.data.yaml$directory, "/", my.data.yaml$file),
                              "curl",
                              extra="-s -L")
    }
  if (!is.null(my.data.yaml$csvkit)) get.file<- system(my.data.yaml$csvkit)
  if (!is.null(my.data.yaml$soffice)) get.file<- system(my.data.yaml$soffice)
  return( paste0(my.data.yaml$directory, "/", my.data.yaml$file))
  }
}

#' reads  data.set into
#' @param data.set
#' @param yaml
#' @param data
#' @param verbose
#'
#' @return dta.frame usually of form columms
#' @export
#'
#' @examples
read_data<- function(data.set="cdiac",
                     yaml=Sys.getenv("R_DATAYAML") ,
                     data=F , verbose=F){
  my.yaml<- yaml::yaml.load_file(yaml)
  my.data.yaml <- rlist::list.first(my.yaml, name==data.set)
   if(!is.null(my.data.yaml$comment ) & verbose) print(my.data.yaml$comment)
   if(!is.null(my.data.yaml$type$units) & verbose) print(my.data.yaml$type$units)
  eval(parse(text=my.data.yaml$r))
  if (!data) {
    my.data.yaml$data=my.data
    if(exists("my.data.1")) {print("reading my.data.1")
      my.data.yaml$data.1=my.data.1}
    return(my.data.yaml)
    } else    return(my.data)
}

#' returns information for sepcified data set
#'
#' @param data.set
#' @param yaml
#'
#' @return
#' @export
#'
#' @examples
get_info <- function(data.set="cdiac",
                     yaml=Sys.getenv("R_DATAYAML")  ){
  my.yaml<- yaml::yaml.load_file(yaml)
  my.data.yaml <- rlist::list.first(my.yaml, name==data.set)
  if (!is.null(my.data.yaml$legend))  out.string<-  my.data.yaml$legend else {
  out.string <- paste0("data sourced from ", my.data.yaml$link )
  if(!is.null(my.data.yaml$type$units)) out.string <-paste0(out.string, "units ", my.data.yaml$type$units, "\n")}
  return(out.string)
}

#'  return  agency for given data set
#'
#' @param data.set
#' @param yaml
#' @param prepend
#'
#' @return
#' @export
#'
#' @examples
get_agency <- function(data.set="cdiac",
                       yaml=Sys.getenv("R_DATAYAML"),
                       prepend="data sourced from" ){
  my.yaml<- yaml::yaml.load_file(yaml)
  my.data.yaml <- rlist::list.first(my.yaml, name==data.set)
  out.string <- paste(prepend, my.data.yaml$agency)
  return(out.string)
}
#' returns link for given data.set
#'
#' @param data.set
#' @param yaml
#'
#' @return
#' @export
#'
#' @examples
get_link <- function(data.set="cdiac",
                       yaml=Sys.getenv("R_DATAYAML") ){
  my.yaml<- yaml::yaml.load_file(yaml)
  my.data.yaml <- rlist::list.first(my.yaml, name==data.set)
  out.string <-  my.data.yaml$link
  return(out.string)
}

#' returns all listed data.sets in yaml file
#'
#' @param yaml
#' @param items
#' @param select
#'
#' @return
#' @export
#'
#' @examples
list_data_sets <- function(yaml=Sys.getenv("R_DATAYAML"), items=NA, select=NULL ){
  my.yaml<- yaml::yaml.load_file(yaml)
  # print(my.yaml[:]$name)
  # out.string <- paste0("data sourced from ", my.yaml$name )
  # out.string
  my.data.sets <-sapply(my.yaml, `[`, 1) %>% unlist()
 if (is.numeric(items)) my.data.sets <-my.data.sets[items]
  if (!is.null(select)) my.data.sets <- my.data.sets[stringr::str_detect(my.data.sets, select)]
    return(my.data.sets)
}

#' updtses all data sets in yaml file
#'
#' @param yaml
#' @param items
#'
#' @return
#' @export
#'
#' @examples
update_all_data <- function(yaml=Sys.getenv("R_DATAYAML"), items=NA) {

  my.data.sets<- list_data_sets(yaml=yaml,items=items)
 for (this.data.set in  my.data.sets) update_data(this.data.set)
}


#' Title
#'
#' @param fn filname
#' @param editor as set by "R_EDITOR envirnemtn variable as set in .R
#'
#' @return side effect is open a
#' @export
#'
#' @examples
#'
#'
Sys.open <- function(fn= "~/.Renviron", editor=Sys.getenv("R_EDITOR")) {
  if (Sys.getenv("R_EDITOR")=="Rstudio")  {
    file.edit(fn)} else{
  if (Sys.getenv("R_EDITOR")=="") editor =="open"
  system(paste(editor, fn))
    }
}

open_YAML <- function(fn=Sys.getenv("R_DATAYAML"), editor=Sys.getenv("R_EDITOR")) {
  Sys.open(fn,editor)
}


RS_open <- function(fn){
  file.edit(fn)
}

#' Title
#'
#' @param ddate
#' @param day
#'
#' @return
#' @export
#'
#' @examples
calendar_date<- function(ddate,day=NA){
  year <- floor(ddate)
  ndays=year*0+365
  ndays[year%%4==0]  <- 365
  yday <- round((ddate-year)*ndays)

  cdate<-as.Date(yday, origin = paste0(year,"-01-01"))
  if (is.numeric(day) ) if( day<=31 & day>0) cdate <-set_mday(cdate,day)
return(cdate)
}

#' Title
#'
#' @param cdate
#' @param day
#'
#' @return
#' @export
#'
#' @examples
set_mday <- function(cdate, day=15)   {
  return(cdate - (mday(cdate)-day))
  }

#' Title
#'
#' @param data.set
#' @param yaml
#'
#' @return
#' @export
#'
#' @examples
open_data<- function(data.set="cdiac",
                     yaml=Sys.getenv("R_DATAYAML")){
  my.yaml<- yaml::yaml.load_file(yaml)
  my.data.yaml <- rlist::list.first(my.yaml, name==data.set)
  fn<- paste0(my.data.yaml$directory, "/", my.data.yaml$file)
  Sys.open(fn)

}



#' Title
#'
#' @param data.set
#' @param yaml
#'
#' @return
#' @export
#'
#' @examples
plot_data <- function(data.set="eia.drygas",
                      yaml=Sys.getenv("R_DATAYAML")){
  my.yaml<- yaml::yaml.load_file(yaml)
  my.data.yaml <- rlist::list.first(my.yaml, name==data.set)

  eval(parse(text=my.data.yaml$ggplot))
}

set_yaml <- function(update, data.set="eia.drygas", field="link", yaml=Sys.getenv("R_DATAYAML") ){
  my.yaml<- yaml::yaml.load_file(yaml)

  for (i in 1:length(my.yaml)){
    if (my.yaml[[i]]$name == data.set) {
      my.yaml[[i]]$link <- update
      break()
    }
  }
  yaml::write_yaml(my.yaml,yaml)
  return(update)
}

#' set_yaml_link
#'
#' @param update  new link
#' @param data.set
#' @param yaml file
#'
#' @return New link as inserted in to yaml file
#' @export
#'
#' @examples
#' set_yaml_link("https://www.eia.gov/naturalgas/weekly/img/shale_gas_201807.xlsx", data.set="eia.drygas")
set_yaml_link<-function(update, data.set="eia.drygas",  yaml=Sys.getenv("R_DATAYAML") ){
   set_yaml(update, data.set, field="link", yaml)

}

#' returns title for a given data set
#'
#' @param data.set  name of datset
#'
#' @return title
#' @export
#'
#' @examples
get_title<- function(data.set ){
  my.list<- read_data(data.set)
  return(c(my.list$station$name,my.list$station$acronym, my.list$station$region))
}

add_yaml <- function(name="test", ...){
yaml.str<- as.yaml(list(list(name = name,
                             category= '',
                             agency="",
                             link="",
                             directory="",
                             file="",
                             download="",
                             type=list(format="",
                                       skip="",
                                       record.names="",
                                       units="",
                                       date.format= "date"),
                             station =list(
                               name= "",
                               acronym="",
                               country=""),
                             r="",
                             ggplot="", comment="", legend="")), indent=2)
yaml.str<-gsub("' ", '', yaml.str)
cat(  gsub("'", '', yaml.str), file=Sys.getenv("R_DATAYAML"), append=TRUE)
}
#str_replace_all("\'\'","")

