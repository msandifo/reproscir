

#' returns sheets in BP file
#'
#' @param filename
#' @param search
#' @param and bool - for multiple search strings defaults to TRUE
#'
#' @return
#' @export
#'
#' @examples
BP_sheets <- function( filename=BP2020_download(), search=NULL , and=T){

  if (is.character(search[1])) search <- stringr::str_to_lower(search)
  if (is.character(search[1])) print(paste(search, collapse = '.*'))
  bp.names <- readxl::excel_sheets(filename) %>% stringr::str_to_lower()
  #   print(str(bp.names))
  bp <- data.frame(sheet=seq(1,length(bp.names) ),name=bp.names)

  if (is.null(search)) bp else
    if (!and) bp[ bp$name %>% stringr::str_detect(search) %>% which() ,] else
      bp[ bp$name %>% stringr::str_detect(paste(search, collapse = '.*')) %>% which() ,]
}
#' Title
#'
#' @param filename
#' @param sheet
#' @param countries
#' @param years
#' @param na.rm
#' @param verbose
#' @param data logical- output only data (tbl), otherwise list containing data (tbl) and name (character)
#' @param units append a colum with units (chr)  to data (tbl)
#'
#' @return
#' @export
#'
#' @examples (BP_sheets( search=c("Oil.*Prod.*To"))$sheet %>%
#' BP_all( sheet=., count=c("Aus", "Ven", "Russ", "US", "Sau")))$data %>%
#' ggplot(aes(year, value, col=region))+
#' geom_line()
BP_all <- function( filename=BP2020_download(),
                    sheet=32,
                    countries=NA,
                    years=1965:2019,
                    na.rm=TRUE,
                    verbose=F,
                    data=FALSE,
                    units=NA){


  if ("UK" %in% countries) countries[countries=="UK"]<- "United Kingdom"
  if ("Russia" %in% countries) countries[countries=="Russia"]<- "Russian Federation"
   bp.names = readxl::excel_sheets(filename)
   bp.sheets <- data.frame(sheet=seq(1,length(bp.names) ),name=bp.names)
  if (verbose) {
    print(bp.sheets)
    print(bp.sheets[sheet,])
  }
  bp.data <-readxl::read_xlsx(filename, sheet = sheet,
                              col_names = TRUE, col_types = NULL ,
                              na = c("N/A","NA","na","n/a",  "-"),
                       skip = 2)
#  bp.data <- bp.data[!is.na(  bp.data[,1] ),]
 # bp.data <-bp.data[ !is.na(  bp.data[,2] ),]
  # bp.data[1]<- str_replace_all(bp.data[1], "Total", "")

  names(bp.data )[1] <- "region"
  bp.data$region<- stringr::str_replace(bp.data$region, "Total ", "")
  bp.data$region<- stringr::str_replace(bp.data$region, "of which: ", "")
  bp.data<- bp.data[,  1:length(head( names(bp.data), -3))]  # drop last 2 columns whcih ahve percentages of total and increase relative to year before
names( bp.data  ) <- stringr::str_sub (names( bp.data) ,1,4 )
names(bp.data )[1] <- "region"
  bp.data <- tidyr::gather(bp.data,     year, value, -region ) %>% subset(!is.na(region))
  #names(bp.data)
  bp.data$year <- as.numeric(bp.data$year)
#print(bp.sheets[sheet,]$name)

   if(!is.na(countries[1])) bp.data<- subset(bp.data, region %in% countries) %>% dplyr::arrange(region)
  # if(!is.na(countries[1])) bp.data<- bp.data %>% dplyr::filter(stringr::str_detect(region,countries))

  if(!is.na(years[1])) bp.data<-subset(bp.data, year %in% years)
  bp.data <-bp.data %>% dplyr::select(year=year ,region, value)
  if (!is.na(units)) bp.data$units<-units
  bp.data$region[ bp.data$region=="United Kingdom"] ="UK"

  if ( data==T) {
    bp.data$fuel<- collapse_to_lower(bp.sheets[sheet,]$name)
    return(bp.data)
    } else
    return(list(data=dplyr::as.tbl(bp.data), name=as.character(bp.sheets[sheet,]$name)))

}


#' Title
#'
#' @param country
#' @param file
#' @param measure
#' @param percent
#'
#' @return
#' @export
#'
#' @examples
 read_IMF <- function( country="World",measure = "NGDPRPPPPC",#"NGDP_RPCH",
                      file= IMF2018_download(world=country=="World"),

                      percent=F
) {

  if (country=="Total World") country="World"
  if (country=="US") country="United States"
  if (country == "UK") country = "United Kingdom"
  if (country == "Russia")  country = "Russia"
  if (country == "South Korea")   country = "Korea"
  if (country == "Iran")   country = "Islamic Republic of Iran"


  if (country=="World")  {
    if (measure =="NGDPRPPPPC") measure = "NGDP_RPCH";
   # percent=F
    #df = "/Volumes/data/data/global/economy/imf/WEOApr2018alla.xls"
    country.index=3
    year.index.start=9
    measure.index=2
  }
  else{
    #df = "/Volumes/data/data/global/economy/imf/WEOApr2018all.xls"
    country.index=4
    year.index.start=10
    measure.index=3
  }


  ws <- readr::read_delim(file,   "\t" ,na = c("", "n/a", "--"))
  inds <- which(ws[, measure.index] == measure & ws[, country.index]== country)
  print(inds)
  year.inds <- seq(year.index.start, length(names(ws))-1)
  # years <- as.numeric(c(names(ws)[9], names(ws)[length(names(ws))-1]))
  my.df <- tidyr::gather( as.data.frame(ws[inds, year.inds ]))
  names(my.df) <- c("year", "value")
  if(percent) my.df$value <- 100*c(NA, diff(my.df$value)/ head(my.df$value,-1))
  my.df$region <- country
  my.df$region[my.df$region=="United States"] ="US"
  my.df$region[my.df$region=="United Kingdom"] ="UK"
  my.df$region[my.df$region=="Russia"] ="Russian Federation"

  my.df$measure=measure
  my.df
}




#' Title
#'
#' @param my.country
#' @param measure
#' @param years
#' @param sheet
#' @param flag.years
#' @param percent
#'
#' @return
#' @export
#'
#' @examples
get_IMF_BP <- function(my.country = "World",
                          measure="NGDPRPPPPC" ,
                          years=1965:2018,
                          sheet= 65,
                          flag.years=NULL,
                          percent=T ){

    if  (my.country=="World") {measure="NGDP_RPCH"
    percent=F}

    imf<-read_IMF(country=my.country, measure=measure, percent=percent)
    #print(imf)
    if (my.country == "US") imf$region=my.country

    bp.t <-BP_all(sheet=sheet,countries=my.country,years=years, ver=F )
    bp <-bp.t
    paris<- Paris(country= my.country, year=c(2005,tail(years,1)) )
    paris.t<- Paris(country= my.country, year=c(2005,tail(years,1)), total=T)
    #print(paris.t)
    #names(bp$data)[2]<- "region"
    bp$data$value <- c(NA, diff(bp$data$value)/head(bp$data$value, -1)*100 )
    wd <-merge(bp$data, imf, by=c("year", "region"))
    names(wd) <- c("year" ,"region","bp", "imf", "measure")
    wd$years <- paste0("'", stringr::str_sub(as.character(wd$year),3,4) )
    wd$decade <- paste0(stringr::str_sub(as.character(floor((wd$year-0)/10)*10),3,4), "'s")


    wd.decade <- wd %>% dplyr::group_by(decade) %>%
      dplyr::summarise(bp=mean(bp, na.rm=T), imf=mean(imf, na.rm=T), years=years[1])

    list(wd=wd,  wd.decade =wd.decade, bp.t=bp.t, bp=bp,
         paris=paris, paris.t=paris.t, my.country =my.country,measure=measure, years=years)
}


#' Title
#'
#' @param my.country
#' @param measure
#'
#' @return
#' @export
#'
#' @examples
plot_IMF_BP <- function(df =NA,
                        my.country = "World",
                        measure="NGDPRPPPPC" ,
                        years=1965:2018, sheet= 65,
                        flag.years=NULL,
                        percent=T,
                        labels=T,
                        repel=T,
                        flag.colour="firebrick3",
                        flag.size=5,
                        flag.shape=18,
                        group=NULL){

  `%ni%` <-Negate(`%in%`)
if (is.na(df[1])){

  df<- get_IMF_BP  (my.country = my.country,
  measure=measure ,
  years=years,
  sheet= sheet,
  percent=percent)
  # if  (my.country=="World") {measure="NGDP_RPCH"
  # percent=F}
  #
  # imf<-read_IMF(country=my.country, measure=measure, percent=percent)
  # #print(imf)
  # if (my.country == "US") imf$region=my.country
  #
  # bp.t <-BP_all(sheet=sheet,countries=my.country,years=years, ver=F )
  # bp <-bp.t
  # paris<- Paris(country= my.country, year=tail(years,1))
  # paris.t<- Paris(country= my.country, year=tail(years,1), total=T)
  # #print(paris.t)
  # #names(bp$data)[2]<- "region"
  # bp$data$value <- c(NA, diff(bp$data$value)/head(bp$data$value, -1)*100 )
  # wd <-merge(bp$data, imf, by=c("year", "region"))
  # names(wd) <- c("year" ,"region","bp", "imf", "measure")
  # wd$years <- paste0("'", stringr::str_sub(as.character(wd$year),3,4) )
  # wd$decade <- paste0(stringr::str_sub(as.character(floor((wd$year-0)/10)*10),3,4), "'s")
  #
  #
  # wd.decade <- wd %>% dplyr::group_by(decade) %>%
  #   dplyr::summarise(bp=mean(bp, na.rm=T), imf=mean(imf, na.rm=T), years=years[1])
}

  wd=df$wd %>% subset(year  %in% df$years)
  wd.decade =df$wd.decade
  paris=df$paris
  paris.t=df$paris.t
  my.country =df$my.country
  measure=df$measure
   bp.t=df$bp.t
  bp=df$bp

  #print(bp)
 pa <- wd %>% subset(year < 2008) %>%
    ggplot(aes( imf,bp ) ) +
    geom_smooth(  , method="lm", se=F, fullrange=T, #colour="black",
                size=.4,  level = 0.995, linetype=5)+
    # geom_smooth(data=wd %>% subset(year >= 2008), method="lm", se=F, fullrange=T, colour="darkgreen",
    # size=.4,  level = 0.995, linetype=5)+
    geom_point(shape=18, size=5, colour="white")+
    geom_point(shape=18, size=4)+
    geom_point(data=wd %>% subset(year >= 2008),  colour="white", shape=16, size=5)+
    geom_point(data=wd %>% subset(year >= 2008), aes(colour=decade), shape=16, size=4)
    if ( repel) pa <- pa + ggrepel::geom_text_repel(data=wd %>% subset(year >= 2008 & year %ni% flag.years),
                             aes(label=years,colour=decade),
                             fontface = 'bold',
                             nudge_y =  ifelse(wd$bp[wd$year>=2008] > 1. , 1, -.6),
                             nudge_x =  ifelse(wd$bp[wd$year>=2008] > 1. , -.5, .35),
                             force=30,
                             max.iter = 1e3,
                             size=5, show.legend = F)

   pa <- pa+ theme(legend.position = "NONE") +
     scale_colour_manual(values=c("firebrick2", "green4", "black","grey40", "yellow")) +
    labs(x=paste0("annual % change in GDP (IMF WEO:", measure, ")"),
         y= "BP Statistical Review 2019\nannual % change in energy emissions",
         subtitle=paste0(my.country, ", energy sector"),
         caption= paste0("data sourced from IMF (",measure,") and BP (fit from 1980-2007)"))+
    geom_hline(yintercept = paris[1], size=.25, linetype=2, colour=c("blue3" ))+
    annotate("text", y=paris[1], x=max( wd$imf, na.rm=T),
             # label="Energy sector Paris commitments*",
             label=paste0(round(paris[1],1), "% p.a. for 26% reduction on 2005 levels by 2030"),
             size=3,colour="blue3",
             vjust=-.3, fontface="italic", hjust=1)
  #  p1+  geom_hline(yintercept = paris[1], size=.25, linetype=2, colour=c("blue3", "red3"))+
  #   annotate("text", y=paris[2], x=max( wd$imf, na.rm=T),
  #            # label="Energy sector Paris commitments*",
  #            label=paste0(round(paris[2],1), "% p.a. for 28% reduction on 2005 levels by 2030"),
  #            size=3,colour="red3",
  #            vjust=1.2, fontface="italic", hjust=1) -> pa
  if(!is.null(flag.years)) pa <- pa+
    geom_point(data=wd %>% subset(year %in% flag.years), shape=flag.shape, size=flag.size*1.2, colour="white")+
    geom_point(data=wd %>% subset(year %in% flag.years), shape=flag.shape, size=flag.size, colour=flag.colour)+
    ggrepel::geom_text_repel(data=wd %>% subset(  year %in% flag.years),
                             aes(label=years),colour=flag.colour,
                             fontface = 'bold',
                             nudge_y =  ifelse(wd$bp[wd$year>=2008] > 1. , 1, -.6),
                             nudge_x =  ifelse(wd$bp[wd$year>=2008] > 1. , -.5, .35),
                             force=30,
                             max.iter = 1e3,
                             size=5, show.legend = F)


  if (labels) pa <- pa+  geom_point(data=subset( wd.decade, decade !="70's"), size=11, aes(colour="black"), shape=21, fill="white",alpha=.5) +
    geom_text(data=subset( wd.decade,decade !="70's") ,aes(label=decade),colour="black", hjust=0.5, vjust=.5, show.legend = F)


  bp.t$data %>% subset(year>1980) %>% ggplot(aes(year, value)) +geom_line()+
    geom_hline(yintercept =bp.t$data$value[bp.t$data$year==2005],linetype=2, size=.2)+
    geom_point( data=bp.t$data %>% subset(year==2005),  colour="white",size=2.5)+
    geom_point( data=bp.t$data %>% subset(year==2005),  size=1.5)+
    labs(x=NULL, y="million tonnes")+
    geom_hline(yintercept = paris.t, size=.25, linetype=2, colour=c("blue3", "red3"))+
    annotate("text", y=paris.t[1], x=max( bp.t$data$year, na.rm=T),
             # label="Energy sector Paris commitments*",
             label=paste0(round(paris.t[1],0), "mt, 26% reduction on 2005 level"),
             size=3,colour="blue3",
             vjust=-.3, fontface="italic", hjust=1)+
    annotate("text", y=paris.t[2], x=max(  bp.t$data$year,  na.rm=T),
             # label="Energy sector Paris commitments*",
             label=paste0(round(paris.t[2],0), "mt, 28% reduction on 2005 level"),
             size=3,colour="red2",
             vjust=1.2, fontface="italic", hjust=1)+
    annotate("text", y=bp.t$data$value[bp.t$data$year==2005], x=max(  bp.t$data$year,  na.rm=T),
             # label="Energy sector Paris commitments*",
             label=paste0(round(bp.t$data$value[bp.t$data$year==2005],0), "mt,  2005 level"),
             size=3,colour="black",
             vjust=-.3, fontface="italic", hjust=1) ->pb

  #  return(gridExtra::grid.arrange(pa,pb, ncol=1, heights=2:1))
  return(list(p1=pa, p2=pb))


  # geom_segment(
  #   aes(x = 2.5, y = -1.5, xend =2.5, yend =-3.25),
  #   arrow = arrow(length = unit(0.03, "npc"),
  #                 type="closed"),
  #   lineend="butt", colour="blue3"
  # )+
  # geom_segment(
  #   aes(x = 1., y = -4, xend = 2.35, yend =-4),
  #   arrow = arrow(length = unit(0.03, "npc"),
  #                 type="closed"),
  #   lineend="butt", colour="blue3"
  # )

}

#' Title
#'
#' @param country
#' @param year
#' @param percent
#' @param total
#'
#' @return
#' @export
#'
#' @examples
Paris<- function(  country="Australia", year=c(2005,2018), percent=c(26,28), total=FALSE) {

  df <- BP_all(sheet=65, countries=country, years=year[1]:year[2])$data
  current= tail(df$value, 1)
  target= df$value[1] * (1- percent/100)
  reductions = tail(df$value,1) - target
  n.years=2030-tail(df$year,1)
  cagr<- 100*(((target/current)^(1/n.years))-1)
  if (total) return(target) else return( cagr)
}

#' tests for existence and, if not, retrieves bp-stats-review-2019-all-data.xlsx
#'
#' @param local.path  to directory which will hold eht folder and file defaults to cache (on osx ~/Library/Caches)
#' @param folder  to hold file in local.path
#' @param remote.url url of BP stats review, currently defaults to 2019
#'
#' @return  string full path to local copy
#' @export
#'
#' @examples
BP2019_download <- function(local.path=NULL,
                            folder="BP",
                            remote.url =
                              "https://www.bp.com/content/dam/bp/business-sites/en/global/corporate/xlsx/energy-economics/statistical-review/bp-stats-review-2019-all-data.xlsx"
  #https://www.bp.com/content/dam/bp/en/corporate/excel/energy-economics/statistical-review/bp-stats-review-2018-all-data.xlsx"
){
   local.path=reproscir::validate_directory(local.path, folder=folder)
  if (!dir.exists(local.path)) dir.create(local.path, recursive=TRUE)

      local.file <- paste0(local.path, "/" ,basename(remote.url))
 if(!file.exists(local.file)) utils::download.file(remote.url, local.file )
 return(local.file)
}

#' tests for existence and, if not, retrieves bp-stats-review-2020-all-data.xlsx
#'
#' @param local.path  to directory which will hold eht folder and file defaults to cache (on osx ~/Library/Caches)
#' @param folder  to hold file in local.path
#' @param remote.url url of BP stats review, currently defaults to 2019
#'
#' @return  string full path to local copy
#' @export
#'
#' @examples
BP2020_download <- function(local.path=NULL,
                            folder="BP",
                            remote.url =
                              "https://www.bp.com/content/dam/bp/business-sites/en/global/corporate/xlsx/energy-economics/statistical-review/bp-stats-review-2020-all-data.xlsx"
                            #https://www.bp.com/content/dam/bp/en/corporate/excel/energy-economics/statistical-review/bp-stats-review-2018-all-data.xlsx"
){
  local.path=reproscir::validate_directory(local.path, folder=folder)
  if (!dir.exists(local.path)) dir.create(local.path, recursive=TRUE)

  local.file <- paste0(local.path, "/" ,basename(remote.url))
  if(!file.exists(local.file)) utils::download.file(remote.url, local.file )
  return(local.file)
}

#' Title
#'
#' @param local.path
#' @param folder
#' @param remote.url
#'
#' @return
#' @export
#'
#' @examples
BP2018_download <- function(local.path=NULL,
                            folder="BP",
                            remote.url =
                              "https://www.bp.com/content/dam/bp/business-sites/en/global/corporate/xlsx/energy-economics/statistical-review/bp-stats-review-2018-all-data.xlsx"
                            #https://www.bp.com/content/dam/bp/en/corporate/excel/energy-economics/statistical-review/bp-stats-review-2018-all-data.xlsx"
){
  message( "nb. same as BP2019_download()")
  local.path=reproscir::validate_directory(local.path, folder=folder)
  if (!dir.exists(local.path)) dir.create(local.path, recursive=TRUE)

  local.file <- paste0(local.path, "/" ,basename(remote.url))
  if(!file.exists(local.file)) utils::download.file(remote.url, local.file )
  return(local.file)
}

#' Title
#'
#' @param local.path
#' @param folder
#' @param remote.url
#'
#' @return
#' @export
#'
#' @examples
IMF2018_download <- function(local.path=NULL,
                            folder="IMF",
                            world=FALSE,
                            remote.url = "https://www.imf.org/external/pubs/ft/weo/2019/01/weodata/WEOApr2019all.xls"
){
 if (world==TRUE) remote.url <- stringr::str_replace(remote.url, "all.xls", "alla.xls")
   local.path=reproscir::validate_directory(local.path, folder=folder)
  if (!dir.exists(local.path)) dir.create(local.path, recursive=TRUE)

  local.file <- paste0(local.path, "/" ,basename(remote.url))
  if(!file.exists(local.file)) utils::download.file(remote.url, local.file )
  return(local.file)
}

