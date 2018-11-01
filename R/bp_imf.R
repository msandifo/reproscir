
#' Title
#'
#' @param filename
#' @param sheet
#' @param countries
#' @param years
#' @param na.rm
#' @param perc
#' @param verbose
#' @param fuel
#' @param units
#'
#' @return
#' @export
#'
#' @examples
BP_all <- function( filename=BP2018_download(),
                    sheet=32,
                    countries=NA,
                    years=1979:2017,
                    na.rm=TRUE,
                    perc=F,
                    verbose=F,
                    fuel=NA,
                    units=NA){


  if ("UK" %in% countries) countries[countries=="UK"]<- "United Kingdom"
  if ("Russia" %in% countries) countries[countries=="Russia"]<- "Russian Federation"
   bp.names = readxl::excel_sheets(filename)
  # print(str(bp.names))
  bp.sheets <- data.frame(SHEET=seq(1,length(bp.names) ),NAME=bp.names)
  if (verbose) {
    print(bp.sheets)
    print(bp.sheets[sheet,])
  }
  bp.data <-readxl::read_excel(filename, sheet = sheet, col_names = TRUE, col_types = NULL , na = "n/a",
                       skip = 2)
  bp.data <- bp.data[!is.na(  bp.data[,1] ),]
  bp.data <-bp.data[ !is.na(  bp.data[,2] ),]
  # bp.data[1]<- str_replace_all(bp.data[1], "Total", "")

  names(bp.data )[1] <- "region"
  bp.data$region<- stringr::str_replace(bp.data$region, "Total ", "")
  bp.data$region<- stringr::str_replace(bp.data$region, "of which: ", "")
  bp.data<- bp.data[,  1:length(head( names(bp.data), -3))]  # drop last 2 columns whcih ahve percentages of total and increase relative to year before

  bp.data <- tidyr::gather(bp.data,     YEAR, value, -region )
  names(bp.data)
   bp.data$YEAR <- as.numeric(bp.data$YEAR)

  if (!is.na(fuel)) bp.data$fuel<-fuel
  if (!is.na(units)) bp.data$units<-units
  if(!is.na(countries[1])) bp.data<- subset(bp.data, region %in% countries)
  if(!is.na(years[1])) bp.data<-subset(bp.data, YEAR %in% years)
  bp.data <-bp.data %>% dplyr::select(year=YEAR, region, value)
  bp.data$region[ bp.data$region=="United Kingdom"] ="UK"

  return(list(data=dplyr::as.tbl(bp.data), name=as.character(bp.sheets[sheet,]$NAME)))

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
 read_IMF <- function( country="World",
                      file= IMF2018_download(world=country=="World"),
                      measure = "NGDPRPPPPC",#"NGDP_RPCH",
                      percent=T
) {

  if (country=="Total World") country="World"
  if (country=="US") country="United States"
  if (country == "UK") country = "United Kingdom"
  if (country == "Russia")  country = "Russian Federation"
  if (country == "South Korea")   country = "Korea"
  if (country == "Iran")   country = "Islamic Republic of Iran"


  if (country=="World")  {measure = "NGDP_RPCH";
  percent=F
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

  my.df
}



#' Title
#'
#' @param country
#' @param measure
#' @param percent
#' @param percap
#' @param cum
#' @param ratio
#' @param fac
#'
#' @return
#' @export
#'
#' @examples
get_IMF<- function(country = "Australia", measure = "NGDP_RPCH",
                   percent = FALSE, percap=F, cum=F, ratio=F, fac=NA ) {

  sd <- reproscir::read_IMF(country = country, measure = measure[1] )
  if (length(measure)>1) {
    sd1 <- reproscir::read_IMF(country = country, measure = measure[2]) #, df = df)
    if (ratio==F) {sd$df$IMF <- sd$df$IMF- sd1$df$IMF} else {sd$df$IMF <- sd$df$IMF / sd1$df$IMF}
    if (ratio==F) {sd$subjectDescriptor<-  paste(sd$subjectDescriptor, '-',sd1$subjectDescriptor)} else {
      sd$subjectDescriptor<-  paste(sd$subjectDescriptor, '/',sd1$subjectDescriptor)
    }
  }
  if (percap==T & measure !="LP") {
    pcap<- reproscir::read_IMF(country = country, measure = "LP" )
    yeardiff <- sd$df$year[1]-pcap$df$year[1]   #assumes "LP" > measure
    if (yeardiff> 0) sd$df$IMF<-sd$df$IMF/tail(pcap$df$IMF, -yeardiff) else sd$df$IMF<-sd$df$IMF/pcap$df$IMF
    sd$subjectDescriptor<-  paste(sd$subjectDescriptor, '(per capita)')
  }
  if (percent == TRUE) {
    sd$df = data.frame(year = sd$df$year[-1], IMF = diff(sd$df$IMF) * 100/sd$df$IMF[1:(length(sd$df$IMF) - 1)])
    sd$scale = "annual % change"
  }
  if (is.list(fac)  ){
    if (length(sd$df$IMF) < length(fac$df$IMF))  {
      sd$df$IMF <- sd$df$IMF *tail(fac$df$IMF,-length(sd$df$IMF))
    } else {
      sd$df$IMF <- sd$df$IMF * fac$df$IMF
    }
    sd$units = "US dollars"
  }

  if (cum==T) {
    sd$df <- data.frame(year= sd$df$year, IMF=cumsum(sd$df$IMF))
    sd$subjectDescriptor<-  paste(sd$subjectDescriptor, '- cumulative')
  }
  return(sd)
}


#' Title
#'
#' @param sd
#' @param country
#' @param measure
#' @param df
#' @param percent
#' @param colour
#' @param over
#' @param plot
#' @param percap
#' @param cum
#' @param ratio
#' @param fac
#'
#' @return
#' @export
#'
#' @examples
plot_IMF <- function(sd=NA, country = "Australia", measure = "NGDP_RPCH",
                     df = "/Users/msandifo/data/global/economy/IMF/WEOOct2018all.xls",
                     percent = FALSE, colour="Red", over=F, plot=NA, percap=F, cum=F, ratio=F, fac=NA) {

  if (is.list(sd)==F) {
    sd <- get_IMF(country = country,
                  measure = measure,
                #  df = df,
                  percent=percent,
                  percap=percap,
                  cum=cum,
                  ratio=ratio,
                  fac=fac)
  }
  if (over==F) p1 <- ggplot(data = sd$df, aes(x = year, y = IMF)) +
      geom_line(colour = colour) +
      geom_point(colour = colour, size = 3,
                 alpha = 0.45) + ylab(paste(sd$units, "-", sd$scale)) + xlab("year") +
      ggtitle(paste(sd$country, ":", sd$subjectDescriptor))

  if (over==T) p1 <- plot + geom_line(data = sd$df, aes(x = year, y = IMF), colour = colour) +
      geom_point(data = sd$df, aes(x = year, y = IMF), colour = colour, size = 3, alpha = 0.45) +
      ylab(paste(sd$units, "-", sd$scale)) + xlab("year") +
      ggtitle(paste(sd$country, ":", sd$subjectDescriptor))
  show(p1)
  return(p1)
}


#' Title
#'
#' @param country
#'
#' @return
#' @export
#'
#' @examples
exchange_IMF<-function(country){
  imf<-get_IMF(country="Australia",mea=c("NGDPDPC","NGDPPC" ), ratio=T )
  return(imf)
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
plot_IMF_BP <- function(my.country = "World", measure="NGDPRPPPPC" , flag.years=NULL){
  if  (my.country=="World") measure="NGDP_RPCH"
  imf<-read_IMF(country=my.country, measure=measure)
  print(imf)
  if (my.country == "US") imf$region=my.country

  bp.t <-BP_all(sheet=57,countries=my.country,years=1965:2017, ver=T )
  bp <-bp.t
  paris<- Paris(country= my.country)
  paris.t<- Paris(country= my.country, total=T)
  print(paris.t)
  names(bp$data)[2]<- "region"
  bp$data$value <- c(NA, diff(bp$data$value)/head(bp$data$value, -1)*100 )
  wd <-merge(bp$data, imf, by=c("year", "region"))
  names(wd) <- c("year" ,"region","bp", "imf")
  wd$years <- paste0("'", str_sub(as.character(wd$year),3,4) )
  wd$decade <- paste0(str_sub(as.character(floor((wd$year-0)/10)*10),3,4), "'s")


  wd.decade <- wd %>% group_by(decade) %>% dplyr::summarise(bp=mean(bp, na.rm=T), imf=mean(imf, na.rm=T), years=years[1])
  print(wd)
  wd %>% subset(year < 2008) %>%
    ggplot(aes( imf,bp) ) +
    geom_smooth(method="lm", se=T, fullrange=T, colour="black",
                size=.4,  level = 0.995, linetype=5)+
    geom_smooth(data=wd %>% subset(year >= 2008), method="lm", se=F, fullrange=T, colour="darkgreen",
                size=.4,  level = 0.995, linetype=5)+
    geom_point(shape=18, size=5, colour="white")+
    geom_point(shape=18, size=4)+
    geom_point(data=wd %>% subset(year >= 2008),  colour="white", shape=16, size=5)+
    geom_point(data=wd %>% subset(year >= 2008), aes(colour=decade), shape=16, size=4)+
    ggrepel::geom_text_repel(data=wd %>% subset(year >= 2008),
                             aes(label=years,colour=decade),
                             fontface = 'bold',
                             nudge_y =  ifelse(wd$bp[wd$year>=2008] > 1. , 1, -.6),
                             nudge_x =  ifelse(wd$bp[wd$year>=2008] > 1. , -.5, .35),
                             force=30,
                             max.iter = 1e3,
                             size=5)+
    theme(legend.position = "NONE") +
    geom_point(data=subset( wd.decade, decade !="70's"), size=11, aes(colour="black"), shape=21, fill="white",alpha=.5) +
    geom_text(data=subset( wd.decade,decade !="70's") ,aes(label=decade),colour="black", hjust=0.5, vjust=.5) +
    scale_colour_manual(values=c("firebrick2", "green4", "black","black")) +
    labs(x=paste0("annual % change in GDP"),
         y= "annual % change in emissions",
         subtitle=paste0(my.country, ", energy sector"),
         caption= paste0("data sourced from IMF (",measure,") and BP (fit from 1980-2007)"))+
    geom_hline(yintercept = paris, size=.25, linetype=2, colour=c("blue3", "red3"))+
    annotate("text", y=paris[1], x=max( wd$imf, na.rm=T),
             # label="Energy sector Paris commitments*",
             label=paste0(round(paris[1],1), "% p.a. for 26% reduction on 2005 levels by 2030"),
             size=3,colour="blue3",
             vjust=-.3, fontface="italic", hjust=1)+
    annotate("text", y=paris[2], x=max( wd$imf, na.rm=T),
             # label="Energy sector Paris commitments*",
             label=paste0(round(paris[2],1), "% p.a. for 28% reduction on 2005 levels by 2030"),
             size=3,colour="red3",
             vjust=1.2, fontface="italic", hjust=1) -> pa
  if(!is.null(flag.years)) pa <- pa+
    geom_point(data=wd %>% subset(year %in% flag.years), shape=18, size=4, colour="red2")

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

  return(grid.arrange(pa,pb, ncol=1, heights=2:1))

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
Paris<- function(  country="Australia", year=c(2005,2017), percent=c(26,28), total=FALSE) {

  df <- BP_all(sheet=57, countries=country, years=year[1]:year[2])$data
  current= tail(df$value, 1)
  target= df$value[1] * (1- percent/100)
  reductions = tail(df$value,1) - target
  n.years=2030-tail(df$year,1)
  cagr<- 100*(((target/current)^(1/n.years))-1)
  if (total) return(target) else return( cagr)
}

#' tests for existence and, if not, retreives bp-stats-review-2018-all-data.xlsx
#'
#' @param local.path  to directory whcih will hold eht folder and file defaults to cache (on osx ~/Library/Caches)
#' @param folder  to hold file in local.path
#' @param remote.url url of BP stats review, ciurrnely defaults to 2018
#'
#' @return  string full path to local copy
#' @export
#'
#' @examples
BP2018_download <- function(local.path=NULL,
                            folder="BP",
                            remote.url = "https://www.bp.com/content/dam/bp/en/corporate/excel/energy-economics/statistical-review/bp-stats-review-2018-all-data.xlsx"
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
IMF2018_download <- function(local.path=NULL,
                            folder="IMF",
                            world=FALSE,
                            remote.url = "https://www.imf.org/external/pubs/ft/weo/2018/01/weodata/WEOApr2018all.xls"
){
 if (world==TRUE) remote.url <- stringr::str_replace(remote.url, "all.xls", "alla.xls")
   local.path=reproscir::validate_directory(local.path, folder=folder)
  if (!dir.exists(local.path)) dir.create(local.path, recursive=TRUE)

  local.file <- paste0(local.path, "/" ,basename(remote.url))
  if(!file.exists(local.file)) utils::download.file(remote.url, local.file )
  return(local.file)
}

