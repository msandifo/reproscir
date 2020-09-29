library(magrittr)
library(tidyverse)
my.country = "Germany"
measure="NGDPRPPPPC"
years=1965:2018
sheet= 65
flag.years=NULL
perce$nt=T


if  (my.country=="World") {measure="NGDP_RPCH"
percent=F}

imf<-read_IMF(country=my.country, measure=measure, percent=T)
#print(imf)
if (my.country == "US") imf$region=my.country

bp.t <-BP_all(sheet=sheet,countries=my.country,years=years, ver=F )
bp <-bp.t
paris<- Paris(country= my.country)
paris.t<- Paris(country= my.country, total=T)
#print(paris.t)
#names(bp$data)[2]<- "region"
bp$data$value <- c(NA, diff(bp$data$value)/head(bp$data$value, -1)*100 )
wd <-merge(bp$data, imf, by=c("year", "region"))
names(wd) <- c("year" ,"region","bp", "imf", "measure")
wd$years <- paste0("'", stringr::str_sub(as.character(wd$year),3,4) )
wd$decade <- paste0(stringr::str_sub(as.character(floor((wd$year-0)/10)*10),3,4), "'s")


wd.decade <- wd %>% dplyr::group_by(decade) %>%
  dplyr::summarise(bp=mean(bp, na.rm=T), imf=mean(imf, na.rm=T), years=years[1])
#print(wd)
wd %>% subset(year < 2008) %>%
  ggplot(aes( imf,bp) ) +
  geom_smooth(method="lm", se=T, fullrange=T, colour="black",
              size=.4,  level = 0.995, linetype=5)+
  # geom_smooth(data=wd %>% subset(year >= 2008), method="lm", se=F, fullrange=T, colour="darkgreen",
  # size=.4,  level = 0.995, linetype=5)+
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
  labs(x=paste0("annual % change in GDP (IMF WEO:", measure, ")"),
       y= "BP Statistical Review 2018\nannual % change in energy emissions",
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

#  return(gridExtra::grid.arrange(pa,pb, ncol=1, heights=2:1))
return(list(p1=pa, p2=pb))

pb
