#' Title
#'
#' @return
#' @export
#'
#' @examples
theme_twitter <- function(){
# library(ggplot2)
# library(hrbrthemes)

if (!exists("base_size")) base_size<-16
if (!exists("ffamily")) ffamily="Arial"  #"Gill Sans"
ggplot2::theme_set( ggplot2::theme_linedraw(base_size = base_size, base_family=ffamily))
ggplot2::theme_set( hrbrthemes::theme_ipsum(axis="xy",
                                            base_size=base_size,
                                            grid=T,
                                            plot_margin = margin(10,10,10,10),
                                            axis_title_size = base_size*.8,
                                            axis_text_size = base_size*.7))
ggplot2::theme_update(
  plot.title = element_text(size = rel(1.15),family = ffamily, angle=0),
  plot.subtitle = element_text(size = rel(1.), family = ffamily, colour="grey50", angle=0),
  plot.caption = element_text(size = rel(.35),family = ffamily, colour="grey80", angle=0,hjust=1),
  panel.grid.major = element_line(colour = "grey85", size=.05,  linetype=1),
  panel.grid.minor =element_line(colour = "grey90", size=.0, linetype=1),
  axis.text= element_text(size = rel(1), angle=0 , family = ffamily)
)
}
