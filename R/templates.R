
#' Title
#'
#' @param name
#' @param directory
#' @param remove
#'
#' @return
#' @export
#'
#' @examples
create_twitter <- function(name="000",directory="/Volumes/data/Dropbox/msandifo/documents/programming/r/twitter/2018/",  remove=F){
create_drake(name, directory,remove)
  }

#' Title
#'
#' @param directory
#' @param name
#' @param remove
#'
#' @return
#' @export
#'
#' @examples
create_drake <- function(name="000",directory="/Volumes/data/Dropbox/msandifo/documents/programming/r/2018/drake/",   remove=F){

  full.name=paste0(directory,"/", name) %>%stringr::str_replace_all("//", "/")
  if (dir.exists(full.name))
  { if (remove) unlink(full.name, recursive=T) else
    message("directory already exists")
  }
   if (!dir.exists(full.name)){

    dir.create(full.name, recursive=T)

    dir.create(paste0(full.name, "/src"))
    dir.create(paste0(full.name, "/figs"))
    dir.create(paste0(full.name, "/data"))
    setwd(full.name)

    rstudioapi::initializeProject(path = getwd())

    rmd_template(name) %>% cat(file="Readme.Rmd")

    outputs_template(name) %>% cat(file="./src/outputs.R")
    print("---")
    plots_template(name) %>% cat(file="./src/plots.R")
    print("---")
    downloads_template(name) %>% cat(file="./src/downloads.R")
    plan_template(name) %>% cat(file="./src/plan.R")
    functions_template(name) %>% cat(file="./src/functions.R")
    print("--")
    packages_template(name) %>% cat(file="./src/packages.R")
    settings_template(name) %>% cat(file="./src/settings.R")
    theme_template(name) %>% cat(file="./src/theme.R")
    drake_template(name) %>% cat(file="drake.R")

    # "" %>% cat(file="./src/downloads.R")
    # ""
   }

  openProject(path =  getwd(), newSession = FALSE)
}

#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
rmd_template <- function(  name) {
templates::tmpl("---
title: '{{name}}'
output: github_document
always_allow_html: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE,  message=FALSE, warning=FALSE)
drake.path<-'~/Dropbox/msandifo/documents/programming/r/twitter/2018/{{name}}/'
full.repro<-F
library(reproscir)
source('./src/settings.R')
source('./src/theme.R')
source('./src/functions.R')
source('./src/plan.R')
source('./src/downloads.R')
```

##  Desription

Reprosci {{name}} has no description as yet.


## Data Sources

*  to be added

##  Caveats


## Code

The code base is in ```r``` and is managed  within RStudio, using  the  ```drake```  package, and my ```reoproscir``` package on ```github```  obtained with ```devtools::install_github('msandifo/reproscir')```. _Note this package is very much a  work in progress, and while it  is installed automagically,it will likely need reinstalling with _```devtools::install_github('msandifo/reproscir')```.

The code can be executed by opening the ```Rstudio``` project  ```{{name}}.Rproj```
and sourcing ```drake.R```.

```{r echo=T, eval=F}
source('drake.R')
```
Details of the steps invoked by ```drake.R``` are summarised below.

* `source('./src/packages.R')` checks for and automatically installs missing package dependencies
<!-- ```tidyverse```, ```ggplot2```, ```magrittr```, ```purrr```, ```stringr```, ```drake```, ```lubridate```, ```rvest```, ```rappdirs```,```data.table```, ```fasttime```, ```devtools```, ```wbstats```  -->
<!--  from cran, and ```hrbrthemes```  and ```reproscir``` from the github repos ```hrbrmstr/hrbrthemes``` and ```msandifo/reproscir``` -->

* `source('./src/settings.R')` sets variables, such as the ```drake.path```,
* `source('./src/functions.R')` reads any functions not in ```reproscir```
* `source('./src/theme.R')`  sets a ggplot theme derived from ```hrbrthemes```
* `source('./src/plots.R')` plot functions
* `source('./src/downloads.R')` directs the download of the  relevant data files to be downloaded  into the local directory set by
``` local.path```. By default ``` local.path=NULL``` in which case data is downloaded via ```rappdirs::user_cache_dir()``` to a folder in the users cache directory (for macOSX, ```~/Library/cache```) to ```file.path(local.path, aemo)```.


* `source('./src/plan.R')` defines the drake plan ```reproplan```
* `source('./src/ouputs.R')` potsprocessing output functions

The dependency structure of the reprplan is obtained by procesing as follows
```{r drake_plan, echo=TRUE, cache=F}
source('./src/settings.R')
source('./src/theme.R')
source('./src/functions.R')
source('./src/plots.R')
source('./src/plan.R')
drake::make( reproplan )
config <- drake::drake_config(reproplan)
graph <- drake::drake_graph_info(config, group = 'status', clusters = 'imported')
drake::render_drake_graph(graph, file='figs/rmd_render_drake.png')
```

<img src='./figs/rmd_render_drake.png' alt='hist1' align='center' style = 'border: none; float: center;' width = '1000px'>

Note that ```reproplan``` processes the files downloaded  by ```./src/downloads.R```, returning ```merged.data```
```{r  cache=FALSE}
print(head(readd(merged.data)))

```

* ```source(drake::make( reproplan ))```
* ```source('./src/ouputs.R')```
output charts  to the ```./figs``` directory :

```{r  echo=TRUE, cache=TRUE, eval=F}
p{{name}}<-drake::readd(p{{name}})

 ```

 <img src='./figs/p{{name}}_01.png' alt='hist1' align='center' style = 'border: none; float: center;' width = '1000px'>


## Code Notes

## Errata
",
name=name)
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
drake_template<- function(...) {
  "
    full.repro=T

    source('./src/settings.R')
    source('./src/packages.R')
    source('./src/theme.R')
    source('./src/functions.R')
    source('./src/downloads.R')
    if (file.exists('./src/plots.R')) source('./src/plots.R')
    if (file.exists('./src/tables.R')) source('./src/tables.R')
    if (file.exists('./src/reports.R')) source('./src/reports.R')

    source('./src/plan.R')
    if (full.repro==TRUE) drake::clean(force=T)
    drake::make( reproplan )
    source('./src/outputs.R')"

}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
settings_template<- function(...) {
"if (!exists('full.repro')) full.repro=T
library(drake)
library(magrittr)
pkgconfig::set_config('drake::strings_in_dots' = 'literals')
local.path=NULL
if (!exists('drake.path')) drake.path <- dirname(rstudioapi::getSourceEditorContext()$path ) %>% stringr::str_remove('/src')
setwd(drake.path)
"

}


#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
outputs_template <- function(name)
{
  templates::tmpl("p{{name}}<-drake::readd(p{{name}})
  ggsave('./figs/p{{name}}_01.png',  p{{name}}$p1 ,width=8, height=5)
  merged.data<-drake::readd(merged.data)
  save(merged.data, file='data/data.Rdata')", name=name)

}


#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
packages_template<- function(...){
  "# packages neded
  # tidyverse, lubridate, rvest, rappdirs, data.table, fasttime, purrr, hrbrthemes

  # --------------------
  # check packages
  #---------------------

  `%ni%` = Negate(`%in%`)
  cran.depend <- c('tidyverse', 'lubridate', 'rvest', 'rappdirs','data.table', 'fasttime', 'magrittr', 'devtools', 'wbstats', 'ggplot2', 'templates' )
  plic <- installed.packages()
  cran.installs <-cran.depend[cran.depend  %ni% plic]
  if (length(cran.installs >0)) install.packages(cran.installs)
  if  ('hrbrthemes' %ni% plic) devtools::install_github('hrbrmstr/hrbrthemes')
  if  ('reproscir' %ni% plic) devtools::install_github('msandifo/reproscir')"

}

#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
plan_template <- function(name){
  templates::tmpl(
    "#--------
#drake plan
#------
pkgconfig::set_config('drake::strings_in_dots' = 'literals')
reproplan = drake::drake_plan(
  #add data munging here

  #merged.data =merge(x,y)
  p{{name}}= plots( merged.data )

)
", name=name )
}

#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
downloads_template <- function(name){
templates::tmpl("#-----
# {{name}} download
#-----
#library(magrittr)
", name=name)
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
functions_template <- function(...){
  "`%ni%` = Negate(`%in%`) "
}


#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plots_template <- function(...){
"library(ggplot2)
# --------------------
# ggplot routines
#---------------------
plots <- function(m.data) {

p01<-ggplot(m.data,aes())+


return (list(p1=p01  )
}"
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
theme_template <- function(...){
  "

library(ggplot2)
library(hrbrthemes)

if (!exists('base_size')) base_size<-16
if (!exists('ffamily')) ffamily='Arial'  #'Gill Sans'
ggplot2::theme_set( ggplot2::theme_linedraw(base_size = base_size, base_family=ffamily))
ggplot2::theme_set( hrbrthemes::theme_ipsum(axis='xy',
                                            base_size=base_size,
                                            grid=T,
                                            plot_margin = margin(10,10,10,10),
                                            axis_title_size = base_size*.8,
                                            axis_text_size = base_size*.7))
ggplot2::theme_update(
  plot.title = element_text(size = rel(1.15),family = ffamily, angle=0),
  plot.subtitle = element_text(size = rel(1.), family = ffamily, colour='grey50', angle=0),
  plot.caption = element_text(size = rel(.35),family = ffamily, colour='grey80', angle=0,hjust=1),
  panel.grid.major = element_line(colour = 'grey85', size=.05,  linetype=1),
  panel.grid.minor =element_line(colour = 'grey90', size=.0, linetype=1),
  axis.text= element_text(size = rel(1), angle=0 , family = ffamily)
)


  "
}

