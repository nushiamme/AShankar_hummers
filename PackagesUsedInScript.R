library(here)
library(stringr)
library(NCmisc)
library(readr)
fn <- here("Code", "TorporShallowDeep", "Thermal_Interpolation.R")
if (str_detect(fn,"\\.Rmd")) {
  new.fn <- str_replace(fn,"\\.Rmd","\\.R")
  knitr::purl(fn,output=new.fn)
  made.new <- T
} else {
  new.fn <- fn
  made.new <- F
}
the.funs <- NCmisc::list.functions.in.file(new.fn)
packages <- names(the.funs)
packages.used <- unique(unlist(str_extract_all(packages,"(?<=package:)[:alnum:]+")))
to.parse <- read_lines(new.fn)
packages.loaded <- unique(unlist(str_extract_all(to.parse,"(?<=library\\()[:alnum:]+(?=\\))")))
packages.loaded[!packages.loaded %in% packages.used]
# if (made.new) file.remove(new.fn)
