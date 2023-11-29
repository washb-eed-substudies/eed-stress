
#-------------------------------------
# EE substudies analysis 

# configure data directories
# source base functions
# load libraries
#-------------------------------------

library(tidyverse)
library(haven)
library(washb)
library(foreign)
library(data.table)
library(tmle)
library(SuperLearner)
library(devtools)
library(kableExtra)
library(here)
library(washbgam)

dropboxDir <- NULL
if(dir.exists("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/")){ 
  dropboxDir <- "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/"
}
if(dir.exists("/Users/audrielin/Dropbox/WBB-EE-analysis/")){ 
  dropboxDir <- "/Users/audrielin/Dropbox/WBB-EE-analysis/"
}
if(dir.exists("C:/Users/Sophia/Dropbox/WASH/")){ 
  dropboxDir <- "C:/Users/Sophia/Dropbox/WASH/"
}
if(dir.exists("/Users/lisa/Dropbox/WASH/")){ 
  dropboxDir <- "/Users/lisa/Dropbox/WASH/"
}
if(dir.exists("/Users/caitlinhemlock/Dropbox/")){ 
  dropboxDir <- "/Users/caitlinhemlock/"
}
if(dir.exists("/Users/zbutzindozier/Dropbox/WBB-EE-analysis/")){ 
  dropboxDir <- "/Users/zbutzindozier/Dropbox/WBB-EE-analysis/"
}




theme_ki<-function(){
  theme_bw() %+replace%
    theme(
      strip.background = element_blank(),
      legend.position="none",
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size=14),
      axis.title = element_text(size=12),
      axis.text.y = element_text(size=10),
      axis.text.x = element_text(size=10, angle = 0, hjust = 0.5, vjust=.1)
    )
}

theme_set(theme_ki())

tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F",
               "#BCBD22","#17BECF")

if(!require(faraway)){
  install.packages("faraway")
  library(faraway)
}




#-----------------------------------------------------------
# Functions
#-----------------------------------------------------------

# to calculate missings for each 
# exposure-outcome pair
paired_missing <- function(Xvars = Xvars, Yvars = Yvars){
  xvar <- list()
  yvar <- list()
  missing_x  <- list()
  missing_y  <- list()
  missing_or  <- list()
  total <- list()
  for(x in Xvars){
    for(y in Yvars){
      
      xvar = c(xvar, x)
      yvar = c(yvar, y)
      missing_x = c(missing_x, sum(is.na(d[x])))
      missing_y = c(missing_y, sum(is.na(d[y])))
      missing_or = c(missing_or, sum(is.na(d[x]) | is.na(d[y])))
      total = c(total, max(dim(d[x][1]), dim(d[y][1])))
    }
  }
  df <- data.frame(list('xvar' = unlist(xvar), 'yvar' = unlist(yvar), 
                        'missing_x' = unlist(missing_x),  
                        'missing_y' = unlist(missing_y), 
                        'missing_one' = unlist(missing_or), 'total' = unlist(total)))
  df <- mutate(df, percent = round(missing_one/total, 3))
  
  return(df)
}

