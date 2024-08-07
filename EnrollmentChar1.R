rm(list=ls())

#source(here::here("0-config.R"))
library(tidyverse)
library(flextable)
library(officer)

d <- readRDS("~/Desktop/EED-Stress/final-data/wbb-eed-stress-data.RDS")

filtering <- function(row){
  any(!is.na(row))
}

# EED GUT BIOMARKERS UNCOMMENT AND FILL IN THIS CODE (UNCOMMENT WITH CTRL+SHIFT+C ON PC)
exp <- c("neopterin", "lactulose", "mannitol", "AAT", "myeloperoxidase", "REGB1") 
out <- c("AA", "iPF(2a)-VI", "iPF(2a)-III", "8,12-iso-iPF(2a)-VI", 
         "2,3-dinor-iPF(2a)-III") 
d1 <- d[apply(select(d, all_of(exp)), 1, filtering),] # only has rows where we have exposure data for the mom
d1 <- d1[apply(select(d1, all_of(out)), 1, filtering),] # only has rows where we have both some exposure data and some outcome data (all kids included in analyses)

m <- d1 %>% distinct(dataid, .keep_all = T)

nperc <- function(vector){
  n <- sum(vector==1, na.rm=T)
  perc <- round(n/sum(!is.na(vector))*100)
  paste(n, " (", perc, "%)", sep="")}

mediqr <- function(vector){
  quantiles <- round(quantile(vector, na.rm=T), 2)
  paste(quantiles[3], " (", quantiles[2], ", ", quantiles[4], ")", sep="")
}

child <- c('sex', 'birthord','momage','momheight','momedu',
           'NIt18','Ncomp','hfiacat','watmin',
           'elec','floor','asset_radio','asset_refrig',
           'asset_bike','asset_moto','asset_sewmach','asset_tv',
           'asset_wardrobe','asset_table','asset_chair','asset_clock',
           'asset_khat','asset_chouki','asset_mobile','n_cattle',
           'n_goat','n_chicken')


hfiacat_ind <- ifelse(m$hfiacat=="Food Secure", 0, 1)
household <- c('hfiacat_ind')
sum_hfiacat_ind <- sum(na.omit(hfiacat_ind))

n_med_col <- NULL
for (var in c(child)) {
  if (var %in% c('sex', 'diar7d_t2', 'diar7d_t3', 'ari7d_t2', "ari7d_t3") | is.factor(d[[var]])) {
    if (var == 'sex') {
      n <- sum(d$sex=='female', na.rm=T)
      perc <- round(n/sum(!is.na(d$sex))*100)
      n_med_col <- c(n_med_col, paste(n, " (", perc, "%)", sep=""))
    }else {
      d[[var]] <- na_if(d[[var]], "Missing")
      n_med_col <- c(n_med_col, nperc(d[[var]]))
    }
  }else {
    n_med_col <- c(n_med_col, mediqr(d[[var]]))
  }
}

for (var in c(mom)) {
  if (var %in% c('life_viol_any_t3') | is.factor(m[[var]])) {
    m[[var]] <- na_if(m[[var]], "Missing")
    n_med_col <- c(n_med_col, nperc(m[[var]]))
  }else {
    n_med_col <- c(n_med_col, mediqr(m[[var]]))
  }
}

n_med_col <- c(n_med_col, nperc(hfiacat_ind))


tbl1 <- data.table("C1" = c("Child", rep("", length(child)-1),
                            "Household", rep("",length(household)-1)))
                   "C2" = c("",
                            "Anthropometry (3 months)","","","",
                            "Anthropometry (14 months)","","","",
                            "Anthropometry (28 months)","","","", 
                            "Diarrhea (14 months)", "Diarrhea (28 months)",
                            "Acute respiratory illness (14 months)", "Acute respiratory illness (28 months)",
                            "","",
                            "Anthropometry at enrollment",
                            "Household Food Insecurity")
                   "C3" = c("Female",
                            "Length-for-age Z score", 
                            "Weight-for-age Z score", "Weight-for-length Z score", 
                            "Head circumference-for-age Z score",
                            "Length-for-age Z score", 
                            "Weight-for-age Z score", "Weight-for-length Z score", 
                            "Head circumference-for-age Z score",
                            "Length-for-age Z score", 
                            "Weight-for-age Z score", "Weight-for-length Z score", 
                            "Head circumference-for-age Z score",
                            "Caregiver-reported 7-day recall", 
                            "Caregiver-reported 7-day recall", 
                            "Caregiver-reported 7-day recall", 
                            "Caregiver-reported 7-day recall",
                   "C4" = n_med_col)

tbl1flex <- flextable(tbl1, col_keys=names(tbl1))
tbl1flex <- set_header_labels(tbl1flex,
                              values = list("C1" = "", "C2" = "", "C3" = "", "C4" = "n (%) or median (IQR)"))
tbl1flex <- hline_top(tbl1flex, part="header", border=fp_border(color="black", width = 1))
tbl1flex <- hline_bottom(tbl1flex, part="all", border=fp_border(color="black", width = 1))
tbl1flex <- autofit(tbl1flex, part = "all")
tbl1flex <- align(tbl1flex, j = c(1, 2, 3), align = "left", part="all")
tbl1flex <- align(tbl1flex, j = 4, align = "center", part="all")
tbl1flex <- fit_to_width(tbl1flex, max_width=8)
tbl1flex %>% add_footer_row(top=F, values = "*CESD-20 = Center for Epidemiologic Studies Depression Scale Revised.", colwidths = 4)


#sum(m$hfiacat_ind)

sect_properties <- prop_section(
  page_size = page_size(orient = "portrait", width=8.5, height=11),
  page_margins = page_mar(bottom=.3, top=.3, right=.3, left=.3, gutter = 0)
)
save_as_docx("Table 1" = tbl1flex, path="tables/enrollment/stress-eed-enrollment.docx", 
             pr_section = sect_properties) 
