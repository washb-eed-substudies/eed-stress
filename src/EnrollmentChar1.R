rm(list=ls())

#source(here::here("0-config.R"))
library(tidyverse)
library(flextable)
library(officer)

#d <- readRDS("~/Desktop/EED-Stress/final-data/wbb-eed-stress-data.RDS")
d <- readRDS(file=paste0(here::here(),"/final-data/wbb-eed-stress-data.RDS"))
d$ageday_st2 <- d$ageday_st2/30.4167
d$ageday_st3 <- d$ageday_st3/30.4167
#add child age 

head(d)



filtering <- function(row){
  any(!is.na(row))
}

# EED GUT BIOMARKERS UNCOMMENT AND FILL IN THIS CODE (UNCOMMENT WITH CTRL+SHIFT+C ON PC)
exp <- c("ln_neo1", "ln_neo2", "ln_L_conc_t1", "ln_L_conc_t2", "ln_M_conc_t1", "ln_M_conc_t2","ln_aat1","ln_aat2","ln_mpo1","ln_mpo2","ln_reg2") 
out <- c("t3_saa_z01", "t3_saa_z02", "t2_f2_8ip", "t2_f2_23d","t2_f2_VI","t2_f2_12i","t3_map","t3_hr_mean","t3_cort_z01","t3_cort_z03","t3_gcr_mean","t3_gcr_cpg12") 
d1 <- d[apply(select(d, all_of(exp)), 1, filtering),] # only has rows where we have exposure data for the mom
d1 <- d1[apply(select(d1, all_of(out)), 1, filtering),] # only has rows where we have both some exposure data and some outcome data (all kids included in analyses)

m <- d1 %>% distinct(dataid, .keep_all = T)
table(d$momedu)

nperc <- function(vector){
  n <- sum(vector==1, na.rm=T)
  perc <- round(n/sum(!is.na(vector))*100)
  paste(n, " (", perc, "%)", sep="")}

mediqr <- function(vector){
  quantiles <- round(quantile(vector, na.rm=T), 2)
  paste(quantiles[3], " (", quantiles[2], ", ", quantiles[4], ")", sep="")
}

vector=m$n_goat

mediqr(m$n_goat)


m$hfiacat_ind <- factor(ifelse(m$hfiacat=="Food Secure", 0, 1))

#recode education
table(m$momedu)
m$momedu_bin <- factor(ifelse(m$momedu=="Secondary (>5y)", "1", "0"))

table(m$n_goat)
class(m$n_goat)

vars <- c('sex', 'birthord',"ageday_st2","ageday_st3",'momage','momheight','momedu_bin',
          'Ncomp','Nlt18','watmin',
           'elec','floor','asset_radio','asset_refrig',
           'asset_bike','asset_moto','asset_sewmach','asset_tv',
           'asset_wardrobe','asset_table','asset_chair','asset_clock',
           'asset_khat','asset_chouki','asset_mobile','n_cattle',
           'n_goat','n_chicken','hfiacat_ind')




n_med_col <- NULL
for (var in c(vars)) {
  if (var %in% c('sex', 'diar7d_t2', 'diar7d_t3', 'ari7d_t2', "ari7d_t3") | is.factor(m[[var]])) {
    if (var == 'sex') {
      n <- sum(m$sex=='female', na.rm=T)
      perc <- round(n/sum(!is.na(m$sex))*100)
      n_med_col <- c(n_med_col, paste(n, " (", perc, "%)", sep=""))
    }else {
      m[[var]][m[[var]]=="missing" | m[[var]]=="Missing"] <- NA 
      n_med_col <- c(n_med_col, nperc(m[[var]]))
    }
  }else {
    n_med_col <- c(n_med_col, mediqr(m[[var]]))
  }
}


tbl1 <- data.frame(var=vars, Variable= c('Sex', 'Birth order','Child age- 14 month visit','Child age- 28 month visit','Maternal age','Maternal height','Mom has at least secondary education',
                                 'Number in compound','Number under 18 in compound','Distance to water source',
                                 'Electricity','Improved floor','Owns radio','Owns refrigerator',
                                 'Owns bike','Owns motorcycle','Owns sewing machine','Owns tv',
                                 'Owns wardrobe','Owns table','Owns chair','Owns clock',
                                 'Owns Khat','Owns Chouki','Owns mobile phone','Number of cattle',
                                 'Number of goats','Number of chickens',"Household Food Insecurity"),
                                Statistic=n_med_col)

tbl1flex <- flextable(tbl1, col_keys=c('Variable', 'Statistic'))
tbl1flex <- set_header_labels(tbl1flex, values = list("Variable" = "Variable", "Statistic" = "n (%) or median (IQR)"))
tbl1flex <- hline_top(tbl1flex, part="header", border=fp_border(color="black", width = 1))
tbl1flex <- hline_bottom(tbl1flex, part="all", border=fp_border(color="black", width = 1))
tbl1flex <- autofit(tbl1flex, part = "all")
tbl1flex <- align(tbl1flex, j = c(1, 2), align = "left", part="all")
#tbl1flex <- align(tbl1flex, j = 4, align = "center", part="all")
tbl1flex <- fit_to_width(tbl1flex, max_width=8)
#tbl1flex %>% add_footer_row(top=F, values = "*CESD-20 = Center for Epidemiologic Studies Depression Scale Revised.", colwidths = 4)


#sum(m$hfiacat_ind)

sect_properties <- prop_section(
  page_size = page_size(orient = "portrait", width=8.5, height=11),
  page_margins = page_mar(bottom=.3, top=.3, right=.3, left=.3, gutter = 0)
)
save_as_docx("Table 1" = tbl1flex, path="tables/stress-eed-enrollment.docx", 
             pr_section = sect_properties) 


#From Lexie:

df <- tibble(
  Type = c("Exposure","Exposure", "Exposure", "Exposure", "Exposure", "Exposure", 
           "Exposure","Exposure","Exposure", "Exposure","Exposure", "Outcome", 
           "Outcome", "Outcome", "Outcome", "Outcome", "Outcome", "Outcome", "Outcome", 
           "Outcome","Outcome","Outcome"),
  Biomarker = c("Alpha-1-antitrypsin","Alpha-1-antitrypsin","Lactulose","Lactulose",
                "Mannitol","Mannitol","Myeloperoxidase","Myeloperoxidase","Neopterin","Neopterin",
                "REGB1",
                "Alpha-amylase","Alpha-amylase","Cortisol","Cortisol","F2_8IP","F2_23D",
                "F2_VI","F2_12i","GCR Methyl","HR","MAP"),
  Timepoint = c("3 mo.", "14 mo.","3 mo.", "14 mo.","3 mo.", "14 mo.","3 mo.", "14 mo.","3 mo.", "14 mo.","14 mo.",
                "28 mo.", "28 mo.", "28 mo.", "28 mo.","14 mo.", "14 mo.","14 mo.", "14 mo.","14 mo.","28 mo.","28 mo."),
  Median = c(10, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 6, 7, 8, 8, 9, 22)
)

#pivot wide, with columns as  timepoints, and values from the median column
df %>% 
  pivot_wider(names_from = c(Timepoint), values_from = Median)


wide_df <- df %>%
  pivot_wider(names_from = Biomarker, values_from = Timepoint)
  #pivot_wider(names_from = Biomarker, values_from = Timepoint, values_fill = list(Timepoint = 0))
print(wide_df)
summary_df <- df %>%
  group_by(Type, Biomarker) %>%
  summarize(Total_Value = sum(Value), .groups = 'drop')
print(summary_df)

