
rm(list=ls())

#source(here::here("0-config.R"))
library(tidyverse)
library(flextable)
library(officer)

#d <- readRDS("~/Desktop/EED-Stress/final-data/wbb-eed-stress-data.RDS")
d <- readRDS(file=paste0(here::here(),"/final-data/wbb-eed-stress-data.RDS"))

head(d)

# Create a table of the biomarker means and IQRs
Yvars_14 <- c("t2_f2_8ip","t2_f2_23d","t2_f2_VI", "t2_f2_12i")

Yvars_28 <- c("t3_map","t3_hr_mean",
              "t3_gcr_mean","t3_gcr_cpg12","t3_saa_slope","t3_cort_slope","t3_residual_saa","t3_residual_cort")

Xvars_14 <- c('ln_aat1', 'ln_mpo1', 'ln_neo1', 
              'ln_L_conc_t1', 'ln_M_conc_t1', 'ln_reg2')
Xvars_28 <- c('ln_aat2', 'ln_mpo2', 'ln_neo2', 
              'ln_L_conc_t2', 'ln_M_conc_t2')  


#fix above table code to calculate the interquartile range:
tab <- d %>% 
  select(all_of(c("childid", "dataid", "clusterid", Xvars_14, Xvars_28,Yvars_14,Yvars_28))) %>% 
  gather(key="biomarker", value="value", -c("childid", "dataid", "clusterid")) %>% 
  mutate(biomarker = factor(biomarker, levels=c(Xvars_14, Yvars_14, Xvars_28, Yvars_28))) %>% 
  group_by(biomarker) %>% 
  summarise(mean=mean(value, na.rm=TRUE), 
            q1=quantile(value, 0.25, na.rm=TRUE), 
            q3=quantile(value, 0.75, na.rm=TRUE), 
            n=sum(!is.na(value)),
            iqr=q3-q1) %>% 
  select(biomarker, mean, q1, q3, iqr, n) %>% 
  mutate(mean=round(mean, 2), q1=round(q1, 2), q3=round(q3, 2), iqr=round(iqr, 2))

#format biomarker label and save to a word document as a formated table
tab <- tab %>% 
  #add time column
  mutate(age = case_when(
    biomarker %in% Xvars_14 ~ "14 months",
    biomarker %in% Yvars_14 ~ "14 months",
    biomarker %in% Xvars_28 ~ "28 months",
    biomarker %in% Yvars_28 ~ "28 months"
  )) %>%
  mutate(biomarker = case_when(
    biomarker == "ln_aat1" ~ "log AAT",
    biomarker == "ln_mpo1" ~ "log MPO",
    biomarker == "ln_neo1" ~ "log NEO",
    biomarker == "ln_L_conc_t1" ~ "log Lactoferrin",
    biomarker == "ln_M_conc_t1" ~ "log MMP-9",
    biomarker == "ln_reg2" ~ "log Reg-2",
    biomarker == "t2_f2_8ip" ~ "F2 8-IP",
    biomarker == "t2_f2_23d" ~ "F2 23D",
    biomarker == "t2_f2_VI" ~ "F2 VI",
    biomarker == "t2_f2_12i" ~ "F2 12i",
    biomarker == "t3_map" ~ "Mean arterial pressure",
    biomarker == "t3_hr_mean" ~ "Heart rate",
    biomarker == "t3_gcr_mean" ~ "GCR",
    biomarker == "t3_gcr_cpg12" ~ "GCR CPG12",
    biomarker == "t3_saa_slope" ~ "Salivary alpha amalyse slope",
    biomarker == "t3_cort_slope" ~ "Cortisol slope",
    biomarker == "t3_residual_saa" ~ "Salivary alpha amalyse residual score",
    biomarker == "t3_residual_cort" ~ "Cortisol residual score",
    biomarker == "ln_aat2" ~ "log alpha-1-antitrypsin",
    biomarker == "ln_mpo2" ~ "log myloperoxidase",
    biomarker == "ln_neo2" ~ "log neopterin",
    biomarker == "ln_L_conc_t2" ~ "log lactulose",
    biomarker == "ln_M_conc_t2" ~ "log manitol"
  )) %>% 
  select(biomarker, age, n, mean, q1, q3) 

#paste mean, q1, q3 columns to have mean (q1, q3)
tab <- tab %>% 
  mutate(mean = paste0(mean, " (", q1, ", ", q3, ")")) %>% 
  select(-q1, -q3)

#format the table column headers
tab <- tab %>% rename("Biomarker" = biomarker, "Age" = age, "N" = n, "Mean (IQR)" = mean)

#save to word doc with officer package
library(officer)
library(flextable)
tab <- flextable(tab)
tab

doc <- read_docx()
doc <- doc %>% 
  body_add_flextable(value=tab, align="center") %>% 
  print(target=paste0(here::here(),"/tables/biomarker_tab.docx"))
