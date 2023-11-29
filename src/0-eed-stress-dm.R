

rm(list=ls())
source(here::here("0-config.R"))

full_data <- readRDS("C:/Users/andre/Dropbox/WASHB-EE-analysis/bangladesh-cleaned-master-data.RDS")
colnames(full_data)


# Enrollment characteristics:
#   Child sex
# Child birth order (first born, second born or greater)
# Mother’s age (years)
# Mother’s height (cm)
# Mother’s education level (no education, primary, secondary)
# Household food insecurity (4-level HFIAS categories)
# Number of children < 18 years in the household
# Number of individuals living in the compound
# Distance (in minutes) to the household's primary drinking water source
# Housing materials (floor, walls, roof)
# Asset-based household-wealth variable (continuous), calculated from the first principal
# component of a principal components analysis of the following household assets:
# electricity, wardrobe, table, chair or bench, khat, chouki, working radio, working
# black/white or color television, refrigerator, bicycle, motorcycle, sewing machine, mobile
# phone, land phone, number of cows, number of goats, number of chickens.
# 
# We will also test characteristics measured during follow-up (see directed acyclic graph (DAG) in Figure 1 below):
# Month of measurement
# Child age (in days)
# Treatment arm [control or combined nutrition, water, sanitation, and handwashing intervention (N+WSH)]
# Season of measurement 
# Perceived parental stress
# Recent diarrheal episode


#check that these are correct!
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", 
         "floor", 'roof', "HHwealth_scaled", 
         "life_viol_any_t3", "tr",
         'laz_t1', 'waz_t1')

Yvars <- c("t2_f2_8ip","t2_f2_23d","t2_f2_VI", "t2_f2_12i",
           "t3_map","t3_hr_mean",
           "t3_saa_z01","t3_saa_z02","t3_cort_z01","t3_cort_z03",
           "t3_gcr_mean","t3_gcr_cpg12","t3_saa_slope","t3_cort_slope","t3_residual_saa","t3_residual_cort")

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1', 
           'ln_L_conc_t1', 'ln_M_conc_t1',
           'ln_aat2', 'ln_mpo2', 'ln_neo2', 
           'ln_L_conc_t2', 'ln_M_conc_t2')   


d <- full_data %>% select("childid","clusterid", all_of(Xvars), all_of(Yvars), all_of(Wvars)) %>% 
  filter(!rowSums(is.na(select(., , all_of(Yvars), all_of(Xvars)))) == length(c(Xvars, Yvars)))

saveRDS(d, file=paste0(here::here(),"/final-data/wbb-eed-stress-data.RDS"))
