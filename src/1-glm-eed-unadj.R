rm(list=ls())

source(here::here("0-config.R"))

d <- readRDS(file=paste0(here::here(),"/final-data/wbb-eed-stress-data.RDS"))


#Loop over exposure-outcome pairs

#### Hypothesis 1 ####
Yvars <- c("t2_f2_8ip","t2_f2_23d","t2_f2_VI", "t2_f2_12i",
           "t3_map","t3_hr_mean",
           "t3_saa_z01","t3_saa_z02","t3_cort_z01","t3_cort_z03",
           "t3_gcr_mean","t3_gcr_cpg12","t3_saa_slope","t3_cort_slope","t3_residual_saa","t3_residual_cort")

Xvars <- c('ln_aat1', 'ln_mpo1', 'ln_neo1', 
           'ln_L_conc_t1', 'ln_M_conc_t1',
           'ln_aat2', 'ln_mpo2', 'ln_neo2', 
           'ln_L_conc_t2', 'ln_M_conc_t2')   

h1_missing <- paired_missing(Xvars, Yvars)


i=Xvars[1]
j=Yvars[1]

res1 <- fit_washb_glm(d=d, X=i, Y=j,  W=NULL)
res2 <- fit_washb_glm(d=d, X=i, Y=j,  W=c('sex', 'birthord', 'momage'))
res1
res2
# 
# d=d
# X=i
# Y=j
# W=NULL
# id=d$clusterid
# 
# W = NULL
# forcedW = NULL
# V = NULL
# id = "clusterid"
# family = "gaussian"
# pval = 0.2
# print = TRUE

# washb_glm(Y=d[[j]], tr=d[[i]], pair = NULL, W = NULL, forcedW = NULL, V = NULL, 
#           id, contrast, family = "gaussian", pval = 0.2, print = TRUE, 
#           verbose = FALSE, FECR = NULL)

#Fit models
H1_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    cat(i,"\n")
    cat(j,"\n")
    res <- fit_washb_glm(d=d, X=i, Y=j,  W=NULL)

    H1_res <- bind_rows(H1_res, res)
  }
}




#Save results
saveRDS(H1_res, here("results/unadjusted/H1_res.RDS"))



# ----------------------------------------------------------------------
#### Hypothesis 2 ####

##Add here###
