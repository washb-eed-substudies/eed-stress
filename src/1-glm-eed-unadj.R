rm(list=ls())

source(here::here("0-config.R"))

d <- readRDS(file=paste0(here::here(),"/final-data/wbb-eed-stress-data.RDS"))

#NOTE! Just do concurrent analyses


Yvars_14 <- c("t2_f2_8ip","t2_f2_23d","t2_f2_VI", "t2_f2_12i")

Yvars_28 <- c("t3_map","t3_hr_mean",
              #"t3_saa_z01","t3_saa_z02","t3_cort_z01","t3_cort_z03", #just look at slope and residuals
              "t3_gcr_mean","t3_gcr_cpg12","t3_saa_slope","t3_cort_slope","t3_residual_saa","t3_residual_cort")


Xvars_14 <- c('ln_aat1', 'ln_mpo1', 'ln_neo1', 
           'ln_L_conc_t1', 'ln_M_conc_t1', 'ln_reg2')
Xvars_28 <- c('ln_aat2', 'ln_mpo2', 'ln_neo2', 
           'ln_L_conc_t2', 'ln_M_conc_t2')  

missing_14 <- paired_missing(Xvars_14, Yvars_14)

res_14 <- NULL
for(i in Xvars_14){
  for(j in Yvars_14){
    cat(i,"\n")
    cat(j,"\n")
    res <- fit_washb_glm(d=d, X=i, Y=j,  W=NULL)
    res$X <- i
    res$Y <- j
    res_14 <- bind_rows(res_14, res)
  }
}
res_14


missing_28 <- paired_missing(Xvars_28, Yvars_28)

res_28 <- NULL
for(i in Xvars_28){
  for(j in Yvars_28){
    cat(i,"\n")
    cat(j,"\n")
    res <- fit_washb_glm(d=d, X=i, Y=j,  W=NULL)
    res$X <- i
    res$Y <- j
    res_28 <- bind_rows(res_28, res)
  }
}
res_28



#correct p-values 
full_res <- bind_rows(res_14 %>% mutate(time='14 months'),
                      res_28 %>% mutate(time='28 months'))

full_res <- full_res %>% group_by(time) %>%
  mutate(corrected.Pval=p.adjust(pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()


#Save results
saveRDS(full_res, here("results/unadjusted/unadjusted_res.RDS"))
write.csv(full_res, here("results/unadjusted/unadjusted_res.csv"))

