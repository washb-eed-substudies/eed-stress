rm(list=ls())

source(here::here("0-config.R"))

d <- readRDS(file=paste0(here::here(),"/final-data/wbb-eed-stress-data.RDS"))

#Set list of adjustment variables
#Make vectors of adjustment variable names
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


Wvars[!(Wvars %in% colnames(d))]

# Loop over exposure-outcome pairs

# --------------------------------------------------------------------------
#### Hypothesis 1 ####


#Fit models
H1a_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wvars)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1a_adj_models <- bind_rows(H1a_adj_models, res)
  }
}



#Get primary contrasts
H1a_adj_res <- NULL
for(i in 1:nrow(H1a_adj_models)){
  res <- data.frame(X=H1a_adj_models$X[i], Y=H1a_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H1a_adj_models$fit[i][[1]], d=H1a_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1a_adj_res <-  bind_rows(H1a_adj_res , preds$res)
}

#Make list of plots
H1a_adj_plot_list <- NULL
H1a_adj_plot_data <- NULL
for(i in 1:nrow(H1a_adj_models)){
  res <- data.frame(X=H1a_adj_models$X[i], Y=H1a_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H1a_adj_models$fit[i][[1]], H1a_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1a_adj_plot_list[[i]] <-  simul_plot$p
  H1a_adj_plot_data <-  rbind(H1a_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H1a_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H1a_adj_models.RDS"))

#Save results
saveRDS(H1a_adj_res, here("results/adjusted/H1a_adj_res.RDS"))


#Save plots
#saveRDS(H1a_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1a_adj_splines.RDS"))

#Save plot data
#saveRDS(H1a_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H1a_adj_spline_data.RDS"))
