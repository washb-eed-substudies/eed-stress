
# i=Xvars[1]
# j=Yvars[1]
# 
# d=d
# X=i
# Y=j
# W=NULL
# id=d$clusterid
# 
# W=c('sex', 'birthord', 'momage')
# forcedW = NULL
# V = NULL
# id = "clusterid"
# family = "gaussian"
# pval = 0.2
# print = TRUE


fit_washb_glm <- function (d, Y, X, W = NULL, forcedW = NULL, V = NULL, id = "clusterid",
          family = "gaussian", pval = 0.2, print = TRUE){
 
 cat("\nNon-prescreened covariates: ", paste(forcedW, sep = "", 
                                              collapse = ", "), "\n")
  set.seed(12345)
  require(dplyr)
  #require(faraway)
  if(!is.null(V)) {
    require(lmtest)
  }
  if(!is.null(W)) {
    W <- subset(d, select = W)
  }
  Y <- subset(d, select = Y)
  colnames(Y) <- "Y"
  X <- subset(d, select = X)
  colnames(X) <- "X"
  id <- subset(d, select = id)
  colnames(id) <- "id"
  if(!is.null(V)) {
    Vvar <- subset(d, select = V)
    colnames(Vvar) <- "V"
  }else{
    Vvar <- data.frame(V = rep(1, nrow(d)))
  }
  collinear_vars <- NULL
  if(!is.null(W)) {
    glmdat <- data.frame(Y, X, id, Vvar, W)
  }else{
    glmdat <- data.frame(Y, X, id, Vvar)
  }
  if(!is.null(W)) {
    if(sum(is.na(forcedW)) != 0) {
      colnamesW <- names(W)
    }else{
      if(is.null(forcedW)) {
        Wnames <- names(W)
        forcedW <- c(Wnames[Wnames == "tr" | grepl("age_", 
                                                   Wnames) | grepl("agedays_", Wnames) | grepl("ageday_", 
                                                                                               Wnames)])
      }
      cat("\nNon-prescreened covariates: ", paste(forcedW, 
                                                  sep = "", collapse = ", "), "\n")
      colnamesW <- names(W)[!(names(W) %in% forcedW)]
    }
    screenW <- subset(glmdat, select = colnamesW)
  }else{
    screenW <- NULL
  }
  if(!is.null(screenW)) {
    if(print == TRUE) {
      cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
    }
    suppressWarnings(Wscreen <- washb_prescreen(Y = glmdat$Y, Ws = screenW, family = family, pval = pval, print = print))
    if(!is.null(forcedW)) {
      Wscreen <- c(as.character(Wscreen), as.character(forcedW))
    }
    W <- subset(glmdat, select = Wscreen)
    Wdf <- W
    Wdf$constant <- rep(1, nrow(glmdat))
    for (i in 1:ncol(W)) {
      tmp <- glm(constant ~ ., data = Wdf, family = family)
      todrop <- NULL
      todrop <- suppressWarnings(names(tmp$coefficients)[-1][as.vector(vif(tmp)) > 
                                                               10][1])
      if(!is.null(todrop) & !is.na(todrop)) {
        collinear_vars <- c(collinear_vars, todrop)
        Wdf <- Wdf[, colnames(Wdf) != todrop]
      }
    }
    to_keep <- colnames(W)[!(colnames(W) %in% collinear_vars)]
    if(length(to_keep) != length(colnames(W))) {
      cat("\nDropped for collinearity with other covariates:\n", 
          colnames(W)[!(colnames(W) %in% to_keep)])
    }
    W_processed <- W[which(colnames(W) %in% to_keep)]
    Wscreen <- colnames(W_processed)
    cat("\n\nCovariated included in model:\n", Wscreen)
  }else{
    Wscreen = NULL
  }
  if(!is.null(Wscreen)) {
    d <- subset(glmdat, select = c("Y", "X", "id", "V", Wscreen))
  }else{
    d <- subset(glmdat, select = c("Y", "X", "id", "V"))
  }
  fullrows <- nrow(d)
  d <- d %>% filter(!is.na(Y))
  Yrows <- nrow(d)
  cat("\nRows dropped due to missing outcome: ", fullrows - 
        Yrows, "\n")
  d <- d %>% filter(!is.na(X))
  Xrows <- nrow(d)
  cat("Rows dropped due to missing exposure: ", Yrows - Xrows, 
      "\n")
  if(!is.null(W) & length(Wscreen) > 0) {
    cat("Percent missingness by covariate:\n")
    print(sapply(d[, -c(1:3)], function(x) round(sum(is.na(x))/nrow(X) * 
                                                   100, 1)))
    d <- d[complete.cases(d), ]
    cat("\nRows dropped due to missing covariates: ", Xrows - 
          nrow(d), "\n")
  }
  cat("Final sample size: ", nrow(d), "\n")

  if(!is.null(W) & length(Wscreen) > 0){
    glmdat <- subset(d, select = c('Y','X',Wscreen))
 
    suppressWarnings(fit <- glm(Y ~ ., family = family, data = glmdat))
    vcovCL <- sandwichSE(glmdat, fm = fit, cluster = d$id)
    rfit <- coeftest(fit, vcovCL)
    res <- data.frame(t(rfit[2,]))
    colnames(res) <- c("coef","se","zval","pval")
    res$lb <- res$coef - 1.96 * res$se
    res$ub <- res$coef + 1.96 * res$se
    
  }else{
  
    suppressWarnings(fit <- glm(Y ~ X, family = family, data = d))
    vcovCL <- sandwichSE(d, fm = fit, cluster = d$id)
    rfit <- coeftest(fit, vcovCL)
    res <- data.frame(t(rfit[2,]))
    colnames(res) <- c("coef","se","zval","pval")
    res$lb <- res$coef - 1.96 * res$se
    res$ub <- res$coef + 1.96 * res$se
  }
   
   return(res)
}












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



####################Plot_sig_heatmap#################
##Function
plot_sig_heatmap <- function(d, title="",
                             Outcome="Outcome", Exposure="Exposure",
                             print.est=T, print.ci=F,
                             null=0){
  
  require(RColorBrewer)
  
  
  dfull <- expand_grid(unique(d$Y), unique(d$X))
  colnames(dfull) <- c("Y","X")
  d <- left_join(dfull, d, by=c("Y","X"))
  d <- distinct(d)
  
  #Get direction of estimate
  if(null==0){
    d$sign <- sign(d$coef)
  }else{
    d$sign <- ifelse(d$coef>1,1,-1)
  }
  
  #Get significance category
  d$pval_cat <- cut(d$pval, breaks = c(-1,0.01, 0.05, 0.2, 0.5, 2), labels = c("<0.01","<0.05","0.05-0.2","0.2-0.5","0.5-1"))
  d$pval_cat <- ifelse(d$sign== 1, paste0(d$pval_cat, " increase"), paste0(d$pval_cat, " decrease"))
  d$pval_cat[d$pval_cat %in% c("0.5-1 decrease", "0.5-1 increase")] <- "0.5-1"
  table(d$pval_cat)
  d$pval_cat <- factor(d$pval_cat, levels = c("<0.01 decrease",
                                              "<0.05 decrease", "0.05-0.2 decrease", "0.2-0.5 decrease",
                                              "0.5-1", "0.05-0.2 increase", "0.2-0.5 increase",
                                              "<0.05 increase", "<0.01 increase"))
  
  d$pval_cat <- addNA(d$pval_cat)
  levels(d$pval_cat) = c(levels(d$pval_cat), "Not estimated")
  d$pval_cat[is.na(d$pval_cat)] <- "Not estimated"
  
  table(d$pval_cat)
  table(is.na(d$pval_cat))
  
  d$est=""
  if(print.est){
    d$est=round(d$coef, 2)
    if(print.ci){
      d$est= paste0(
        round(d$est, 2), " (",
        round(d$lb.diff, 2), ", ",
        round(d$ub.diff, 2), ")"
      )
    }
  }
  d$est=gsub("NA \\(NA, NA\\)","",d$est)
  
  
  textcol = "grey20"
  cols = rev(brewer.pal(n = 9, name = "Spectral"))
  
  colours <- c("<0.01 decrease" = cols[1],
               "<0.05 decrease" = cols[2],
               "0.05-0.2 decrease"  = cols[3],
               "0.2-0.5 decrease"  = cols[4],
               "0.5-1" = cols[5],
               "0.2-0.5 increase" = cols[6],
               "0.05-0.2 increase" = cols[7],
               "<0.05 increase" = cols[8],
               "<0.01 increase" = cols[9],
               "Not estimated"="gray80")
  d <- droplevels(d)
  
  hm <- ggplot(d, aes(x=X, y=Y, fill=pval_cat)) +
    geom_tile(colour="grey80",size=0.25) +
    scale_x_discrete(expand=c(0,0), limits = rev(levels(d$X)))+
    scale_y_discrete(expand=c(0,0))+
    theme_minimal(base_size=10) +
    scale_fill_manual(#labels = levels(d$pval_cat),
      values = colours, drop = FALSE) +
    geom_text(aes(label=est)) +
    theme(
      aspect.ratio = 1,
      legend.title=element_text(color=textcol,size=8),
      legend.margin = margin(grid::unit(0.1,"cm")),
      legend.text=element_text(colour=textcol,size=8),
      legend.key.height=grid::unit(0.2,"cm"),
      legend.key.width=grid::unit(1,"cm"),
      legend.position = "right",
      axis.text.x=element_text(size=8,colour=textcol,angle=45,hjust=1),
      #axis.text.x=element_text(size=8,colour=textcol),
      text = element_text(family = "Times New Roman"),
      axis.text.y=element_text(size=8,vjust = 0.2,colour=textcol),
      axis.ticks=element_line(size=0.4),
      plot.title=element_text(colour=textcol,hjust=0,size=12),
      strip.text.x = element_text(size=10),
      strip.text.y = element_text(angle=0,size=10),
      plot.background=element_blank(),
      panel.border=element_blank(),
      strip.background = element_blank(),
      panel.background=element_rect(fill="grey80", colour="grey80"),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank()
    ) +
    guides(fill = guide_legend("P-value strength", ncol=1)) +
    labs(x=Exposure,y=Outcome,title=title)
  hm
  
  return(hm)
}




#clean biomarker names
clean_results <- function(d, tab_format=F){
  
  head(d)
  d <- d %>% mutate(
    X = case_when(str_detect(X, "aat") ~ "AAT",
                  str_detect(X, "mpo") ~ "MPO",
                  str_detect(X, "neo") ~ "NEO", 
                  str_detect(X, "reg2") ~ "REG1B",
                  str_detect(X, "L_") ~ "Lact", 
                  str_detect(X, "M_") ~ "Mann")
  )
  
  stress_outcome_mapping <- rbind(
    data.frame(Y="t2_f2_8ip", name="iPF(2a)-III", unit="ng/mg creatinine", age=14, group="Oxidative stress (Year 1)"),
    data.frame(Y="t2_f2_23d", name="2,3-dinor-iPF(2a)-III", unit="ng/mg creatinine", age=14, group="Oxidative stress (Year 1)"),
    data.frame(Y="t2_f2_VI", name="iPF(2a)-VI", unit="ng/mg creatinine", age=14, group="Oxidative stress (Year 1)"),
    data.frame(Y="t2_f2_12i", name="8,12-iso-iPF(2a)-VI", unit="ng/mg creatinine", age=14, group="Oxidative stress (Year 1)"),
    data.frame(Y="t3_saa_z01", name="Pre-stressor salivary alpha-amylase", unit="U/ml", age=28, group="Sympathetic adrenomedullary axis (Year 2)"),
    data.frame(Y="t3_saa_z02", name="Post-stressor salivary alpha-amylase", unit="U/ml", age=28, group="Sympathetic adrenomedullary axis (Year 2)"),
    data.frame(Y="t3_saa_slope", name="Slope between pre- and  post-stressor alpha-amylase", unit="U/ml/min", age=28, group="Sympathetic adrenomedullary axis (Year 2)"),
    data.frame(Y="t3_residual_saa", name="Residualized gain score for alpha-amylase", unit="U/ml", age=28, group="Sympathetic adrenomedullary axis (Year 2)"),
    data.frame(Y="t3_cort_z01", name="Pre-stressor salivary cortisol", unit="ug/dl", age=28, group="Hypothalamic-pituitary-adrenal axis (Year 2)"),
    data.frame(Y="t3_cort_z03", name="Post-stressor salivary cortisol", unit="ug/dl", age=28, group="Hypothalamic-pituitary-adrenal axis (Year 2)"),
    data.frame(Y="t3_cort_slope", name="Slope between pre- and  post-stressor cortisol", unit="\u03bcg/dl/min", age=28, group="Hypothalamic-pituitary-adrenal axis (Year 2)"),
    data.frame(Y="t3_residual_cort", name="Residualized gain score for cortisol", unit="\u03bcg/dl", age=28, group="Hypothalamic-pituitary-adrenal axis (Year 2)"),
    data.frame(Y="t3_map", name="Mean arterial pressure", unit="mmHg", age=28, group="Sympathetic adrenomedullary axis (Year 2)"),
    data.frame(Y="t3_hr_mean", name="Resting heart rate", unit="bpm", age=28, group="Sympathetic adrenomedullary axis (Year 2)"),
    data.frame(Y="t3_gcr_mean", name="NR3C1 exon 1F promoter methylation",unit="", age=28, group="Hypothalamic-pituitary-adrenal axis (Year 2)"),
    data.frame(Y="t3_gcr_cpg12", name="NGFI-A transcription factor binding site methylation",unit="", age=28, group="Hypothalamic-pituitary-adrenal axis (Year 2)")
  ) %>% 
    mutate(group=factor(group, levels=c("Oxidative stress (Year 1)", "Hypothalamic-pituitary-adrenal axis (Year 2)", "Sympathetic adrenomedullary axis (Year 2)"))) %>%
    arrange(group)
  
  d <- left_join(d, stress_outcome_mapping, by=c("Y"))
  head(d)
  
  if(tab_format){
    d <- d %>% mutate(Estimate=paste0(sprintf("%1.3f",coef), " (", sprintf("%1.3f",lb),", ", sprintf("%1.3f",ub), ")"), pval=sprintf("%1.4f",pval)) %>%
      select(X,name, unit,time, group,Estimate, pval, corrected.Pval) %>% 
      rename(`EED exposure`=X, `Stress outcome`=name, `Meas. unit`=unit, `Child age`=time, `P-value`=pval, `FDR-corrected P-value`=corrected.Pval) 
  }
  
  return(d)
}

