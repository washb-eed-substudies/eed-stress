

rm(list=ls())

source(here::here("0-config.R"))

d <- readRDS(file=paste0(here::here(),"/results/adjusted/adjusted_res.RDS"))
d_unadj <- readRDS(file=paste0(here::here(),"/results/unadjusted/unadjusted_res.RDS"))

d <- clean_results(d, tab_format=T)

unique(d$group)

tab_ox_stress <- d %>% filter(`Child age`=='14 months') %>% subset(., select=-c(`Child age`,group))

tab_hpa <- d %>% filter(group=='Hypothalamic-pituitary-adrenal axis (Year 2)') %>% subset(., select=-c(`Child age`,group))
tab_spa <- d %>% filter(group=='Sympathetic adrenomedullary axis (Year 2)') %>% subset(., select=-c(`Child age`,group))

saveRDS(list(tab_ox_stress=tab_ox_stress,
             tab_hpa=tab_hpa,
             tab_spa=tab_spa), 
        file=paste0(here::here(),"/tables/results-table-objects.rds"))
