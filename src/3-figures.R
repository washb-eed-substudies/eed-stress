
rm(list=ls())

source(here::here("0-config.R"))

d <- readRDS(file=paste0(here::here(),"/results/adjusted/adjusted_res.RDS"))
d_unadj <- readRDS(file=paste0(here::here(),"/results/unadjusted/unadjusted_res.RDS"))

d <- clean_results(d) %>% mutate(Y=name)
d_unadj <- clean_results(d_unadj) %>% mutate(Y=name)


#forest plots
p_14mo <- ggplot(d %>% filter(time=='14 months'), (aes(x=X, y=coef))) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=lb, ymax=ub), width = 0.75, linewidth = 1) +
  #ylim(-.4, 0.6) +
  geom_hline(yintercept = 0) +
  facet_wrap(~Y, ncol=2, scales="free") +
  coord_flip() +
  labs(y = "Adjusted mean difference", x = "EED biomarker - 14 months") +
  ggtitle("") +
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        strip.text = element_text(vjust=1),
        axis.text.y=ggtext::element_markdown(),
        text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0, face = "plain", size=12),
        panel.spacing = unit(1, "lines")) + theme_bw()

p_28mo <- ggplot(d %>% filter(time=='28 months'), (aes(x=X, y=coef))) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=lb, ymax=ub), width = 0.75, linewidth = 1) +
  #ylim(-.4, 0.6) +
  geom_hline(yintercept = 0) +
  facet_wrap(~Y, ncol=2, scales="free") +
  coord_flip() +
  labs(y = "Adjusted mean difference", x = "EED biomarker - 28 months") +
  ggtitle("") +
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        strip.text = element_text(vjust=1),
        axis.text.y=ggtext::element_markdown(),
        text = element_text(family = "Times New Roman"),
        plot.title = element_text(hjust = 0, face = "plain", size=12),
        panel.spacing = unit(1, "lines"))  + theme_bw()


#add heatmaps
plotdf14 <- d %>% filter(time=='14 months')
plotdf28 <- d %>% filter(time=='28 months')


p_heatmap14 <- plot_sig_heatmap(plotdf14, title="",
                                Outcome="Stress outcome", Exposure="EED exposure",
                                print.est=T, print.ci=F, null=0)
p_heatmap28 <- plot_sig_heatmap(plotdf28, title="",
                             Outcome="Stress outcome", Exposure="EED exposure",
                             print.est=T, print.ci=F, null=0)

#unadjusted heatmaps
plotdf14_unadj <- d_unadj %>% filter(time=='14 months')
plotdf28_unadj <- d_unadj %>% filter(time=='28 months')
p_heatmap14_unadj <- plot_sig_heatmap(plotdf14_unadj, title="",
                                Outcome="Stress outcome", Exposure="EED exposure",
                                print.est=T, print.ci=F, null=0)
p_heatmap28_unadj <- plot_sig_heatmap(plotdf28_unadj, title="",
                                Outcome="Stress outcome", Exposure="EED exposure",
                                print.est=T, print.ci=F, null=0)


#save plots
saveRDS(list(p_14mo=p_14mo, p_28mo=p_28mo), 
        file=paste0(here::here(),"/figure-data/forest_plots.rds"))
saveRDS(list(p_heatmap14=p_heatmap14,
             p_heatmap28=p_heatmap28,
             p_heatmap14_unadj=p_heatmap14_unadj,
             p_heatmap28_unadj=p_heatmap28_unadj), 
        file=paste0(here::here(),"/figure-data/heatmaps.rds"))