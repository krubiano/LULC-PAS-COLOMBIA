library(ggplot2)
library(tibble)
library(ggpubr)
library(rstatix)

#Load and prepare data
data_boxplot <- read.csv("maps_and_figures/boxplot_regions/datos_boxplot.csv")
data_boxplot$location <- factor(data_boxplot$location, levels = c("within", "buffer10", "buffer20"), ordered = TRUE)
data_boxplot$region <- factor(data_boxplot$region,levels = c("Amazon", "Andean","Caribbean", "Orinoquia", "Pacific"), ordered = TRUE)

#Tests
data_test <- data_boxplot[c("id", "location", "region", "tnv_loss_perc")]
    
data_test_andean <- data_test[data_test$region == "Andean",]
ft_andean <- friedman_test(formula = tnv_loss_perc ~ location | id, data = data_test_andean)
ft_andean
ft_label_andean <- get_test_label(ft_andean, detailed = TRUE, type = "text")

fe<-friedman_effsize(formula = tnv_loss_perc ~ location | id, data = data_test_andean,conf.level = 0.95, ci = TRUE)

pwc_andean <- PMCMRplus::frdAllPairsConoverTest(y = data_test_andean$tnv_loss_perc, g = data_test_andean$location, b = data_test_andean$id, p.adjust.method = "bonferroni")
pwc_andean

data_test_amazon <- data_test[data_test$region == "Amazon",]
ft_amazon <- friedman_test(formula = tnv_loss_perc ~ location | id, data = data_test_amazon)
ft_amazon
ft_label_amazon <- get_test_label(ft_amazon, detailed = TRUE, type = "expression")

pwc_amazon <- PMCMRplus::frdAllPairsConoverTest(y = data_test_amazon$tnv_loss_perc, g = data_test_amazon$location, b = data_test_amazon$id, p.adjust.method = "bonferroni")
pwc_amazon

data_test_caribbean <- data_test[data_test$region == "Caribbean",]
ft_caribbean <- friedman_test(formula = tnv_loss_perc ~ location | id, data = data_test_caribbean)
ft_caribbean

ft_label_caribbean <- get_test_label(ft_caribbean, detailed = TRUE, type = "expression")

pwc_caribbean <- PMCMRplus::frdAllPairsConoverTest(y = data_test_caribbean$tnv_loss_perc, g = data_test_caribbean$location, b = data_test_caribbean$id, p.adjust.method = "bonferroni")
pwc_caribbean

data_test_pacific <- data_test[data_test$region == "Pacific",]
ft_pacific <- friedman_test(formula = tnv_loss_perc ~ location | id, data = data_test_pacific)
ft_pacific

ft_label_pacific <- get_test_label(ft_pacific, detailed = TRUE, type = "expression")

pwc_pacific <- PMCMRplus::frdAllPairsConoverTest(y = data_test_pacific$tnv_loss_perc, g = data_test_pacific$location, b = data_test_pacific$id, p.adjust.method = "bonferroni")
pwc_pacific

data_test_orinoquia <- data_test[data_test$region == "Orinoquia",]
# ft_orinoquia <- friedman_test(formula = tnv_loss_perc ~ location | id, data = data_test_orinoquia)
# ft_orinoquia

pwc_pacific <- PMCMRplus::frdAllPairsConoverTest(y = data_test_pacific$tnv_loss_perc, g = data_test_pacific$location, b = data_test_pacific$id, p.adjust.method = "bonferroni")
pwc_pacific

pwc_tibble <- tribble(
  ~region, ~.y., ~group1, ~group2, ~n1, ~n2, ~statistic, ~df, ~p.adj,
  "Andean", "tnv_loss_perc", "Inside PA", "10-km buffer", 28, 28, 3.474396, 2, "< 0.01",
  "Andean", "tnv_loss_perc","10-km buffer", "20-km buffer", 28, 28,2.672612, 2, "< 0.05",
  "Andean", "tnv_loss_perc","Inside PA", "20-km buffer", 28, 28, 6.147009, 2, "< 0.001",
  "Amazon", "tnv_loss_perc","Inside PA", "10-km buffer", 10, 10, 2.012461, 2, "ns",
  "Amazon", "tnv_loss_perc","10-km buffer", "20-km buffer", 10, 10,  0, 2, "ns",
  "Amazon", "tnv_loss_perc","Inside PA", "20-km buffer", 10, 10,  2.012461, 2, "ns",
  "Caribbean", "tnv_loss_perc","Inside PA", "10-km buffer", 9, 9, 0.2357023, 2, "ns",
  "Caribbean", "tnv_loss_perc","10-km buffer", "20-km buffer", 9, 9,  1.649916, 2, "ns",
  "Caribbean", "tnv_loss_perc","Inside PA", "20-km buffer", 9, 9,  1.8856181, 2, "ns",
  "Orinoquia", "tnv_loss_perc","Inside PA", "10-km buffer", 1, 1, 0, 2, "ns",
  "Orinoquia", "tnv_loss_perc","10-km buffer", "20-km buffer", 1, 1, 0, 2, "ns",
  "Orinoquia", "tnv_loss_perc","Inside PA", "20-km buffer", 1, 1, 0, 2, "ns",
  "Pacific", "tnv_loss_perc","Inside PA", "10-km buffer", 3, 3, 1.224745, 2, "ns",
  "Pacific", "tnv_loss_perc","10-km buffer", "20-km buffer", 3, 3,  1.224745, 2, "ns",
  "Pacific", "tnv_loss_perc","Inside PA", "20-km buffer", 3, 3,  2.449490, 2, "ns")

pwc_tibble$region <- as.factor(pwc_tibble$region)

levels(data_boxplot$location) <- c("Inside PA", "10-km buffer", "20-km buffer")

# FACETS PLOT
##Without global analysis
ggplot(data_boxplot, aes(x = location, y = tnv_loss_perc, fill = location)) +
  stat_boxplot(geom = "errorbar", width = 0.15, position = position_dodge(0.75)) +
  geom_boxplot(outlier.shape = 1,
               outlier.size = 2,
               # notch = T,
               # size = 0.5
  ) +
  stat_pvalue_manual(inherit.aes = FALSE, data = pwc_tibble, label = "p.adj", step.increase = 0.05, hide.ns = TRUE,
                     y.position = 21, bracket.size = 0.5, tip.length = 0.02, size = 3) +
  # scale_y_continuous(expand = expansion(mult = c(0, 0.001))) +
  ylab("Total natural vegetation loss (%)") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.title.y = element_text(face = "bold", size = 12),
        axis.text = element_text(color = "black", size = 10),
        legend.position = "none") +
  scale_fill_brewer(palette = "Pastel2", labels = c("Inside PA", "10-km buffer", "20-km buffer")) +
  labs(fill = NULL) + 
facet_wrap(~region)

#ggsave("maps_and_figures/boxplot_regions/boxplot_tnv2.png", width = 25, height = 20, units = c("cm"), dpi = 600)


#With global analysis
data_boxplot2 <- data_boxplot
data_boxplot2$region <- "Colombia"
data_boxplot2 <- rbind(data_boxplot, data_boxplot2)

ft_all <- friedman_test(formula = tnv_loss_perc ~ location | id, data = data_test)
ft_label_all <- get_test_label(ft_all, detailed = TRUE, type = "expression")

pwc_all <- PMCMRplus::frdAllPairsConoverTest(y = data_test$tnv_loss_perc, g = data_test$location, b = data_test$id, p.adjust.method = "bonferroni")

pwc_tibble_colombia <- tribble(
  ~region, ~.y., ~group1, ~group2, ~n1, ~n2, ~statistic, ~df, ~p.adj,
  "Andean", "tnv_loss_perc", "Inside PA", "10-km buffer", 28, 28, 3.474396, 2, "< 0.01",
  "Andean", "tnv_loss_perc","10-km buffer", "20-km buffer", 28, 28,2.672612, 2, "< 0.05",
  "Andean", "tnv_loss_perc","Inside PA", "20-km buffer", 28, 28, 6.147009, 2, "< 0.001",
  "Amazon", "tnv_loss_perc","Inside PA", "10-km buffer", 10, 10, 2.012461, 2, "ns",
  "Amazon", "tnv_loss_perc","10-km buffer", "20-km buffer", 10, 10,  0, 2, "ns",
  "Amazon", "tnv_loss_perc","Inside PA", "20-km buffer", 10, 10,  2.012461, 2, "ns",
  "Caribbean", "tnv_loss_perc","Inside PA", "10-km buffer", 9, 9, 0.2357023, 2, "ns",
  "Caribbean", "tnv_loss_perc","10-km buffer", "20-km buffer", 9, 9,  1.649916, 2, "ns",
  "Caribbean", "tnv_loss_perc","Inside PA", "20-km buffer", 9, 9,  1.8856181, 2, "ns",
  "Orinoquia", "tnv_loss_perc","Inside PA", "10-km buffer", 1, 1, 0, 2, "ns",
  "Orinoquia", "tnv_loss_perc","10-km buffer", "20-km buffer", 1, 1, 0, 2, "ns",
  "Orinoquia", "tnv_loss_perc","Inside PA", "20-km buffer", 1, 1, 0, 2, "ns",
  "Pacific", "tnv_loss_perc","Inside PA", "10-km buffer", 3, 3, 1.224745, 2, "ns",
  "Pacific", "tnv_loss_perc","10-km buffer", "20-km buffer", 3, 3,  1.224745, 2, "ns",
  "Pacific", "tnv_loss_perc","Inside PA", "20-km buffer", 3, 3,  2.449490, 2, "ns",
  "Colombia", "tnv_loss_perc","Inside PA", "10-km buffer", 51, 51, 3.960590, 2, "< 0.01",
  "Colombia", "tnv_loss_perc","10-km buffer", "20-km buffer", 51, 51,  2.772413, 2, "<0.05",
  "Colombia", "tnv_loss_perc","Inside PA", "20-km buffer", 51, 51,  6.733003, 2, "< 0.001")

pwc_tibble_colombia$region <- as.factor(pwc_tibble_colombia$region)
levels(data_boxplot2$location) <- c("Inside PA", "10-km buffer", "20-km buffer")

ggplot(data_boxplot2, aes(x = location, y = tnv_loss_perc, fill = location)) +
  stat_boxplot(geom = "errorbar", width = 0.15, position = position_dodge(0.75)) +
  geom_boxplot(outlier.shape = 1,
               outlier.size = 2,
               color = "black") +
  stat_pvalue_manual(inherit.aes = FALSE, data = pwc_tibble_colombia, label = "p.adj", step.increase = 0.07, hide.ns = TRUE,
                     y.position = 21, bracket.size = 0.5, tip.length = 0.02, size = 3) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 2, fill = "black") +
  ylab("Total natural vegetation loss (%)") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.title.y = element_text(face = "bold", size = 12),
        axis.text = element_text(color = "black", size = 10),
        legend.position = "none",
        strip.text = element_text(size = 12)) +
  scale_fill_brewer(palette = "Pastel2", labels = c("Inside PA", "10-km buffer", "20-km buffer")) +
  labs(fill = NULL) + 
  facet_wrap(~factor(region, levels = c("Colombia", "Amazon", "Andean", "Caribbean", "Orinoquia", "Pacific")),
             ncol = 2,
             nrow = 3,)

ggsave("maps_and_figures/boxplot_regions/boxplot_tnv.jpeg", width = 20, height = 25, units = c("cm"), dpi = 1000)




# JOIN INDIVIDUAL PLOTS
## All PAs
levels(data_boxplot$location) <- c("Inside PA", "10-km buffer", "20-km buffer")

p_all <- ggplot(data_boxplot, aes(x = location, y = tnv_loss_perc, fill = location)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(outlier.shape = 1,
               outlier.size = 2) +
  stat_pvalue_manual(inherit.aes = FALSE, data = pwc_tibble_all, y.position = 21, step.increase = 0.05, label = "p.adj",
                     bracket.size = 0.8, tip.length = 0.02) +
  ylab("Total natural vegetation loss (%)") +
  xlab(NULL) +
  scale_x_discrete(labels = c("Inside PA", "10-km buffer", "20-km buffer")) +
  labs(subtitle = ft_label_all) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text = element_text(color = "black", size = 12)) + 
  scale_fill_brewer(palette = "Pastel2") #adjust to the study area map colors


## Amazon region
pwc_tibble_amazon <- tribble(
  ~group1, ~group2, ~p.adj,
  "Inside PA", "10-km buffer", "ns",
  "10-km buffer", "20-km buffer", "ns",
  "Inside PA", "20-km buffer", "ns")

levels(data_test_amazon$location) <- c("Inside PA", "10-km buffer", "20-km buffer")

p_amazon <- ggplot(data_test_amazon, aes(x = location, y = tnv_loss_perc, fill = location)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(outlier.shape = 1,outlier.size = 2) +
  stat_pvalue_manual(inherit.aes = FALSE, data = pwc_tibble_amazon, y.position = 21, step.increase = 0.05, label = "p.adj",
                     bracket.size = 0.8, tip.length = 0.02, hide.ns = FALSE) +
  ylab("Total natural vegetation loss (%)") +
  xlab(NULL) +
  # scale_y_continuous(breaks=seq(0,30,5)) +
  # ylim(0, 25.7)+
  scale_x_discrete(labels = c("Inside PA", "10-km buffer", "20-km buffer")) +
  labs(subtitle = ft_label_amazon) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text = element_text(color = "black", size = 12)) + 
  scale_fill_brewer(palette = "Pastel2") #adjust to the study area map colors
p_amazon

## Andes region
pwc_tibble_andean <- tribble(
  ~group1, ~group2, ~p.adj,
  "Inside PA", "10-km buffer", "< 0.01",
  "10-km buffer", "20-km buffer", "< 0.05",
  "Inside PA", "20-km buffer", "< 0.001")

levels(data_test_andean$location) <- c("Inside PA", "10-km buffer", "20-km buffer")

p_andean <- ggplot(data_test_andean, aes(x = location, y = tnv_loss_perc, fill = location)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(outlier.shape = 1,outlier.size = 2) +
  stat_pvalue_manual(inherit.aes = FALSE, data = pwc_tibble_andean, y.position = 21, step.increase = 0.05, label = "p.adj",
                     bracket.size = 0.8, tip.length = 0.02, hide.ns = FALSE) +
  ylab("Total natural vegetation loss (%)") +
  xlab(NULL) +
  # scale_y_continuous(breaks=seq(0,30,5)) +
  # ylim(0, 25.7)+
  scale_x_discrete(labels = c("Inside PA", "10-km buffer", "20-km buffer")) +
  labs(subtitle = ft_label_andean) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text = element_text(color = "black", size = 12)) + 
  scale_fill_brewer(palette = "Pastel2") #adjust to the study area map colors
p_andean


## Caribbean region
pwc_tibble_caribbean <- tribble(
  ~group1, ~group2, ~p.adj,
  "Inside PA", "10-km buffer", "ns",
  "10-km buffer", "20-km buffer", "ns",
  "Inside PA", "20-km buffer", "ns")

levels(data_test_caribbean$location) <- c("Inside PA", "10-km buffer", "20-km buffer")

p_caribbean <- ggplot(data_test_caribbean, aes(x = location, y = tnv_loss_perc, fill = location)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(outlier.shape = 1,outlier.size = 2) +
  stat_pvalue_manual(inherit.aes = FALSE, data = pwc_tibble_caribbean, y.position = 21, step.increase = 0.05, label = "p.adj",
                     bracket.size = 0.8, tip.length = 0.02, hide.ns = FALSE) +
  ylab("Total natural vegetation loss (%)") +
  xlab(NULL) +
  # scale_y_continuous(breaks=seq(0,30,5)) +
  # ylim(0, 25.7)+
  scale_x_discrete(labels = c("Inside PA", "10-km buffer", "20-km buffer")) +
  labs(subtitle = ft_label_caribbean) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text = element_text(color = "black", size = 12)) + 
  scale_fill_brewer(palette = "Pastel2") #adjust to the study area map colors
p_caribbean


## Orinoquia region
pwc_tibble_orinoquia <- tribble(
  ~group1, ~group2, ~p.adj,
  "Inside PA", "10-km buffer", "ns",
  "10-km buffer", "20-km buffer", "ns",
  "Inside PA", "20-km buffer", "ns")

levels(data_test_orinoquia$location) <- c("Inside PA", "10-km buffer", "20-km buffer")

p_orinoquia <- ggplot(data_test_orinoquia, aes(x = location, y = tnv_loss_perc, fill = location)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(outlier.shape = 1,outlier.size = 2) +
  stat_pvalue_manual(inherit.aes = FALSE, data = pwc_tibble_caribbean, y.position = 21, step.increase = 0.05, label = "p.adj",
                     bracket.size = 0.8, tip.length = 0.02, hide.ns = FALSE) +
  ylab("Total natural vegetation loss (%)") +
  xlab(NULL) +
  # scale_y_continuous(breaks=seq(0,30,5)) +
  # ylim(0, 25.7)+
  scale_x_discrete(labels = c("Inside PA", "10-km buffer", "20-km buffer")) +
  labs(subtitle = " ") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text = element_text(color = "black", size = 12)) + 
  scale_fill_brewer(palette = "Pastel2") #adjust to the study area map colors
p_orinoquia


## Pacific region
pwc_tibble_pacific <- tribble(
  ~group1, ~group2, ~p.adj,
  "Inside PA", "10-km buffer", "ns",
  "10-km buffer", "20-km buffer", "ns",
  "Inside PA", "20-km buffer", "ns")

levels(data_test_pacific$location) <- c("Inside PA", "10-km buffer", "20-km buffer")

p_pacific <- ggplot(data_test_pacific, aes(x = location, y = tnv_loss_perc, fill = location)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(outlier.shape = 1,outlier.size = 2) +
  stat_pvalue_manual(inherit.aes = FALSE, data = pwc_tibble_pacific, y.position = 21, step.increase = 0.05, label = "p.adj",
                     bracket.size = 0.8, tip.length = 0.02, hide.ns = FALSE) +
  ylab("Total natural vegetation loss (%)") +
  xlab(NULL) +
  # scale_y_continuous(breaks=seq(0,30,5)) +
  # ylim(0, 25.7)+
  scale_x_discrete(labels = c("Inside PA", "10-km buffer", "20-km buffer")) +
  labs(subtitle = ft_label_pacific) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text = element_text(color = "black", size = 12)) + 
  scale_fill_brewer(palette = "Pastel2") #adjust to the study area map colors
p_pacific


## Join plots
p_join <- ggarrange(p_all, p_amazon, p_andean, p_caribbean, p_orinoquia, p_pacific,
                    ncol = 2,
                    nrow = 3,)
p_join
#ggsave("maps_and_figures/boxplot_regions/boxplot_tnv3.png", width = 25, height = 30, units = c("cm"), dpi = 600)

# PLOT

ggplot(data_boxplot, aes(x = reorder(region,tnv_loss_perc), y = tnv_loss_perc, fill = location)) +
  stat_boxplot(geom = "errorbar", width = 0.15, position = position_dodge(0.75)) +
  geom_boxplot(outlier.shape = 1,
               outlier.size = 2.5,
               # notch = T,
               # size = 0.5
  ) +
  stat_pvalue_manual(inherit.aes = FALSE, data = pwc_tibble, label = "p.adj", step.increase = 0.05, x = "region", hide.ns = TRUE,
                     y.position = c(22, 23, 24), bracket.size = 0.8, tip.length = 0.02, size = 5) +
  ylab("Total natural vegetation loss (%)") +
  xlab(NULL) +
  # stat_summary(fun = mean, color = "black", geom = "point", size = 2,  position = position_dodge(.75)) +
  theme_bw() + 
  theme(axis.title.y = element_text(face = "bold", size = 12),
        axis.text = element_text(color = "black", size = 12),
        legend.text = element_text(size = 12, color = "black"),
        legend.position = c(0.12, 0.9),
        legend.margin = margin (5,5,5,5),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(0.5, "cm")) +
  scale_fill_brewer(palette = "Pastel2", labels = c("Inside PA", "10-km buffer", "20-km buffer")) +
  labs(fill = NULL)
  # facet_wrap(~location)

#ggsave("maps_and_figures/boxplot_regions/boxplot_tnv2.png", width = 20, height = 16, units = c("cm"), dpi = 600)


# SIN FACETS PLOT
##Without global analysis
ggplot(data_boxplot, aes(x = reorder(region, -tnv_loss_perc, median), y = tnv_loss_perc, fill = location)) +
  stat_boxplot(geom = "errorbar", width = 0.15, position = position_dodge(0.75)) +
  geom_boxplot(outlier.shape = 1,
               outlier.size = 2,
               # notch = T,
               # size = 0.5
  ) +
  # stat_pvalue_manual(inherit.aes = FALSE, data = pwc_tibble, label = "p.adj", step.increase = 0.05, hide.ns = TRUE,
                     # y.position = 21, bracket.size = 0.5, tip.length = 0.02, size = 3) +
  # scale_y_continuous(expand = expansion(mult = c(0, 0.001))) +
  ylab("Total natural vegetation loss (%)") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.title.y = element_text(face = "bold", size = 12),
        axis.text = element_text(color = "black", size = 10),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Pastel2", labels = c("Inside PA", "10-km buffer", "20-km buffer")) +
  labs(fill = NULL)

ggsave("maps_and_figures/boxplot_regions/boxplot_tnv_poster.jpeg", width = 25, height = 20, units = c("cm"), dpi = 600)

