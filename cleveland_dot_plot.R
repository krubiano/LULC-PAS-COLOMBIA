library(ggplot2)
library(forcats)
library(dplyr)
library(ggpubr)


data <- read.csv("maps_and_figures/dot_plot/datos_sort_forest.csv", sep = ";")
data$protected_areas_id <- paste0(data$protected_areas, " (", data$id, ")")
# data$region <- factor(x = data$region, levels = c("Amazon", "Andean", "Caribbean", "Orinoquia", "Pacific"))
#shapes <- c("Inside PA" = 19, "10-km buffer" = 17, "20-km buffer" = 15)

###TNV loss inside and around PA
##NO FACETS
ggplot(data) +
  geom_point(aes(x = reorder(paste0(protected_areas, " (", id, ")"), tnv_loss_perc_p), y = tnv_loss_perc_p, colour = region, shape = "Inside PA"), size = 2) +
  geom_point(aes(x = paste0(protected_areas, " (", id, ")"), y = tnv_loss_perc_10, colour = region, shape = "10-km buffer"), size = 2) +
  geom_point(aes(x = paste0(protected_areas, " (", id, ")"), y = tnv_loss_perc_20, colour = region, shape = "20-km buffer"), size = 2) +
  ylab("Total natural vegetation loss (%)") +
  xlab("Protected Area (id)") +
  scale_color_manual(values = c("#4daf4a", "#377eb8", "#e41a1c", "#984ea3","#ff7f00"),
                     labels = c("Amazon", "Andean", "Caribbean", "Orinoquia", "Pacific")) +
  coord_flip() +
  guides(color = guide_legend(title = "Region", override.aes = list(shape = 15)),
         shape = guide_legend(reverse = TRUE, title = "Location")) +
  labs(shape = "Location", colour = "region") + 
  theme_bw() +
  theme(
    panel.grid.major.y = element_line(colour = "grey80", linetype = "dotted"),
    axis.title.x = element_text(color = "black", face = "bold", size = 12),
    axis.title.y = element_text(color = "black", face = "bold", size = 12),
    axis.text = element_text(color = "black", size = 9),
    legend.position = c(0.9, 0.675),           # Put legend inside plot area
    legend.margin = margin (3,3,3,3),
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold"))

# ggsave("maps_and_figures/dot_plot/dot_plot_tnv_loss.png", width = 24, height = 20, units = c("cm"), dpi = 600)


#WITH FACETS and lines for region
data_summary_pa <- read.csv("maps_and_figures/dot_plot/LCC_STATISTICS_PARK_REG_TVNL.csv")
data_summary_10 <- read.csv("maps_and_figures/dot_plot/LCC_STATISTICS_BUFFER_REG_TVNL.csv")
data_summary_20 <- read.csv("maps_and_figures/dot_plot/LCC_STATISTICS_BUFFER_20_REG_TVNL.csv")

ggplot(data) +
  geom_point(aes(x = reorder(paste0(protected_areas, " (", id, ")"), tnv_loss_perc_p), y = tnv_loss_perc_p, colour = region, shape = "Inside PA"), size = 2) +
  geom_point(aes(x = paste0(protected_areas, " (", id, ")"), y = tnv_loss_perc_10, colour = region, shape = "10-km buffer"), size = 2) +
  geom_point(aes(x = paste0(protected_areas, " (", id, ")"), y = tnv_loss_perc_20, colour = region, shape = "20-km buffer"), size = 2) +
  geom_hline(data = data_summary_pa, aes(yintercept = tnv_loss_perc),  color = "black", linetype = "dashed", size = 0.5) +
  geom_hline(data = data_summary_10, aes(yintercept = tnv_loss_perc),  color = "black", linetype = "dotted", size = 0.5) +
  geom_hline(data = data_summary_20, aes(yintercept = tnv_loss_perc),  color = "black", linetype = "dotdash", size = 0.5) +
  ylab("Total natural vegetation loss (%)") +
  xlab("Protected Area (id)") +
  scale_color_manual(values = c("#4daf4a", "#377eb8", "#e41a1c", "#984ea3","#ff7f00"),
                     labels = c("Amazon", "Andean", "Caribbean", "Orinoquia", "Pacific")) +
  coord_flip() +
  guides(color = "none", shape = guide_legend(title = "Location")) +
  # scale_fill_discrete(breaks = c("Inside PA", "10-km buffer", "20-km buffer")) +
  labs(shape = "Location", colour = "region") + 
  theme_bw() +
  theme(
    panel.grid.major.y = element_line(colour = "grey90", linetype = "dotted"),
    axis.title.x = element_text(color = "black", face = "bold", size = 12),
    axis.title.y = element_text(color = "black", face = "bold", size = 12),
    axis.text.x = element_text(color = "black", size = 9),
    axis.text.y = element_text(color = "black", size = 8),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(face = "bold")
    ) +
  facet_grid(region ~ ., scales = "free_y", space = "free_y", labeller = labeller(region = label_wrap_gen(width = 250)))


# ggsave("maps_and_figures/dot_plot/dot_plot_tnv_loss2.png", width = 25, height = 20, units = c("cm"), dpi = 600)



#WITH FACETS and region pane (figure S3)
data_summary_pa <- read.csv("maps_and_figures/dot_plot/LCC_STATISTICS_PARK_REG_TVNL.csv")
data_summary_10 <- read.csv("maps_and_figures/dot_plot/LCC_STATISTICS_BUFFER_REG_TVNL.csv")
data_summary_20 <- read.csv("maps_and_figures/dot_plot/LCC_STATISTICS_BUFFER_20_REG_TVNL.csv")

data2 <- data[c("protected_areas", "id", "protected_areas_id", "region", "tnv_loss_perc_p", "tnv_loss_perc_10", "tnv_loss_perc_20")]
data2 <- rbind(data2,
               data.frame(protected_areas = toupper(data_summary_pa$region), id = c("", "", "", "", ""),
                          protected_areas_id = toupper(data_summary_pa$region),
                          region = rep("Colombia", 5), tnv_loss_perc_p = data_summary_pa$tnv_loss_perc,
                          tnv_loss_perc_10 = data_summary_10$tnv_loss_perc, tnv_loss_perc_20 = data_summary_20$tnv_loss_perc))


data2$region <- factor(data2$region, levels = c("Colombia",  "Caribbean", "Andean", "Pacific", "Amazon", "Orinoquia"))

region.labs <- c("Colombia", "Amazon", "Andean", "Caribbean", "Or.", "Pacific")
names(region.labs) <- c("Colombia", "Amazon", "Andean", "Caribbean", "Orinoquia", "Pacific")

ggplot(data2) +
  geom_point(aes(x = reorder(protected_areas_id, tnv_loss_perc_p), y = tnv_loss_perc_p, colour = region, shape = "Inside PA"), size = 2) +
  geom_point(aes(x = protected_areas_id, y = tnv_loss_perc_10, colour = region, shape = "10-km buffer"), size = 2) +
  geom_point(aes(x = protected_areas_id, y = tnv_loss_perc_20, colour = region, shape = "20-km buffer"), size = 2) +
  ylab("Total natural vegetation loss (%)") +
  # xlab("Protected Area (id)") +
  scale_color_manual(values = c("black","#e41a1c","#377eb8","#ff7f00","#4daf4a","#984ea3"),
                     labels = c("Colombia",  "Caribbean", "Andean", "Pacific", "Amazon", "Orinoquia")) +
  coord_flip() +
  guides(color = "none", shape = guide_legend(title = "Location")) +
  # scale_fill_discrete(breaks = c("Inside PA", "10-km buffer", "20-km buffer")) +
  labs(shape = "Location", colour = "region") + 
  scale_y_continuous(expand = c(0.02 ,0.02)) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_line(colour = "grey80", linetype = "dotted"),
    axis.title.x = element_text(color = "black", face = "bold", size = 12),
    # axis.title.y = element_text(color = "black", face = "bold", size = 12),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "black", size = 9),
    axis.text.y = element_text(color = "black", size = 9),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(face = "bold")
  ) +
  facet_grid(region ~ ., scales = "free_y", space = "free_y", labeller = labeller(region = region.labs))


ggsave("maps_and_figures/dot_plot/dot_plot_tnv_loss_.png", width = 22.5, height = 25, units = c("cm"), dpi = 700)




# Other natural vegetation loss inside and around PA (Figure S5)
data_summary_pa <- read.csv("maps_and_figures/dot_plot/LCC_STATISTICS_PARK_REG_TVNL.csv")
data_summary_10 <- read.csv("maps_and_figures/dot_plot/LCC_STATISTICS_BUFFER_REG_TVNL.csv")
data_summary_20 <- read.csv("maps_and_figures/dot_plot/LCC_STATISTICS_BUFFER_20_REG_TVNL.csv")

data3 <- data[c("protected_areas", "id", "protected_areas_id", "region", "onv_loss_perc_p", "onv_loss_perc_10", "onv_loss_perc_20")]
data3 <- rbind(data3,
               data.frame(protected_areas = toupper(data_summary_pa$region), id = c("", "", "", "", ""),
                          protected_areas_id = toupper(data_summary_pa$region),
                          region = rep("Colombia", 5), onv_loss_perc_p = data_summary_pa$onv_loss_perc,
                          onv_loss_perc_10 = data_summary_10$onv_loss_perc, onv_loss_perc_20 = data_summary_20$onv_loss_perc))

data3$region <- factor(data3$region, levels = c("Colombia",  "Caribbean", "Amazon", "Andean", "Pacific", "Orinoquia"))

region.labs <- c("Colombia", "Amazon", "Andean", "Caribbean", "Or.", "Pacific")
names(region.labs) <- c("Colombia", "Amazon", "Andean", "Caribbean", "Orinoquia", "Pacific")


ggplot(data3) +
  geom_point(aes(x = reorder(protected_areas_id, onv_loss_perc_p), y = onv_loss_perc_p, colour = region, shape = "Inside PA"), size = 2) +
  geom_point(aes(x = protected_areas_id, y = onv_loss_perc_10, colour = region, shape = "10-km buffer"), size = 2) +
  geom_point(aes(x = protected_areas_id, y = onv_loss_perc_20, colour = region, shape = "20-km buffer"), size = 2) +
  ylab("Other natural vegetation loss (%)") +
  # xlab("Protected Area (id)") +
  scale_color_manual(values = c("black","#e41a1c","#4daf4a","#377eb8","#ff7f00","#984ea3"),
                     labels = c("Colombia", "Caribbean", "Amazon", "Andean", "Pacific", "Orinoquia")) +
  coord_flip() +
  guides(color = "none", shape = guide_legend(title = "Location")) +
  # scale_fill_discrete(breaks = c("Inside PA", "10-km buffer", "20-km buffer")) +
  labs(shape = "Location", colour = "region") + 
  scale_y_continuous(expand = c(0.02 ,0.02)) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_line(colour = "grey80", linetype = "dotted"),
    axis.title.x = element_text(color = "black", face = "bold", size = 12),
    # axis.title.y = element_text(color = "black", face = "bold", size = 12),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "black", size = 9),
    axis.text.y = element_text(color = "black", size = 9),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(face = "bold")
  ) +
  facet_grid(region ~ ., scales = "free_y", space = "free_y", labeller = labeller(region = region.labs))


ggsave("maps_and_figures/dot_plot/dot_plot_onv_loss.png", width = 22.5, height = 25, units = c("cm"), dpi = 700)



### Forest loss inside and around PA (Figure S4)
data_summary_pa <- read.csv("maps_and_figures/dot_plot/LCC_STATISTICS_PARK_REG_TVNL.csv")
data_summary_10 <- read.csv("maps_and_figures/dot_plot/LCC_STATISTICS_BUFFER_REG_TVNL.csv")
data_summary_20 <- read.csv("maps_and_figures/dot_plot/LCC_STATISTICS_BUFFER_20_REG_TVNL.csv")

data4 <- data[c("protected_areas", "id", "protected_areas_id", "region", "forest_loss_perc_p", "forest_loss_perc_10", "forest_loss_perc_20")]
data4 <- rbind(data4,
               data.frame(protected_areas = toupper(data_summary_pa$region), id = c("", "", "", "", ""),
                          protected_areas_id = toupper(data_summary_pa$region),
                          region = rep("Colombia", 5), forest_loss_perc_p = data_summary_pa$forest_loss_perc,
                          forest_loss_perc_10 = data_summary_10$forest_loss_perc, forest_loss_perc_20 = data_summary_20$forest_loss_perc))

data4$region <- factor(data4$region, levels = c("Colombia",  "Caribbean", "Andean", "Pacific", "Amazon", "Orinoquia"))

region.labs <- c("Colombia", "Amazon", "Andean", "Caribbean", "Or.", "Pacific")
names(region.labs) <- c("Colombia", "Amazon", "Andean", "Caribbean", "Orinoquia", "Pacific")


ggplot(data4) +
  geom_point(aes(x = reorder(protected_areas_id, forest_loss_perc_p), y = forest_loss_perc_p, colour = region, shape = "Inside PA"), size = 2) +
  geom_point(aes(x = protected_areas_id, y = forest_loss_perc_10, colour = region, shape = "10-km buffer"), size = 2) +
  geom_point(aes(x = protected_areas_id, y = forest_loss_perc_20, colour = region, shape = "20-km buffer"), size = 2) +
  ylab("Forest loss (%)") +
  # xlab("Protected Area (id)") +
  scale_color_manual(values = c("black","#e41a1c","#377eb8","#ff7f00","#4daf4a","#984ea3"),
                     labels = c("Colombia",  "Caribbean", "Andean", "Pacific", "Amazon", "Orinoquia")) +
  coord_flip() +
  guides(color = "none", shape = guide_legend(title = "Location")) +
  # scale_fill_discrete(breaks = c("Inside PA", "10-km buffer", "20-km buffer")) +
  labs(shape = "Location", colour = "region") + 
  scale_y_continuous(expand = c(0.02 ,0.02)) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_line(colour = "grey80", linetype = "dotted"),
    axis.title.x = element_text(color = "black", face = "bold", size = 12),
    # axis.title.y = element_text(color = "black", face = "bold", size = 12),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "black", size = 9),
    axis.text.y = element_text(color = "black", size = 9),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(face = "bold")
  ) +
  facet_grid(region ~ ., scales = "free_y", space = "free_y", labeller = labeller(region = region.labs))


ggsave("maps_and_figures/dot_plot/dot_plot_forest_loss.png", width = 22.5, height = 25, units = c("cm"), dpi = 700)
