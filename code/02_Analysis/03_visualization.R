###Load needed functions from "Functions.R" file.
source("code/02_Analysis/00_analysis_functions")
###load population data
# Quality_overall <- readRDS("output/Quality_Table.rds")

### Load font to be used for the plots (enable latex expression)
font_add_google("noto")
showtext_auto() 

# Introduce latex expression labels
Quality_overall <- Quality_overall %>%
  mutate(
    fn_sen_label = sprintf("&#950;<sup>sen</sup>: %s", fn_sen),
    fp_sen_label = sprintf("&#958;<sup>sen</sup>: %s", fp_sen)
  )
Quality_overall$fn_sen_label <- factor(Quality_overall$fn_sen_label,
                                       levels = unique(Quality_overall$fn_sen_label))
Quality_overall$fp_sen_label <- factor(Quality_overall$fp_sen_label,
                                       levels = unique(Quality_overall$fp_sen_label))
# Split data based on interest (zoom in on fn_sen != 0.9, where differences 
# are hard to see otherwise )
Quality_fn_other <- subset(Quality_overall, fn_sen != 0.9)
Quality_fn_09 <- subset(Quality_overall, fn_sen == 0.9)


##### Create plots of interest
p_RRMSE_other <- ggplot(Quality_fn_other, aes(x = fn_svy, y = RRMSE, group = Estimator, color = Estimator)) +
  geom_line() +
  facet_grid(fn_sen_label ~ fp_sen_label) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal(base_family = "noto") +  
  labs(
    x = "&#950;<sup>svy</sup>", 
    y = "Relative Root Mean Squared Error",
    color = "Estimator"
  ) +
  theme(
    strip.text.y = ggtext::element_markdown(angle = 0),
    strip.text.x = ggtext::element_markdown(),
    axis.title.x = ggtext::element_markdown(),  
    legend.position = "bottom",
    legend.box = "horizontal"
  )
print(p_RRMSE_other)
ggsave("plots/RRMSE_output_other.eps", plot = p_RRMSE_other, width = 6.65625, height = 3.520833, units = "in", dpi = 96)

p_RRMSE_09 <- ggplot(Quality_fn_09, aes(x = fn_svy, y = RRMSE, group = Estimator, color = Estimator)) +
  geom_line() +
  facet_grid(fn_sen_label ~ fp_sen_label) +
  theme_minimal(base_family = "noto") +  
  labs(
    x = "&#950;<sup>svy</sup>", 
    y = "Relative Root Mean Squared Error",
    color = "Estimator"
  ) +
  theme(
    strip.text.y = ggtext::element_markdown(angle = 0),
    strip.text.x = ggtext::element_markdown(),
    axis.title.x = ggtext::element_markdown(),  
    legend.position = "bottom",
    legend.box = "horizontal"
  )
print(p_RRMSE_09)
ggsave("plots/RRMSE_output_09.eps", plot = p_RRMSE_09, width = 6.65625, height = 3.520833, units = "in", dpi = 96)


p_RB_other <- ggplot(Quality_fn_other, aes(x = fn_svy, y = RB, group = Estimator, color = Estimator)) +
  geom_line() +
  facet_grid(fn_sen_label ~ fp_sen_label) +
  theme_minimal(base_family = "noto") +  
  labs(
    x = "&#950;<sup>svy</sup>", 
    y = "Relative Bias",
    color = "Estimator"
  ) +
  theme(
    strip.text.y = ggtext::element_markdown(angle = 0),
    strip.text.x = ggtext::element_markdown(),
    axis.title.x = ggtext::element_markdown(),  
    legend.position = "bottom",
    legend.box = "horizontal"
  )
print(p_RB_other)
ggsave("plots/RB_output_other.eps", plot = p_RB_other, width = 6.65625, height = 3.520833, units = "in", dpi = 96)

p_RB_09 <- ggplot(Quality_fn_09, aes(x = fn_svy, y = RB, group = Estimator, color = Estimator)) +
  geom_line() +
  facet_grid(fn_sen_label ~ fp_sen_label) +
  theme_minimal(base_family = "noto") +  
  labs(
    x = "&#950;<sup>svy</sup>", 
    y = "Relative Bias",
    color = "Estimator"
  ) +
  theme(
    strip.text.y = ggtext::element_markdown(angle = 0),
    strip.text.x = ggtext::element_markdown(),
    axis.title.x = ggtext::element_markdown(),  
    legend.position = "bottom",
    legend.box = "horizontal"
  )
print(p_RB_09)
ggsave("plots/RB_output_09.eps", plot = p_RB_09, width = 6.65625, height = 3.520833, units = "in", dpi = 96)


p_CV_other <- ggplot(Quality_fn_other, aes(x = fn_svy, y = CV, group = Estimator, color = Estimator)) +
  geom_line() +
  facet_grid(fn_sen_label ~ fp_sen_label) +
  theme_minimal(base_family = "noto") +  
  labs(
    x = "&#950;<sup>svy</sup>", 
    y = "Coefficient of Variation",
    color = "Estimator"
  ) +
  theme(
    strip.text.y = ggtext::element_markdown(angle = 0),
    strip.text.x = ggtext::element_markdown(),
    axis.title.x = ggtext::element_markdown(),  
    legend.position = "bottom",
    legend.box = "horizontal"
  )
print(p_CV_other)
ggsave("plots/CV_output_other.eps", plot = p_CV_other, width = 6.65625, height = 3.520833, units = "in", dpi = 96)


p_CV_09 <- ggplot(Quality_fn_09, aes(x = fn_svy, y = CV, group = Estimator, color = Estimator)) +
  geom_line() +
  facet_grid(fn_sen_label ~ fp_sen_label) +
  theme_minimal(base_family = "noto") +  
  labs(
    x = "&#950;<sup>svy</sup>", 
    y = "Coefficient of Variation",
    color = "Estimator"
  ) +
  theme(
    strip.text.y = ggtext::element_markdown(angle = 0),
    strip.text.x = ggtext::element_markdown(),
    axis.title.x = ggtext::element_markdown(),  
    legend.position = "bottom",
    legend.box = "horizontal"
  )
print(p_CV_09)
ggsave("plots/CV_output_09.eps", plot = p_CV_09, width = 6.65625, height = 3.520833, units = "in", dpi = 96)
