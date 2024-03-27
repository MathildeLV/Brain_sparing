## nb of births per year #### 
ggplot_births_per_year <- lausgraph %>%
  ggplot(aes(x = birthyear)) +
  geom_bar(fill = "grey", show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 1200, by = 200), limits = c(0, 1200)) +
  scale_x_continuous(breaks = seq(1911, 1922, by=1)) +
  labs(
    x = "Year",
    y = "Number of births"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(color = "black", size = 20),
    axis.text.x = element_text(color = "black", size = 20, angle = 45, hjust = 1.2),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20)
  ) 
ggplot_births_per_year

ggsave("output/graphs/suppl_data/nb_births_by_year_1911_22.pdf",
       ggplot_births_per_year, width = 10, height = 7)
