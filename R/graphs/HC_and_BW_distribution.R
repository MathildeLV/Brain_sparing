sd(laus2$head_circ, na.rm = TRUE)
sd(laus2$hc.perturbed, na.rm = TRUE)

hc_distr <- laus2 %>%
  ggplot(aes(x=head_circ)) +
  geom_density()+
  # labs(title = "Head circumference distribution")  +  
  xlab("head circumference (cm)") +
  scale_x_continuous(breaks = seq(29, 40, by = 1), limits = c(29, 40))+
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.1), limits = c(0, 0.40))+
  theme_bw()+
  custom_theme()+
  NULL
hc_distr

hc_distr_pert <- laus2 %>%
  ggplot(aes(x=hc.perturbed)) +
  geom_density()+
  # labs(title = "Head circumference distribution")  +  
  xlab("head circumference (cm)") +
  scale_x_continuous(breaks = seq(29, 40, by = 1), limits = c(29, 40))+
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.1), limits = c(0, 0.40))+
  theme_bw()+
  custom_theme()+
  NULL
hc_distr_pert


BW_distr <- laus2 %>%
  ggplot(aes(x=birthweight)) +
  geom_density()+
  # labs(title = "Birth weight distribution")  +  
  xlab("birth weight (g)") +
  scale_x_continuous(breaks = seq(1700,4700, by = 500), limits = c(1700, 4700))+
  scale_y_continuous(labels = scales::scientific_format(accuracy = 1)) + # Scientific notation
  theme_bw()+
  custom_theme()+
  NULL
BW_distr


suppl_fig_1_distrib <- ggarrange(hc_distr,
          hc_distr_pert, 
          BW_distr,
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2) 
ggsave("BW_HC_distr.pdf", plot = suppl_fig_1_distrib, path = here("output/graphs/suppl_data"), width = 8, height = 5)
