# UCL: Flu infection yes/no neonatal health ####
tab_neonat_flu <- pandemy %>%
  group_by(Grippe_during_pregn_3) %>%
  summarise(n = n(),
            mean_birthweight = round(mean(birthweight, na.rm = T), digits = 0),   #birthweight
            sd_birthweight = round(sd(birthweight, na.rm = T), digits = 0),
            
            mean_HC = round(mean(head_circ, na.rm = T), digits = 1),   #head_circ
            sd_HC = round(sd(head_circ, na.rm = T), digits = 1),
            
            count_GA_w_corr_cat_1= sum(GA_weeks_cat_corrected2=="[10,33)", na.rm = T),  perc_GA_w_corr_1= round(100*count_GA_w_corr_cat_1/n,digits = 1),     # age based on var "age enfant" assessed at birth
            count_GA_w_corr_cat_2= sum(GA_weeks_cat_corrected2=="[33,37)", na.rm = T),  perc_GA_w_corr_2= round(100*count_GA_w_corr_cat_2/n,digits = 1),
            count_GA_w_corr_cat_3= sum(GA_weeks_cat_corrected2=="[37,41)", na.rm = T),  perc_GA_w_corr_3= round(100*count_GA_w_corr_cat_3/n,digits = 1),
            count_GA_w_corr_4= sum(GA_weeks_cat_corrected2=="[41,52]", na.rm = T),  perc_GA_w_corr_4= round(100*count_GA_w_corr_4/n,digits = 1),
            
            count_SB_1 = sum(stillbirth=="stillbirth", na.rm=T), percent_SB_1=round(100*count_SB_1/n, digits = 1),#stillbirth
            
            count_mor_1 = sum(postbirth_death=="1", na.rm=T), percent_mor_1=round(100*count_mor_1/n, digits=1), #postbirthdeath
            
            count_PTB_corrected = sum(PTB_corrected=="preterm", na.rm=T), percent_PTB_corrected=round(100*count_PTB_corrected/n,digits = 1), #preterm birth based on var "age enfant" assessed at birth
            
            count_LBW_1 = sum(LBW=="LBW", na.rm=T), percent_LBW_1=round(100*count_LBW_1/n,digits = 1),   #lowbirthweight    
            
  ) %>%     
  ungroup()


formatted_tab_neonat_flu <- tab_neonat_flu %>%
  mutate(n=n,
         "Birthweight, mean ± sd (g)"= stringr::str_glue("{mean_birthweight} ± {sd_birthweight}"),    #birthweight
         "Head circumference, mean ± sd (cm)"= stringr::str_glue("{mean_HC} ± {sd_HC}"),    #head circ
         
         "< 33 (n, %)"=stringr::str_glue("{count_GA_w_corr_cat_1} ({perc_GA_w_corr_1})"),         # age baby based on var "age enfant" assessed at birth
         "[33, 37) (n, %)"=stringr::str_glue("{count_GA_w_corr_cat_2} ({perc_GA_w_corr_2})"),
         "[37, 41) (n, %)"= stringr::str_glue("{count_GA_w_corr_cat_3} ({perc_GA_w_corr_3})"),
         ">= 41 (n, %)"=stringr::str_glue("{count_GA_w_corr_4} ({perc_GA_w_corr_4})"),
         
         "Stillbirth (n, %)"= stringr::str_glue("{count_SB_1} ({percent_SB_1})"),
         "Neonatal mortality (n, %)" =  stringr::str_glue("{count_mor_1} ({percent_mor_1})"),
         "Preterm birth (<37 weeks, n, %)"=stringr::str_glue("{count_PTB_corrected} ({percent_PTB_corrected})"),
         "Low birthweight (<2500g, n, %)" =stringr::str_glue("{count_LBW_1} ({percent_LBW_1})"),
  ) %>%
  tidyr::complete(Grippe_during_pregn_3) %>%
  select(Grippe_during_pregn_3, "Birthweight, mean ± sd (g)", 
         "Head circumference, mean ± sd (cm)",
         "< 33 (n, %)", "[33, 37) (n, %)", "[37, 41) (n, %)", ">= 41 (n, %)",
         "Stillbirth (n, %)", "Neonatal mortality (n, %)",
         "Preterm birth (<37 weeks, n, %)", "Low birthweight (<2500g, n, %)",
         n)

#inverted
neonat_characteristics_flu <- t(formatted_tab_neonat_flu)
rownames(neonat_characteristics_flu) <- colnames(formatted_tab_neonat_flu)
colnames(neonat_characteristics_flu) <- rownames(formatted_tab_neonat_flu)

#removing the first row that is useless, to have fir row (and col names) as birthyear
colnames(neonat_characteristics_flu) <- neonat_characteristics_flu[1,]
neonat_characteristics_flu <- neonat_characteristics_flu[-1, ] 
neonat_characteristics_flu <- as_tibble(neonat_characteristics_flu)


### V2: grouped rows
neonat_characteristics_flu <- add_column(neonat_characteristics_flu, .before = 1,
                                         name = c("Birthweight (mean ± sd (g))",
                                                  "Head circumference (mean ± sd (cm))",
                                                  "< 33 (n, %)", "[33, 37) (n, %)", "[37, 41) (n, %)", ">= 41 (n, %)", 
                                                  "Stillbirth (n, %)", "Neonatal mortality (n, %)", 
                                                  "Preterm birth (<37 weeks, n, %)",
                                                  "Low birthweight (<2500g, n, %)", 
                                                  "n")
)


neonat_characteristics_flu_gt <- neonat_characteristics_flu %>%
  gt() %>%
  # tab_header(title=md("**Neonatal characteristics depending on maternal flu infection**"), 
  #            subtitle = "n=2,228") %>%
  tab_row_group(label="Gestational age (weeks)",
                rows=3:6) %>%
  cols_label(name = "Flu during pregnancy") %>%
  cols_width(name ~ px(240), everything() ~ px(90)) %>%
  tab_options(table.width = 0.1, data_row.padding = px(1),   table.font.size = 14,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 14, heading.subtitle.font.size = 14
  ) %>%
  cols_align(columns = c(2,3), align = "right") # Align all columns except "Flu_during_pregnancy" to the right



neonat_characteristics_flu_gt
neonat_characteristics_flu_gt %>%
  gtsave(here("outputs/tables", "UCL_neonatal_characteristics_flu.html"))

neonat_characteristics_flu_gt %>%
  gtsave(here("outputs/tables", "UCL_neonatal_characteristics_flu.docx"))


## tbl_summary version, including counts and percentages ####
### Flu or syphilis ####
tab_neonat_flu_or_syph2_tbl_summary <- laus4_graph %>%
  select(birthweight,head_circ, GA_weeks_corrected,
         sex,
         PTB_corrected,GA_weeks_cat_corrected2, LBW,
         stillbirth, neonat_mort_d1_d5,flu_or_syph
  ) %>%  
  gtsummary::tbl_summary(
    by=flu_or_syph,
    statistic = list(
      c("birthweight", "head_circ", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("sex","PTB_corrected","GA_weeks_cat_corrected2", "LBW",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("sex","PTB_corrected", "GA_weeks_cat_corrected2","LBW",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "GA_weeks_corrected") ~ 1),
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**") 
tab_neonat_flu_or_syph2_tbl_summary

tab_neonat_flu_or_syph2_tbl_summary %>%
  gtsummary::as_gt() %>%
  gtsave(here("outputs/tables/neonatal_outcomes_depending_on_flu_or_syph_inf", "neonatal_characteristics_based_on_matern_flu_or_syph_tbl_sum_1911_1922.html"))

tab_neonat_flu_or_syph2_tbl_summary %>%
  gtsummary::as_gt() %>%
  gtsave(here("outputs/tables/neonatal_outcomes_depending_on_flu_or_syph_inf", "neonatal_characteristics_based_on_matern_flu_or_syph_tbl_sum_1911_1922.docx"))




### Flu: adding p value ####
tab_neonat_flu_tbl_summary <- laus4_graph %>%
  filter(flu_or_syph!="syphilis") %>%
  select(birthweight,head_circ,GA_weeks_corrected,
         sex,
         PTB_corrected,GA_weeks_cat_corrected2, LBW,
         stillbirth, neonat_mort_d1_d5,Flu_in_pregn_and_in_pandemic
  ) %>%  
  gtsummary::tbl_summary(
    by=Flu_in_pregn_and_in_pandemic,
    statistic = list(
      c("birthweight", "head_circ", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("sex","PTB_corrected","GA_weeks_cat_corrected2", "LBW",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("sex","PTB_corrected", "GA_weeks_cat_corrected2","LBW",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "GA_weeks_corrected") ~ 1),
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**") %>%
add_p()
tab_neonat_flu_tbl_summary

tab_neonat_flu_tbl_summary %>%
  gtsummary::as_gt() %>%
  gtsave(here("outputs/tables/neonatal_outcomes_depending_on_flu_or_syph_inf", "neonatal_characteristics_based_on_matern_flu_tbl_sum_1911_1922.docx"))

### Syphilis: adding p value ####
tab_neonat_syph_tbl_summary <- laus4_graph %>%
  filter(flu_or_syph!="influenza") %>%
  select(birthweight,head_circ,GA_weeks_corrected,
         sex,
         PTB_corrected,GA_weeks_cat_corrected2, LBW,
         stillbirth, neonat_mort_d1_d5,Syphilis_2
  ) %>%  
  gtsummary::tbl_summary(
    by=Syphilis_2,
    statistic = list(
      c("birthweight", "head_circ", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("sex","PTB_corrected","GA_weeks_cat_corrected2", "LBW",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("sex","PTB_corrected", "GA_weeks_cat_corrected2","LBW",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "GA_weeks_corrected") ~ 1),
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**") %>%
  add_p() 
tab_neonat_syph_tbl_summary

tab_neonat_syph_tbl_summary %>%
  gtsummary::as_gt() %>%
  gtsave(here("outputs/tables/neonatal_outcomes_depending_on_flu_or_syph_inf", "neonatal_characteristics_based_on_matern_syph_tbl_sum_1911_1922.docx"))

