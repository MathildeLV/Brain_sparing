# Maternal characteristics of those infected by flu or had positive test for syphilis####
mat_charc_had_flu_syph <- laus4_graph %>%
  select(flu_or_syph,age_mother, height,
         civil_status_cat2, parity_cat2, Lausanne,  
         morphology,Goitre, rickets,
         hisco_class_3) %>%  
  gtsummary::tbl_summary(by=flu_or_syph,
                         statistic = list(
                           c("age_mother", "height") ~ "{mean} ({sd})",
                           c("civil_status_cat2", "parity_cat2",'Lausanne',
                             'morphology', "Goitre", "rickets",
                             'hisco_class_3') ~ "{n} ({p}%)"
                         ),
                         digits = list(
                           c("civil_status_cat2","parity_cat2",'Lausanne',
                             'morphology',"Goitre", "rickets",
                             'hisco_class_3') ~ 0,
                           c("age_mother", "height") ~ 1),
                         missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**") 
mat_charc_had_flu_syph

mat_charc_had_flu_syph %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data", "mat_characteristics_infected_flu_syph.html")) 

mat_charc_had_flu_syph %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data", "mat_characteristics_infected_flu_syph.docx"))



# Maternal characteristics of those infected by flu : with p values####
mat_charc_had_flu_pval <- laus4_graph %>%
  filter(flu_or_syph!="syphilis") %>%
  select(Flu_in_pregn_and_in_pandemic,age_mother, height,
         civil_status_cat2, parity_cat2, Lausanne,  
         morphology,Goitre, rickets,
         hisco_class_3) %>%  
  gtsummary::tbl_summary(by=Flu_in_pregn_and_in_pandemic,
                         statistic = list(
                           c("age_mother", "height") ~ "{mean} ({sd})",
                           c("civil_status_cat2", "parity_cat2",'Lausanne',
                             'morphology', "Goitre", "rickets",
                             'hisco_class_3') ~ "{n} ({p}%)"
                         ),
                         digits = list(
                           c("civil_status_cat2","parity_cat2",'Lausanne',
                             'morphology',"Goitre", "rickets",
                             'hisco_class_3') ~ 0,
                           c("age_mother", "height") ~ 1),
                         missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**") %>%
  add_p()
mat_charc_had_flu_pval

mat_charc_had_flu_pval %>%
  gtsummary::as_gt() %>%
  gtsave(here("outputs/tables/suppl_data", "mat_characteristics_infected_flu_pval.docx"))

# Maternal characteristics of those with positive syphilis test : with p values####
mat_charc_had_syph_pval <- laus4_graph %>%
  filter(flu_or_syph!="influenza") %>%
  select(Syphilis_2,age_mother, height,
         civil_status_cat2, parity_cat2, Lausanne,  
         morphology,Goitre, rickets,
         hisco_class_3) %>%  
  gtsummary::tbl_summary(by=Syphilis_2,
                         statistic = list(
                           c("age_mother", "height") ~ "{mean} ({sd})",
                           c("civil_status_cat2", "parity_cat2",'Lausanne',
                             'morphology', "Goitre", "rickets",
                             'hisco_class_3') ~ "{n} ({p}%)"
                         ),
                         digits = list(
                           c("civil_status_cat2","parity_cat2",'Lausanne',
                             'morphology',"Goitre", "rickets",
                             'hisco_class_3') ~ 0,
                           c("age_mother", "height") ~ 1),
                         missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**") %>%
  add_p()
mat_charc_had_syph_pval

mat_charc_had_syph_pval %>%
  gtsummary::as_gt() %>%
  gtsave(here("outputs/tables/suppl_data", "mat_characteristics_infected_syph_pval.docx"))




# Maternal characteristics of those who had the flu DURING pregn or syphilis####
tab_mat_flu_or_syph2 <- laus2 %>%
  group_by(flu_or_syph) %>%
  summarise(n = n(),
            mat_age = round(mean(age_mother, na.rm = T), digits = 1),        #mat age continuous
            sd_mat_age_c = round(sd(age_mother, na.rm = T), digits = 1),
            count_mat_age_NA = sum(is.na(age_mother)), perc_mat_age_NA=100*count_mat_age_NA/n,
            
            count_parity_1= sum(parity_cat2=="(0,1]", na.rm = T),  perc_parity_1= 100*count_parity_1/n,     #parity
            count_parity_2= sum(parity_cat2=="(1,2]", na.rm = T), perc_parity_2= 100*count_parity_2/n,
            count_parity_3= sum(parity_cat2=="(2,20]", na.rm = T), perc_parity_3= 100*count_parity_3/n,
            # count_parity_NA = sum(is.na(parity_cat2)), perc_parity_NA=100*count_parity_NA/n,
            
            count_civstat_mar= sum(civil_status_cat2=="married", na.rm = T),  perc_civstat_mar= 100*count_civstat_mar/n,     #civil status
            count_civstat_single= sum(civil_status_cat2=="single or missing", na.rm = T),  perc_civstat_single= 100*count_civstat_single/n,
            
            count_rel_prot= sum(Religion2=="protestant", na.rm = T),  perc_rel_prot= 100*count_rel_prot/n,     #Religion
            count_rel_cat= sum(Religion2=="catholic", na.rm = T),  perc_rel_cat= 100*count_rel_cat/n,
            count_rel_other_or_NA= sum(Religion2=="other or missing", na.rm = T),  perc_rel_other_or_NA= 100*count_rel_other_or_NA/n,
            
            count_Lausanne_yes= sum(Lausanne=="yes", na.rm = T),  perc_Lausanne_yes= 100*count_Lausanne_yes/n,     #Living in Lausanne or not
            count_Lausanne_no= sum(Lausanne=="no", na.rm = T),  perc_Lausanne_no= 100*count_Lausanne_no/n,
            count_Lausanne_unsr= sum(Lausanne=="unsure", na.rm = T),  perc_Lausanne_unsr= 100*count_Lausanne_unsr/n,
            
            height_mean = round(mean(height, na.rm = T), digits = 1),        # height
            sd_height = round(sd(height, na.rm = T), digits = 1),
            count_height_NA = sum(is.na(height)), perc_height_NA=100*count_height_NA/n,
            
            waistcir = round(mean(waist_circ, na.rm = T), digits = 1),        # waist circ
            sd_waistcir = round(sd(waist_circ, na.rm = T), digits = 1),
            count_wc_NA = sum(is.na(waist_circ)), perc_wc_NA=100*count_wc_NA/n,
            
            count_obese= sum(Obese=="1", na.rm = T),  perc_obese= 100*count_obese/n,     # dummy var
            count_thin= sum(Thin=="1", na.rm = T),  perc_thin= 100*count_thin/n,
            count_goitre= sum(Goitre=="1", na.rm = T),  perc_goitre= 100*count_goitre/n,
            
            count_rickets= sum(rickets=="1", na.rm = T),  perc_rickets= 100*count_rickets/n,    # Rickets
             
            count_hisco_cat3_1= sum(hisco_class_3=="1", na.rm = T),  perc_hisco_cat3_1= 100*count_hisco_cat3_1/n,   # hisco class 3
            count_hisco_cat3_2= sum(hisco_class_3=="2", na.rm = T),  perc_hisco_cat3_2= 100*count_hisco_cat3_2/n,
            count_hisco_cat3_3= sum(hisco_class_3=="3", na.rm = T),  perc_hisco_cat3_3= 100*count_hisco_cat3_3/n,
            count_hisco_cat3_NA = sum(hisco_class_3=="missing", na.rm = T), perc_hisco_cat3_NA=100*count_hisco_cat3_NA/n,
            
      ) %>%     
  ungroup()


formatted_tab_mat_flu_or_syph2 <- tab_mat_flu_or_syph2 %>%
  mutate(n=n,
         "Maternal age, mean ± sd (years)"= stringr::str_glue("{mat_age} ± {sd_mat_age_c}"),   # mat age continuous
         "missing maternal age"= round({perc_mat_age_NA}, digits=1),
         
         "Height, mean ± sd (cm)"= stringr::str_glue("{height_mean} ± {sd_height}"),  # height
         "missing height"= round({perc_height_NA}, digits=1),
         
         "Waist circumference, mean ± sd (cm)"= stringr::str_glue("{waistcir} ± {sd_waistcir}"),  # waist circ
         "missing waist circumference"= round({perc_wc_NA}, digits=1),
         
         "1"= round({perc_parity_1}, digits=1),  # Parity
         "2"= round({perc_parity_2}, digits=1),
         ">2"= round({perc_parity_3}, digits=1),
         # "Missing parity"= round({perc_parity_NA}, digits=1),
         
         "Married"= round({perc_civstat_mar}, digits=1),  # Civil status
         "Single or missing"= round({perc_civstat_single}, digits=1),
         
         "Protestant"= round({perc_rel_prot}, digits=1),  # Religion
         "Catholic"= round({perc_rel_cat}, digits=1),
         "other or missing"= round({perc_rel_other_or_NA}, digits=1),
         
         "yes"= round({perc_Lausanne_yes}, digits=1),  # Lausanne
         "no"= round({perc_Lausanne_no}, digits=1),
         "unsure"= round({perc_Lausanne_unsr}, digits=1),
         
         "Obese"= round({perc_obese}, digits=1),  # Etat general
         "Thin"= round({perc_thin}, digits=1),
         "Goitre"= round({perc_goitre}, digits=1),
         
         "Hisco.1"= round({perc_hisco_cat3_1}, digits=1),  # Hisco 3
         "Hisco.2"= round({perc_hisco_cat3_2}, digits=1),
         "Hisco.3"= round({perc_hisco_cat3_3}, digits=1),
         "Missing Hisco"= round({perc_hisco_cat3_NA}, digits=1),
         
         "Rickets"= round({perc_rickets}, digits=1),  # Rickets

  ) %>%
  tidyr::complete(flu_or_syph) %>%
  select(flu_or_syph, "Maternal age, mean ± sd (years)", "missing maternal age",
         "Height, mean ± sd (cm)", "missing height", 
         "Waist circumference, mean ± sd (cm)", "missing waist circumference",
         "1", "2", ">2", 
         #"Missing parity",
         "Married", "Single or missing",
         "Protestant", "Catholic", "other or missing", 
         "yes", "no", "unsure",
         "Obese", "Thin", "Goitre", "Rickets",
         "Hisco.1", "Hisco.2", "Hisco.3", "Missing Hisco",  
          n)

#inverted
# formatted_tab_mat2 <- formatted_tab_mat2
mat_flu_or_syph_characteristics2 <- t(formatted_tab_mat_flu_or_syph2)
rownames(mat_flu_or_syph_characteristics2) <- colnames(formatted_tab_mat_flu_or_syph2)
colnames(mat_flu_or_syph_characteristics2) <- rownames(formatted_tab_mat_flu_or_syph2)
colnames(mat_flu_or_syph_characteristics2) <- mat_flu_or_syph_characteristics2[1,]
mat_flu_or_syph_characteristics2 <- mat_flu_or_syph_characteristics2[-1, ] 
mat_flu_or_syph_characteristics2 <- as_tibble(mat_flu_or_syph_characteristics2)

### V2: grouped rows
mat_flu_or_syph_characteristics2 <- add_column(mat_flu_or_syph_characteristics2, .before = 1,
                                          name = c("mean ± sd (years)", "missing (%)",
                                                   "mean ± sd (cm)", "missing (%)", 
                                                   "mean ± sd (cm)", "missing (%)", "1", "2", ">2", 
                                                   #"missing",
                                                   "Married", "Single or missing",
                                                   "Protestant", "Catholic", "other or missing", 
                                                   "yes", "no", "unsure",
                                                   "Obese", "Thin", "Goitre", "Rickets",
                                                   "1", "2", "3", "missing", 
                                                   "n")
)

mat_flu_or_syph_characteristics2_gt <- mat_flu_or_syph_characteristics2 %>%
  gt() %>%
  # tab_header(title=md("**Neonatal and maternal characteristics**"), 
  #            subtitle = "n=8,131") %>%
  tab_row_group(label="Maternal age",
                rows=1:2) %>%
  tab_row_group(label="Height",
                rows=3:4) %>%
  tab_row_group(label="Waist circumference",
                rows=5:6) %>%
  tab_row_group(label="Parity (%)",
                rows=7:9) %>%
  tab_row_group(label="Civil status (%)",
                rows=10:11) %>%
  tab_row_group(label="Religion (%)",
                rows=12:14) %>%
  tab_row_group(label="Living in Lausanne (%)",
                rows=15:17) %>%
  tab_row_group(label="General health status (%)",
                rows=18:21) %>%
  tab_row_group(label="Hisco class 3 (%)",
                rows=22:25) %>%
  row_group_order(groups=c("Maternal age",
                           "Height", "Waist circumference", 
                           "Parity (%)","Civil status (%)", 
                           "Religion (%)", "Living in Lausanne (%)",
                           "General health status (%)", "Hisco class 3 (%)"
                          )
  )%>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = 26)) %>%
  cols_label(name = "Infection") %>%
  cols_width(name ~ px(170), everything() ~ px(75)) %>%
  tab_options(table.width = 0.05, data_row.padding = px(1),   table.font.size = 11,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 12, heading.subtitle.font.size = 12
  ) 

mat_flu_or_syph_characteristics2_gt
mat_flu_or_syph_characteristics2_gt %>%
  gtsave(here("output/tables/description_of_the_population", "mat_characteristics_flu_or_syph_infection.html")) 

mat_flu_characteristics2_gt %>%
  gtsave(here("output/tables/description_of_the_population", "mat_characteristics_flu_or_syph_infection.docx"))
  