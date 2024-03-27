# maternal characteristics  #### 
## overall, simplified, all years together ####
mat_charc <- lausgraph %>%
  select(age_mother, height,
         civil_status_cat2, parity_cat2, Lausanne,  
         morphology, Goitre, rickets,
         flu_or_syph, hisco_class_3) %>%  
  gtsummary::tbl_summary(
    statistic = list(
      c("age_mother", "height") ~ "{mean} ({sd})",
      c("civil_status_cat2", "parity_cat2",'Lausanne',
        'morphology', 'Goitre', 'rickets',
        'flu_or_syph',
        'hisco_class_3') ~ "{n} ({p}%)"
    ),
    digits = list(
      c("civil_status_cat2","parity_cat2",'Lausanne',
        'morphology', 'Goitre', 'rickets', 
        'flu_or_syph',
        'hisco_class_3') ~ 0,
      c("age_mother", "height") ~ 1),
    # label = grade ~ "Tumor Grade",
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**") %>%
  modify_caption("**Maternal characteristics**")
mat_charc

mat_charc %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/description_of_the_population", "maternal_characteristics_overall_1911_22.html"))

mat_charc %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/description_of_the_population", "maternal_characteristics_overall_1911_22.docx"))



## by year, with etat general categ #### 
tab_mat <- laus2 %>%
  group_by(birthyear) %>%
  summarise(n = n(),
            mat_age = round(mean(age_mother, na.rm = T), digits = 1),        #mat age continuous
            sd_mat_age_c = round(sd(age_mother, na.rm = T), digits = 1),
            count_mat_age_NA = sum(is.na(age_mother)), perc_mat_age_NA=100*count_mat_age_NA/n,
            
            count_parity_1= sum(parity_cat2=="(0,1]", na.rm = T),  perc_parity_1= 100*count_parity_1/n,     #parity
            count_parity_2= sum(parity_cat2=="(1,2]", na.rm = T), perc_parity_2= 100*count_parity_2/n,
            count_parity_3= sum(parity_cat2=="(2,20]", na.rm = T), perc_parity_3= 100*count_parity_3/n,
            count_parity_NA = sum(is.na(parity_cat2)), perc_parity_NA=100*count_parity_NA/n,
            
            count_civstat_mar= sum(civil_status_cat2=="married", na.rm = T),  perc_civstat_mar= 100*count_civstat_mar/n,     #civil status
            count_civstat_single= sum(civil_status_cat2=="single or missing", na.rm = T),  perc_civstat_single= 100*count_civstat_single/n,

            count_rel_prot= sum(Religion2=="protestant", na.rm = T),  perc_rel_prot= 100*count_rel_prot/n,     #Religion
            count_rel_cat= sum(Religion2=="catholic", na.rm = T),  perc_rel_cat= 100*count_rel_cat/n,
            count_rel_other_or_NA= sum(Religion2=="other or missing", na.rm = T),  perc_rel_other_or_NA= 100*count_rel_other_or_NA/n,
            
            menarche_mean = round(mean(menarche, na.rm = T), digits = 1),        # menarche
            sd_menarche = round(sd(menarche, na.rm = T), digits = 1),
            count_menarche_NA = sum(is.na(menarche)), perc_menarche_NA=100*count_menarche_NA/n,
            
            height_mean = round(mean(height, na.rm = T), digits = 1),        # height
            sd_height = round(sd(height, na.rm = T), digits = 1),
            count_height_NA = sum(is.na(height)), perc_height_NA=100*count_height_NA/n,
            
            waistcir = round(mean(waist_circ, na.rm = T), digits = 1),        # waist circ
            sd_waistcir = round(sd(waist_circ, na.rm = T), digits = 1),
            count_wc_NA = sum(is.na(waist_circ)), perc_wc_NA=100*count_wc_NA/n,
            
            count_et_gen_cat_1= sum(Etat_general_cat=="1", na.rm = T),  perc_et_gen_cat_1= 100*count_et_gen_cat_1/n,     # etat general
            count_et_gen_cat_2= sum(Etat_general_cat=="2", na.rm = T),  perc_et_gen_cat_2= 100*count_et_gen_cat_2/n,
            count_et_gen_cat_3= sum(Etat_general_cat=="3", na.rm = T),  perc_et_gen_cat_3= 100*count_et_gen_cat_3/n,
            count_et_gen_cat_NA = sum(is.na(Etat_general_cat)), perc_et_gen_cat_NA=100*count_et_gen_cat_NA/n,
            
            count_rickets= sum(rickets=="1", na.rm = T),  perc_rickets= 100*count_rickets/n,    # Rickets
            count_flu_during_pregn= sum(Grippe_during_pregn_2=="yes", na.rm = T),  perc_flu_during_pregn= 100*count_flu_during_pregn/n,    # Flu during preg (categroies B or C)
            count_syphilis= sum(Syphilis=="positive", na.rm = T),  perc_syph= 100*count_syphilis/n,    # syphilis
            
            count_hisco_cat3_1= sum(hisco_class_3=="1", na.rm = T),  perc_hisco_cat3_1= 100*count_hisco_cat3_1/n,   # hisco class 3
            count_hisco_cat3_2= sum(hisco_class_3=="2", na.rm = T),  perc_hisco_cat3_2= 100*count_hisco_cat3_2/n,
            count_hisco_cat3_3= sum(hisco_class_3=="3", na.rm = T),  perc_hisco_cat3_3= 100*count_hisco_cat3_3/n,
            count_hisco_cat3_NA = sum(hisco_class_3=="missing", na.rm = T), perc_hisco_cat3_NA=100*count_hisco_cat3_NA/n
  ) %>%     
  ungroup()


formatted_tab_mat <- tab_mat %>%
  mutate(n=n,
         "Maternal age, mean ± sd (years)"= stringr::str_glue("{mat_age} ± {sd_mat_age_c}"),   # mat age continuous
         "missing maternal age"= round({perc_mat_age_NA}, digits=1),
         
         "Menarche, mean ± sd (years)"= stringr::str_glue("{menarche_mean} ± {sd_menarche}"),   # menarche
         "missing menarche"= round({perc_menarche_NA}, digits=1),
         
         "Height, mean ± sd (cm)"= stringr::str_glue("{height_mean} ± {sd_height}"),  # height
         "missing height"= round({perc_height_NA}, digits=1),
         
         "Waist circumference, mean ± sd (cm)"= stringr::str_glue("{waistcir} ± {sd_waistcir}"),  # waist circ
         "missing waist circumference"= round({perc_wc_NA}, digits=1),
         
         "1"= round({perc_parity_1}, digits=1),  # Parity
         "2"= round({perc_parity_2}, digits=1),
         ">2"= round({perc_parity_3}, digits=1),
         "Missing parity"= round({perc_parity_NA}, digits=1),
         
         "Married"= round({perc_civstat_mar}, digits=1),  # Civil status
         "Single or missing"= round({perc_civstat_single}, digits=1),

         "Protestant"= round({perc_rel_prot}, digits=1),  # Religion
         "Catholic"= round({perc_rel_cat}, digits=1),
         "other or missing"= round({perc_rel_other_or_NA}, digits=1),
         
         "EG.1"= round({perc_et_gen_cat_1}, digits=1),  # Etat general
         "EG.2"= round({perc_et_gen_cat_2}, digits=1),
         "EG.3"= round({perc_et_gen_cat_3}, digits=1),
         "Missing EG"= round({perc_et_gen_cat_NA}, digits=1),
         
         "Hisco.1"= round({perc_hisco_cat3_1}, digits=1),  # Hisco 3
         "Hisco.2"= round({perc_hisco_cat3_2}, digits=1),
         "Hisco.3"= round({perc_hisco_cat3_3}, digits=1),
         "Missing Hisco"= round({perc_hisco_cat3_NA}, digits=1),
         
         "Rickets"= round({perc_rickets}, digits=1),  # Rickets
         "Flu during pregnancy"= round({perc_flu_during_pregn}, digits=1),  # Flu during pregn
         "Syphilis"= round({perc_syph}, digits=1),  # Syphilis
         
  ) %>%
  tidyr::complete(birthyear) %>%
  select(birthyear, "Maternal age, mean ± sd (years)", "missing maternal age", "Menarche, mean ± sd (years)", "missing menarche", "Height, mean ± sd (cm)", "missing height", 
         "Waist circumference, mean ± sd (cm)", "missing waist circumference", "1", "2", ">2", "Missing parity", "Married", "Single or missing",
         "Protestant", "Catholic", "other or missing", "EG.1", "EG.2", "EG.3", "Missing EG", "Hisco.1", "Hisco.2", "Hisco.3",
         "Missing Hisco", "Rickets", "Flu during pregnancy", "Syphilis", n)

#inverted
formatted_tab_mat <- formatted_tab_mat
mat_characteristics <- t(formatted_tab_mat)
rownames(mat_characteristics) <- colnames(formatted_tab_mat)
colnames(mat_characteristics) <- rownames(formatted_tab_mat)

#removing the first row that is useless, to have fir row (and col names) as birthyear
colnames(mat_characteristics) <- mat_characteristics[1,]
mat_characteristics <- mat_characteristics[-1, ] 
mat_characteristics <- as_tibble(mat_characteristics)


### V2: grouped rows
mat_characteristics <- add_column(mat_characteristics, .before = 1,
                                  name = c("mean ± sd (years)", "missing (%)", "mean ± sd (years)", "missing (%)", "mean ± sd (cm)", "missing (%)", 
                                           "mean ± sd (cm)", "missing (%)", "1", "2", ">2", "missing", "Married", "Single or missing",
                                           "Protestant", "Catholic", "other or missing", "1", "2", "3", "missing", "1", "2", "3", "missing",
                                           "Rickets","Flu during pregnancy", "Syphilis", "n")
)

mat_characteristics_gt <- mat_characteristics %>%
  gt() %>%
  tab_header(title=md("**Maternal characteristics**"), 
             subtitle = "n=7,940") %>%
  tab_row_group(label="Maternal age",
                rows=1:2) %>%
  tab_row_group(label="Menarche",
                rows=3:4) %>%
  tab_row_group(label="Height",
                rows=5:6) %>%
  tab_row_group(label="Waist circumference",
                rows=7:8) %>%
  tab_row_group(label="Parity (%)",
                rows=9:12) %>%
  tab_row_group(label="Civil status (%)",
                rows=13:14) %>%
  tab_row_group(label="Religion (%)",
                rows=15:17) %>%
  tab_row_group(label="General health status (%)",
                rows=18:21) %>%
  tab_row_group(label="Hisco class 3 (%)",
                rows=22:25) %>%
  row_group_order(groups=c("Maternal age", "Menarche", "Height", "Waist circumference", "Parity (%)",
                           "Civil status (%)", "Religion (%)", "General health status (%)", "Hisco class 3 (%)")
  )%>%
  cols_label(name = "Birthyear") %>%
  cols_width(name ~ px(170), everything() ~ px(50)) %>%
  tab_options(table.width = 0.1, data_row.padding = px(1),   table.font.size = 16,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 18, heading.subtitle.font.size = 18
  )

mat_characteristics_gt
mat_characteristics_gt %>%
  gtsave(here("output/tables", "maternal_characteristics_1913_1923.html")) 

## by year, with dummy variables  #### 
tab_mat2 <- laus2 %>%
  group_by(birthyear) %>%
  summarise(n = n(),
            mat_age = round(mean(age_mother, na.rm = T), digits = 1),        #mat age continuous
            sd_mat_age_c = round(sd(age_mother, na.rm = T), digits = 1),
            count_mat_age_NA = sum(is.na(age_mother)), perc_mat_age_NA=100*count_mat_age_NA/n,
            
            count_parity_1= sum(parity_cat2=="(0,1]", na.rm = T),  perc_parity_1= 100*count_parity_1/n,     #parity
            count_parity_2= sum(parity_cat2=="(1,2]", na.rm = T), perc_parity_2= 100*count_parity_2/n,
            count_parity_3= sum(parity_cat2=="(2,20]", na.rm = T), perc_parity_3= 100*count_parity_3/n,
            count_parity_NA = sum(is.na(parity_cat2)), perc_parity_NA=100*count_parity_NA/n,
            
            count_civstat_mar= sum(civil_status_cat2=="married", na.rm = T),  perc_civstat_mar= 100*count_civstat_mar/n,     #civil status
            count_civstat_single= sum(civil_status_cat2=="single or missing", na.rm = T),  perc_civstat_single= 100*count_civstat_single/n,
            
            count_rel_prot= sum(Religion2=="protestant", na.rm = T),  perc_rel_prot= 100*count_rel_prot/n,     #Religion
            count_rel_cat= sum(Religion2=="catholic", na.rm = T),  perc_rel_cat= 100*count_rel_cat/n,
            count_rel_other_or_NA= sum(Religion2=="other or missing", na.rm = T),  perc_rel_other_or_NA= 100*count_rel_other_or_NA/n,
            
            count_Lausanne_yes= sum(Lausanne=="yes", na.rm = T),  perc_Lausanne_yes= 100*count_Lausanne_yes/n,     #Living in Lausanne or not
            count_Lausanne_no= sum(Lausanne=="no", na.rm = T),  perc_Lausanne_no= 100*count_Lausanne_no/n,
            count_Lausanne_unsr= sum(Lausanne=="unsure", na.rm = T),  perc_Lausanne_unsr= 100*count_Lausanne_unsr/n,
            
            menarche_mean = round(mean(menarche, na.rm = T), digits = 1),        # menarche
            sd_menarche = round(sd(menarche, na.rm = T), digits = 1),
            count_menarche_NA = sum(is.na(menarche)), perc_menarche_NA=100*count_menarche_NA/n,
            
            height_mean = round(mean(height, na.rm = T), digits = 1),        # height
            sd_height = round(sd(height, na.rm = T), digits = 1),
            count_height_NA = sum(is.na(height)), perc_height_NA=100*count_height_NA/n,
            
            waistcir = round(mean(waist_circ, na.rm = T), digits = 1),        # waist circ
            sd_waistcir = round(sd(waist_circ, na.rm = T), digits = 1),
            count_wc_NA = sum(is.na(waist_circ)), perc_wc_NA=100*count_wc_NA/n,
            
            count_obese= sum(Obese=="1", na.rm = T),  perc_obese= 100*count_obese/n,     # dummy var
            count_thin= sum(Thin=="1", na.rm = T),  perc_thin= 100*count_thin/n,
            count_goitre= sum(Goitre=="1", na.rm = T),  perc_goitre= 100*count_goitre/n,
            count_infection = sum(Infection=="Infected", na.rm = T), perc_infection=100*count_infection/n,
            
            count_rickets= sum(rickets=="1", na.rm = T),  perc_rickets= 100*count_rickets/n,    # Rickets
            count_flu_during_pregn= sum(Grippe_during_pregn_2=="yes", na.rm = T),  perc_flu_during_pregn= 100*count_flu_during_pregn/n,    # Flu during preg (categroies B or C)
            count_syphilis= sum(Syphilis_2=="positive", na.rm = T),  perc_syph= 100*count_syphilis/n,    # syphilis
            
            count_hisco_cat3_1= sum(hisco_class_3=="1", na.rm = T),  perc_hisco_cat3_1= 100*count_hisco_cat3_1/n,   # hisco class 3
            count_hisco_cat3_2= sum(hisco_class_3=="2", na.rm = T),  perc_hisco_cat3_2= 100*count_hisco_cat3_2/n,
            count_hisco_cat3_3= sum(hisco_class_3=="3", na.rm = T),  perc_hisco_cat3_3= 100*count_hisco_cat3_3/n,
            count_hisco_cat3_NA = sum(hisco_class_3=="missing", na.rm = T), perc_hisco_cat3_NA=100*count_hisco_cat3_NA/n
  ) %>%     
  ungroup()


formatted_tab_mat2 <- tab_mat2 %>%
  mutate(n=n,
         "Maternal age, mean ± sd (years)"= stringr::str_glue("{mat_age} ± {sd_mat_age_c}"),   # mat age continuous
         "missing maternal age"= round({perc_mat_age_NA}, digits=1),
         
         "Menarche, mean ± sd (years)"= stringr::str_glue("{menarche_mean} ± {sd_menarche}"),   # menarche
         "missing menarche"= round({perc_menarche_NA}, digits=1),
         
         "Height, mean ± sd (cm)"= stringr::str_glue("{height_mean} ± {sd_height}"),  # height
         "missing height"= round({perc_height_NA}, digits=1),
         
         "Waist circumference, mean ± sd (cm)"= stringr::str_glue("{waistcir} ± {sd_waistcir}"),  # waist circ
         "missing waist circumference"= round({perc_wc_NA}, digits=1),
         
         "1"= round({perc_parity_1}, digits=1),  # Parity
         "2"= round({perc_parity_2}, digits=1),
         ">2"= round({perc_parity_3}, digits=1),
         "Missing parity"= round({perc_parity_NA}, digits=1),
         
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
         "Infection"= round({perc_infection}, digits=1),
         
         "Hisco.1"= round({perc_hisco_cat3_1}, digits=1),  # Hisco 3
         "Hisco.2"= round({perc_hisco_cat3_2}, digits=1),
         "Hisco.3"= round({perc_hisco_cat3_3}, digits=1),
         "Missing Hisco"= round({perc_hisco_cat3_NA}, digits=1),
         
         "Rickets"= round({perc_rickets}, digits=1),  # Rickets
         "Flu during pregnancy"= round({perc_flu_during_pregn}, digits=1),  # Flu during pregn
         "Syphilis positive"= round({perc_syph}, digits=1),  # syphilis
         
  ) %>%
  tidyr::complete(birthyear) %>%
  select(birthyear, "Maternal age, mean ± sd (years)", "missing maternal age", "Menarche, mean ± sd (years)", "missing menarche", "Height, mean ± sd (cm)", "missing height", 
         "Waist circumference, mean ± sd (cm)", "missing waist circumference", "1", "2", ">2", "Missing parity", "Married", "Single or missing",
         "Protestant", "Catholic", "other or missing", 
         "yes", "no", "unsure",
         "Obese", "Thin", "Goitre", "Infection", "Rickets",
         "Flu during pregnancy", "Syphilis positive",
         "Hisco.1", "Hisco.2", "Hisco.3", "Missing Hisco",  n)

#inverted
formatted_tab_mat2 <- formatted_tab_mat2
mat_characteristics2 <- t(formatted_tab_mat2)
rownames(mat_characteristics2) <- colnames(formatted_tab_mat2)
colnames(mat_characteristics2) <- rownames(formatted_tab_mat2)
colnames(mat_characteristics2) <- mat_characteristics2[1,]
mat_characteristics2 <- mat_characteristics2[-1, ] 
mat_characteristics2 <- as_tibble(mat_characteristics2)


### V2: grouped rows
mat_characteristics2 <- add_column(mat_characteristics2, .before = 1,
                                   name = c("mean ± sd (years)", "missing (%)", "mean ± sd (years)", "missing (%)", "mean ± sd (cm)", "missing (%)", 
                                            "mean ± sd (cm)", "missing (%)", "1", "2", ">2", "missing", "Married", "Single or missing",
                                            "Protestant", "Catholic", "other or missing", 
                                            "yes", "no", "unsure",
                                            "Obese", "Thin", "Goitre", "Infection", "Rickets",
                                            "Flu during pregnancy","Syphilis positive",
                                            "1", "2", "3", "missing", 
                                            "n")
)

mat_characteristics2_gt <- mat_characteristics2 %>%
  gt() %>%
  tab_header(title=md("**Maternal characteristics**"), 
             subtitle = "n=8,025") %>%
  tab_row_group(label="Maternal age",
                rows=1:2) %>%
  tab_row_group(label="Menarche",
                rows=3:4) %>%
  tab_row_group(label="Height",
                rows=5:6) %>%
  tab_row_group(label="Waist circumference",
                rows=7:8) %>%
  tab_row_group(label="Parity (%)",
                rows=9:12) %>%
  tab_row_group(label="Civil status (%)",
                rows=13:14) %>%
  tab_row_group(label="Religion (%)",
                rows=15:17) %>%
  tab_row_group(label="Living in Lausanne (%)",
                rows=18:20) %>%
  tab_row_group(label="General health status (%)",
                rows=21:27) %>%
  tab_row_group(label="Hisco class 3 (%)",
                rows=28:31) %>%
  row_group_order(groups=c("Maternal age", "Menarche",
                           "Height", "Waist circumference", 
                           "Parity (%)","Civil status (%)", 
                           "Religion (%)", "Living in Lausanne (%)",
                           "General health status (%)", "Hisco class 3 (%)")
  )%>%
  cols_label(name = "Birthyear") %>%
  cols_width(name ~ px(170), everything() ~ px(50)) %>%
  tab_options(table.width = 0.1, data_row.padding = px(1),   table.font.size = 16,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 18, heading.subtitle.font.size = 18
  ) 

mat_characteristics2_gt
mat_characteristics2_gt %>%
  gtsave(here("output/tables", "maternal_characteristics2_1912_1923.html")) 

mat_characteristics2_gt %>%
  gtsave(here("output/tables", "maternal_characteristics2_1912_1923.docx"))

#  neonatal characteristics  #### 
## overall, simplified, all years together ####
neonat_charc <- lausgraph %>%
  select(birthweight,hc.perturbed,GA_weeks_corrected,
         sex,
         PTB_corrected, GA_weeks_cat_corrected2,
         stillbirth, neonat_mort_d1_d5
         ) %>%  
  gtsummary::tbl_summary(
    statistic = list(
      c("birthweight", "hc.perturbed", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("sex","PTB_corrected", "GA_weeks_cat_corrected2",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("sex","PTB_corrected","GA_weeks_cat_corrected2",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "hc.perturbed", "GA_weeks_corrected") ~ 1),
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**")%>%
  modify_caption("**Neonatal characteristics**")
neonat_charc

neonat_charc %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/description_of_the_population", "neonatal_characteristics_overall_1911_22.html"))

neonat_charc %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/description_of_the_population", "neonatal_characteristics_overall_1911_22.docx"))


## by year ####
tab_neonat <- laus2 %>%
  group_by(birthyear) %>%
  summarise(n = n(),
            mean_birthweight = round(mean(birthweight, na.rm = T), digits = 0),   #birthweight
            sd_birthweight = round(sd(birthweight, na.rm = T), digits = 0),
            count_BW_NA = sum(is.na(birthweight)), perc_BW_NA=100*count_BW_NA/n,
            
            mean_BL = round(mean(babylength, na.rm = T), digits = 1),   #birthlength
            sd_BL = round(sd(babylength, na.rm = T), digits = 1),
            count_BL_NA = sum(is.na(babylength)), perc_BL_NA=100*count_BL_NA/n,
            
            mean_PW = round(mean(placentaweight, na.rm = T), digits = 0),   #placentaweight
            sd_PW = round(sd(placentaweight, na.rm = T), digits = 0),
            count_PW_NA = sum(is.na(placentaweight)), perc_PW_NA=100*count_PW_NA/n,

            mean_BW_r_PW = round(mean(BW_r_PW, na.rm = T), digits = 0),   # BW_r_PW
            sd_BW_r_PW = round(sd(BW_r_PW, na.rm = T), digits = 0),
            count_BW_r_PW_NA = sum(is.na(BW_r_PW)), perc_BW_r_PW_NA=100*count_BW_r_PW_NA/n,

            mean_HC = round(mean(head_circ, na.rm = T), digits = 1),   #head_circ
            sd_HC = round(sd(head_circ, na.rm = T), digits = 1),
            count_HC_NA = sum(is.na(head_circ)), perc_HC_NA=100*count_HC_NA/n,
            
            count_GA_w_corr_cat_1= sum(GA_weeks_cat_corrected2=="[10,33)", na.rm = T),  perc_GA_w_corr_1= 100*count_GA_w_corr_cat_1/n,     # age based on var "age enfant" assessed at birth
            count_GA_w_corr_cat_2= sum(GA_weeks_cat_corrected2=="[33,37)", na.rm = T),  perc_GA_w_corr_2= 100*count_GA_w_corr_cat_2/n,
            count_GA_w_corr_cat_3= sum(GA_weeks_cat_corrected2=="[37,41)", na.rm = T),  perc_GA_w_corr_3= 100*count_GA_w_corr_cat_3/n,
            count_GA_w_corr_4= sum(GA_weeks_cat_corrected2=="[41,52]", na.rm = T),  perc_GA_w_corr_4= 100*count_GA_w_corr_4/n,
            # count_GA_w_corr_NA = sum(is.na(GA_weeks_cat_corrected)), perc_GA_w_corr_NA=100*count_GA_w_corr_NA/n,
            
            count_females = sum(sex == "Female", na.rm = T),   percent_females = 100*count_females/n,       #sex
            count_males = sum(sex == "Male", na.rm = T),     percent_males = 100*count_males/n,
            count_sex_NA = sum(is.na(sex)), perc_sex_NA=100*count_sex_NA/n,
            
            count_SB_1 = sum(stillbirth=="stillbirth", na.rm=T), percent_SB_1=100*count_SB_1/n, #stillbirth

            count_mor_1 = sum(postbirth_death=="1", na.rm=T), percent_mor_1=100*count_mor_1/n, #postbirthdeath

            count_PTB_corrected = sum(PTB_corrected=="preterm", na.rm=T), percent_PTB_corrected=100*count_PTB_corrected/n, #preterm birth based on var "age enfant" assessed at birth
            # count_PTB_NA = sum(is.na(PTB2)), perc_PTB_NA=100*count_PTB_NA/n,
            
            count_LBW_1 = sum(LBW=="LBW", na.rm=T), percent_LBW_1=100*count_LBW_1/n,   #lowbirthweight    
            # count_LBW_NA = sum(is.na(LBW)), perc_LBW_NA=100*count_LBW_NA/n,
            
            count_feeding_mat= sum(feeding=="maternal", na.rm = T),  perc_feeding_mat= 100*count_feeding_mat/n # feeding

            
            ) %>%     
  ungroup()


formatted_tab_neonat <- tab_neonat %>%
  mutate(n=n,
         "Birthweight, mean ± sd (g)"= stringr::str_glue("{mean_birthweight} ± {sd_birthweight}"),    #birthweight
         "missing birthweight"= round({perc_BW_NA}, digits=1),
         
         "Birthlength, mean ± sd (cm)"= stringr::str_glue("{mean_BL} ± {sd_BL}"),    #birthlength
         "missing birthlength"= round({perc_BL_NA}, digits=1),
         
         "Placenta weight, mean ± sd (g)"= stringr::str_glue("{mean_PW} ± {sd_PW}"),    #placenta weight
         "missing placenta weight"= round({perc_PW_NA}, digits=1),
         
         "Birthweight/Placenta weight ratio"= stringr::str_glue("{mean_BW_r_PW} ± {sd_BW_r_PW}"),    #BW/PW ratio
         "missing BW/PW ratio"= round({perc_BW_r_PW_NA}, digits=1),
         
         "Head circumference, mean ± sd (cm)"= stringr::str_glue("{mean_HC} ± {sd_HC}"),    #head circ
         "missing head circumference"= round({perc_HC_NA}, digits=1),
         
         "< 33"=round({perc_GA_w_corr_1}, digits=1),   # age baby based on var "age enfant" assessed at birth
         "[33, 37)"=round({perc_GA_w_corr_2}, digits=1),
         "[37, 41)"=round({perc_GA_w_corr_3}, digits=1),
         ">= 41"=round({perc_GA_w_corr_4}, digits=1),
         # "missing gestational age"= round({perc_GA_w_corr_NA}, digits=1),
         
         "female (%)" =round({percent_females}, digits=1), # sex
         "male (%)" = round({percent_males}, digits=1),
         "missing sex"= round({perc_sex_NA}, digits=1),
                
         "Stillbirth (%)"= round({percent_SB_1}, digits=1), #stillbirth

         "Neonatal mortality (%)" = round({percent_mor_1}, digits=1), #neonat mortality

         "Preterm birth (<37 weeks, %)"=round({percent_PTB_corrected}, digits=2), #PTB
         
         "Low birthweight (<2500g, %)" =round({percent_LBW_1}, digits=2),
         
         "Feeding: maternal milk (%)" =round({perc_feeding_mat}, digits=2),
         
  ) %>%
  tidyr::complete(birthyear) %>%
  select(birthyear, "Birthweight, mean ± sd (g)", "missing birthweight", "Birthlength, mean ± sd (cm)", "missing birthlength",
         "Placenta weight, mean ± sd (g)", "missing placenta weight", "Birthweight/Placenta weight ratio", "missing BW/PW ratio",
         "Head circumference, mean ± sd (cm)", "missing head circumference", 
         "< 33", "[33, 37)", "[37, 41)", ">= 41",
         "female (%)", "male (%)", "missing sex", 
         "Stillbirth (%)", "Neonatal mortality (%)",
         "Preterm birth (<37 weeks, %)", "Low birthweight (<2500g, %)", "Feeding: maternal milk (%)",
         n)

#inverted
neonat_characteristics <- t(formatted_tab_neonat)
rownames(neonat_characteristics) <- colnames(formatted_tab_neonat)
colnames(neonat_characteristics) <- rownames(formatted_tab_neonat)

#removing the first row that is useless, to have fir row (and col names) as birthyear
colnames(neonat_characteristics) <- neonat_characteristics[1,]
neonat_characteristics <- neonat_characteristics[-1, ] 
neonat_characteristics <- as_tibble(neonat_characteristics)


### V2: grouped rows
neonat_characteristics <- add_column(neonat_characteristics, .before = 1,
                                  name = c("mean ± sd (g)", "missing (%)", "mean ± sd (cm)", "missing (%)",
                                           "mean ± sd (g)", "missing (%)", "mean ± sd", "missing (%)",
                                           "mean ± sd (cm)", "missing (%)",
                                           "< 33", "[33, 37)", "[37, 41)", ">= 41", 
                                           "female", "male", "missing",
                                           "Stillbirth (%)", "Neonatal mortality (%)", 
                                           "Preterm birth (<37 weeks, %)",
                                           "Low birthweight (<2500g, %)", "Feeding: maternal milk (%)",
                                           "n")
)


neonat_characteristics_gt <- neonat_characteristics %>%
  gt() %>%
  tab_header(title=md("**Neonatal characteristics**"), 
             subtitle = "n=8,025") %>%
  tab_row_group(label="Birthweight",
                rows=1:2) %>%
  tab_row_group(label="Birthlength",
                rows=3:4) %>%
  tab_row_group(label="Placenta weight",
                rows=5:6) %>%
  tab_row_group(label="Birthweight/placenta weight ratio",
                rows=7:8) %>%
  tab_row_group(label="Head circumference",
                rows=9:10) %>%
  tab_row_group(label="Gestational age (weeks, %)",
                rows=11:14) %>%
  tab_row_group(label="Neonatal sex (%)",
                rows=15:17) %>%
  row_group_order(groups=c("Birthweight", "Birthlength", "Placenta weight", "Birthweight/placenta weight ratio",
                           "Head circumference", "Gestational age (weeks, %)", "Neonatal sex (%)")
  )%>%
  cols_label(name = "Birthyear") %>%
  cols_width(name ~ px(170), everything() ~ px(50)) %>%
  tab_options(table.width = 0.1, data_row.padding = px(1),   table.font.size = 16,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 18, heading.subtitle.font.size = 18
  )
# source_note = md("In <span style='color: #FFE1FF;'>pink</span> are the years who are not yet fully transcribed")
# tab_style(
#   style = list(
#     cell_fill(color = "#FFE1FF")
#   ),
#   locations = cells_body(rows=23, columns=c(2,4,12))) %>%
#   tab_source_note(
#     source_note = md("In *pink* are the years which are not yet fully transcribed"))
neonat_characteristics_gt
neonat_characteristics_gt %>%
  gtsave(here("output/tables", "neonatal_characteristics_1912_1923.html"))

neonat_characteristics_gt %>%
  gtsave(here("output/tables", "neonatal_characteristics_1912_1923.docx"))