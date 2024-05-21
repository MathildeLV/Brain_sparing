# If BW > or < 3 sd for each sex separately, depending on GA, then exclude?
laus1.1 <- laus1%>%
filter(birthyear<1923 & birthyear >1910)

# and exclude home births
laus1.2 <- laus1.1 %>%
  filter(!(Homebirth2 == "yes"))

# keep only GA >20 weeks and birthweight >500g 
laus1.3 <- laus1.2 %>%
  filter(!(GA_weeks_corrected < 22),
         !birthweight < 500)

# excl. extreme outliers for birthweight and head circumference 
  laus2 <- laus1.3 %>%
  filter(((Birthweight_Z< 7&Birthweight_Z>(-7)) |is.na(Birthweight_Z)) &
          ((Birthweight_Z_sex2< 7&Birthweight_Z_sex2>(-7)) |is.na(Birthweight_Z_sex2))  &
           ((Head_circ_Z< 7&Head_circ_Z>(-7)) |is.na(Head_circ_Z))  &
           ((head_circ_Z_sex2< 7&head_circ_Z_sex2>(-7)) |is.na(head_circ_Z_sex2))  
             )

# Exclude cases with both flu AND syphilis
  laus4 <- laus2 %>%
    filter(flu_or_syph != "influenza and syphilis") %>%
    mutate(flu_or_syph = droplevels(flu_or_syph))
    # primiparous only 
  laus4_primi <- laus4 %>%
    filter(parity_cat2=="(0,1]")

# Exclude cases with both flu AND syphilis, LIVEBIRTHS only
  laus4_livebirths <- laus4 %>%
    filter(stillbirth=="livebirth") 
  # primiparous women only
  laus4_livebirths_primi <- laus4_livebirths %>%
    filter(parity_cat2=="(0,1]")
  
laus2 <- laus2 %>%
    select(-one_of("age_baby_cat",
                   "PreviousBirths",
                   "placenta_diam", "Kilograms", 
                   "age_baby_text", "age_baby_final_num", 
                   "age_baby_cont2","age_baby_cat",
                   "Homebirth"))
  
# Livebirths only
  laus_livebirth <- laus2 %>%
    filter(stillbirth=="livebirth")
  laus_PTB_corrected <- laus2 %>%
    filter(PTB_corrected=="term")
  
  
  ## LABELS AND NAME OF CATEGORIES, FOR TABLES #### 
  lausgraph<- laus2
  attr(lausgraph$parity_cat2, 'levels')<- c("1","2", ">2")
  attr(lausgraph$Obese, 'levels')<- c("no", "yes")
  attr(lausgraph$Thin, 'levels')<- c("no", "yes")
  attr(lausgraph$Goitre, 'levels')<- c("no", "yes")
  attr(lausgraph$rickets, 'levels')<- c("no", "yes")
  attr(lausgraph$Grippe_during_pregn_3, 'levels')<- c("no", "yes")
  attr(lausgraph$Syphilis_2, 'levels')<- c("no", "yes")
  attr(lausgraph$stillbirth, 'levels')<- c("no", "yes")
  attr(lausgraph$postbirth_death, 'levels')<- c("no", "yes")
  attr(lausgraph$neonat_mort_day1, 'levels')<- c("no", "yes")
  attr(lausgraph$neonat_mort_d1_d5, 'levels')<- c("no", "yes")
  attr(lausgraph$PTB_corrected, 'levels')<- c("no", "yes")
  attr(lausgraph$LBW, 'levels')<- c("no", "yes")
  desired_order <- c("[10,33)", "[33,37)", "[37,41)", "[41,52]")  # Replace with your desired order
  lausgraph <- lausgraph %>%
    mutate(GA_weeks_cat_corrected2 = factor(GA_weeks_cat_corrected2, levels = desired_order))
  attr(lausgraph$GA_weeks_cat_corrected2, 'levels')<- c("<33","33-37", "37-41", ">41")
  
  
  my_labels <- c(age_mother = "maternal age (years)",  # Create labels
                 height= "height (cm)",
                 waist_circ="waist circumference (cm)",
                 parity_cat2 = "parity",
                 civil_status_cat2= "civil status",
                 Lausanne= "living in Lausanne",
                 hisco_class_3="HISCO class",
                 Obese="obese",
                 Thin="thin",
                 Goitre="goitre",
                 Infection="disease",
                 rickets="rickets",
                 birthweight="birth weight (g)", 
                 head_circ="head circumference (cm)",hc.perturbed="head circumference (cm)",
                 sex="sex",
                 GA_weeks_cat_corrected2="gestational age (weeks)",
                 stillbirth="stilllbirth", postbirth_death="died after birth",
                 PTB_corrected="preterm birth (<37 weeks) ",LBW="low birth weight (<2'500g)",
                 babylength="birth length (cm)",
                 Grippe_during_pregn_3="flu during pregnancy",
                 Syphilis_2="syphilis",
                 morphology="morphology",
                 neonat_mort_d1_d5="neonatal mortality d1-5",
                 flu_or_syph="influenza or syphilis",
                 NEWTestDone="Wasserman test done",
                 GA_weeks_corrected="gestational age (weeks)"
  )
  label(lausgraph) <- as.list(my_labels[match(names(lausgraph), # Assign labels to data frame variables
                                              names(my_labels))])
  label(lausgraph)
  
  
  # laus4
  laus4_graph <- laus4
  attr(laus4_graph $parity_cat2, 'levels')<- c("1","2", ">2")
  attr(laus4_graph $Obese, 'levels')<- c("no", "yes")
  attr(laus4_graph $Thin, 'levels')<- c("no", "yes")
  attr(laus4_graph $Goitre, 'levels')<- c("no", "yes")
  attr(laus4_graph $rickets, 'levels')<- c("no", "yes")
  attr(laus4_graph $Grippe_during_pregn_3, 'levels')<- c("no", "yes")
  attr(laus4_graph $Syphilis_2, 'levels')<- c("no", "yes")
  attr(laus4_graph $stillbirth, 'levels')<- c("no", "yes")
  attr(laus4_graph $postbirth_death, 'levels')<- c("no", "yes")
  attr(laus4_graph $neonat_mort_day1, 'levels')<- c("no", "yes")
  attr(laus4_graph $neonat_mort_d1_d5, 'levels')<- c("no", "yes")
  attr(laus4_graph $PTB_corrected, 'levels')<- c("no", "yes")
  attr(laus4_graph $LBW, 'levels')<- c("no", "yes")
  desired_order <- c("[10,33)", "[33,37)", "[37,41)", "[41,52]")  # Replace with your desired order
  laus4_graph  <- laus4_graph  %>%
    mutate(GA_weeks_cat_corrected2 = factor(GA_weeks_cat_corrected2, levels = desired_order))
  attr(laus4_graph $GA_weeks_cat_corrected2, 'levels')<- c("<33","33-37", "37-41", ">41")
  
  label(laus4_graph ) <- as.list(my_labels[match(names(laus4_graph ), # Assign labels to data frame variables
                                                 names(my_labels))])
  label(laus4_graph )  
  
    #Exclusions based on INTERGROWTH-21: We dont use anymore   
# laus_IG_exclusions <- laus2 %>%
#   filter(!(Birthweight_Z_sex2 <= (-2)   | Birthweight_Z_sex2 >= (2)))
# laus_IG_exclusionsa <- laus2 %>%
#   filter(
#     ((birthweight<=1210 & birthweight>=340) & (GA_weeks>=24 & GA_weeks <25) & sex=="Female") |
#     ((birthweight<=1380 & birthweight>=390) & (GA_weeks>=25 & GA_weeks <26) & sex=="Female") |
#     ((birthweight<=1570 & birthweight>=440) & (GA_weeks>=26 & GA_weeks <27) & sex=="Female") |
#     ((birthweight<=1780 & birthweight>=500) & (GA_weeks>=27 & GA_weeks <28) & sex=="Female") |
#     ((birthweight<=2010 & birthweight>=570) & (GA_weeks>=28 & GA_weeks <29) & sex=="Female") |
#     ((birthweight<=2270 & birthweight>=640) & (GA_weeks>=29 & GA_weeks <30) & sex=="Female") |
#     ((birthweight<=2560 & birthweight>=730) & (GA_weeks>=30 & GA_weeks <31) & sex=="Female")|
#     ((birthweight<=2890 & birthweight>=820) & (GA_weeks>=31 & GA_weeks <32) & sex=="Female") |
#     ((birthweight<=3240 & birthweight>=920) & (GA_weeks>=32 & GA_weeks <33) & sex=="Female") |
#     ((birthweight<=3420 & birthweight>=750) & (GA_weeks>=33 & GA_weeks <34) & sex=="Female") |
#     ((birthweight<=3700 & birthweight>=1010) & (GA_weeks>=34 & GA_weeks <35) & sex=="Female") |
#     ((birthweight<=3940 & birthweight>=1240) & (GA_weeks>=35 & GA_weeks <36) & sex=="Female") |
#     ((birthweight<=4160 & birthweight>=1450) & (GA_weeks>=36 & GA_weeks <37) & sex=="Female") |
#     ((birthweight<=4360 & birthweight>=1640) & (GA_weeks>=37 & GA_weeks <38) & sex=="Female") |
#     ((birthweight<=4530 & birthweight>=1800) & (GA_weeks>=38 & GA_weeks <39) & sex=="Female") |
#     ((birthweight<=4680 & birthweight>=1940) & (GA_weeks>=39 & GA_weeks <40) & sex=="Female") |
#     ((birthweight<=4810 & birthweight>=2060) & (GA_weeks>=40 & GA_weeks <41) & sex=="Female") |
#     ((birthweight<=4910 & birthweight>=2160) & (GA_weeks>=41 & GA_weeks <42) & sex=="Female") |
#     ((birthweight<=5000 & birthweight>=2240) & (GA_weeks>=42 & GA_weeks <43) & sex=="Female") |
#     ((birthweight<=5100 & birthweight>=2340) & (GA_weeks>=43 & GA_weeks <44) & sex=="Female") |
# 
#     ((birthweight<=1280 & birthweight>=360) & (GA_weeks>=24 & GA_weeks <25) & sex=="Male") |
#     ((birthweight<=1460 & birthweight>=410) & (GA_weeks>=25 & GA_weeks <26) & sex=="Male") |
#     ((birthweight<=1660 & birthweight>=470) & (GA_weeks>=26 & GA_weeks <27) & sex=="Male") |
#     ((birthweight<=1880 & birthweight>=530) & (GA_weeks>=27 & GA_weeks <28) & sex=="Male") |
#     ((birthweight<=2130 & birthweight>=600) & (GA_weeks>=28 & GA_weeks <29) & sex=="Male") |
#     ((birthweight<=2410 & birthweight>=680) & (GA_weeks>=29 & GA_weeks <30) & sex=="Male") |
#     ((birthweight<=2720 & birthweight>=770) & (GA_weeks>=30 & GA_weeks <31) & sex=="Male")|
#     ((birthweight<=3060 & birthweight>=870) & (GA_weeks>=31 & GA_weeks <32) & sex=="Male") |
#     ((birthweight<=3430 & birthweight>=980) & (GA_weeks>=32 & GA_weeks <33) & sex=="Male") |
#     ((birthweight<=3070 & birthweight>=630) & (GA_weeks>=33 & GA_weeks <34) & sex=="Male") |
#     ((birthweight<=3940 & birthweight>=910) & (GA_weeks>=34 & GA_weeks <35) & sex=="Male") |
#     ((birthweight<=4160 & birthweight>=1160) & (GA_weeks>=35 & GA_weeks <36) & sex=="Male") |
#     ((birthweight<=4360 & birthweight>=1390) & (GA_weeks>=36 & GA_weeks <37) & sex=="Male") |
#     ((birthweight<=4540 & birthweight>=1590) & (GA_weeks>=37 & GA_weeks <38) & sex=="Male") |
#     ((birthweight<=4700 & birthweight>=1780) & (GA_weeks>=38 & GA_weeks <39) & sex=="Male") |
#     ((birthweight<=4840 & birthweight>=1950) & (GA_weeks>=39 & GA_weeks <40) & sex=="Male") |
#     ((birthweight<=4970 & birthweight>=2100) & (GA_weeks>=40 & GA_weeks <41) & sex=="Male") |
#     ((birthweight<=5090 & birthweight>=2210) & (GA_weeks>=41 & GA_weeks <42) & sex=="Male") |
#     ((birthweight<=5180 & birthweight>=2340) & (GA_weeks>=42 & GA_weeks <43) & sex=="Male") |
#     ((birthweight<=5280 & birthweight>=2500) & (GA_weeks>=43 & GA_weeks <44) & sex=="Male")
#     )
# plot(laus_IG_exclusionsa$GA_weeks, laus_IG_exclusionsa$birthweight)

#         # I adedd this last line arbitrarily to not lost to many entries (43-44weeks)
# #
# (dim(laus2)-dim(laus_IG_exclusionsa))/dim(laus2)
#
# ### If I use GA_weeks corrected instead of GA_weeks
# laus_IG_exclusionsb <- laus2 %>%
#   filter(
#     ((birthweight<=1210 & birthweight>=340) & (GA_weeks_corrected>=24 & GA_weeks_corrected <25) & sex=="Female") |
#       ((birthweight<=1380 & birthweight>=390) & (GA_weeks_corrected>=25 & GA_weeks_corrected <26) & sex=="Female") |
#       ((birthweight<=1570 & birthweight>=440) & (GA_weeks_corrected>=26 & GA_weeks_corrected <27) & sex=="Female") |
#       ((birthweight<=1780 & birthweight>=500) & (GA_weeks_corrected>=27 & GA_weeks_corrected <28) & sex=="Female") |
#       ((birthweight<=2010 & birthweight>=570) & (GA_weeks_corrected>=28 & GA_weeks_corrected <29) & sex=="Female") |
#       ((birthweight<=2270 & birthweight>=640) & (GA_weeks_corrected>=29 & GA_weeks_corrected <30) & sex=="Female") |
#       ((birthweight<=2560 & birthweight>=730) & (GA_weeks_corrected>=30 & GA_weeks_corrected <31) & sex=="Female")|
#       ((birthweight<=2890 & birthweight>=820) & (GA_weeks_corrected>=31 & GA_weeks_corrected <32) & sex=="Female") |
#       ((birthweight<=3240 & birthweight>=920) & (GA_weeks_corrected>=32 & GA_weeks_corrected <33) & sex=="Female") |
#       ((birthweight<=3420 & birthweight>=750) & (GA_weeks_corrected>=33 & GA_weeks_corrected <34) & sex=="Female") |
#       ((birthweight<=3700 & birthweight>=1010) & (GA_weeks_corrected>=34 & GA_weeks_corrected <35) & sex=="Female") |
#       ((birthweight<=3940 & birthweight>=1240) & (GA_weeks_corrected>=35 & GA_weeks_corrected <36) & sex=="Female") |
#       ((birthweight<=4160 & birthweight>=1450) & (GA_weeks_corrected>=36 & GA_weeks_corrected <37) & sex=="Female") |
#       ((birthweight<=4360 & birthweight>=1640) & (GA_weeks_corrected>=37 & GA_weeks_corrected <38) & sex=="Female") |
#       ((birthweight<=4530 & birthweight>=1800) & (GA_weeks_corrected>=38 & GA_weeks_corrected <39) & sex=="Female") |
#       ((birthweight<=4680 & birthweight>=1940) & (GA_weeks_corrected>=39 & GA_weeks_corrected <40) & sex=="Female") |
#       ((birthweight<=4810 & birthweight>=2060) & (GA_weeks_corrected>=40 & GA_weeks_corrected <41) & sex=="Female") |
#       ((birthweight<=4910 & birthweight>=2160) & (GA_weeks_corrected>=41 & GA_weeks_corrected <42) & sex=="Female") |
#       ((birthweight<=5000 & birthweight>=2240) & (GA_weeks_corrected>=42 & GA_weeks_corrected <43) & sex=="Female") |
#       ((birthweight<=5100 & birthweight>=2340) & (GA_weeks_corrected>=43 & GA_weeks_corrected <44) & sex=="Female") |
# 
#       ((birthweight<=1280 & birthweight>=360) & (GA_weeks_corrected>=24 & GA_weeks_corrected <25) & sex=="Male") |
#       ((birthweight<=1460 & birthweight>=410) & (GA_weeks_corrected>=25 & GA_weeks_corrected <26) & sex=="Male") |
#       ((birthweight<=1660 & birthweight>=470) & (GA_weeks_corrected>=26 & GA_weeks_corrected <27) & sex=="Male") |
#       ((birthweight<=1880 & birthweight>=530) & (GA_weeks_corrected>=27 & GA_weeks_corrected <28) & sex=="Male") |
#       ((birthweight<=2130 & birthweight>=600) & (GA_weeks_corrected>=28 & GA_weeks_corrected <29) & sex=="Male") |
#       ((birthweight<=2410 & birthweight>=680) & (GA_weeks_corrected>=29 & GA_weeks_corrected <30) & sex=="Male") |
#       ((birthweight<=2720 & birthweight>=770) & (GA_weeks_corrected>=30 & GA_weeks_corrected <31) & sex=="Male")|
#       ((birthweight<=3060 & birthweight>=870) & (GA_weeks_corrected>=31 & GA_weeks_corrected <32) & sex=="Male") |
#       ((birthweight<=3430 & birthweight>=980) & (GA_weeks_corrected>=32 & GA_weeks_corrected <33) & sex=="Male") |
#       ((birthweight<=3070 & birthweight>=630) & (GA_weeks_corrected>=33 & GA_weeks_corrected <34) & sex=="Male") |
#       ((birthweight<=3940 & birthweight>=910) & (GA_weeks_corrected>=34 & GA_weeks_corrected <35) & sex=="Male") |
#       ((birthweight<=4160 & birthweight>=1160) & (GA_weeks_corrected>=35 & GA_weeks_corrected <36) & sex=="Male") |
#       ((birthweight<=4360 & birthweight>=1390) & (GA_weeks_corrected>=36 & GA_weeks_corrected <37) & sex=="Male") |
#       ((birthweight<=4540 & birthweight>=1590) & (GA_weeks_corrected>=37 & GA_weeks_corrected <38) & sex=="Male") |
#       ((birthweight<=4700 & birthweight>=1780) & (GA_weeks_corrected>=38 & GA_weeks_corrected <39) & sex=="Male") |
#       ((birthweight<=4840 & birthweight>=1950) & (GA_weeks_corrected>=39 & GA_weeks_corrected <40) & sex=="Male") |
#       ((birthweight<=4970 & birthweight>=2100) & (GA_weeks_corrected>=40 & GA_weeks_corrected <41) & sex=="Male") |
#       ((birthweight<=5090 & birthweight>=2210) & (GA_weeks_corrected>=41 & GA_weeks_corrected <42) & sex=="Male") |
#       ((birthweight<=5180 & birthweight>=2340) & (GA_weeks_corrected>=42 & GA_weeks_corrected <43) & sex=="Male") |
#       ((birthweight<=5280 & birthweight>=2500) & (GA_weeks_corrected>=43 & GA_weeks_corrected <44) & sex=="Male")
#   )
# plot(laus_IG_exclusionsb$GA_weeks_corrected, laus_IG_exclusionsb$birthweight)
# (dim(laus2)-dim(laus_IG_exclusionsb))/dim(laus2) #2.28% excluded
# #
#
# ## Looking at these 112 entries we excluded
# differences <- anti_join(laus2, laus_IG_exclusionsb, by = "Keyid") %>%
#   select(Keyid,Booknr, Journalnr, stillbirth,birthweight, weightA, weightB, GA_weeks_corrected, GA_weeks, age_baby_cat, age_baby_text,
#          sex, babylength, waist_circ)
# laus_outliers <- differences
# # openxlsx::write.xlsx(laus_outliers, file=paste0("output/laus_outliers_",today(),".xlsx"), rowNames = FALSE)
# # Marquer les entrées différentes comme outliers=true
# laus3 <- laus2 %>%
#   mutate(outliers = ifelse(Keyid %in% differences$Keyid, TRUE, FALSE))
# # Marquer les entrées communes comme outliers=false
# laus3 <- laus3 %>%
#   mutate(outliers = ifelse(Keyid %in% laus_IG_exclusionsb$Keyid, FALSE, outliers))
# 
# table(laus3$birthyear, laus3$outliers, useNA="always")
# table(laus3$sex, laus3$outliers, useNA="always")
# 
# round(prop.table(table(laus3$birthyear, laus3$outliers), margin=1)*100,2)
# 

# ######## Excluding outliers for head circumference
# # laus_IG_exclusions <- laus2 %>%
# #   filter(!(Birthweight_Z_sex2 <= (-2)   | Birthweight_Z_sex2 >= (2)))
# laus_IG_exclusions <- laus_IG_exclusionsb %>%
#   filter(
#     ((head_circ<27.5 & head_circ>=17.4) & (GA_weeks_corrected>=24 & GA_weeks_corrected <25) & sex=="Female") |
#       ((head_circ<=28.4 & head_circ>=18.3) & (GA_weeks_corrected>=25 & GA_weeks_corrected <26) & sex=="Female") |
#       ((head_circ<=29.3 & head_circ>=19.2) & (GA_weeks_corrected>=26 & GA_weeks_corrected <27) & sex=="Female") |
#       ((head_circ<=30.3 & head_circ>=20.1) & (GA_weeks_corrected>=27 & GA_weeks_corrected <28) & sex=="Female") |
#       ((head_circ<=31.1 & head_circ>=21) & (GA_weeks_corrected>=28 & GA_weeks_corrected <29) & sex=="Female") |
#       ((head_circ<=32 & head_circ>=21.8) & (GA_weeks_corrected>=29 & GA_weeks_corrected <30) & sex=="Female") |
#       ((head_circ<=32.9 & head_circ>=22.7) & (GA_weeks_corrected>=30 & GA_weeks_corrected <31) & sex=="Female")|
#       ((head_circ<=33.7 & head_circ>=23.6) & (GA_weeks_corrected>=31 & GA_weeks_corrected <32) & sex=="Female") |
#       ((head_circ<=34.6 & head_circ>=24.5) & (GA_weeks_corrected>=32 & GA_weeks_corrected <33) & sex=="Female") |
#       ((head_circ<=35.5 & head_circ>=26.2) & (GA_weeks_corrected>=33 & GA_weeks_corrected <34) & sex=="Female") |
#       ((head_circ<=35.9 & head_circ>=26.9) & (GA_weeks_corrected>=34 & GA_weeks_corrected <35) & sex=="Female") |
#       ((head_circ<=36.2 & head_circ>=27.6) & (GA_weeks_corrected>=35 & GA_weeks_corrected <36) & sex=="Female") |
#       ((head_circ<=36.6 & head_circ>=28.3) & (GA_weeks_corrected>=36 & GA_weeks_corrected <37) & sex=="Female") |
#       ((head_circ<=36.9 & head_circ>=28.9) & (GA_weeks_corrected>=37 & GA_weeks_corrected <38) & sex=="Female") |
#       ((head_circ<=37.2 & head_circ>=29.4) & (GA_weeks_corrected>=38 & GA_weeks_corrected <39) & sex=="Female") |
#       ((head_circ<=37.4 & head_circ>=29.9) & (GA_weeks_corrected>=39 & GA_weeks_corrected <40) & sex=="Female") |
#       ((head_circ<=37.7 & head_circ>=30.3) & (GA_weeks_corrected>=40 & GA_weeks_corrected <41) & sex=="Female") |
#       ((head_circ<=37.9 & head_circ>=30.7) & (GA_weeks_corrected>=41 & GA_weeks_corrected <42) & sex=="Female") |
#       ((head_circ<=38.1 & head_circ>=31) & (GA_weeks_corrected>=42 & GA_weeks_corrected <43) & sex=="Female") |
#       ((head_circ<=38.3 & head_circ>=31.3) & (GA_weeks_corrected>=43 & GA_weeks_corrected <44) & sex=="Female") |
#       
#       ((head_circ<=27.8 & head_circ>17.7) & (GA_weeks_corrected>=24 & GA_weeks_corrected <25) & sex=="Male") |
#       ((head_circ<=28.7 & head_circ>=18.5) & (GA_weeks_corrected>=25 & GA_weeks_corrected <26) & sex=="Male") |
#       ((head_circ<=29.6 & head_circ>=19.4) & (GA_weeks_corrected>=26 & GA_weeks_corrected <27) & sex=="Male") |
#       ((head_circ<=30.4 & head_circ>=20.3) & (GA_weeks_corrected>=27 & GA_weeks_corrected <28) & sex=="Male") |
#       ((head_circ<=31.3 & head_circ>=21.2) & (GA_weeks_corrected>=28 & GA_weeks_corrected <29) & sex=="Male") |
#       ((head_circ<=32.2 & head_circ>=22.1) & (GA_weeks_corrected>=29 & GA_weeks_corrected <30) & sex=="Male") |
#       ((head_circ<=33.1 & head_circ>=23) & (GA_weeks_corrected>=30 & GA_weeks_corrected <31) & sex=="Male")|
#       ((head_circ<=34 & head_circ>=23.9) & (GA_weeks_corrected>=31 & GA_weeks_corrected <32) & sex=="Male") |
#       ((head_circ<=34.9 & head_circ>=24.8) & (GA_weeks_corrected>=32 & GA_weeks_corrected <33) & sex=="Male") |
#       ((head_circ<=35.9 & head_circ>=26.5) & (GA_weeks_corrected>=33 & GA_weeks_corrected <34) & sex=="Male") |
#       ((head_circ<=36.3 & head_circ>=27.2) & (GA_weeks_corrected>=34 & GA_weeks_corrected <35) & sex=="Male") |
#       ((head_circ<=36.7 & head_circ>=27.9) & (GA_weeks_corrected>=35 & GA_weeks_corrected <36) & sex=="Male") |
#       ((head_circ<=37.1 & head_circ>=28.5) & (GA_weeks_corrected>=36 & GA_weeks_corrected <37) & sex=="Male") |
#       ((head_circ<=37.4 & head_circ>=29.1) & (GA_weeks_corrected>=37 & GA_weeks_corrected <38) & sex=="Male") |
#       ((head_circ<=37.8 & head_circ>=29.7) & (GA_weeks_corrected>=38 & GA_weeks_corrected <39) & sex=="Male") |
#       ((head_circ<=38.1 & head_circ>=30.2) & (GA_weeks_corrected>=39 & GA_weeks_corrected <40) & sex=="Male") |
#       ((head_circ<=38.4 & head_circ>=30.7) & (GA_weeks_corrected>=40 & GA_weeks_corrected <41) & sex=="Male") |
#       ((head_circ<=38.7 & head_circ>=31.1) & (GA_weeks_corrected>=41 & GA_weeks_corrected <42) & sex=="Male") |
#       ((head_circ<=39 & head_circ>=31.6) & (GA_weeks_corrected>=42 & GA_weeks_corrected <43) & sex=="Male") |
#       ((head_circ<=39.3 & head_circ>=32.1) & (GA_weeks_corrected>=43 & GA_weeks_corrected <44) & sex=="Male")
#      )
# plot(laus_IG_exclusions$GA_weeks_corrected, laus_IG_exclusions$head_circ)
# # I adedd this last line arbitrarily to not lost to many entries (43-44weeks)
# dim(laus_IG_exclusionsb)-dim(laus_IG_exclusions)
# (dim(laus_IG_exclusionsb)-dim(laus_IG_exclusions))/dim(laus_IG_exclusionsb) #additional 2.3% excluded
# 
# (dim(laus2)-dim(laus_IG_exclusions))/dim(laus2) # 4.97% excluded overall (birthweight+ head circ outliers)

# laus_IG_livebirth <- laus_IG_exclusions %>%
#   filter(stillbirth==0)
  ## males only
  # laus_IG_livebirth_males <- laus_IG_exclusions %>%
  # filter(stillbirth==0) %>%
  # filter(sex=="Male")
  ## females only
  # laus_IG_livebirth_females <- laus_IG_exclusions %>%
  # filter(stillbirth==0) %>%
  # filter(sex=="Female")

# # Flu-specific subsets
#   ## 1918-1919 pandemy
#   pandemy <- laus2 %>%
#     filter(birthdate> "1918-06-01") %>%
#     filter(birthdate < "1921-01-01" & month_1<"1920-04-01")

  # table(pandemy$Grippe, useNA = "always")
  # round(prop.table(table(pandemy$Grippe, useNA="always"))*100,2)
  # table(pandemy$Grippe_cat, useNA = "always")
  # round(prop.table(table(pandemy$Grippe_cat, useNA="always"))*100,2)
  # pandemy_PTB1 <- subset(pandemy, PTB1=="preterm")
  # pandemy_PTB2 <- subset(pandemy, PTB2=="preterm")
  # pandemy_term1 <- subset(pandemy, PTB1=="term")
  # pandemy_term2 <- subset(pandemy, PTB2=="term")
  # pandemy_livebirths <- subset(pandemy, stillbirth=="livebirth")

  
  ## for flu_cases only, during the pandemic
  # flu_cases_pandemy <- subset(pandemy, Grippe=="yes")
  # table(flu_cases_pandemy$Grippe, useNA = "always")
  # table(flu_cases_pandemy$Grippe_cat, useNA = "always")
  # round(prop.table(table(flu_cases_pandemy$Grippe_cat, useNA="always"))*100,2)