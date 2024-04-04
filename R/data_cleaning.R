#Formatting variables
# Put variables in the correct format

laus1 <- laus %>%
  select(-one_of("Check_com", "Check_again", "User", "Division", 
                 "Date", "Comment","Etat.general", 
                 "Check_nom", "Check_prenom", "Check_origine", "Check_domicile", "Check_profession",
                 "Check_etatGeneral", "Check_squelette", "Picture",
                 "age_baby2", "age_baby3", "first_nr","second_nr","first_nr_num", "second_nr_num",
                 "mean_age_enfant"
                 ))
str(laus1)

laus1 <- laus1 %>%
  mutate(across(c(civil_status,Religion,Lausanne,
                  sex,stillbirth, postbirth_death, Mother_deceased,
                  feeding, Obese, Thin, Goitre, Infection,
                  Etat_general_cat,rickets,eclampsia,
                  hisco, hisco_class_12, hisco_class_3,
                  agemother_cat, menarche_cat, parity_cat,
                  LBW, age_baby_cat, GA_weeks_cat, GA_weeks_cat_corrected,
                  PTB1, PTB2, PTB_corrected, post_term_corrected,
                  Grippe, Grippe_cat, Grippe_severity,
                  Syphilis, NEWSyphilisInvolved, 
                  NEWMotherDiagBefore, NEWMotherDiagDuring, NEWMotherDiagAtbirth,
                  NEWTestMotherPositive,NEWTestChildPositive, NEWChilddiagnosed,NEWTestDone,
                  Gonorrhoea), as.factor),
        birthdate=as.Date(birthdate, format = "%Y-%m-%d", useNA= "always"),
        lastperiod=as.Date(lastperiod, format = "%Y-%m-%d", useNA= "always"),
        birthyear=as.numeric(birthyear),
        birthmonth=as.numeric(birthmonth),
      # PTB 1: based on the calculated gestational age : delivery date-last period date
      # PTB 2: based on the estimated age at birth
      # PTB_corrected: based on both GA var
        Obese=replace(Obese, (is.na(Obese)), 0),
        Infection=replace(Infection, (is.na(Infection)), 0),
        Thin=replace(Thin, (is.na(Thin)), 0),
        Goitre=replace(Goitre, (is.na(Goitre)), 0)) 


laus1$civil_status <-  relevel(laus1$civil_status, ref=3)
laus1$Lausanne <-  relevel(laus1$Lausanne, ref="yes")
laus1$agemother_cat <-  relevel(laus1$agemother_cat, ref=2)
laus1$feeding <-  relevel(laus1$feeding, ref=3)
laus1$age_baby_cat <-  relevel(laus1$age_baby_cat, ref=3)
laus1$menarche_cat <-  relevel(laus1$menarche_cat, ref=2)
laus1$LBW <-  relevel(laus1$LBW, ref=1)
laus1$GA_weeks_cat <-  relevel(laus1$GA_weeks_cat, ref=7)
laus1$GA_weeks_cat_corrected <-  relevel(laus1$GA_weeks_cat_corrected, ref=6)
laus1$hisco_class_3 <-  relevel(laus1$hisco_class_3, ref=2)
laus1$birthyear1 <-  relevel(as.factor(laus1$birthyear), ref="1913") 
# when year 1913 is complete, use it as a reference year (and then continue backwards when 1912.. finished)


laus1 <- laus1 %>%
  mutate(civil_status_cat2 = recode (civil_status, 
                           "divorcee"= "single or missing",
                           "veuve"= "single or missing",
                           "celibataire"= "single or missing",
                            "mariee" = "married"), # pooling divorcee veuve as single
         civil_status_cat2=replace(civil_status_cat2, (is.na(civil_status_cat2)),"single or missing"),
         parity_cat2 = cut(parity, breaks=c(0,1, 2, 20)), # creating parity cat 2: 1, 2, >2 parities 
         agemother_cat2 = cut(age_mother, breaks=c(12,20,25,30,35,50)), 
         Religion2 = recode (Religion, "catholic"= "catholic",
                                   "protestant"= "protestant",
                                   "other"= "other or missing",
                                   "missing" = "other or missing"),
         feeding2 = recode(feeding, "maternal"="maternal",
                              "artifical"="artificial",
                              "human milk" ="other",
                              "maternal then artificial"= "maternal then other",
                              "maternal then mixed"= "maternal then other",
                              "maternal then mixed then artificial"= "maternal then other",
                              "mixed"="mixed",
                              "other"="other"),
          Homebirth2=recode(Homebirth, "yes" = "yes", "no" = "no", "0"="no"),
          Homebirth2=replace(Homebirth2, (is.na(Homebirth)), "no"),
          morphology=case_when(Thin==1 ~"thin",
                              Obese==1 ~"obese",
                              (Thin==0 & Obese==0 ~"neither")),
          Syphilis_2=case_when(
          Syphilis=="positive" ~"positive",
          Syphilis=="negative" | Syphilis == "no" | Syphilis=="douteux"~"no"),
          GA_weeks_cat2= cut(as.numeric(GA_weeks), 
                           breaks=c(10, 33, 37, 41, 52), include.lowest = TRUE,  right = FALSE),
          GA_weeks_cat_corrected2= cut(as.numeric(GA_weeks_corrected), 
                           breaks=c(10, 33, 37, 41, 52), include.lowest = TRUE,  right = FALSE),
        #   day_of_death=ifelse(postbirth_death=="0",0,day_of_death),
        #   neonat_mort_day1=ifelse((day_of_death)==1, 1,0),
        # neonat_mort_day1=as.factor(neonat_mort_day1),
        # neonat_mort_d1_d5=case_when((day_of_death<6 & day_of_death>0)~ 1,
        #                             (day_of_death==0 | day_of_death>5) ~0),
        # neonat_mort_d1_d5=as.factor(neonat_mort_d1_d5))
        across(c(civil_status_cat2,parity_cat2, agemother_cat2,Religion2, Homebirth2,
                 morphology,Syphilis_2,neonat_mort_d1_d5,neonat_mort_day1), as.factor))

laus1$agemother_cat2 <-  relevel(laus1$agemother_cat2, ref=2)
laus1$GA_weeks_cat2 <-  relevel(laus1$GA_weeks_cat2, ref=3)
laus1$GA_weeks_cat_corrected2 <-  relevel(laus1$GA_weeks_cat_corrected2, ref=3)



table(laus1$postbirth_death, useNA = "a")
table(laus1$day_of_death, useNA = "a") 
# day of death: counting from day of delivery= day1
# missing (NA) means: we know the neonate died after birth but we do not know on which day
# 0 means: the neonate did not die (i.e. not concerned)

#date
laus1$birthdate1 <- as.factor(laus1$birthdate)
laus1$birthdate_num <- as.numeric(laus1$birthdate1)
table(laus1$birthdate_num)
#extract week
# laus1 <- laus1 %>%
#   mutate(birthweek=format(birthdate, format = "%V"))

 laus1$birthweek <- format(as.Date(laus1$birthdate), "%Y-%V")
 # laus1$birthweek <- format(as.Date(laus1$birthweek))
 # laus1$birthweek <- as.Date(paste0(laus1$birthweek, "-1"), format = "%Y-%V-%u")

 
laus1 <- laus1 %>%
  mutate(GA_weeks_corr_rounded=round(GA_weeks_corrected, digits=0))


# add 0.25cm random noise to head circumference
set.seed(101)
rnorm(1,0,1)
table(laus1$head_circ, useNA = "a")
laus1$hc.perturbed<- laus1$head_circ + rnorm(dim(laus1)[1], 0, 1/4) # adding 0.25cm random error to the measurement
summary(laus1$head_circ)
summary(laus1$hc.perturbed)
sd(laus1$head_circ, na.rm=TRUE)
sd(laus1$hc.perturbed, na.rm=TRUE) # SD almost same 

### create anthropometric Z-scores
laus1 <- laus1 %>%
  mutate(Birthweight_Z =(birthweight - mean(birthweight, na.rm=TRUE)) / sd(birthweight, na.rm=TRUE),
         Placentaweight_Z =(placentaweight - mean(placentaweight, na.rm=TRUE)) / sd(placentaweight, na.rm=TRUE),
         Head_circ_Z =(head_circ - mean(head_circ, na.rm=TRUE)) / sd(head_circ, na.rm=TRUE),
         head_circ_perturb_Z =(hc.perturbed - mean(hc.perturbed, na.rm=TRUE)) / sd(hc.perturbed, na.rm=TRUE)) 

## Z transformed variables
### compute sex-related Z-score 
  # aux<- laus1$birthweight
  # b1<- !is.na(aux) & !is.na(laus1$sex)
  # auxZ<- rep(NA,dim(laus1)[1])
  # aux<- laus1 %>% reframe(Z=scale(birthweight),
  #                       .by=sex)
  # laus1$Birthweight_Z_sex<- aux$Z
  
### create sex-related anthropometric Z-scores
  laus1 <- laus1 %>%
    group_by(sex)%>%
    mutate(Birthweight_Z_sex2 =(birthweight - mean(birthweight, na.rm=TRUE)) / sd(birthweight, na.rm=TRUE),
           placentaweight_Z_sex2 =(placentaweight - mean(placentaweight, na.rm=TRUE)) / sd(placentaweight, na.rm=TRUE),
           head_circ_Z_sex2 =(head_circ - mean(head_circ, na.rm=TRUE)) / sd(head_circ, na.rm=TRUE),
           head_circ_perturb_Z_sex2 =(hc.perturbed - mean(hc.perturbed, na.rm=TRUE)) / sd(hc.perturbed, na.rm=TRUE)) %>%
    ungroup()
  summary(laus1$Birthweight_Z)
  summary(laus1$Birthweight_Z_sex2)
  summary(laus1$placentaweight_Z_sex2)
  summary(laus1$head_circ_Z_sex2)
  summary(laus1$head_circ_perturb_Z_sex2)
  sd(laus1$head_circ_Z_sex2, na.rm=TRUE)
  sd(laus1$head_circ_perturb_Z_sex2, na.rm=TRUE)
  
### create sex and GA-related anthropometric Z-scores
  laus1 <- laus1 %>%
    group_by(sex)%>%
    group_by(GA_weeks_corrected) %>%
    mutate(Birthweight_Z_sex_GA =(birthweight - mean(birthweight, na.rm=TRUE)) / sd(birthweight, na.rm=TRUE),
           head_circ_perturb_Z_sex_GA =(hc.perturbed - mean(hc.perturbed, na.rm=TRUE)) / sd(hc.perturbed, na.rm=TRUE)) %>%
    ungroup()
  
### create GA-related anthropometric Z-scores
  laus1 <- laus1 %>%
    group_by(GA_weeks_corrected) %>%
    mutate(Birthweight_Z_GA =(birthweight - mean(birthweight, na.rm=TRUE)) / sd(birthweight, na.rm=TRUE),
           head_circ_perturb_Z_GA =(hc.perturbed - mean(hc.perturbed, na.rm=TRUE)) / sd(hc.perturbed, na.rm=TRUE)) %>%
    ungroup()
#maternal year of birth & menarche
laus1 <- laus1 %>%
  mutate(mat_year_of_birth =birthyear - (round((age_mother), digits=0)),
         mat_year_of_birth_cat = cut(mat_year_of_birth, breaks=c(1865,1875,1885,1895,
                                                                 1905,1915)),
         menarche_rounded=round(menarche, digits=0))
table(laus1$mat_year_of_birth)
table(laus1$menarche_rounded)


# # exclude outliers for BW and GA_weeks
# laus1 <- laus1 %>%
#   filter((GA_weeks>24 & GA_weeks <44) | is.na(GA_weeks)) %>%
#   filter((birthweight>300 & birthweight <5300 )| is.na(birthweight))

#renaming some categories
attr(laus1$PTB1, "levels")<- c('term', 'preterm')
attr(laus1$PTB2, "levels")<- c('term', 'preterm')
attr(laus1$PTB_corrected, "levels")<- c('term', 'preterm')
attr(laus1$LBW, "levels")<- c('normal BW', 'LBW')
attr(laus1$sex, 'levels')<- c("Male", "Female")
attr(laus1$stillbirth, 'levels')<- c("livebirth", "stillbirth")
attr(laus1$Infection, "levels")<- c('Not Infected', 'Infected')

# sex ratio by year
laus1<- laus1 %>%
  group_by(as.factor(birthyear)) %>%
  mutate(SR = (sum(sex == "Male", na.rm = TRUE) / sum(sex == "Female", na.rm = TRUE))) %>%
  ungroup()


table(laus1$SR,useNA = "always")
plot(laus1$birthyear, laus1$SR)


laus1$birthdate_num_yr<- 
  laus1$birthdate_num/365.25

aux<- lubridate::yday(laus1$birthdate)
summary(aux)
sum(aux==60)
sum(aux==59)
sum(aux==61) 

# transform dates to angles
# aux<- aux * 2*pi/366
# summary(aux)
laus1 <- laus1 %>%
#   mutate(sin1=sin(aux), cos1=cos(aux)) %>%
  mutate(day366=aux)
summary(laus1$day366)
# sin should starts in 0 and cos in 1


laus1<- laus1 %>%
  mutate(day0=0)