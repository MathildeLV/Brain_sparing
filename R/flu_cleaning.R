# Recoding Flu cases, categories and symptoms

# Grippe_during_pregn_2 and Grippe_during_pregn_3 : for flu cases during pregnancy, not necessarily during the pandemic
# Grippe_during_pregn_2: category missing means that the mother had the flu but we dont know when
# Grippe_during_pregn_3: the "missing category" are pooled with the "no". I.e. if we are not sure whether the infection took place during pregancy, 
# we consider it was not the case )= more conservative)
# Flu_in_pregn_and_in_pandemic: infection during pregnancy (based on Grippe_during_pregn_3) and during the pandemic (based on date of delivery)
# flu_or_syph: infection by either flu or syphilis or both (during pregnancy), and for flu also during the pandemic
laus1 <- laus1 %>%
  mutate(Grippe_during_pregn_2=case_when(Grippe_cat=="B" |Grippe_cat=="C" ~"yes",
                                             Grippe_cat=="A" | Grippe_cat=="D"  ~ "no",
                                             Grippe_cat=="NULL" ~"no",
                                             is.na(Grippe_cat) ~ "missing"),
          Grippe_during_pregn_2=as.factor(Grippe_during_pregn_2),
          
          Grippe_during_pregn_3=case_when(Grippe_cat=="B" |Grippe_cat=="C" ~"yes",
                                          Grippe_cat=="A" | Grippe_cat=="D"  ~ "no",
                                          Grippe_cat=="NULL" ~"no",
                                          is.na(Grippe_cat) ~ "no"),
          Grippe_during_pregn_3=as.factor(Grippe_during_pregn_3),

          Flu_in_pregn_and_in_pandemic = 
           ifelse(Grippe_during_pregn_3 == "yes" &
                    birthdate> "1918-06-01" &
                    (birthdate < "1921-01-01" & month_1<"1920-04-01"), "yes", "no"),
          Flu_in_pregn_and_in_pandemic=as.factor(Flu_in_pregn_and_in_pandemic),
         
          flu_or_syph= case_when(Flu_in_pregn_and_in_pandemic=="no" & Syphilis_2=="no"~"no",
                                Flu_in_pregn_and_in_pandemic=="yes" & Syphilis_2=="no"~"influenza",
                                Flu_in_pregn_and_in_pandemic=="no" & Syphilis_2=="positive"~"syphilis",
                                Flu_in_pregn_and_in_pandemic=="yes" & Syphilis_2=="positive"~"influenza and syphilis"),
          flu_or_syph=as.factor(flu_or_syph))

table(laus1$Grippe_during_pregn_2, useNA = "a")
laus1$Grippe_during_pregn_2 <-  relevel(laus1$Grippe_during_pregn_2, ref=2)
table(laus1$Grippe_during_pregn_3, useNA = "a")
table(laus1$Flu_in_pregn_and_in_pandemic, useNA = "a")

order <- c(3, 1, 4, 2)
laus1$flu_or_syph <- factor(laus1$flu_or_syph, levels = levels(laus1$flu_or_syph)[order])
table(laus1$flu_or_syph)
