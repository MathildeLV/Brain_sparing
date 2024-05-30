M_PW_BW <- gam(birthweight  ~ s(birthdate_num)+ s(day366, bs="cc") + s(height)+  s(age_mother) +
              flu_or_syph+parity_cat2+sex+morphology +civil_status_cat2+Lausanne, data=laus4_livebirths)
summary(M_PW_BW)


fit <- gam(list(y ~ s(x), # <- model for location 
                ~ s(x),   # <- model for log-scale
                ~ s(x),   # <- model for skewness
                ~ s(x, k = 20)), # <- model for log-kurtosis
           data = dataf, 
           family = shash, # <- new family 
           optimizer = "efs")
u<- laus4_graph%>%
  select("parity_cat2", "age_mother", "height", "Lausanne")%>%
  tbl_summary()
u

table(laus4$GA_weeks_cat_corrected2, useNA = "a") 
table(laus4$GA_weeks, useNA = "a")
table(laus4$GA_weeks_cat, useNA = "a")
table(laus4$age_baby_cat, useNA = "a")
table(laus4$age_baby_cont2, useNA = "a")
laus4 <- laus %>%
  mutate(age_baby_cat = cut(as.numeric(age_baby_cont2), 
                           breaks=c(10, 15, 20, 25, 30, 33, 37, 41, 44, 52), 
                           include.lowest = TRUE,  right = FALSE),
         GA_weeks_cat_corrected = cut(as.numeric(GA_weeks_corrected), 
                            breaks=c(10, 15, 20, 25, 30, 33, 37, 41, 44, 52), 
                            include.lowest = TRUE,  right = FALSE),
         age_baby_cat = cut(as.numeric(age_baby_cont2), 
                            breaks=c(10, 15, 20, 25, 30, 33, 37, 41, 44, 52), 
                            include.lowest = TRUE,  right = FALSE),
         GA_weeks_cat_sp = cut(as.numeric(GA_weeks), 
                                      breaks=c(22, 25, 30, 33, 37, 41, 52), 
                                      include.lowest = TRUE,  right = FALSE),
         age_baby_cat_sp = cut(as.numeric(age_baby_cont2), 
                            breaks=c(22, 25, 30, 33, 37, 41, 52), 
                            include.lowest = TRUE,  right = FALSE))
table(laus4$GA_weeks_cat, laus4$age_baby_cat, useNA = "a")

table(laus4$age_baby_cat, useNA = "a")
table(laus4$GA_weeks_cat_corrected, useNA = "a") 

a <- table(laus4$GA_weeks_cat, laus4$age_baby_cat, useNA = "a") 
openxlsx::write.xlsx(a, file=("outputs/GA_weeks.xlsx"), rowNames = FALSE)

d <- round(prop.table(table(laus4$GA_weeks_cat, laus4$age_baby_cat))*100,2)
d
openxlsx::write.xlsx(d, file=("outputs/GA_weeks_prop3.xlsx"), rowNames = FALSE)


e <- round(prop.table(table(laus4$GA_weeks_cat_corrected))*100,2)
e
openxlsx::write.xlsx(e, file=("outputs/GA_weeks_corrected.xlsx"), rowNames = FALSE)


# Cohen's Kappa: to measure concordance
table(laus4$GA_weeks_cat_sp, useNA = "a")
table(laus4$age_baby_cat_sp, useNA = "a")
table(laus4$GA_weeks_cat_sp, laus4$age_baby_cat_sp)
b <- table(laus4$GA_weeks_cat_sp, laus4$age_baby_cat_sp)
psych::cohen.kappa(b)
openxlsx::write.xlsx(table(laus4$GA_weeks_cat_sp, laus4$age_baby_cat_sp, useNA = "a"), 
                     file=("outputs/GA_weeks_sp.xlsx"), rowNames = FALSE)
