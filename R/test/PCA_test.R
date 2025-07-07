

# TQ1 is if lived with someone depressed or suicidal

# TF45 CLIMATE CHANGE MAKES YOU NERVOUS/DEPRESSED/STRESSED F-22

#K 6
# TG11 FEEL NERVOUS PAST 30 DAYS F-13
# TG12 FEEL HOPELESS PAST 30 DAYS F-14
# TG13 FELT RESTLESS PAST 30 DAYS F-14
# TG14 FEEL DEPRESSED PAST 30 DAYS F-15
# TG15 FEEL EVERYTHING AN EFFORT PAST 30 DAYS F-15
# TG16 FEEL WORTHLESS PAST 30 DAYS F-16

# DISTRESS should be K6 but looks wrong

# TF30 ANY MONTH PAST 12 MONTHS FELT WORSE F-19

# K6 for worst month
# TF31 FEEL NERVOUS WORST MONTH F-19
# TF32 FEEL HOPELESS WORST MONTH F-20
# TF33 FEEL RESTLESS OR FIDGETY WORST MONTH F-20
# TF34 FEEL DEPRESSED WORST MONTH F-21
# TF35 MONTH FEEL EVERYTHING IS AN EFFORT WORST MONTH F-21
# TF36 FEEL WORTHLESS WORST MONTH F-22

# DSTRSYR should be worst K6 but also looks wrong....

# suicide
# TK1 EVER THOUGHT TO COMMIT SUICIDE K-1
# TK2 EVER THOUGHT TO COMMIT SUICIDE PAST 12 M K-1
# TK3 THOUGHT TO COMMIT SUICIDE PAST 2 MOS K-2
# TK4 EVER ATTEMPTED SUICIDE K-2
# TK5 ATTEMPTED SUICIDE PAST 12 MOS K-3


# TODO 
# [ ] try K6
# [ ] try K6 worst
# [ ] try suicide
# [X] PCA (factor version) on all three together

#### PCA ####
pca_glm <- svyglm(formula = as.formula(paste0("s_depPCA ~", paste0("scale(K6_coding(",c(K6_vars,worst_K6_vars),"))", collapse = "+"),"+ tk1 + tk3 + tk5 + dstrsyr + dstrs12 + dstrs30 + distress" )), design = chis_design, family = "gaussian")

pca_glm %>% summary()

pca_outcome <- svyglm(formula = as.formula(paste0("s_depPCA ~ as.factor(year) + I(tf45=='Yes')+", paste0("as.factor(",intermed_demo,")", collapse = "+") ))
                      , design = chis_design, family = "gaussian")

pca_outcome %>% summary()

pca_outcome <- svyglm(formula = as.formula(s_depPCA ~  as.factor(year) + I(tf45 == "Yes") + as.factor(srage_p) +  I(racecn_p =="American Indian/Alaska Native") + I(srsex=="Female") + I(povll == "0-99% FPL") + I(!grepl("English", lnghmt_p1)))
                      , design = chis_design, family = "gaussian") 

pca_outcome %>% summary()

pca_outcome <- svyglm(formula = as.formula(s_depPCA ~  as.factor(year) + I(tf45 == "Yes") + as.factor(srage_p) +  I(racecn_p =="American Indian/Alaska Native") + I(srsex=="Female") + I(povll == "0-99% FPL") + I(!grepl("English", lnghmt_p1)) + tf11)
                      , design = chis_design, family = "gaussian") 

pca_outcome %>% summary()