
# R code to reproduce analysis presented in the paper:
# Does Interaction with People in Poverty Mitigate Classism Hostility or Reinforce Paternalism? The Role of Intergroup Contact in Ambivalent Classism Toward Women and Men in Poverty
# Written by: Mario Sainz 
# e-mail: msainz@psi.uned.es
# Please email us if you see any errors or have any questions
# Last update: 16/01/2025

#### 0. Packages ####
library("haven")
library("dplyr")
library("tidyr")
library("plyr")
library("psych")
library("Hmisc")
library("apaTables")
library("lavaan")
library("semTools")
library("here")
library(gtools)

#### 1. Upload and organize the dataset ####
#setwd("~/Documents/R/Contact and classisms") #To reproduce it change the directory!!

DB <- read_sav("input/data/original/SOGEDI_dataset_V1.sav")

##### 1.1. Selecting countries ####
DB <- DB %>% 
  filter(DB$natio_recoded %in% c("1", "3", "4", "9", "13")) 

##### 1.2. Adjunst labels ####
DB$natio_recoded <- factor(DB$natio_recoded, labels = c("Argentina", "Chile", "Colombia",
                                                        "Spain", "Mexico"))
DB$sex <- factor(DB$sex, labels = c("Other", "Female", "Male"))
DB$sex <- factor(DB$sex, levels = c("Male", "Female", "Other"))

##### 1.3. Rename variables ####
DB <- DB %>% 
  dplyr::rename(hc1_pw = hc_pw_1) %>%
  dplyr::rename(hc2_pw = hc_pw_2) %>%
  dplyr::rename(hc3_pw = hc_pw_3) %>%
  dplyr::rename(hc4_pw = hc_pw_4) %>%
  dplyr::rename(pp1_pw = pp_pw_1) %>%
  dplyr::rename(pp2_pw = pp_pw_2) %>%
  dplyr::rename(pp3_pw = pp_pw_3) %>%
  dplyr::rename(pp4_pw = pp_pw_4) %>%
  dplyr::rename(cc1_pw = cc_pw_1) %>%
  dplyr::rename(cc2_pw = cc_pw_2) %>%
  dplyr::rename(cc3_pw = cc_pw_3) %>%
  dplyr::rename(cc4_pw = cc_pw_4) %>%  
  dplyr::rename(qun_pw = quan_pw) %>%
  dplyr::rename(qul_pw = qual_pw) %>%
  dplyr::rename(dep_pw = depe_pw_1) %>%
  dplyr::rename(aut_pw = aut_pw_1) %>%  
  dplyr::rename(hc1_pm = hc_pm_1) %>%
  dplyr::rename(hc2_pm = hc_pm_2) %>%
  dplyr::rename(hc3_pm = hc_pm_3) %>%
  dplyr::rename(hc4_pm = hc_pm_4) %>%
  dplyr::rename(pp1_pm = pp_pm_1) %>%
  dplyr::rename(pp2_pm = pp_pm_2) %>%
  dplyr::rename(pp3_pm = pp_pm_3) %>%
  dplyr::rename(pp4_pm = pp_pm_4) %>%
  dplyr::rename(cc1_pm = cc_pm_1) %>%
  dplyr::rename(cc2_pm = cc_pm_2) %>%
  dplyr::rename(cc3_pm = cc_pm_3) %>%
  dplyr::rename(cc4_pm = cc_pm_4) %>%  
  dplyr::rename(qun_pm = quan_pm) %>%
  dplyr::rename(qul_pm = qual_pm) %>%
  dplyr::rename(dep_pm = depe_pm_1) %>%
  dplyr::rename(aut_pm = aut_pm_1)

##### 1.4. Select variables ####
DB <- DB %>%
  dplyr::select("ID",
                "hc1_pw", "hc2_pw", "hc3_pw", "hc4_pw",
                "pp1_pw", "pp2_pw", "pp3_pw", "pp4_pw",
                "cc1_pw", "cc2_pw", "cc3_pw", "cc4_pw",
                "qun_pw", "qul_pw", "dep_pw", "aut_pw",
                "hc1_pm", "hc2_pm", "hc3_pm", "hc4_pm",
                "pp1_pm", "pp2_pm", "pp3_pm", "pp4_pm",
                "cc1_pm", "cc2_pm", "cc3_pm", "cc4_pm",
                "qun_pm", "qul_pm", "dep_pm", "aut_pm", 
                 "age", "sex", "edu", "ses", "po", 
                  "natio_recoded") 

db_or <- DB

##### 1.5. Split variables by target ####
DB <- DB %>%
  select(ID, age, sex, edu, ses, po, 
         natio_recoded, starts_with(c("hc", "pp", "cc", "qun", "qul", "dep", "aut"))) %>% 
  tidyr::pivot_longer(cols = -c(ID, age, sex, edu, ses, po, 
                                natio_recoded),    
                      names_to = "variables",
                      values_to = "values")  %>% 
  tidyr::separate(variables, into = c("variables", "target"), sep = 3) %>% 
  pivot_wider(names_from = "variables",
              values_from = "values")
DB$target <- revalue(DB$target, c("_pw" = "PW", "_pm" = "PM"))
DB$target <- factor(DB$target, levels = c("PW", "PM"))

DB$sex <- factor(DB$sex)
DB$country <- factor(DB$natio_recoded)

# Crear dummies excluyendo la primera categoría de cada uno
sex_dummies <- model.matrix(~ sex, data = DB)[, -1]
country_dummies <- model.matrix(~ country, data = DB)[, -1]

# Añadir al dataframe
DB <- cbind(DB, sex_dummies, country_dummies)

sex_vars <- colnames(sex_dummies)
country_vars <- colnames(country_dummies)


#### 2. Reliability ####
##### 2.1. Hostile classism reliability ####
###### 2.1.1. Hostile classism reliability in Argentina ####
pp_rel <- psych::alpha(subset(DB, natio_recoded == "Argentina", select = c("hc1", "hc2", "hc3", "hc4")))
pp_rel$total[2]

###### 2.1.2. Hostile classism reliability in Chile ####
pp_rel <- psych::alpha(subset(DB, natio_recoded == "Chile", select = c("hc1", "hc2", "hc3", "hc4")))
pp_rel$total[2]

###### 2.1.3. Hostile classism reliability in Colombia ####
pp_rel <- psych::alpha(subset(DB, natio_recoded == "Colombia", select = c("hc1", "hc2", "hc3", "hc4")))
pp_rel$total[2]

###### 2.1.4. Hostile classism reliability in Spain ####
pp_rel <- psych::alpha(subset(DB, natio_recoded == "Spain", select = c("hc1", "hc2", "hc3", "hc4")))
pp_rel$total[2]

###### 2.1.5. Hostile classism reliability in Mexico ####
pp_rel <- psych::alpha(subset(DB, natio_recoded == "Mexico", select = c("hc1", "hc2", "hc3", "hc4")))
pp_rel$total[2]

##### 2.2. Protective paternalism reliability ####
###### 2.2.1. Protective paternalism reliability in Argentina ####
pp_rel <- psych::alpha(subset(DB, natio_recoded == "Argentina", select = c("pp1", "pp2", "pp3", "pp4")))
pp_rel$total[2]

###### 2.2.2. Protective paternalism reliability in Chile ####
pp_rel <- psych::alpha(subset(DB, natio_recoded == "Chile", select = c("pp1", "pp2", "pp3", "pp4")))
pp_rel$total[2]

###### 2.2.3. Protective paternalism reliability in Colombia ####
pp_rel <- psych::alpha(subset(DB, natio_recoded == "Colombia", select = c("pp1", "pp2", "pp3", "pp4")))
pp_rel$total[2]

###### 2.2.4. Protective paternalism reliability in Spain ####
pp_rel <- psych::alpha(subset(DB, natio_recoded == "Spain", select = c("pp1", "pp2", "pp3", "pp4")))
pp_rel$total[2]

###### 2.2.5. Protective paternalism reliability in Mexico ####
pp_rel <- psych::alpha(subset(DB, natio_recoded == "Mexico", select = c("pp1", "pp2", "pp3", "pp4")))
pp_rel$total[2]

##### 2.3. Complementary class differentiation reliability ####
###### 2.3.1. Complementary class differentiation reliability in Argentina ####
pp_rel <- psych::alpha(subset(DB, natio_recoded == "Argentina", select = c("cc1", "cc2", "cc3", "cc4")))
pp_rel$total[2]

###### 2.3.2. Complementary class differentiationm reliability in Chile ####
pp_rel <- psych::alpha(subset(DB, natio_recoded == "Chile", select = c("cc1", "cc2", "cc3", "cc4")))
pp_rel$total[2]

###### 2.3.3. Complementary class differentiation reliability in Colombia ####
pp_rel <- psych::alpha(subset(DB, natio_recoded == "Colombia", select = c("cc1", "cc2", "cc3", "cc4")))
pp_rel$total[2]

###### 2.3.4. Complementary class differentiation reliability in Spain ####
pp_rel <- psych::alpha(subset(DB, natio_recoded == "Spain", select = c("cc1", "cc2", "cc3", "cc4")))
pp_rel$total[2]

###### 2.3.5. Complementary class differentiation reliability in Mexico ####
pp_rel <- psych::alpha(subset(DB, natio_recoded == "Mexico", select = c("cc1", "cc2", "cc3", "cc4")))
pp_rel$total[2]

#### 3. Compute variables ####
DB <- DB %>%
  mutate(hc = (hc1+hc2+hc3+hc4)/4) %>%
  mutate(pp = (pp1+pp2+pp3+pp4)/4) %>%
  mutate(cc = (cc1+cc2+cc3+cc4)/4)

#### 4. Descriptive statistics ####
##### 4.1. Descriptive statistics of sex by country ####
###### 4.1.1. Descriptive statistics of sex in Argentina ####
table(subset(DB, natio_recoded == "Argentina", select = c("sex")))
prop.table(table(subset(DB, natio_recoded == "Argentina", select = c("sex"))))

###### 4.1.2. Descriptive statistics of sex in Chile ####
table(subset(DB, natio_recoded == "Chile", select = c("sex")))
prop.table(table(subset(DB, natio_recoded == "Chile", select = c("sex"))))

###### 4.1.3. Descriptive statistics of sex in Colombia ####
table(subset(DB, natio_recoded == "Colombia", select = c("sex")))
prop.table(table(subset(DB, natio_recoded == "Colombia", select = c("sex"))))

###### 4.1.4. Descriptive statistics of sex in Spain ####
table(subset(DB, natio_recoded == "Spain", select = c("sex")))
prop.table(table(subset(DB, natio_recoded == "Spain", select = c("sex"))))

###### 4.1.5. Descriptive statistics of sex in Mexico ####
table(subset(DB, natio_recoded == "Mexico", select = c("sex")))
prop.table(table(subset(DB, natio_recoded == "Mexico", select = c("sex"))))

###### 4.1.6. Descriptive statistics of sex in all countries ####
table(subset(DB, select = c("sex")))
prop.table(table(subset(DB, select = c("sex"))))

##### 4.2. Descriptive statistics cuantitative variables ####
###### 4.2.1. Descriptive statistics cuantitative variables in all countries ####
DB_des_all <- subset(DB, select = c("age" ,"qul", "qun", "hc", "pp", "cc", "aut", "dep", "target", "natio_recoded"))
describeBy(DB_des_all)

###### 4.2.2. Descriptive statistics cuantitative variables by country ####
DB_des_cou <- subset(DB, select = c("age" ,"qul", "qun", "hc", "pp", "cc", "aut", "dep", "target", "natio_recoded"))
describeBy(DB_des_cou, group = c("natio_recoded"))

###### 4.2.3. Descriptive statistics cuantitative variables by target ####
DB_des_tar <- subset(DB, select = c("age" ,"qul", "qun", "hc", "pp", "cc", "aut", "dep", "target", "natio_recoded"))
describeBy(DB_des_tar, group = c("target"))

###### 4.2.4. Descriptive statistics cuantitative variables by country and target ####
DB_des_cou_tar <- subset(DB, select = c("age" ,"qul", "qun", "hc", "pp", "cc", "aut", "dep", "target", "natio_recoded"))
describeBy(DB_des_cou_tar, group = c("target", "natio_recoded"))

#### 6. Correlations ####
##### 6.1. Correlations in all countries ####
###### 6.1.1. Correlations for target women in all countries ####
DB_cor_pw_all <- subset(DB, target == "PW", select = c("age", "qul", "qun", "hc", "pp", "cc", "aut", "dep"))
apa.cor.table(DB_cor_pw_all, show.conf.interval = FALSE,
              filename = "CC_Table 1.doc", table.number = 1)
DB_cor_pw_all %>%
  as.data.frame() %>%                
  as.matrix() %>%                   
  rcorr()

###### 6.1.2. Correlations for target men in all countries ####
DB_cor_pm_all <- subset(DB, target == "PM", select = c("age", "qul", "qun", "hc", "pp", "cc", "aut", "dep"))
apa.cor.table(DB_cor_pm_all, show.conf.interval = FALSE,
              filename = "CC_Table 2.doc", table.number = 2)
DB_cor_pm_all %>%
  as.data.frame() %>%                
  as.matrix() %>%                   
  rcorr()

##### 6.2. Correlations in Argentina ####
###### 6.2.1. Correlations for target women in Argentina ####
DB_cor_pw_arg <- subset(DB, natio_recoded == "Argentina" & target == "PW", select = c("age", "qul", "qun", "hc", "pp", "cc", "aut", "dep"))
apa.cor.table(DB_cor_pw_arg, show.conf.interval = FALSE,
              filename = "CC_Table 3.doc", table.number = 3)
DB_cor_pw_arg %>%
  as.data.frame() %>%                
  as.matrix() %>%                   
  rcorr()

###### 6.2.2. Correlations for target men in Argentina ####
DB_cor_pm_arg <- subset(DB, natio_recoded == "Argentina" & target == "PM", select = c("age", "qul", "qun", "hc", "pp", "cc", "aut", "dep"))
apa.cor.table(DB_cor_pm_arg, show.conf.interval = FALSE,
              filename = "CC_Table 4.doc", table.number = 4)
DB_cor_pm_arg %>%
  as.data.frame() %>%                
  as.matrix() %>%                   
  rcorr()

##### 6.3. Correlations in Chile ####
###### 6.3.1. Correlations for target women in Chile ####
DB_cor_pw_ch <- subset(DB, natio_recoded == "Chile" & target == "PW", select = c("age", "qul", "qun", "hc", "pp", "cc", "aut", "dep"))
apa.cor.table(DB_cor_pw_ch, show.conf.interval = FALSE,
              filename = "CC_Table 5.doc", table.number = 5)
DB_cor_pw_ch %>%
  as.data.frame() %>%                
  as.matrix() %>%                   
  rcorr()

###### 6.3.2. Correlations for target men in Chile ####
DB_cor_pm_ch <- subset(DB, natio_recoded == "Chile" & target == "PM", select = c("age", "qul", "qun", "hc", "pp", "cc", "aut", "dep"))
apa.cor.table(DB_cor_pm_ch, show.conf.interval = FALSE,
              filename = "CC_Table 6.doc", table.number = 6)
DB_cor_pm_ch %>%
  as.data.frame() %>%                
  as.matrix() %>%                   
  rcorr()

##### 6.4. Correlations in Colombia ####
###### 6.4.1. Correlations for target women in Colombia ####
DB_cor_pw_cl <- subset(DB, natio_recoded == "Colombia" & target == "PW", select = c("age", "qul", "qun", "hc", "pp", "cc", "aut", "dep"))
apa.cor.table(DB_cor_pw_cl, show.conf.interval = FALSE,
              filename = "CC_Table 7.doc", table.number = 7)
DB_cor_pw_cl %>%
  as.data.frame() %>%                
  as.matrix() %>%                   
  rcorr()

###### 6.4.2. Correlations for target men in Colombia ####
DB_cor_pm_cl <- subset(DB, natio_recoded == "Colombia" & target == "PM", select = c("age", "qul", "qun", "hc", "pp", "cc", "aut", "dep"))
apa.cor.table(DB_cor_pm_cl, show.conf.interval = FALSE,
              filename = "CC_Table 8.doc", table.number = 8)
DB_cor_pm_cl %>%
  as.data.frame() %>%                
  as.matrix() %>%                   
  rcorr()

##### 6.5. Correlations in Spain ####
###### 6.5.1. Correlations for target women in Spain ####
DB_cor_pw_sp <- subset(DB, natio_recoded == "Spain" & target == "PW", select = c("age", "qul", "qun", "hc", "pp", "cc", "aut", "dep"))
apa.cor.table(DB_cor_pw_sp, show.conf.interval = FALSE,
              filename = "CC_Table 9.doc", table.number = 9)
DB_cor_pw_sp %>%
  as.data.frame() %>%                
  as.matrix() %>%                   
  rcorr()

###### 6.5.2. Correlations for target men in Spain ####
DB_cor_pm_sp <- subset(DB, natio_recoded == "Spain" & target == "PM", select = c("age", "qul", "qun", "hc", "pp", "cc", "aut", "dep"))
apa.cor.table(DB_cor_pm_sp, show.conf.interval = FALSE,
              filename = "CC_Table 10.doc", table.number = 10)
DB_cor_pm_sp %>%
  as.data.frame() %>%                
  as.matrix() %>%                   
  rcorr()

##### 6.6. Correlations in Mexico ####
###### 6.6.1. Correlations for target women in Mexico ####
DB_cor_pw_mx <- subset(DB, natio_recoded == "Mexico" & target == "PW", select = c("age", "qul", "qun", "hc", "pp", "cc", "aut", "dep"))
apa.cor.table(DB_cor_pw_mx, show.conf.interval = FALSE,
              filename = "CC_Table 11.doc", table.number = 11)
DB_cor_pw_mx %>%
  as.data.frame() %>%                
  as.matrix() %>%                   
  rcorr()

###### 6.6.2. Correlations for target men in Mexico ####
DB_cor_pm_mx <- subset(DB, natio_recoded == "Mexico" & target == "PM", select = c("age", "qul", "qun", "hc", "pp", "cc", "aut", "dep"))
apa.cor.table(DB_cor_pm_mx, show.conf.interval = FALSE,
              filename = "CC_Table 12.doc", table.number = 12)
DB_cor_pm_mx %>%
  as.data.frame() %>%                
  as.matrix() %>%                   
  rcorr()

#### 7. Multigroup SEM ####
Model <- ('
#Latent variables
hc =~ hc1 + hc2 + hc3 + hc4
pp =~ pp1 + pp2 + pp3 + pp4
cc =~ cc1 + cc2 + cc3 + cc4

#Regressions
hc ~ a11*qul + a21*qun 
pp ~ a12*qul + a22*qun 
cc ~ a13*qul + a23*qun 
aut ~ b11*hc + b21*pp + b31*cc 
dep ~ b12*hc + b22*pp + b32*cc

#Covariance
qul ~~ qun
hc ~~ pp + cc
pp ~~ cc
aut ~~ dep

#Indirect effects
IEqul_hc_aut := a11*b11
IEqul_hc_dep := a11*b12
IEqun_hc_aut := a21*b11
IEqun_hc_dep := a21*b12
IEqul_pp_aut := a12*b21
IEqul_pp_dep := a12*b22
IEqun_pp_aut := a22*b21
IEqun_pp_dep := a22*b22
IEqul_cc_aut := a13*b31
IEqul_cc_dep := a13*b32
IEqun_cc_aut := a23*b31
IEqun_cc_dep := a23*b32
')

controls <- c(sex_vars, "ses", country_vars)

control_formula <- paste(controls, collapse = " + ")

control_block <- paste0(
  "hc ~ ", control_formula, "\n",
  "pp ~ ", control_formula, "\n",
  "cc ~ ", control_formula, "\n",
  "aut ~ ", control_formula, "\n",
  "dep ~ ", control_formula, "\n",
  "qul ~ ", control_formula, "\n",
  "qun ~ ", control_formula, "\n"
)

model <- paste(Model, control_block)

##### 7.1. SEM for all countries by target ####
###### 7.1.1. Multigroup SEM for all countries with women as target ####
####### 7.1.1.1. Select variables ####

DB_sem_pw_all <- DB %>% 
  select(-c("hc", "pp", "cc")) %>% 
  dplyr::filter(target == "PW")
  
DB_sem_pw_all$ses <- as.numeric(DB_sem_pw_all$ses)
DB_sem_pw_all$sex <- as.numeric(DB_sem_pw_all$sex)

####### 7.1.1.2. Multivariate normality ####
mardia(subset(DB_sem_pw_all, select = c(hc1, hc2, hc3, hc4, 
                                        pp1, pp2, pp3, pp4, 
                                        cc1, cc2, cc3, cc4,
                                        qul, qun, aut, dep, ses, sex)), na.rm = TRUE, plot=TRUE)

####### 7.1.1.3. Model fit ####
fit_pw_all <- sem(model, data = DB_sem_pw_all, estimator = "MLR")
summary(fit_pw_all, fit.measures = T, ci = T, standardized = T, rsquare = T)
fitmeasures(fit_pw_all, c("chisq", "pvalue", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

####### 7.1.1.4. Monte Carlo confidence intervals ####
monteCarloCI(fit_pw_all, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)

###### 7.1.2. Multigroup SEM for all countries with men as target ####
####### 7.1.2.1. Select variables ####
DB_sem_pm_all <- DB %>% 
  select(-c("hc", "pp", "cc")) %>% 
  dplyr::filter(target == "PM")
  
DB_sem_pm_all$ses <- as.numeric(DB_sem_pm_all$ses)
DB_sem_pm_all$sex <- as.numeric(DB_sem_pm_all$sex)

####### 7.1.2.2. Multivariate normality ####
mardia(subset(DB_sem_pm_all, select = c(hc1, hc2, hc3, hc4, 
                                        pp1, pp2, pp3, pp4, 
                                        cc1, cc2, cc3, cc4,
                                        qul, qun, aut, dep, ses, sex)), na.rm = TRUE, plot=TRUE)

####### 7.1.2.3. Model fit ####
fit_pm_all <- sem(model, data = DB_sem_pm_all, estimator = "MLR")
summary(fit_pm_all, fit.measures = T, ci = T, standardized = T, rsquare = T)
fitmeasures(fit_pm_all, c("chisq", "pvalue", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

####### 7.1.2.4. Monte Carlo confidence intervals ####
monteCarloCI(fit_pm_all, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)

##### 7.2. Multigroup SEM for all countries with target clustered ####
Model2 <- ('
#Latent variables
hc =~ hc1 + hc2 + hc3 + hc4
pp =~ pp1 + pp2 + pp3 + pp4
cc =~ cc1 + cc2 + cc3 + cc4

#Regressions
hc ~ c(a11,x11)*qul + c(a21,x21)*qun 
pp ~ c(a12,x12)*qul + c(a22,x22)*qun 
cc ~ c(a13,x13)*qul + c(a23,x23)*qun 
aut ~ c(b11,y11)*hc + c(b21,y21)*pp + c(b31,y31)*cc 
dep ~ c(b12,y12)*hc + c(b22,y22)*pp + c(b32,y32)*cc

#Covariance
qul ~~ qun
hc ~~ pp + cc
pp ~~ cc
aut ~~ dep

#Indirect effects target women 
T1IEqul_hc_aut := a11*b11
T1IEqul_hc_dep := a11*b12
T1IEqun_hc_aut := a21*b11
T1IEqun_hc_dep := a21*b12
T1IEqul_pp_aut := a12*b21
T1IEqul_pp_dep := a12*b22
T1IEqun_pp_aut := a22*b21
T1IEqun_pp_dep := a22*b22
T1IEqul_cc_aut := a13*b31
T1IEqul_cc_dep := a13*b32
T1IEqun_cc_aut := a23*b31
T1IEqun_cc_dep := a23*b32

#Indirect effects target men 
T2IEqul_hc_aut := x11*y11
T2IEqul_hc_dep := x11*y12
T2IEqun_hc_aut := x21*y11
T2IEqun_hc_dep := x21*y12
T2IEqul_pp_aut := x12*y21
T2IEqul_pp_dep := x12*y22
T2IEqun_pp_aut := x22*y21
T2IEqun_pp_dep := x22*y22
T2IEqul_cc_aut := x13*y31
T2IEqul_cc_dep := x13*y32
T2IEqun_cc_aut := x23*y31
T2IEqun_cc_dep := x23*y32
')

controls <- c(sex_vars, "ses", country_vars)

control_formula <- paste(controls, collapse = " + ")

control_block <- paste0(
  "hc ~ ", control_formula, "\n",
  "pp ~ ", control_formula, "\n",
  "cc ~ ", control_formula, "\n",
  "aut ~ ", control_formula, "\n",
  "dep ~ ", control_formula, "\n",
  "qul ~ ", control_formula, "\n",
  "qun ~ ", control_formula, "\n"
)

model <- paste(Model2, control_block)

###### 7.2.1. Select variables ####
DB_sem_tar <- DB %>% 
  select(-c(hc, pp, cc))

DB_sem_tar$ses <- as.numeric(DB_sem_tar$ses)
DB_sem_tar$sex <- as.numeric(DB_sem_tar$sex)

levels(DB_sem_tar$target)

###### 7.2.2. Multivariate normality ####
mardia(subset(DB_sem_tar, select = c(hc1, hc2, hc3, hc4, 
                                     pp1, pp2, pp3, pp4, 
                                     cc1, cc2, cc3, cc4,
                                     qul, qun, aut, dep, ses, sex)), na.rm = TRUE, plot=TRUE)

###### 7.2.3. Model fit ####
fit_tar <- sem(model, data = DB_sem_tar, group = "target", estimator = "MLR")
summary(fit_tar, fit.measures = T, ci = T, standardized = T, rsquare = T)
fitmeasures(fit_tar, c("chisq", "pvalue", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

###### 7.2.4. Monte Carlo confidence intervals ####
monteCarloCI(fit_tar, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)

##### 7.3. Multigroup SEM with countries and target clustered ####

# Results are too large so we printed
sink("Model3.txt")

Model3 <- '
#Latent variables
hc =~ hc1 + hc2 + hc3 + hc4
pp =~ pp1 + pp2 + pp3 + pp4
cc =~ cc1 + cc2 + cc3 + cc4

#Regressions
hc ~ c(MArg_a11,MCh_a11,MCo_a11,MSp_a11,MMx_a11,WArg_a11,WCh_a11,WCo_a11,WSp_a11,WMx_a11)*qul + c(MArg_a21,MCh_a21,MCo_a21,MSp_a21,MMx_a21,WArg_a21,WCh_a21,WCo_a21,WSp_a21,WMx_a21)*qun 
pp ~ c(MArg_a12,MCh_a12,MCo_a12,MSp_a12,MMx_a12,WArg_a12,WCh_a12,WCo_a12,WSp_a12,WMx_a12)*qul + c(MArg_a22,MCh_a22,MCo_a22,MSp_a22,MMx_a22,WArg_a22,WCh_a22,WCo_a22,WSp_a22,WMx_a22)*qun 
cc ~ c(MArg_a13,MCh_a13,MCo_a13,MSp_a13,MMx_a13,WArg_a13,WCh_a13,WCo_a13,WSp_a13,WMx_a13)*qul + c(MArg_a23,MCh_a23,MCo_a23,MSp_a23,MMx_a23,WArg_a23,WCh_a23,WCo_a23,WSp_a23,WMx_a23)*qun 
aut ~ c(MArg_b11,MCh_b11,MCo_b11,MSp_b11,MMx_b11,WArg_b11,WCh_b11,WCo_b11,WSp_b11,WMx_b11)*hc + c(MArg_b21,MCh_b21,MCo_b21,MSp_b21,MMx_b21,WArg_b21,WCh_b21,WCo_b21,WSp_b21,WMx_b21)*pp + c(MArg_b31,MCh_b31,MCo_b31,MSp_b31,MMx_b31,WArg_b31,WCh_b31,WCo_b31,WSp_b31,WMx_b31)*cc 
dep ~ c(MArg_b12,MCh_b12,MCo_b12,MSp_b12,MMx_b12,WArg_b12,WCh_b12,WCo_b12,WSp_b12,WMx_b12)*hc + c(MArg_b22,MCh_b22,MCo_b22,MSp_b22,MMx_b22,WArg_b22,WCh_b22,WCo_b22,WSp_b22,WMx_b22)*pp + c(MArg_b32,MCh_b32,MCo_b32,MSp_b32,MMx_b32,WArg_b32,WCh_b32,WCo_b32,WSp_b32,WMx_b32)*cc

#Covariance
qul ~~ qun
hc ~~ pp + cc
pp ~~ cc
aut ~~ dep

#Control
hc ~ sex + ses
pp ~ sex + ses
cc ~ sex + ses
aut ~ sex + ses
dep ~ sex + ses
qul ~ sex + ses
qun ~ sex + ses

#Indirect effects target men in Argentina
MArg_IEqul_hc_aut := MArg_a11*MArg_b11
MArg_IEqul_hc_dep := MArg_a11*MArg_b12
MArg_IEqun_hc_aut := MArg_a21*MArg_b11
MArg_IEqun_hc_dep := MArg_a21*MArg_b12
MArg_IEqul_pp_aut := MArg_a12*MArg_b21
MArg_IEqul_pp_dep := MArg_a12*MArg_b22
MArg_IEqun_pp_aut := MArg_a22*MArg_b21
MArg_IEqun_pp_dep := MArg_a22*MArg_b22
MArg_IEqul_cc_aut := MArg_a13*MArg_b31
MArg_IEqul_cc_dep := MArg_a13*MArg_b32
MArg_IEqun_cc_aut := MArg_a23*MArg_b31
MArg_IEqun_cc_dep := MArg_a23*MArg_b32

#Indirect effects target men in Chile
MCh_IEqul_hc_aut := MCh_a11*MCh_b11
MCh_IEqul_hc_dep := MCh_a11*MCh_b12
MCh_IEqun_hc_aut := MCh_a21*MCh_b11
MCh_IEqun_hc_dep := MCh_a21*MCh_b12
MCh_IEqul_pp_aut := MCh_a12*MCh_b21
MCh_IEqul_pp_dep := MCh_a12*MCh_b22
MCh_IEqun_pp_aut := MCh_a22*MCh_b21
MCh_IEqun_pp_dep := MCh_a22*MCh_b22
MCh_IEqul_cc_aut := MCh_a13*MCh_b31
MCh_IEqul_cc_dep := MCh_a13*MCh_b32
MCh_IEqun_cc_aut := MCh_a23*MCh_b31
MCh_IEqun_cc_dep := MCh_a23*MCh_b32

#Indirect effects target men in Colombia
MCo_IEqul_hc_aut := MCo_a11*MCo_b11
MCo_IEqul_hc_dep := MCo_a11*MCo_b12
MCo_IEqun_hc_aut := MCo_a21*MCo_b11
MCo_IEqun_hc_dep := MCo_a21*MCo_b12
MCo_IEqul_pp_aut := MCo_a12*MCo_b21
MCo_IEqul_pp_dep := MCo_a12*MCo_b22
MCo_IEqun_pp_aut := MCo_a22*MCo_b21
MCo_IEqun_pp_dep := MCo_a22*MCo_b22
MCo_IEqul_cc_aut := MCo_a13*MCo_b31
MCo_IEqul_cc_dep := MCo_a13*MCo_b32
MCo_IEqun_cc_aut := MCo_a23*MCo_b31
MCo_IEqun_cc_dep := MCo_a23*MCo_b32

#Indirect effects target men in Spain
MSp_IEqul_hc_aut := MSp_a11*MSp_b11
MSp_IEqul_hc_dep := MSp_a11*MSp_b12
MSp_IEqun_hc_aut := MSp_a21*MSp_b11
MSp_IEqun_hc_dep := MSp_a21*MSp_b12
MSp_IEqul_pp_aut := MSp_a12*MSp_b21
MSp_IEqul_pp_dep := MSp_a12*MSp_b22
MSp_IEqun_pp_aut := MSp_a22*MSp_b21
MSp_IEqun_pp_dep := MSp_a22*MSp_b22
MSp_IEqul_cc_aut := MSp_a13*MSp_b31
MSp_IEqul_cc_dep := MSp_a13*MSp_b32
MSp_IEqun_cc_aut := MSp_a23*MSp_b31
MSp_IEqun_cc_dep := MSp_a23*MSp_b32

#Indirect effects target men in Mexico
MMx_IEqul_hc_aut := MMx_a11*MMx_b11
MMx_IEqul_hc_dep := MMx_a11*MMx_b12
MMx_IEqun_hc_aut := MMx_a21*MMx_b11
MMx_IEqun_hc_dep := MMx_a21*MMx_b12
MMx_IEqul_pp_aut := MMx_a12*MMx_b21
MMx_IEqul_pp_dep := MMx_a12*MMx_b22
MMx_IEqun_pp_aut := MMx_a22*MMx_b21
MMx_IEqun_pp_dep := MMx_a22*MMx_b22
MMx_IEqul_cc_aut := MMx_a13*MMx_b31
MMx_IEqul_cc_dep := MMx_a13*MMx_b32
MMx_IEqun_cc_aut := MMx_a23*MMx_b31
MMx_IEqun_cc_dep := MMx_a23*MMx_b32

#Indirect effects target women in Argentina
WArg_IEqul_hc_aut := WArg_a11*WArg_b11
WArg_IEqul_hc_dep := WArg_a11*WArg_b12
WArg_IEqun_hc_aut := WArg_a21*WArg_b11
WArg_IEqun_hc_dep := WArg_a21*WArg_b12
WArg_IEqul_pp_aut := WArg_a12*WArg_b21
WArg_IEqul_pp_dep := WArg_a12*WArg_b22
WArg_IEqun_pp_aut := WArg_a22*WArg_b21
WArg_IEqun_pp_dep := WArg_a22*WArg_b22
WArg_IEqul_cc_aut := WArg_a13*WArg_b31
WArg_IEqul_cc_dep := WArg_a13*WArg_b32
WArg_IEqun_cc_aut := WArg_a23*WArg_b31
WArg_IEqun_cc_dep := WArg_a23*WArg_b32

#Indirect effects target women in Chile
WCh_IEqul_hc_aut := WCh_a11*WCh_b11
WCh_IEqul_hc_dep := WCh_a11*WCh_b12
WCh_IEqun_hc_aut := WCh_a21*WCh_b11
WCh_IEqun_hc_dep := WCh_a21*WCh_b12
WCh_IEqul_pp_aut := WCh_a12*WCh_b21
WCh_IEqul_pp_dep := WCh_a12*WCh_b22
WCh_IEqun_pp_aut := WCh_a22*WCh_b21
WCh_IEqun_pp_dep := WCh_a22*WCh_b22
WCh_IEqul_cc_aut := WCh_a13*WCh_b31
WCh_IEqul_cc_dep := WCh_a13*WCh_b32
WCh_IEqun_cc_aut := WCh_a23*WCh_b31
WCh_IEqun_cc_dep := WCh_a23*WCh_b32

#Indirect effects target women in Colombia
WCo_IEqul_hc_aut := WCo_a11*WCo_b11
WCo_IEqul_hc_dep := WCo_a11*WCo_b12
WCo_IEqun_hc_aut := WCo_a21*WCo_b11
WCo_IEqun_hc_dep := WCo_a21*WCo_b12
WCo_IEqul_pp_aut := WCo_a12*WCo_b21
WCo_IEqul_pp_dep := WCo_a12*WCo_b22
WCo_IEqun_pp_aut := WCo_a22*WCo_b21
WCo_IEqun_pp_dep := WCo_a22*WCo_b22
WCo_IEqul_cc_aut := WCo_a13*WCo_b31
WCo_IEqul_cc_dep := WCo_a13*WCo_b32
WCo_IEqun_cc_aut := WCo_a23*WCo_b31
WCo_IEqun_cc_dep := WCo_a23*WCo_b32

#Indirect effects target women in Spain
WSp_IEqul_hc_aut := WSp_a11*WSp_b11
WSp_IEqul_hc_dep := WSp_a11*WSp_b12
WSp_IEqun_hc_aut := WSp_a21*WSp_b11
WSp_IEqun_hc_dep := WSp_a21*WSp_b12
WSp_IEqul_pp_aut := WSp_a12*WSp_b21
WSp_IEqul_pp_dep := WSp_a12*WSp_b22
WSp_IEqun_pp_aut := WSp_a22*WSp_b21
WSp_IEqun_pp_dep := WSp_a22*WSp_b22
WSp_IEqul_cc_aut := WSp_a13*WSp_b31
WSp_IEqul_cc_dep := WSp_a13*WSp_b32
WSp_IEqun_cc_aut := WSp_a23*WSp_b31
WSp_IEqun_cc_dep := WSp_a23*WSp_b32

#Indirect effects target women in Mexico
WMx_IEqul_hc_aut := WMx_a11*WMx_b11
WMx_IEqul_hc_dep := WMx_a11*WMx_b12
WMx_IEqun_hc_aut := WMx_a21*WMx_b11
WMx_IEqun_hc_dep := WMx_a21*WMx_b12
WMx_IEqul_pp_aut := WMx_a12*WMx_b21
WMx_IEqul_pp_dep := WMx_a12*WMx_b22
WMx_IEqun_pp_aut := WMx_a22*WMx_b21
WMx_IEqun_pp_dep := WMx_a22*WMx_b22
WMx_IEqul_cc_aut := WMx_a13*WMx_b31
WMx_IEqul_cc_dep := WMx_a13*WMx_b32
WMx_IEqun_cc_aut := WMx_a23*WMx_b31
WMx_IEqun_cc_dep := WMx_a23*WMx_b32'

###### 7.3.1. Select variables ####
DB_sem_all <- subset(DB, select = c("hc1", "hc2", "hc3", "hc4", 
                                    "pp1", "pp2", "pp3", "pp4", 
                                    "cc1", "cc2", "cc3", "cc4",
                                    "qul", "qun", "aut", "dep",
                                    "target", "natio_recoded", "ses", "sex"))
DB_sem_all$ses <- as.numeric(DB_sem_all$ses)
DB_sem_all$sex <- as.numeric(DB_sem_all$sex)

###### 7.3.2. Multivariate normality ####
mardia(subset(DB_sem_all, select = c(hc1, hc2, hc3, hc4, 
                                     pp1, pp2, pp3, pp4, 
                                     cc1, cc2, cc3, cc4,
                                     qul, qun, aut, dep, ses, sex)), na.rm = TRUE, plot=TRUE)

###### 7.3.3. Compute group interaction between target and nation ####
DB_sem_all$group <- interaction(DB_sem_all$natio_recoded, DB_sem_all$target)

###### 7.3.4. Model fit ####
fit_all <- sem(Model3, data = DB_sem_all, group = "group", estimator = "MLR")
summary(fit_all, fit.measures = T, ci = T, standardized = T, rsquare = T)
fitmeasures(fit_all, c("chisq", "pvalue", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

###### 7.3.5. Monte Carlo confidence intervals ####
monteCarloCI(fit_all, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)

sink()


## 8. Invariance: between targets ----------------------------------------

DB_inv <- DB %>% 
  select(-c(hc, cc, pp, natio_recoded, country))

Model <- ('
#Latent variables
hc =~ hc1 + hc2 + hc3 + hc4
pp =~ pp1 + pp2 + pp3 + pp4
cc =~ cc1 + cc2 + cc3 + cc4

#Regressions
hc ~ a11*qul + a21*qun 
pp ~ a12*qul + a22*qun 
cc ~ a13*qul + a23*qun 
aut ~ b11*hc + b21*pp + b31*cc 
dep ~ b12*hc + b22*pp + b32*cc

#Covariance
qul ~~ qun
hc ~~ pp + cc
pp ~~ cc
aut ~~ dep

#Indirect effects
IEqul_hc_aut := a11*b11
IEqul_hc_dep := a11*b12
IEqun_hc_aut := a21*b11
IEqun_hc_dep := a21*b12
IEqul_pp_aut := a12*b21
IEqul_pp_dep := a12*b22
IEqun_pp_aut := a22*b21
IEqun_pp_dep := a22*b22
IEqul_cc_aut := a13*b31
IEqul_cc_dep := a13*b32
IEqun_cc_aut := a23*b31
IEqun_cc_dep := a23*b32
')

controls <- c(sex_vars, "ses", country_vars)

control_formula <- paste(controls, collapse = " + ")

control_block <- paste0(
  "hc ~ ", control_formula, "\n",
  "pp ~ ", control_formula, "\n",
  "cc ~ ", control_formula, "\n",
  "aut ~ ", control_formula, "\n",
  "dep ~ ", control_formula, "\n",
  "qul ~ ", control_formula, "\n",
  "qun ~ ", control_formula, "\n"
)

model_inv <- paste(Model, control_block)

###### 8.1 Configural model ######

configural.model <- cfa(model_inv, 
                 data = DB_inv, 
                 estimator = "MLR", 
                 group = "target")

summary(configural.model, fit.measures = TRUE, standardized = TRUE)

###### 8.2 Metric model ######
metric.model <- cfa(model_inv, 
                    data = DB_inv, 
                    estimator = "MLR", 
                    group = "target",
                    group.equal= "loadings")

summary(metric.model, fit.measures = TRUE, standardized = TRUE)

###### 8.3  Scalar model ######
scalar.model <- cfa(model_inv, 
                    data = DB_inv, 
                    estimator = "MLR", 
                    group = "target",
                    group.equal= c("loadings", "intercepts"))

summary(scalar.model, fit.measures = TRUE, standardized = TRUE)

###### 8.4 Strict model ######
strict.model <- cfa(model_inv, 
                    data = DB_inv, 
                    estimator = "MLR", 
                    group = "target",
                    group.equal= c("loadings", "intercepts", "residuals"))

summary(strict.model, fit.measures = TRUE, standardized = TRUE)

###### 8.5 Table fit comparitions ######

an1 <- anova(configural.model, metric.model)
an2 <- anova(metric.model, scalar.model)
an3 <- anova(scalar.model, strict.model)

tab01 <- bind_rows(
  as_tibble(an1)[2,],
  as_tibble(an2)[2,],
  as_tibble(an3)[2,] 
) %>%
  select("Chisq", "Df", chisq.diff = `Chisq diff`, df.diff = `Df diff`, pvalue = `Pr(>Chisq)`) %>%
  mutate(
    stars = stars.pval(pvalue),
    chisqt = paste0(round(Chisq, 2), " (", Df, ")"),
    decision = ifelse(pvalue > 0.05, "Accept", "Reject"),
    model = c("Metric", "Scalar", "Strict") 
  ) %>%
  bind_rows(
    tibble(
      Chisq = an1$Chisq[1],
      Df = an1$Df[1],
      chisq.diff = NA,
      df.diff = NA,
      pvalue = NA,
      stars = "",
      chisqt = paste0(round(an1$Chisq[1], 2), " (", an1$Df[1], ")"),
      decision = "Reference",
      model = "Configural" 
    )
  ) %>%
  select(model, chisqt, chisq.diff, df.diff, pvalue, stars, decision) %>%
  mutate(model = factor(model, levels = c("Configural", "Metric", "Scalar", "Strict"))) %>%
  arrange(model)


fit.meas <- bind_rows(
  fitMeasures(configural.model, output = "matrix")[c("chisq", "df", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"),],
  fitMeasures(metric.model, output = "matrix")[c("chisq", "df", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"),],
  fitMeasures(scalar.model, output = "matrix")[c("chisq", "df", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"),],
  fitMeasures(strict.model, output = "matrix")[c("chisq", "df", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"),]
  
)

fit.meas <- fit.meas %>% 
  mutate(               
    diff.chi2 = chisq - lag(chisq, default = first(chisq)),
    diff.df = df - lag(df, default = first(df)),
    diff.cfi = cfi - lag(cfi, default = first(cfi)),
    diff.rmsea = rmsea - lag(rmsea, default = first(rmsea))
  ) %>%
  round(3) %>%
  mutate(rmsea.ci = paste0(rmsea, " \n(", rmsea.ci.lower, "-", rmsea.ci.upper, ")"))

# Tabla final (mantener diff.df hasta formatear)

tab.inv <- bind_cols(tab01, fit.meas) %>%
  select(model, chisqt, cfi, rmsea.ci, diff.chi2, diff.df, diff.cfi, diff.rmsea, stars, decision) %>%
  mutate(diff.chi2 = paste0(diff.chi2, " (", diff.df, ") ", stars)) %>%
  select(model, chisqt, cfi, rmsea.ci, diff.chi2, diff.cfi, diff.rmsea, decision)

col.nam <- c(
  "Model", "&chi;^2 (df)", "CFI", "RMSEA (90 CI)",
  "&Delta; &chi;^2 (&Delta; df)", "&Delta; CFI", "&Delta; RMSEA", "Decision"
)

tab.inv %>%
  kableExtra::kable(
    format = "html",
    align = "c",
    booktabs = TRUE,
    escape = FALSE,
    col.names = col.nam
  ) %>%
  kableExtra::kable_styling(
    full_width = TRUE,
    latex_options = "hold_position",
    bootstrap_options = c("striped", "bordered", "condensed"),
    font_size = 23
  ) %>%
  kableExtra::column_spec(c(1, 8), width = "3.5cm") %>%
  kableExtra::column_spec(2:7, width = "4cm") %>%
  kableExtra::column_spec(4, width = "5cm")


# For testing loading invariance, a change of >= 0.010 in CFI, 
# supplemented by a change of >= 0.015 in RMSEA 
# would indicate noninvariance; 
# for testing intercept or residual invariance, a change of >= 0.010 in CFI, 
# supplemented by a change of >= 0.015 in RMSEA or a change 
# would indicate noninvariance.

# Model is scalar invariance

lavTestScore(strict.model)$uni %>% 
  as.data.frame() %>% 
  filter(p.value < 0.05)

pt <- lavaan::parTable(strict.model)

pt[pt$plabel %in% c(".p2.", ".p153.", ".p6.", ".p157."),
   c("id","group","lhs","op","rhs","label","plabel","free","ustart","est")]


# 9. Paths comparative ----------------------------------------------------

##### 9.1 Free scalar paths model ####
# Complete multigroup SEM (scalar invariance) with:
# - freely estimated structural paths across 2 groups
# - indirect effects by group
# - between-group differences for ALL direct paths and ALL indirect effects
# - estimation under MLR
# - extraction of (i) direct-path diffs and (ii) indirect diffs with SE/CI/p

model_free <- ('
# Latent variables
hc =~ hc1 + hc2 + hc3 + hc4
pp =~ pp1 + pp2 + pp3 + pp4
cc =~ cc1 + cc2 + cc3 + cc4

# Regressions (FREE across 2 groups)
hc  ~ c(a11_g1,a11_g2)*qul + c(a21_g1,a21_g2)*qun
pp  ~ c(a12_g1,a12_g2)*qul + c(a22_g1,a22_g2)*qun
cc  ~ c(a13_g1,a13_g2)*qul + c(a23_g1,a23_g2)*qun

aut ~ c(b11_g1,b11_g2)*hc  + c(b21_g1,b21_g2)*pp  + c(b31_g1,b31_g2)*cc
dep ~ c(b12_g1,b12_g2)*hc  + c(b22_g1,b22_g2)*pp  + c(b32_g1,b32_g2)*cc

# Covariances
qul ~~ qun
hc  ~~ pp + cc
pp  ~~ cc
aut ~~ dep

# ----------------------------
# Indirect effects (BY GROUP)
# qul -> (hc,pp,cc) -> aut/dep
# ----------------------------
IEqul_hc_aut_g1 := a11_g1*b11_g1
IEqul_hc_aut_g2 := a11_g2*b11_g2
IEqul_hc_dep_g1 := a11_g1*b12_g1
IEqul_hc_dep_g2 := a11_g2*b12_g2

IEqul_pp_aut_g1 := a12_g1*b21_g1
IEqul_pp_aut_g2 := a12_g2*b21_g2
IEqul_pp_dep_g1 := a12_g1*b22_g1
IEqul_pp_dep_g2 := a12_g2*b22_g2

IEqul_cc_aut_g1 := a13_g1*b31_g1
IEqul_cc_aut_g2 := a13_g2*b31_g2
IEqul_cc_dep_g1 := a13_g1*b32_g1
IEqul_cc_dep_g2 := a13_g2*b32_g2

# qun -> (hc,pp,cc) -> aut/dep
IEqun_hc_aut_g1 := a21_g1*b11_g1
IEqun_hc_aut_g2 := a21_g2*b11_g2
IEqun_hc_dep_g1 := a21_g1*b12_g1
IEqun_hc_dep_g2 := a21_g2*b12_g2

IEqun_pp_aut_g1 := a22_g1*b21_g1
IEqun_pp_aut_g2 := a22_g2*b21_g2
IEqun_pp_dep_g1 := a22_g1*b22_g1
IEqun_pp_dep_g2 := a22_g2*b22_g2

IEqun_cc_aut_g1 := a23_g1*b31_g1
IEqun_cc_aut_g2 := a23_g2*b31_g2
IEqun_cc_dep_g1 := a23_g1*b32_g1
IEqun_cc_dep_g2 := a23_g2*b32_g2

# --------------------------------
# Between-group differences: DIRECT paths (g1 - g2)
# --------------------------------
d_a11 := a11_g1 - a11_g2
d_a21 := a21_g1 - a21_g2
d_a12 := a12_g1 - a12_g2
d_a22 := a22_g1 - a22_g2
d_a13 := a13_g1 - a13_g2
d_a23 := a23_g1 - a23_g2

d_b11 := b11_g1 - b11_g2
d_b21 := b21_g1 - b21_g2
d_b31 := b31_g1 - b31_g2
d_b12 := b12_g1 - b12_g2
d_b22 := b22_g1 - b22_g2
d_b32 := b32_g1 - b32_g2

# --------------------------------
# Between-group differences: INDIRECT effects (g1 - g2)
# --------------------------------
d_IEqul_hc_aut := IEqul_hc_aut_g1 - IEqul_hc_aut_g2
d_IEqul_hc_dep := IEqul_hc_dep_g1 - IEqul_hc_dep_g2
d_IEqul_pp_aut := IEqul_pp_aut_g1 - IEqul_pp_aut_g2
d_IEqul_pp_dep := IEqul_pp_dep_g1 - IEqul_pp_dep_g2
d_IEqul_cc_aut := IEqul_cc_aut_g1 - IEqul_cc_aut_g2
d_IEqul_cc_dep := IEqul_cc_dep_g1 - IEqul_cc_dep_g2

d_IEqun_hc_aut := IEqun_hc_aut_g1 - IEqun_hc_aut_g2
d_IEqun_hc_dep := IEqun_hc_dep_g1 - IEqun_hc_dep_g2
d_IEqun_pp_aut := IEqun_pp_aut_g1 - IEqun_pp_aut_g2
d_IEqun_pp_dep := IEqun_pp_dep_g1 - IEqun_pp_dep_g2
d_IEqun_cc_aut := IEqun_cc_aut_g1 - IEqun_cc_aut_g2
d_IEqun_cc_dep := IEqun_cc_dep_g1 - IEqun_cc_dep_g2
')

model_free_full <- paste(model_free, control_block)

fit_free <- sem(
  model_free_full,
  data = DB_inv,
  group = "target",
  estimator = "MLR",
  group.equal = c("loadings", "intercepts"),
  meanstructure = TRUE
)

fitmeasures(fit_free, c("chisq", "pvalue", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

pe <- parameterEstimates(fit_free, ci = TRUE)

direct_diffs <- pe[grepl("^d_[ab][0-9]+$", pe$lhs),
                   c("lhs","est","se","z","pvalue","ci.lower","ci.upper")]

indirect_diffs <- pe[grepl("^d_IEqu", pe$lhs),
                     c("lhs","est","se","z","pvalue","ci.lower","ci.upper")]

direct_diffs
indirect_diffs

##### 9.1 Restricted scalar paths model ####
# Complete multigroup SEM (scalar invariance) with:
# - FREE model (paths differ across 2 groups)  [for reference]
# - RESTRICTED model (ALL structural paths equal across groups)
# - robust LRT (Satorra-Bentler) to compare FREE vs RESTRICTED under MLR
# - indirect effects included in BOTH models
# - (in FREE) differences for direct + indirect effects (g1 - g2)
# - (in RESTRICTED) the indirects are common (no between-group diffs by definition)

model_eq <- ('
# Latent variables
hc =~ hc1 + hc2 + hc3 + hc4
pp =~ pp1 + pp2 + pp3 + pp4
cc =~ cc1 + cc2 + cc3 + cc4

# Regressions (EQUAL across 2 groups)
hc  ~ c(a11,a11)*qul + c(a21,a21)*qun
pp  ~ c(a12,a12)*qul + c(a22,a22)*qun
cc  ~ c(a13,a13)*qul + c(a23,a23)*qun

aut ~ c(b11,b11)*hc  + c(b21,b21)*pp  + c(b31,b31)*cc
dep ~ c(b12,b12)*hc  + c(b22,b22)*pp  + c(b32,b32)*cc

# Covariances
qul ~~ qun
hc  ~~ pp + cc
pp  ~~ cc
aut ~~ dep

# Indirect effects (common across groups under equality)
IEqul_hc_aut := a11*b11
IEqul_hc_dep := a11*b12
IEqul_pp_aut := a12*b21
IEqul_pp_dep := a12*b22
IEqul_cc_aut := a13*b31
IEqul_cc_dep := a13*b32

IEqun_hc_aut := a21*b11
IEqun_hc_dep := a21*b12
IEqun_pp_aut := a22*b21
IEqun_pp_dep := a22*b22
IEqun_cc_aut := a23*b31
IEqun_cc_dep := a23*b32

# (Optional, redundant): explicitly define "differences" as 0 for reporting convenience
d_a11 := a11 - a11
d_a21 := a21 - a21
d_a12 := a12 - a12
d_a22 := a22 - a22
d_a13 := a13 - a13
d_a23 := a23 - a23

d_b11 := b11 - b11
d_b21 := b21 - b21
d_b31 := b31 - b31
d_b12 := b12 - b12
d_b22 := b22 - b22
d_b32 := b32 - b32
')

model_eq_full <- paste(model_eq, control_block)

fit_eq <- sem(
  model_eq_full,
  data = DB_inv,
  group = "target",
  estimator = "MLR",
  group.equal = c("loadings", "intercepts"),
  meanstructure = TRUE
)

lavTestLRT(fit_free, fit_eq, method = "satorra.bentler.2001")

# Restricted model doesn't improve the fit



# Efectos directos, indirectos y totales
parameterEstimates(fit_tar, standardized = TRUE) %>%
  dplyr::filter(op %in% c("~", ":="))


# Todos los efectos
est <- parameterEstimates(fit_tar, standardized = TRUE)

# Efectos directos (regresiones)
direct <- est %>%
  filter(op == "~", lhs %in% c("hc", "cc", "pp", "aut", "dep"), 
         rhs %in% c("qul", "qun", "hc", "cc", "pp")) %>%
  select(group, lhs, rhs, est.std = std.all, p.direct = pvalue)

# Efectos indirectos (los que tú definiste con :=)
indirect <- est %>%
  filter(op == ":=", grepl("T", lhs)) %>%
  select(group, lhs,  est.indirect = est, p.indirect = pvalue, std.indirect = std.all, se, ci.lower, ci.upper)

# Unir por outcome y predictor
effects_total <- left_join(direct,
                           indirect,
                           by = c("lhs" = "outcome", "rhs" = "predictor")) %>%
  mutate(total = est.std + std.indirect)

indirect %>% 
  select(lhs, est.indirect, se, p.indirect, ci.lower, ci.upper, std.indirect) %>% 
  slice_tail(n = 12) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  View()


