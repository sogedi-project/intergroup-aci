
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
library("lsr")
library("rstatix")
library("ggpubr")
library("ggplot2")
library("jmv")

#### 1. Preliminary analyses ####
###### 1.1. Upload the data set

setwd("/Users/mario/Desktop/") # Working directory
DB <- read_sav("procesamiento/data_set_expe_2.sav") # Reading data from the SPSS file

###### 1.1. Select participants and variables
table(subset(DB, select = c("filter"))) # Participants that passes the study are 1, those who failed AC are 0

DB$MC_sex <- as.numeric(DB$MC_sex)
DB$MC_valence <- as.numeric(DB$MC_valence)
DB$MC_frecuency <- as.numeric(DB$MC_frecuency)
DB$condi_valence <- as.numeric(DB$condi_valence)
DB$condi_gender <- as.numeric(DB$condi_gender)
DB$condi_frequ <- as.numeric(DB$condi_frequ)

corr_mc <- subset(DB, filter == 1, select = c("MC_sex", "MC_valence","MC_frecuency", "condi_valence","condi_gender", "condi_frequ"))
apa.cor.table(corr_mc, show.conf.interval = FALSE,
              filename = "corr_mc.docx", table.number = 1) # Ensuring the all participants passes the MC.

DB <- DB %>% 
  filter(DB$filter %in% c(1)) %>% 
  dplyr::select(hc_1, hc_2, hc_3, hc_4,
                pp_1, pp_2, pp_3, pp_4,
                cc_1, cc_2, cc_3, cc_4,
                depe, aut,
                condi_valence, condi_gender, condi_frequ, condi, 
                sex, ses, age)

DB$hc_1 <- as.numeric(DB$hc_1)
DB$hc_2 <- as.numeric(DB$hc_2)
DB$hc_3 <- as.numeric(DB$hc_3)
DB$hc_4 <- as.numeric(DB$hc_4)

DB$pp_1 <- as.numeric(DB$pp_1)
DB$pp_2 <- as.numeric(DB$pp_2)
DB$pp_3 <- as.numeric(DB$pp_3)
DB$pp_4 <- as.numeric(DB$pp_4)

DB$cc_1 <- as.numeric(DB$cc_1)
DB$cc_2 <- as.numeric(DB$cc_2)
DB$cc_3 <- as.numeric(DB$cc_3)
DB$cc_4 <- as.numeric(DB$cc_4)
DB$depe <- as.numeric(DB$depe)
DB$aut <- as.numeric(DB$aut)

DB$condi <- as.numeric(DB$condi)
DB$sex <- as.numeric(DB$sex)
DB$ses <- as.numeric(DB$ses)
DB$age <- as.numeric(DB$age)

summary(DB)

#### 2. Reliability ####
##### 2.1. Hostile classism reliability ####
pp_rel <- psych::alpha(subset(DB, condi_gender == "1", select = c("hc_1", "hc_2", "hc_3", "hc_4"))) # Target: Women
pp_rel$total[2]

pp_rel <- psych::alpha(subset(DB, condi_gender == "0", select = c("hc_1", "hc_2", "hc_3", "hc_4"))) # Target: Men
pp_rel$total[2]

##### 2.2. Protective paternalism reliability ####
pp_rel <- psych::alpha(subset(DB, condi_gender == "1", select = c("pp_1", "pp_2", "pp_3", "pp_4")))
pp_rel$total[2]

pp_rel <- psych::alpha(subset(DB, condi_gender == "0", select = c("pp_1", "pp_2", "pp_3", "pp_4")))
pp_rel$total[2]

##### 2.3. Complementary class differentiation reliability ####
pp_rel <- psych::alpha(subset(DB, condi_gender == "1", select = c("cc_1", "cc_2", "cc_3", "cc_4")))
pp_rel$total[2]

pp_rel <- psych::alpha(subset(DB, condi_gender == "0", select = c("cc_1", "cc_2", "cc_3", "cc_4")))
pp_rel$total[2]

#### 3. Compute variables ####
DB <- DB %>%
  mutate(hc = (hc_1+hc_2+hc_3+hc_4)/4) %>%
  mutate(pp = (pp_1+pp_2+pp_3+pp_4)/4) %>%
  mutate(cc = (cc_1+cc_2+cc_3+cc_4)/4)

#### 4. Descriptive statistics ####
###### 4.1.1. Descriptive statistics of sex and age
table(subset(DB, select = c("sex")))
prop.table(table(subset(DB, select = c("sex"))))

table(subset(DB, select = c("age")))
summary(DB$age)
sd(DB$age, na.rm = TRUE)

###### 4.2.2. Descriptive statistics quantitative variables by condition ####
DB_des_condi1 <- subset(DB, select = c("hc", "pp", "cc", "aut", "depe", "condi"))
describeBy(DB_des_condi1, group = c("condi"))

###### 4.2.2. Descriptive statistics quantitative variables by sex_mani and valence_mani ####
DB_des_condi2 <- subset(DB, select = c("hc", "pp", "cc", "aut", "depe", "condi_valence", "condi_gender", "condi_frequ", "condi"))
describeBy(DB_des_condi2, group = c("condi_valence", "condi_gender", "condi_frequ"))

#### 5. ANOVAs ####
##### 5.1. MANCOVA #####
mancova(data = DB, deps = vars(hc, pp, cc, aut, depe), factors = vars(condi_valence, condi_gender, condi_frequ), covs = vars(ses, sex))


##### 5.2. ANCOVA for HC #####
###### 5.2.1. Test ANCOVA for HC ######
aov_hc <- DB %>% 
  anova_test(hc ~ condi_valence*condi_gender*condi_frequ + ses + sex)
get_anova_table(aov_hc)

###### 5.2.2. Simple Main Effect for Quality ANCOVA for HC ######
DB %>%
  group_by(condi_gender) %>%
  anova_test(hc ~ condi_valence + condi_frequ + ses + sex)

DB %>%
  group_by(condi_gender, condi_frequ) %>%
  anova_test(hc ~ condi_valence + ses + sex)

sm_hc <- DB %>% 
  group_by(condi_gender, condi_frequ) %>%
  emmeans_test(
    hc ~ condi_valence, covariate = c(ses, sex),
    p.adjust.method = "bonferroni")
sm_hc

###### 5.2.3. Simple Main Effect for Quantity ANCOVA for HC ######
DB %>%
  group_by(condi_gender) %>%
  anova_test(hc ~ condi_valence + condi_frequ + ses + sex)

DB %>%
  group_by(condi_gender, condi_valence) %>%
  anova_test(hc ~ condi_frequ + ses + sex)

sm_hc <- DB %>% 
  group_by(condi_gender, condi_valence) %>%
  emmeans_test(
    hc ~ condi_frequ, covariate = c(ses, sex),
    p.adjust.method = "bonferroni")
sm_hc

###### 5.2.4. Simple Main Effect for Target Sex ANCOVA for HC ######
DB %>%
  group_by(condi_valence, condi_frequ) %>%
  anova_test(hc ~ condi_gender + ses + sex)

sm_hc_sex <- DB %>% 
  group_by(condi_valence, condi_frequ) %>%
  emmeans_test(
    hc ~ condi_gender, covariate = c(ses, sex),
    p.adjust.method = "bonferroni")
sm_hc_sex

##### 5.3. ANCOVA for PP #####
###### 5.3.1. Test ANCOVA for PP ######
aov_hc <- DB %>% 
  anova_test(pp ~ condi_valence*condi_gender*condi_frequ + ses + sex)
get_anova_table(aov_hc)

###### 5.3.2. Simple Main Effect for Quality ANCOVA for PP ######
DB %>%
  group_by(condi_gender) %>%
  anova_test(pp ~ condi_valence + condi_frequ + ses + sex)

DB %>%
  group_by(condi_gender, condi_frequ) %>%
  anova_test(pp ~ condi_valence + ses + sex)

sm_hc <- DB %>% 
  group_by(condi_gender, condi_frequ) %>%
  emmeans_test(
    hc ~ condi_valence, covariate = c(ses, sex),
    p.adjust.method = "bonferroni")
sm_hc

###### 5.3.3. Simple Main Effect for Quantity ANCOVA for PP ######
DB %>%
  group_by(condi_gender) %>%
  anova_test(pp ~ condi_valence + condi_frequ + ses + sex)

DB %>%
  group_by(condi_gender, condi_valence) %>%
  anova_test(pp ~ condi_frequ + ses + sex)

sm_hc <- DB %>% 
  group_by(condi_gender, condi_valence) %>%
  emmeans_test(
    hc ~ condi_frequ, covariate = c(ses, sex),
    p.adjust.method = "bonferroni")
sm_hc

###### 5.3.4. Simple Main Effect for Target Sex ANCOVA for PP ######
DB %>%
  group_by(condi_valence, condi_frequ) %>%
  anova_test(pp ~ condi_gender + ses + sex)

sm_hc_sex <- DB %>% 
  group_by(condi_valence, condi_frequ) %>%
  emmeans_test(
    hc ~ condi_gender, covariate = c(ses, sex),
    p.adjust.method = "bonferroni")
sm_hc_sex

###### 5.4.1. Test ANCOVA for CC ######
aov_hc <- DB %>% 
  anova_test(cc ~ condi_valence*condi_gender*condi_frequ + ses + sex)
get_anova_table(aov_hc)

###### 5.4.2. Simple Main Effect for Quality ANCOVA for CC ######
DB %>%
  group_by(condi_gender) %>%
  anova_test(cc ~ condi_valence + condi_frequ + ses + sex)

DB %>%
  group_by(condi_gender, condi_frequ) %>%
  anova_test(cc ~ condi_valence + ses + sex)

sm_hc <- DB %>% 
  group_by(condi_gender, condi_frequ) %>%
  emmeans_test(
    hc ~ condi_valence, covariate = c(ses, sex),
    p.adjust.method = "bonferroni")
sm_hc

###### 5.4.3. Simple Main Effect for Quantity ANCOVA for CC ######
DB %>%
  group_by(condi_gender) %>%
  anova_test(cc ~ condi_valence + condi_frequ + ses + sex)

DB %>%
  group_by(condi_gender, condi_valence) %>%
  anova_test(cc ~ condi_frequ + ses + sex)

sm_hc <- DB %>% 
  group_by(condi_gender, condi_valence) %>%
  emmeans_test(
    hc ~ condi_frequ, covariate = c(ses, sex),
    p.adjust.method = "bonferroni")
sm_hc

###### 5.4.4. Simple Main Effect for Target Sex ANCOVA for CC ######
DB %>%
  group_by(condi_valence, condi_frequ) %>%
  anova_test(cc ~ condi_gender + ses + sex)

sm_hc_sex <- DB %>% 
  group_by(condi_valence, condi_frequ) %>%
  emmeans_test(
    hc ~ condi_gender, covariate = c(ses, sex),
    p.adjust.method = "bonferroni")
sm_hc_sex

###### 5.5.1. Test ANCOVA for AUT ######
aov_aut <- DB %>% 
  anova_test(aut ~ condi_valence*condi_gender*condi_frequ + ses + sex)
get_anova_table(aov_aut)

###### 5.5.2. Simple Main Effect for Quality ANCOVA for AUT ######
DB %>%
  group_by(condi_gender) %>%
  anova_test(aut ~ condi_valence + condi_frequ + ses + sex)

DB %>%
  group_by(condi_gender, condi_frequ) %>%
  anova_test(aut ~ condi_valence + ses + sex)

sm_hc <- DB %>% 
  group_by(condi_gender, condi_frequ) %>%
  emmeans_test(
    hc ~ condi_valence, covariate = c(ses, sex),
    p.adjust.method = "bonferroni")
sm_hc

###### 5.5.3. Simple Main Effect for Quantity ANCOVA for AUT ######
DB %>%
  group_by(condi_gender) %>%
  anova_test(aut ~ condi_valence + condi_frequ + ses + sex)

DB %>%
  group_by(condi_gender, condi_valence) %>%
  anova_test(aut ~ condi_frequ + ses + sex)

sm_hc <- DB %>% 
  group_by(condi_gender, condi_valence) %>%
  emmeans_test(
    hc ~ condi_frequ, covariate = c(ses, sex),
    p.adjust.method = "bonferroni")
sm_hc

###### 5.5.4. Simple Main Effect for Target Sex ANCOVA for AUT ######
DB %>%
  group_by(condi_valence, condi_frequ) %>%
  anova_test(aut ~ condi_gender + ses + sex)

sm_hc_sex <- DB %>% 
  group_by(condi_valence, condi_frequ) %>%
  emmeans_test(
    hc ~ condi_gender, covariate = c(ses, sex),
    p.adjust.method = "bonferroni")
sm_hc_sex

###### 5.6.1. Test ANCOVA for DEPE ######
aov_depe <- DB %>% 
  anova_test(depe ~ condi_valence*condi_gender*condi_frequ + ses + sex)
get_anova_table(aov_depe)

###### 5.5.2. Simple Main Effect for Quality ANCOVA for DEPE ######
DB %>%
  group_by(condi_gender) %>%
  anova_test(depe ~ condi_valence + condi_frequ + ses + sex)

DB %>%
  group_by(condi_gender, condi_frequ) %>%
  anova_test(depe ~ condi_valence + ses + sex)

sm_hc <- DB %>% 
  group_by(condi_gender, condi_frequ) %>%
  emmeans_test(
    hc ~ condi_valence, covariate = c(ses, sex),
    p.adjust.method = "bonferroni")
sm_hc

###### 5.5.3. Simple Main Effect for Quantity ANCOVA for DEPE ######
DB %>%
  group_by(condi_gender) %>%
  anova_test(depe ~ condi_valence + condi_frequ + ses + sex)

DB %>%
  group_by(condi_gender, condi_valence) %>%
  anova_test(depe ~ condi_frequ + ses + sex)

sm_hc <- DB %>% 
  group_by(condi_gender, condi_valence) %>%
  emmeans_test(
    hc ~ condi_frequ, covariate = c(ses, sex),
    p.adjust.method = "bonferroni")
sm_hc

###### 5.5.4. Simple Main Effect for Target Sex ANCOVA for DEPE ######
DB %>%
  group_by(condi_valence, condi_frequ) %>%
  anova_test(depe ~ condi_gender + ses + sex)

sm_hc_sex <- DB %>% 
  group_by(condi_valence, condi_frequ) %>%
  emmeans_test(
    hc ~ condi_gender, covariate = c(ses, sex),
    p.adjust.method = "bonferroni")
sm_hc_sex

##### 6. Multigroup SEM ####

DB$qun <- DB$condi_frequ
DB$qul <- DB$condi_valence
DB$dep <- DB$depe

Model <- '
#Latent variables
hc =~ hc_1 + hc_2 + hc_3 + hc_4
pp =~ pp_1 + pp_2 + pp_3 + pp_4
cc =~ cc_1 + cc_2 + cc_3 + cc_4

#Regressions
hc ~ c(a11,x11)*qul + c(a21,x21)*qun 
pp ~ c(a12,x12)*qul + c(a22,x22)*qun 
cc ~ c(a13,x13)*qul + c(a23,x23)*qun 
aut ~ c(b11,y11)*hc + c(b21,y21)*pp + c(b31,y31)*cc 
dep ~ c(b12,y12)*hc + c(b22,y22)*pp + c(b32,y32)*cc

#Covariance
hc ~~ pp + cc
pp ~~ cc
aut ~~ dep

#Control
hc ~ sex + ses
pp ~ sex + ses
cc ~ sex + ses
aut ~ sex + ses
dep ~ sex + ses
qun ~ sex + ses

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
T2IEqun_cc_dep := x23*y32'

###### 6.1. Select variables ####
DB_sem_condi <- subset(DB, select = c("hc_1", "hc_2", "hc_3", "hc_4", 
                                      "pp_1", "pp_2", "pp_3", "pp_4", 
                                      "cc_1", "cc_2", "cc_3", "cc_4",
                                      "qul", "qun", "aut", "dep",
                                      "condi_gender", "ses", "sex"))

###### 6.2. Multivariate normality ####
mardia(subset(DB_sem_condi, select = c(hc_1, hc_2, hc_3, hc_4, 
                                       pp_1, pp_2, pp_3, pp_4, 
                                       cc_1, cc_2, cc_3, cc_4,
                                       qul, qun, aut, dep, ses, sex, condi_gender)), na.rm = TRUE, plot=TRUE)

###### 6.3. Model fit ####
fit_condi <- sem(Model, data = DB_sem_condi, group = "condi_gender", estimator = "MLR")
summary(fit_condi, fit.measures = T, ci = T, standardized = T, rsquare = T)
fitmeasures(fit_condi, c("chisq", "pvalue", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

###### 6.4. Monte Carlo confidence intervals ####
monteCarloCI(fit_condi, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)

modindices(fit_condi, sort = TRUE, maximum.number = 25)

