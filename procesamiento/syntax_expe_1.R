
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
#setwd("~/Documents/R/Contact and classisms")
setwd("/Users/mario/Desktop/") # Working directory
DB <- read_sav("procesamiento/data_set_expe_1.sav") # Reading data from the SPSS file

###### 1.1. Select participants and variables
table(subset(DB, select = c("filter_final"))) # Participants that passes the study are 1, those who failed AC are 0

corr_mc <- subset(DB, filter_final == "1", select = c("sex_mani", "mc_sex", "valence_mani", "mc_valence"))
apa.cor.table(corr_mc, show.conf.interval = FALSE,
              filename = "corr_mc.docx", table.number = 1) # Ensuring the all participants passes the MC.

DB <- DB %>% 
  filter(DB$filter_final %in% c("1")) %>% 
  dplyr::select(hc_1, hc_2, hc_3, hc_4,
                      pp_1, pp_2, pp_3, pp_4,
                      cc_1, cc_2, cc_3, cc_4,
                      depe, aut,
                      valence_mani, quantity_pre, quality_pre, ID, filter_final, sex_mani, sex, ses, age, condition) 

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

DB$valence_mani <- as.numeric(DB$valence_mani)
DB$quantity_pre <- as.numeric(DB$quantity_pre)
DB$quality_pre <- as.numeric(DB$quality_pre)
DB$sex_mani <- as.numeric(DB$sex_mani)
DB$sex <- as.numeric(DB$sex)
DB$ses <- as.numeric(DB$ses)
DB$age <- as.numeric(DB$age)

summary(DB)

#### 2. Reliability ####
##### 2.1. Hostile classism reliability ####
pp_rel <- psych::alpha(subset(DB, sex_mani == "1", select = c("hc_1", "hc_2", "hc_3", "hc_4"))) # Target: Women
pp_rel$total[2]

pp_rel <- psych::alpha(subset(DB, sex_mani == "0", select = c("hc_1", "hc_2", "hc_3", "hc_4"))) # Target: Men
pp_rel$total[2]

##### 2.2. Protective paternalism reliability ####
pp_rel <- psych::alpha(subset(DB, sex_mani == "1", select = c("pp_1", "pp_2", "pp_3", "pp_4")))
pp_rel$total[2]

pp_rel <- psych::alpha(subset(DB, sex_mani == "0", select = c("pp_1", "pp_2", "pp_3", "pp_4")))
pp_rel$total[2]

##### 2.3. Complementary class differentiation reliability ####
pp_rel <- psych::alpha(subset(DB, sex_mani == "1", select = c("cc_1", "cc_2", "cc_3", "cc_4")))
pp_rel$total[2]

pp_rel <- psych::alpha(subset(DB, sex_mani == "0", select = c("cc_1", "cc_2", "cc_3", "cc_4")))
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
DB_des_condi1 <- subset(DB, select = c("quality_pre", "quantity_pre", "hc", "pp", "cc", "aut", "depe", "condition"))
describeBy(DB_des_condi1, group = c("condition"))

###### 4.2.2. Descriptive statistics quantitative variables by sex_mani and valence_mani ####
DB_des_condi2 <- subset(DB, select = c("quality_pre", "quantity_pre", "hc", "pp", "cc", "aut", "depe", "sex_mani", "valence_mani"))
describeBy(DB_des_condi2, group = c("sex_mani", "valence_mani"))

#### 5. ANOVAs ####
##### 5.1. MANCOVA #####
mancova(data = DB, deps = vars(hc, pp, cc, aut, depe), factors = vars(valence_mani, sex_mani), covs = vars(quantity_pre, ses, sex))

##### 5.2. ANCOVA for HC #####
###### 5.2.1. Test ANCOVA for HC ######
aov_hc <- DB %>% 
  anova_test(hc ~ valence_mani*sex_mani + quantity_pre + ses + sex)
get_anova_table(aov_hc)

###### 5.2.2. Simple Main Effect for Quality ANCOVA for HC ######
DB %>%
  group_by(sex_mani) %>%
  anova_test(hc ~ valence_mani + quantity_pre + ses + sex)

sm_hc_qul <- DB %>% 
  group_by(sex_mani) %>%
  emmeans_test(
    hc ~ valence_mani, covariate = c(quantity_pre, ses, sex),
    p.adjust.method = "bonferroni")
sm_hc_qul

###### 5.2.2. Simple Main Effect for Target Sex ANCOVA for HC ######
DB %>%
  group_by(valence_mani) %>%
  anova_test(hc ~ sex_mani + quantity_pre + ses + sex)

sm_hc_sex <- DB %>% 
  group_by(valence_mani) %>%
  emmeans_test(
    hc ~ sex_mani, covariate = c(quantity_pre, ses, sex),
    p.adjust.method = "bonferroni")
sm_hc_sex

###### 5.2.4. Graph ANCOVA for HC ######
aov_hc <- ggline(get_emmeans(sm_hc_sex), x = "valence_mani", y = "emmean", 
  color = "sex_mani", palette = "jco") +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = sex_mani), 
    width = 0.1)
aov_hc

##### 5.3. ANCOVA for PP #####
###### 5.3.1. Test ANCOVA for PP ######
aov_pp <- DB %>% 
  anova_test(pp ~ valence_mani*sex_mani + quantity_pre + ses + sex)
get_anova_table(aov_pp)

###### 5.3.2. Simple Main Effect for Quality ANCOVA for PP ######
DB %>%
  group_by(sex_mani) %>%
  anova_test(pp ~ valence_mani + quantity_pre + ses + sex)

sm_pp_qul <- DB %>% 
  group_by(sex_mani) %>%
  emmeans_test(
    pp ~ valence_mani, covariate = c(quantity_pre, ses, sex),
    p.adjust.method = "bonferroni")
sm_pp_qul

###### 5.3.2. Simple Main Effect for Target Sex ANCOVA for PP ######
DB %>%
  group_by(valence_mani) %>%
  anova_test(pp ~ sex_mani + quantity_pre + ses + sex)

sm_pp_sex <- DB %>% 
  group_by(valence_mani) %>%
  emmeans_test(
    pp ~ sex_mani, covariate = c(quantity_pre, ses, sex),
    p.adjust.method = "bonferroni")
sm_pp_sex

###### 5.3.4. Graph ANCOVA for PP ######
aov_pp <- ggline(get_emmeans(sm_pp_sex), x = "valence_mani", y = "emmean", 
                   color = "sex_mani", palette = "jco") +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = sex_mani), 
    width = 0.1)
aov_pp

##### 5.4. ANCOVA for CC #####
###### 5.4.1. Test ANCOVA for CC ######
aov_cc <- DB %>% 
  anova_test(cc ~ valence_mani*sex_mani + quantity_pre + ses + sex)
get_anova_table(aov_cc)

###### 5.4.2. Simple Main Effect for Quality ANCOVA for CC ######
DB %>%
  group_by(sex_mani) %>%
  anova_test(cc ~ valence_mani + quantity_pre + ses + sex)

sm_cc_qul <- DB %>% 
  group_by(sex_mani) %>%
  emmeans_test(
    cc ~ valence_mani, covariate = c(quantity_pre, ses, sex),
    p.adjust.method = "bonferroni")
sm_cc_qul

###### 5.4.2. Simple Main Effect for Target Sex ANCOVA for CC ######
DB %>%
  group_by(valence_mani) %>%
  anova_test(cc ~ sex_mani + quantity_pre + ses + sex)

sm_cc_sex <- DB %>% 
  group_by(valence_mani) %>%
  emmeans_test(
    cc ~ sex_mani, covariate = c(quantity_pre, ses, sex),
    p.adjust.method = "bonferroni")
sm_cc_sex

###### 5.4.4. Graph ANCOVA for CC ######
aov_cc <- ggline(get_emmeans(sm_cc_sex), x = "valence_mani", y = "emmean", 
                 color = "sex_mani", palette = "jco") +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = sex_mani), 
    width = 0.1)
aov_cc

##### 5.5. ANCOVA for AUT #####
###### 5.5.1. Test ANCOVA for AUT ######
aov_aut <- DB %>% 
  anova_test(aut ~ valence_mani*sex_mani + quantity_pre + ses + sex)
get_anova_table(aov_aut)

###### 5.5.2. Simple Main Effect for Quality ANCOVA for AUT ######
DB %>%
  group_by(sex_mani) %>%
  anova_test(aut ~ valence_mani + quantity_pre + ses + sex)

sm_aut_qul <- DB %>% 
  group_by(sex_mani) %>%
  emmeans_test(
    aut ~ valence_mani, covariate = c(quantity_pre, ses, sex),
    p.adjust.method = "bonferroni")
sm_aut_qul

###### 5.5.2. Simple Main Effect for Target Sex ANCOVA for AUT ######
DB %>%
  group_by(valence_mani) %>%
  anova_test(aut ~ sex_mani + quantity_pre + ses + sex)

sm_aut_sex <- DB %>% 
  group_by(valence_mani) %>%
  emmeans_test(
    aut ~ sex_mani, covariate = c(quantity_pre, ses, sex),
    p.adjust.method = "bonferroni")
sm_aut_sex

###### 5.5.4. Graph ANCOVA for AUT ######
aov_aut <- ggline(get_emmeans(sm_aut_sex), x = "valence_mani", y = "emmean", 
                 color = "sex_mani", palette = "jco") +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = sex_mani), 
    width = 0.1)
aov_aut

##### 5.6. ANCOVA for DEPE #####
###### 5.6.1. Test ANCOVA for DEPE ######
aov_depe <- DB %>% 
  anova_test(depe ~ valence_mani*sex_mani + quantity_pre + ses + sex)
get_anova_table(aov_depe)

###### 5.6.2. Simple Main Effect for Quality ANCOVA for DEPE ######
DB %>%
  group_by(sex_mani) %>%
  anova_test(depe ~ valence_mani + quantity_pre + ses + sex)

sm_depe_qul <- DB %>% 
  group_by(sex_mani) %>%
  emmeans_test(
    depe ~ valence_mani, covariate = c(quantity_pre, ses, sex),
    p.adjust.method = "bonferroni")
sm_depe_qul

###### 5.6.2. Simple Main Effect for Target Sex ANCOVA for DEPE ######
DB %>%
  group_by(valence_mani) %>%
  anova_test(depe ~ sex_mani + quantity_pre + ses + sex)

sm_depe_sex <- DB %>% 
  group_by(valence_mani) %>%
  emmeans_test(
    depe ~ sex_mani, covariate = c(quantity_pre, ses, sex),
    p.adjust.method = "bonferroni")
sm_depe_sex

###### 5.6.4. Graph ANCOVA for AUT ######
aov_depe <- ggline(get_emmeans(sm_depe_sex), x = "valence_mani", y = "emmean", 
                  color = "sex_mani", palette = "jco") +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = sex_mani), 
    width = 0.1)
aov_depe

##### 6. Multigroup SEM ####

DB$qun <- DB$quantity_pre
DB$qul <- DB$valence_mani
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

#Control: These two variables do not have an effect so we are not controlling by them.
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
                                    "sex_mani", "ses", "sex"))

###### 6.2. Multivariate normality ####
mardia(subset(DB_sem_condi, select = c(hc_1, hc_2, hc_3, hc_4, 
                                     pp_1, pp_2, pp_3, pp_4, 
                                     cc_1, cc_2, cc_3, cc_4,
                                     qul, qun, aut, dep, ses, sex, sex_mani)), na.rm = TRUE, plot=TRUE)

###### 6.3. Model fit ####
fit_condi <- sem(Model, data = DB_sem_condi, group = "sex_mani", estimator = "MLR")
summary(fit_condi, fit.measures = T, ci = T, standardized = T, rsquare = T)
fitmeasures(fit_condi, c("chisq", "pvalue", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

###### 6.4. Monte Carlo confidence intervals ####
monteCarloCI(fit_condi, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)

modindices(fit_condi, sort = TRUE, maximum.number = 25)

