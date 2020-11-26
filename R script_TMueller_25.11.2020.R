##########################################
#
# exploring a Heart Disesae Data Set
#
# author: Tanja Mueller
#
# date: 25.11.2020
#
###########################################


library(dplyr)
library(janitor)
library(epitools)

setwd("//[...]")

heart_disease <- read.csv("//[...]/processed.cleveland.data", header=FALSE, stringsAsFactors=FALSE)


#### preparation of data ####

names(heart_disease)[1:14] <- c("age", "sex", "chest_pain", "blood_pressure", "cholesterol", "blood_sugar", "ECG", "heart_rate", "angina", "ST_depression", "ST_slope", "vessel_fluor", "defect", "outcome_IHD")


# inspect data for availability, consistency, and completeness; change coding of missing variables

str(heart_disease)
head(heart_disease)
summary(heart_disease)
table(heart_disease$vessel_fluor)
table(heart_disease$defect)

heart_disease$vessel_fluor[heart_disease$vessel_fluor == "?"] <- NA
heart_disease$defect[heart_disease$defect == "?"] <- NA


# change formatting of variables

heart_disease <- as.data.frame(heart_disease %>% 
                                       mutate_at(vars(sex, chest_pain, blood_sugar, ECG, angina, ST_slope, vessel_fluor, defect), list(as.factor)))

levels(heart_disease$sex) = c("female", "male")
levels(heart_disease$chest_pain) = c("typical angina", "atypical angina", "non-anginal pain", "asymptomatic")
levels(heart_disease$blood_sugar) = c("<= 120mg/dl", ">120mg/dl")
levels(heart_disease$ECG) = c("normal", "ST-T wave abnormality", "left ventricular hypertrophy")
levels(heart_disease$angina) = c("no", "yes")
levels(heart_disease$ST_slope) = c("upsloping", "flat", "downsloping")
levels(heart_disease$defect) = c("normal", "fixed", "reversable")


#### basic descriptive summary of data ####

# distribution of heart disease in dataset

table(heart_disease$outcome_IHD, useNA="ifany")
heart_disease$IHD_cat <- as.factor(ifelse(heart_disease$outcome_IHD == 0, 0, 1))
levels(heart_disease$IHD_cat) = c("no", "yes")
table(heart_disease$IHD_cat, useNA="ifany")

dev.new()
barplot(table(heart_disease$outcome_IHD), ylim = c(0, 200), main = "diagnosis of heart disease based on angiography", xlab = "diameter narrowing, categorial", ylab = "number patients")

dev.new()
barplot(table(heart_disease$IHD_cat), ylim = c(0, 200), main = "diagnosis of heart disease based on angiography", xlab = "presence of heart disease", ylab = "number patients")


# age 

summary(heart_disease$age)
sd(heart_disease$age)
summary(heart_disease$age[heart_disease$IHD_cat == "yes"])
sd(heart_disease$age[heart_disease$IHD_cat == "yes"])
summary(heart_disease$age[heart_disease$IHD_cat == "no"])
sd(heart_disease$age[heart_disease$IHD_cat == "no"])
t.test(age ~ IHD_cat, data=heart_disease)

dev.new() 
hist(heart_disease$age[heart_disease$IHD_cat == "no"], xlim=c(20,90), ylim=c(0,55), col=rgb(1,0,0,0.5), xlab="age [years]", ylab="number of patients", main="age distribution, patients with and without heart disease")
hist(heart_disease$age[heart_disease$IHD_cat == "yes"], xlim=c(20,90), col=rgb(0,0,1,0.5), add=TRUE)
legend("topright", legend=c("without heart disease", "with heart disease"), col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), pt.cex=2, pch=15)

dev.new()
boxplot(age ~ IHD_cat, data = heart_disease, main = "patient age, by presence of heart disease", xlab = "presence of heart disease", ylab = "age [years]")


# blood pressure

summary(heart_disease$blood_pressure)
sd(heart_disease$blood_pressure)
summary(heart_disease$blood_pressure[heart_disease$IHD_cat == "yes"])
sd(heart_disease$blood_pressure[heart_disease$IHD_cat == "yes"])
summary(heart_disease$blood_pressure[heart_disease$IHD_cat == "no"])
sd(heart_disease$blood_pressure[heart_disease$IHD_cat == "no"])
t.test(blood_pressure ~ IHD_cat, data = heart_disease)

dev.new()
hist(heart_disease$blood_pressure[heart_disease$IHD_cat == "no"], xlim=c(80,220), ylim=c(0,50), col=rgb(1,0,0,0.5), xlab="blood pressure [mmHg]", ylab="number of patients", main="blood pressure, patients with and without heart disease")
hist(heart_disease$blood_pressure[heart_disease$IHD_cat == "yes"], xlim=c(80,220), col=rgb(0,0,1,0.5), add=TRUE)
legend("topright", legend=c("without heart disease", "with heart disease"), col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), pt.cex=2, pch=15)

dev.new()
boxplot(blood_pressure ~ IHD_cat, data = heart_disease, main = "blood pressure, by presence of heart disease", xlab = "presence of heart disease", ylab = "blood pressure [mmHg]")


# cholesterol

summary(heart_disease$cholesterol)
sd(heart_disease$cholesterol)
summary(heart_disease$cholesterol[heart_disease$IHD_cat == "yes"])
sd(heart_disease$cholesterol[heart_disease$IHD_cat == "yes"])
summary(heart_disease$cholesterol[heart_disease$IHD_cat == "no"])
sd(heart_disease$cholesterol[heart_disease$IHD_cat == "no"])
t.test(cholesterol ~ IHD_cat, data = heart_disease)

dev.new()
hist(heart_disease$cholesterol[heart_disease$IHD_cat == "no"], xlim=c(90,600), ylim=c(0,100), col=rgb(1,0,0,0.5), xlab="cholesterol [mg/dl]", ylab="number of patients", main="overall cholesterol, patients with and without heart disease")
hist(heart_disease$cholesterol[heart_disease$IHD_cat == "yes"], xlim=c(90,600), col=rgb(0,0,1,0.5), add=TRUE)
legend("topright", legend=c("without heart disease", "with heart disease"), col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), pt.cex=2, pch=15)

dev.new()
boxplot(cholesterol ~ IHD_cat, data = heart_disease, main = "cholesterol levels, by presence of heart disease", xlab = "presence of heart disease", ylab = "cholestrol [mg/dl]")


# blood sugar

heart_disease %>% 
        tabyl(blood_sugar, IHD_cat) %>% 
        adorn_totals(c("row", "col")) %>% 
        adorn_percentages("row") %>% 
        adorn_pct_formatting(rounding = "half up", digits = 1) %>% 
        adorn_ns() %>% 
        adorn_title("combined")

BS.IHD <- table(subset(heart_disease, select=c(blood_sugar, IHD_cat)))
chisq.test(BS.IHD)

dev.new()
plot(BS.IHD, main = "presence of heart disease, by level of cholesterol", color = TRUE)


# sex

heart_disease %>% 
        tabyl(sex, IHD_cat) %>% 
        adorn_totals(c("row", "col")) %>% 
        adorn_percentages("row") %>% 
        adorn_pct_formatting(rounding = "half up", digits = 1) %>% 
        adorn_ns() %>% 
        adorn_title("combined")

sex.IHD <- table(subset(heart_disease, select=c(sex, IHD_cat)))
chisq.test(sex.IHD)

dev.new()
plot(sex.IHD, main = "presence of heart disease by gender", color = TRUE)


#### addendum - sex as a risk factor for heart disease ####

sex.IHD <- table(subset(heart_disease, select=c(sex, IHD_cat)))
or_fit1 <- oddsratio(sex.IHD)
or_fit1$data
or_fit1$measure
or_fit1$p.value


#### factors potentially related to developing heart disease ####

summary(glm(IHD_cat ~ age, data = heart_disease, family = binomial))
# significant

summary(glm(IHD_cat ~ sex, data = heart_disease, family = binomial))
# significant

summary(glm(IHD_cat ~ blood_pressure, data = heart_disease, family = binomial))
# significant

summary(glm(IHD_cat ~ cholesterol, data = heart_disease, family = binomial))
# not significant (should be; overall cholesterol though)

summary(glm(IHD_cat ~ blood_sugar, data = heart_disease, family = binomial))
# not significant (cut-off?)


summary(glm(IHD_cat ~ age + sex + blood_pressure, data = heart_disease, family = binomial))

summary(glm(IHD_cat ~ age + sex + blood_pressure + cholesterol, data = heart_disease, family = binomial))
summary(glm(IHD_cat ~ age + sex + blood_pressure + blood_sugar, data = heart_disease, family = binomial))
summary(glm(IHD_cat ~ age + sex + blood_pressure + cholesterol + blood_sugar, data = heart_disease, family = binomial))


#### sessionInfo() ####

# R version 3.5.3 (2019-03-11)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 18363)

# Matrix products: default

# locale:
# [1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252   
# [3] LC_MONETARY=English_United Kingdom.1252 LC_NUMERIC=C                           
# [5] LC_TIME=English_United Kingdom.1252    

# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
# [1] epitools_0.5-10.1 janitor_2.0.1     dplyr_0.8.4      

# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.1       lubridate_1.7.4  tidyr_1.0.2      crayon_1.3.4     assertthat_0.2.1
# [6] R6_2.4.0         lifecycle_0.1.0  magrittr_1.5     pillar_1.3.1     stringi_1.4.3   
# [11] rlang_0.4.5      rstudioapi_0.10  snakecase_0.11.0 ellipsis_0.3.0   vctrs_0.2.4     
# [16] tools_3.5.3      stringr_1.4.0    glue_1.3.1       purrr_0.3.2      compiler_3.5.3  
# [21] pkgconfig_2.0.2  tidyselect_1.0.0 tibble_2.1.1   









