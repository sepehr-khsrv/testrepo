library(haven)
library(survey)
library(jtools)
library(dplyr)
steps <- read_dta("steps_2020_7.11.2021_01_p3_final_4.1.2.dta")
summary(steps$HDLC3)
###############################LDL groups
steps <- steps %>% mutate(
  LDL_groups = dplyr::case_when(
    LDL < 100 ~ "0",
    LDL >= 100 & LDL < 130 ~ "1",
    LDL >= 130 & LDL < 160 ~ "2",
    LDL >= 160 & LDL < 190 ~ "3",
    LDL >= 190 ~ "4",
    TRUE ~ NA_character_
  ),
  LDL_groups = factor(LDL_groups, levels = c("0", "1", "2", "3", "4"))
)
####################################cholestrol groups
steps <- steps %>% mutate(
  chol_groups = dplyr::case_when(
    CH02l < 200 ~ "0",
    CH02l >= 200 & CH02l < 240 ~ "1",
    CH02l >= 240 ~ "2",
    TRUE ~ NA_character_
  ),
  chol_groups = factor(chol_groups, levels = c("0", "1", "2"))
)
####################################HDL groups
steps <- steps %>% mutate(
  HDL_groups = dplyr::case_when(HDLC3 < 40 ~ "0",
                                HDLC3 >= 60 ~ "1",
                                TRUE ~ NA_character_),
  HDL_groups = factor(HDL_groups, levels = c("0", "1"))
)
############################################
steps <- steps %>% mutate(
  age_group = dplyr::case_when(
    age >= 25 & age < 30 ~ "1",
    age >= 30 & age < 35 ~ "2",
    age >= 35 & age < 40  ~ "3",
    age >= 40 & age < 45 ~ "4",
    age >= 45 & age < 50 ~ "5",
    age >= 50 & age < 55 ~ "6",
    age >= 55 & age < 60 ~ "7",
    age >= 60 & age < 65 ~ "8",
    age >= 65 ~ "9",
    TRUE ~ NA_character_
  ),
  age_group = factor(age_group, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
)

steps <- steps %>% mutate(
  senility = dplyr::case_when(
    (age>=45) & (c1==1) ~ "1",
    (age>=55) & (c1==0) ~ "1",
    (age<45)  & (c1==1) ~ "0",
    (age<55) & (c1==0)  ~ "0",
    TRUE ~ NA_character_
  ),
  senility = factor(senility, levels = c("0","1"))
)


############################################################
steps <- steps %>% mutate(
  employment_group = dplyr::case_when(
    (i21 == 1 | i21 == 2 | i21 == 3 | i21 == 4 | i21 == 5) ~ "worker",
    i21 == 10 ~ "retired",
    (i21 == 11 | i21 == 12 | i21 == 13) ~ "unemployed",
    (i21 == 6 | i21 == 7 | i21 == 8 | i21 == 9) ~ "unpaid",
    TRUE ~ NA_character_
  ),
  employment_group = factor(
    employment_group,
    levels = c("worker", "retired", "unemployed", "unpaid")
  )
)
# steps <- steps[!is.na(steps$employment_group), ]

#############################################################
# steps <- steps[!is.na(steps$diabetes_HbA1c), ]
# steps <- steps[!is.na(steps$diabetes_FBS), ]
steps$h88ma[steps$h88ma == -555] <- 0
steps <- steps %>% mutate(
  diabetes = dplyr::case_when(
    (h88ma == 1 | diabetes_FBS == 1 | diabetes_HbA1c == 1) ~ "1",
    (h88ma == 0 | diabetes_FBS == 0 | diabetes_HbA1c == 0) ~ "0",
    TRUE ~ NA_character_
  ),
  diabetes = factor(diabetes, levels = c("0", "1"))
)

#############################################################
# steps <- steps[!is.na(steps$bmi), ]
steps <- steps %>% mutate(
  bmi_group = dplyr::case_when(
    bmi < 18.5 ~ "0",
    bmi >= 18.5 & bmi < 25 ~ "1",
    bmi >= 25 & bmi < 30 ~ "2",
    bmi >= 30 & bmi < 35 ~ "3",
    bmi >= 35 & bmi < 40 ~ "4",
    bmi >= 40 ~ "5",
    TRUE ~ NA_character_
  ),
  bmi_group = factor(bmi_group, levels = c("0", "1", "2", "3", "4", "5"))
)

#############################################################
steps$s1_1[steps$s1_1 == -555] <- 0
steps$t5a_day[steps$t5a_day == -555] <- NA
# steps <- steps[!is.na(steps$t5a_day), ]
# mean(steps$t5a_day)
steps$monthlycigar <- (steps$t5a_day * 30)
steps$h14a[steps$h14a == -555] <- 0
#############################################################
# steps <- steps[!is.na(steps$h14a), ]
# steps <- steps[!is.na(steps$CH02l), ]
steps <- steps %>% mutate(
  cholestrol_control = dplyr::case_when((h14a == 1) &
                                          (CH02l >= 240) ~ "0",
                                        (h14a == 1) &
                                          (CH02l < 240) ~ "1",
                                        TRUE ~ NA_character_
  ),
  cholestrol_control = factor(cholestrol_control, levels = c("0", "1"))
)

#############################################################
# steps <- steps[!is.na(steps$CH02l), ]
# steps <- steps[!is.na(steps$h14_1), ]
steps <- steps %>% mutate(
  bilacid_group = dplyr::case_when((h14a == 1) & (h14_1 == 0) ~ "0",
                                   (h14a == 1) & (h14_1 == 1) ~ "1",
                                   TRUE ~ NA_character_
  ),
  bilacid_group = factor(bilacid_group, levels = c("0", "1"))
)

############################################################