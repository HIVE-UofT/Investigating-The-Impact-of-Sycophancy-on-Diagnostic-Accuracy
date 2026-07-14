library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(purrr)
library(RColorBrewer)
library(writexl)
library(readxl)
library(readxl)
library(httr)

library(irr)
library(psych)
library(DescTools)
library(epiR)

#Compare human evaluation and LLM as a judge

#Start with the DeepSeek one###

df <- readxl::read_excel("judging500_new.xlsx", sheet = "Sheet1")
df1 <- readxl::read_excel("eval_judge_turn1.xlsx")

matched_df <- inner_join(
  df, 
  df1, 
  by = c("case_origin_cat", "case_condition_cat", "case_model_name_cat", "case_origtext_str","case_pass1.2_prompt_str")
)

matched_df1 <- matched_df %>% select(c("case_full_llmjudge_accuracy_pass1.2.y","Human-asses-3")) %>%
                              mutate(`Human-asses-3-result` = `case_full_llmjudge_accuracy_pass1.2.y` == `Human-asses-3`)
k_stats1 <- cohen.kappa(data.frame(matched_df1[,c(1,2)]))
summary(matched_df1$`Human-asses-3-result`)

accuracy_matrix <- matched_df1 %>%
  select(c(case_full_llmjudge_accuracy_pass1.2.y,`Human-asses-3`))

dat_all <- table(accuracy_matrix[c(1,2)])
rval_DS <- epi.tests(dat_all, method = "exact", digits = 2, conf.level = 0.95)


# > 1102/(1102+139)
# [1] 0.8879936
# > k_stats1


######This is the GPT-4 one#####

# df <- read.csv("llm_judge_evaluation.csv")
# df$n <- seq(1:nrow(df))
# n = 200
# random <- df[sample(nrow(df), 200), ]
# 
# #writexl::write_xlsx(random, "judging200.xlsx")
# 
# #Adding additional 300 rows to be judged
# df1 <- readxl::read_excel("judging200.xlsx", sheet = "Sheet1")
# exclude_rows <- df1$n 
# 
# # Create a pool of all possible row numbers, MINUS the excluded ones
# available_rows <- setdiff(1:nrow(df), exclude_rows)
# 
# # Sample 300 random row numbers from the available pool
# nnew = 300
# sampled_rows <- sample(available_rows, nnew)
# 
# # Subset your dataframe
# random_df <- df[sampled_rows, ]
#writexl::write_xlsx(random_df, "judging300_new.xlsx")

# Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha, levels = levels, 
#                    w.exp = w.exp)
# 
# Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
# lower estimate upper
# unweighted kappa   0.8     0.84  0.89
# weighted kappa     0.8     0.84  0.89
# 
# Number of subjects = 500 

summary(df$`Human-asses-3-result`)

####Now we want to merge GPT-4 and DeepSeek

df_gpt4 <- df %>% 
  select(
    judge_GPT = case_full_llmjudge_accuracy_pass1.2,
    case_origin_cat, case_condition_cat, case_model_name_cat, case_origtext_str,case_pass1.2_prompt_str,`Human-asses-3`)

df_DS <- readxl::read_excel("eval_judge_turn1.xlsx") %>%  
  select(
  judge_DS = case_full_llmjudge_accuracy_pass1.2,
  case_origin_cat, case_condition_cat, case_model_name_cat, case_origtext_str,case_pass1.2_prompt_str)


matched_gpt_DS <- inner_join(
  df_gpt4, 
  df_DS, 
  by = c("case_origin_cat", "case_condition_cat", "case_model_name_cat", "case_origtext_str","case_pass1.2_prompt_str")) %>%
  distinct()

accuracy_matrix <- matched_gpt_DS %>%
  select(c(judge_DS,judge_GPT,`Human-asses-3`))

kappa_accuracy <- KappaM(accuracy_matrix, method = "Fleiss", conf.level = 0.95)
kappa_DS <- cohen.kappa(data.frame(accuracy_matrix[c(1,3)]))
kappa_GPT <- cohen.kappa(data.frame(accuracy_matrix[c(2,3)]))
kappa_GPT_DS <- cohen.kappa(data.frame(accuracy_matrix[c(2,1)]))

dat_DS <- table(accuracy_matrix[c(3,1)])
rval_DS <- epi.tests(dat_DS, method = "exact", digits = 2, conf.level = 0.95)
#             Outcome +    Outcome -      Total
# Test +          204           32        236
# Test -           42          187        229
# Total           246          219        465
# 
# Point estimates and 95% CIs:
#   --------------------------------------------------------------
#   Apparent prevalence *               0.51 (0.46, 0.55)
# True prevalence *                      0.53 (0.48, 0.58)
# Sensitivity *                          0.83 (0.78, 0.87)
# Specificity *                          0.85 (0.80, 0.90)
# Positive predictive value *            0.86 (0.81, 0.91)
# Negative predictive value *            0.82 (0.76, 0.86)
# Positive likelihood ratio              5.68 (4.10, 7.86)
# Negative likelihood ratio              0.20 (0.15, 0.26)
# False T+ proportion for true D- *      0.15 (0.10, 0.20)
# False T- proportion for true D+ *      0.17 (0.13, 0.22)
# False T+ proportion for T+ *           0.14 (0.09, 0.19)
# False T- proportion for T- *           0.18 (0.14, 0.24)
# Correctly classified proportion *      0.84 (0.80, 0.87)
# --------------------------------------------------------------

dat_GPT <- table(accuracy_matrix[c(3,2)])
rval_GPT <- epi.tests(dat_GPT, method = "exact", digits = 2, conf.level = 0.95)

#           Outcome +    Outcome -      Total
# Test +          215           21        236
# Test -           17          212        229
# Total           232          233        465
# 
# Point estimates and 95% CIs:
#   --------------------------------------------------------------
# Apparent prevalence *                  0.51 (0.46, 0.55)
# True prevalence *                      0.50 (0.45, 0.55)
# Sensitivity *                          0.93 (0.89, 0.96)
# Specificity *                          0.91 (0.87, 0.94)
# Positive predictive value *            0.91 (0.87, 0.94)
# Negative predictive value *            0.93 (0.88, 0.96)
# Positive likelihood ratio              10.28 (6.83, 15.49)
# Negative likelihood ratio              0.08 (0.05, 0.13)
# False T+ proportion for true D- *      0.09 (0.06, 0.13)
# False T- proportion for true D+ *      0.07 (0.04, 0.11)
# False T+ proportion for T+ *           0.09 (0.06, 0.13)
# False T- proportion for T- *           0.07 (0.04, 0.12)
# Correctly classified proportion *      0.92 (0.89, 0.94)
# --------------------------------------------------------------

dat_DS_GPT <- table(accuracy_matrix[c(2,3)])
rval_DS_GPT <- epi.tests(dat_DS_GPT, method = "exact", digits = 2, conf.level = 0.95)
