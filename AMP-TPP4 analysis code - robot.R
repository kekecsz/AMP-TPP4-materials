
### Confirmatory data analysis code of the AMP-TPP4 - robot trials at preregistration

#############################################################
#                                                           #
#                        Packages                           #
#                                                           #
#############################################################

library(lme4) # for glmer()
library(tidyverse)


#############################################################
#                                                           #
#                   Custom functions                        #
#                                                           #
#############################################################

### to convert logit to probability
### this is used for conversion of the results of the
### logistic regression to the probability scale

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}


#############################################################
#                                                           #
#                 Set analysis parameters                   #
#                                                           #
#############################################################

max_num_trials = 217800
Inference_threshold_NHST = 0.05
M0_prob = 0.5 # probability of correct guesses if M0 is true


#############################################################
#                                                           #
#                Load and manage data                       #
#                                                           #
#############################################################

### include link to a folder containing all .csv files included in the https://github.com/amp4-tpp/transparent-psi-results/tree/master/live_data repository

data_folder <- "C:\\Users\\User\\Documents\\Temp\\AMP-TPP-4-data_final\\"

data_file_list <- list.files(data_folder)

list_of__s = lapply(strsplit(data_file_list, ''), function(x) which(x == '_'))
list_of__s_post = sapply(list_of__s, function(x) tail(x, n=1))
length_of_linknames = lapply(data_file_list, nchar)
start_dates = substr(data_file_list, list_of__s_post+1, unlist(length_of_linknames)-4)
data_file_list_ordered_pre = data_file_list[order(as.Date(start_dates, format = "%d-%m-%Y"))]

data_file_list_ordered_fullpath <- paste0(data_folder, data_file_list_ordered_pre)

raw_data_pre_list <- list(NA)

for(i in 1:length(data_file_list_ordered_fullpath)){
  raw_data_pre_list[[i]] <- read.csv(data_file_list_ordered_fullpath[i])
}

raw_data <- do.call("rbind", raw_data_pre_list)

raw_data[,"sides_match"] = as.factor(tolower(as.logical(raw_data[,"sides_match"])))
raw_data[,"participant_ID"] = as.factor(raw_data[,"participant_ID"])

# sides matching as a numerical variable
raw_data[,"sides_match_numeric"] = as.numeric(as.logical(raw_data[,"sides_match"]))

# Only include sessions conducted with the AMP-TPP4 live account
# robot account experimenter ID hash: 837c6b4d9ef96c13798a57a3f2ba4eddc0eda39db79419ae5e6c30763aa57911
data_nontest = raw_data %>% 
  filter(experimenter_ID_code == "837c6b4d9ef96c13798a57a3f2ba4eddc0eda39db79419ae5e6c30763aa57911")     # live account

# add a row_counter, which will be useful to distinguish data coming in after the stopping rule was met.
data_nontest[, "row_counter"] = 1:nrow(data_nontest)

data_nontest_trials = data_nontest[!is.na(data_nontest[, "trial_number"]),]

## extract data from erotic trials 
data_nontest_trials_erotic = data_nontest_trials[data_nontest_trials[, "reward_type"] == "erotic", ]

# drop unused factor levels
data_nontest_trials_erotic[,"participant_ID"] = droplevels(data_nontest_trials_erotic[,"participant_ID"])

# drop any data that is above the maximum trial size
if(nrow(data_nontest_trials_erotic) > max_num_trials){
  data_nontest_trials_erotic_maxtrialnum = data_nontest_trials_erotic[1:max_num_trials,]
} else {data_nontest_trials_erotic_maxtrialnum = data_nontest_trials_erotic}


## extract data from non-erotic trials 
data_nontest_trials_nonerotic = data_nontest_trials[data_nontest_trials[, "reward_type"] == "neutral", ]

# drop unused factor levels
data_nontest_trials_nonerotic[,"participant_ID"] = droplevels(data_nontest_trials_nonerotic[,"participant_ID"])

# drop any data that is above the maximum trial size
if(nrow(data_nontest_trials_nonerotic) > max_num_trials){
  data_nontest_trials_nonerotic_maxtrialnum = data_nontest_trials_nonerotic[1:max_num_trials,]
} else {data_nontest_trials_nonerotic_maxtrialnum = data_nontest_trials_nonerotic}

######################################################################
#                                                                    #
#                    Primary confirmatory test                       #
#                                                                    #
######################################################################

#### Hypothesis 1

### Primary confirmatory analysis: mixed model binary logistic regression

mod_mixed_H1 = glmer(sides_match_numeric ~ 1 + (1|participant_ID), data = data_nontest_trials_erotic_maxtrialnum, family = "binomial")

estimate_mixed_H1 = summary(mod_mixed_H1)$coefficients[1,1]
se_mixed_H1 = summary(mod_mixed_H1)$coefficients[1,2]

wald_ci_mixed_logit_H1 <- c(estimate_mixed_H1 - se_mixed_H1* qnorm(1-((Inference_threshold_NHST)/2)),
                            estimate_mixed_H1 + se_mixed_H1* qnorm(1-((Inference_threshold_NHST)/2)))
wald_ci_mixed_H1 = logit2prob(wald_ci_mixed_logit_H1)

CI_lower_mixed_H1 = wald_ci_mixed_H1[1]
CI_upper_mixed_H1 = wald_ci_mixed_H1[2]

# results of the mixed model analysis
CI_lower_mixed_H1
CI_upper_mixed_H1


# final statistical inference based on the mixed model
if(CI_upper_mixed_H1 < M0_prob){conclusion = "M1"} else if(CI_lower_mixed_H1 > M0_prob){conclusion = "M1"} else {conclusion = "Inconclusive"}
conclusion

### Robustness analysis using binomial test

successes_H1 = sum(as.logical(data_nontest_trials_erotic_maxtrialnum[,"sides_match"]))
total_n_of_trials_H1 = nrow(data_nontest_trials_erotic_maxtrialnum)


CI_lower_binomtest_H1 = binom.test(x = successes_H1, n = total_n_of_trials_H1, p = 0.5, conf.level = (1-Inference_threshold_NHST))$conf.int[1]
CI_upper_binomtest_H1 = binom.test(x = successes_H1, n = total_n_of_trials_H1, p = 0.5, conf.level = (1-Inference_threshold_NHST))$conf.int[2]

## results of the binomial test
CI_lower_binomtest_H1
CI_upper_binomtest_H1




#### Hypothesis 2

### Primary confirmatory analysis: mixed model binary logistic regression

mod_mixed_H2 = glmer(sides_match_numeric ~ 1 + (1|participant_ID), data = data_nontest_trials_nonerotic_maxtrialnum, family = "binomial")

estimate_mixed_H2 = summary(mod_mixed_H2)$coefficients[1,1]
se_mixed_H2 = summary(mod_mixed_H2)$coefficients[1,2]

wald_ci_mixed_logit_H2 <- c(estimate_mixed_H2 - se_mixed_H2* qnorm(1-((Inference_threshold_NHST)/2)),
                            estimate_mixed_H2 + se_mixed_H2* qnorm(1-((Inference_threshold_NHST)/2)))
wald_ci_mixed_H2 = logit2prob(wald_ci_mixed_logit_H2)

CI_lower_mixed_H2 = wald_ci_mixed_H2[1]
CI_upper_mixed_H2 = wald_ci_mixed_H2[2]

# results of the mixed model analysis
CI_lower_mixed_H2
CI_upper_mixed_H2


# final statistical inference based on the mixed model
if(CI_upper_mixed_H2 < M0_prob){conclusion = "M1"} else if(CI_lower_mixed_H2 > M0_prob){conclusion = "M1"} else {conclusion = "Inconclusive"}
conclusion

### Robustness analysis using binomial test

successes_H2 = sum(as.logical(data_nontest_trials_nonerotic_maxtrialnum[,"sides_match"]))
total_n_of_trials_H2 = nrow(data_nontest_trials_nonerotic_maxtrialnum)


CI_lower_binomtest_H2 = binom.test(x = successes_H2, n = total_n_of_trials_H2, p = 0.5, conf.level = (1-Inference_threshold_NHST))$conf.int[1]
CI_upper_binomtest_H2 = binom.test(x = successes_H2, n = total_n_of_trials_H2, p = 0.5, conf.level = (1-Inference_threshold_NHST))$conf.int[2]

## results of the binomial test
CI_lower_binomtest_H2
CI_upper_binomtest_H2

############# Number of sessions ########################

## Number of sessions started
length(unique(data_nontest_trials$participant_ID))

## Number of sessions with at least 1 trial
sum(data_nontest_trials$trial_number == 1)

## Number of sessions with at least 1 erotic trial
length(unique(data_nontest_trials_erotic$participant_ID))

## Number of finished sessions
sum(data_nontest_trials$trial_number == 36)




finalrownames = c(row.names(data_nontest_trials_erotic_maxtrialnum)[nrow(data_nontest_trials_erotic_maxtrialnum)], row.names(data_nontest_trials_nonerotic_maxtrialnum)[nrow(data_nontest_trials_nonerotic_maxtrialnum)])
final_row_name = finalrownames[which.max(finalrownames)]

data_nontest_trials_untilstudystop = data_nontest_trials[1:which(row.names(data_nontest_trials) == final_row_name),]

## Number of sessions started until stopping point
length(unique(data_nontest_trials_untilstudystop$participant_ID))

## Number of sessions with at least 1 trial  until stopping point
sum(data_nontest_trials_untilstudystop$trial_number == 1)

## Number of finished sessions until stopping point
sum(data_nontest_trials_untilstudystop$trial_number == 36)




## Number of sessions started that were included in main analysis - erotic
length(unique(data_nontest_trials_erotic_maxtrialnum$participant_ID))

## Number of sessions started that were included in main analysis - nonerotic
length(unique(data_nontest_trials_nonerotic_maxtrialnum$participant_ID))


############ Plot results #############################

estimate_prob_mixed_H1 = logit2prob(estimate_mixed_H1)
estimate_prob_mixed_H2 = logit2prob(estimate_mixed_H2)

plot_data = data.frame(
  trial_type = c("erotic", "neutral"),
  probability_of_success = c(estimate_prob_mixed_H1, estimate_prob_mixed_H2),
  CI_lb = c(CI_lower_mixed_H1, CI_lower_mixed_H2),
  CI_ub = c(CI_upper_mixed_H1, CI_upper_mixed_H2),
  p_value = paste0("p = ", round(c(summary(mod_mixed_H1)$coefficients[1,4], summary(mod_mixed_H2)$coefficients[1,4]),3)))

plot_data %>% 
  ggplot() +
  aes(x = trial_type, y = probability_of_success, label = p_value) +
  geom_point(shape = 21, size = 4, fill = "white") +
  geom_errorbar(aes(x=trial_type, ymin=CI_lb, ymax=CI_ub), width=0.1, size=1) +
  geom_text(aes(y = CI_lb-0.0005))+
  ylim(c(0.49, 0.51))+
  geom_hline(yintercept = 0.5)


######################################################################
#                                                                    #
#               Do analysis sequentially for plotting                #
#                                                                    #
######################################################################

### The analysis below takes several hours

trial_cap = seq(100, max_num_trials, by = 100)


### Erotic trials - sequential analysis

wide_plot_data_erotic = data.frame(
  probability_of_success_mixmod = NA,
  CI_lb_mixmod = NA,
  CI_ub_mixmod = NA,
  probability_of_success_binom = NA,
  CI_lb_binom = NA,
  CI_ub_binom = NA,
  trial_number = NA)



for(i in 1:length(trial_cap)){
  
  print(paste0("computing for trial = ", trial_cap[i]))
  
  data_segment = data_nontest_trials_erotic_maxtrialnum[1:trial_cap[i],]
  
  
  #### Hypothesis 1
  
  ### Primary confirmatory analysis: mixed model binary logistic regression
  
  mod_mixed_H1 = glmer(sides_match_numeric ~ 1 + (1|participant_ID), data = data_segment, family = "binomial")
  
  estimate_mixed_H1 = summary(mod_mixed_H1)$coefficients[1,1]
  se_mixed_H1 = summary(mod_mixed_H1)$coefficients[1,2]
  
  wald_ci_mixed_logit_H1 <- c(estimate_mixed_H1 - se_mixed_H1* qnorm(1-((Inference_threshold_NHST)/2)),
                              estimate_mixed_H1 + se_mixed_H1* qnorm(1-((Inference_threshold_NHST)/2)))
  wald_ci_mixed_H1 = logit2prob(wald_ci_mixed_logit_H1)
  
  CI_lower_mixed_H1 = wald_ci_mixed_H1[1]
  CI_upper_mixed_H1 = wald_ci_mixed_H1[2]
  
  # results of the mixed model analysis
  wide_plot_data_erotic[i, "probability_of_success_mixmod"] = logit2prob(estimate_mixed_H1)
  wide_plot_data_erotic[i, "CI_lb_mixmod"] = CI_lower_mixed_H1
  wide_plot_data_erotic[i, "CI_ub_mixmod"] = CI_upper_mixed_H1
  
  
  
  # final statistical inference based on the mixed model
  if(CI_upper_mixed_H1 < M0_prob){conclusion = "M1"} else if(CI_lower_mixed_H1 > M0_prob){conclusion = "M1"} else {conclusion = "Inconclusive"}
  conclusion
  
  ### Robustness analysis using binomial test
  
  successes_H1 = sum(as.logical(data_segment[,"sides_match"]))
  total_n_of_trials_H1 = nrow(data_segment)
  
  
  CI_lower_binomtest_H1 = binom.test(x = successes_H1, n = total_n_of_trials_H1, p = 0.5, conf.level = (1-Inference_threshold_NHST))$conf.int[1]
  CI_upper_binomtest_H1 = binom.test(x = successes_H1, n = total_n_of_trials_H1, p = 0.5, conf.level = (1-Inference_threshold_NHST))$conf.int[2]
  
  ## results of the binomial test
  
  wide_plot_data_erotic[i, "probability_of_success_binom"] = binom.test(x = successes_H1, n = total_n_of_trials_H1, p = 0.5, conf.level = (1-Inference_threshold_NHST))$estimate
  wide_plot_data_erotic[i, "CI_lb_binom"] = CI_lower_binomtest_H1
  wide_plot_data_erotic[i, "CI_ub_binom"] = CI_upper_binomtest_H1
  
  wide_plot_data_erotic[i, "trial_number"] = trial_cap[i]
}


### Nonerotic trials - sequential analysis

wide_plot_data_nonerotic = data.frame(
  probability_of_success_mixmod = NA,
  CI_lb_mixmod = NA,
  CI_ub_mixmod = NA,
  probability_of_success_binom = NA,
  CI_lb_binom = NA,
  CI_ub_binom = NA,
  trial_number = NA)



for(i in 1:length(trial_cap)){
  
  print(paste0("computing for trial = ", trial_cap[i]))
  
  data_segment = data_nontest_trials_nonerotic_maxtrialnum[1:trial_cap[i],]
  
  
  #### Hypothesis 1
  
  ### Primary confirmatory analysis: mixed model binary logistic regression
  
  mod_mixed_H1 = glmer(sides_match_numeric ~ 1 + (1|participant_ID), data = data_segment, family = "binomial")
  
  estimate_mixed_H1 = summary(mod_mixed_H1)$coefficients[1,1]
  se_mixed_H1 = summary(mod_mixed_H1)$coefficients[1,2]
  
  wald_ci_mixed_logit_H1 <- c(estimate_mixed_H1 - se_mixed_H1* qnorm(1-((Inference_threshold_NHST)/2)),
                              estimate_mixed_H1 + se_mixed_H1* qnorm(1-((Inference_threshold_NHST)/2)))
  wald_ci_mixed_H1 = logit2prob(wald_ci_mixed_logit_H1)
  
  CI_lower_mixed_H1 = wald_ci_mixed_H1[1]
  CI_upper_mixed_H1 = wald_ci_mixed_H1[2]
  
  # results of the mixed model analysis
  wide_plot_data_nonerotic[i, "probability_of_success_mixmod"] = logit2prob(estimate_mixed_H1)
  wide_plot_data_nonerotic[i, "CI_lb_mixmod"] = CI_lower_mixed_H1
  wide_plot_data_nonerotic[i, "CI_ub_mixmod"] = CI_upper_mixed_H1
  
  
  
  # final statistical inference based on the mixed model
  if(CI_upper_mixed_H1 < M0_prob){conclusion = "M1"} else if(CI_lower_mixed_H1 > M0_prob){conclusion = "M1"} else {conclusion = "Inconclusive"}
  conclusion
  
  ### Robustness analysis using binomial test
  
  successes_H1 = sum(as.logical(data_segment[,"sides_match"]))
  total_n_of_trials_H1 = nrow(data_segment)
  
  
  CI_lower_binomtest_H1 = binom.test(x = successes_H1, n = total_n_of_trials_H1, p = 0.5, conf.level = (1-Inference_threshold_NHST))$conf.int[1]
  CI_upper_binomtest_H1 = binom.test(x = successes_H1, n = total_n_of_trials_H1, p = 0.5, conf.level = (1-Inference_threshold_NHST))$conf.int[2]
  
  ## results of the binomial test
  
  wide_plot_data_nonerotic[i, "probability_of_success_binom"] = binom.test(x = successes_H1, n = total_n_of_trials_H1, p = 0.5, conf.level = (1-Inference_threshold_NHST))$estimate
  wide_plot_data_nonerotic[i, "CI_lb_binom"] = CI_lower_binomtest_H1
  wide_plot_data_nonerotic[i, "CI_ub_binom"] = CI_upper_binomtest_H1
  
  wide_plot_data_nonerotic[i, "trial_number"] = trial_cap[i]
}




### plot mixed model CIs over time

wide_plot_data_erotic %>% 
  ggplot() +
  aes(x = trial_number, y = probability_of_success_mixmod) +
  geom_line() +
  geom_ribbon(aes(ymin = CI_lb_mixmod, ymax = CI_ub_mixmod), color = "grey", alpha = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_x_continuous(expand = c(0, 0)) +
  ylim(c(0.46, 0.54))


wide_plot_data_nonerotic %>% 
  ggplot() +
  aes(x = trial_number, y = probability_of_success_mixmod) +
  geom_line() +
  geom_ribbon(aes(ymin = CI_lb_mixmod, ymax = CI_ub_mixmod), color = "grey", alpha = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_x_continuous(expand = c(0, 0)) +
  ylim(c(0.46, 0.54))



######################################################################
#                                                                    #
#                    Side-balance analysis                           #
#                                                                    #
######################################################################

############# guessed side

balance_analysis_table_guessed_side = as.data.frame(matrix(data = NA, nrow = 2, ncol = 5))
names(balance_analysis_table_guessed_side) = c("N_side_left", "proportion_side_left", "CIlb_side_left", "CIub_side_left", "reward_type")
balance_analysis_table_guessed_side[,"reward_type"] = c("erotic", "nonerotic")

data_for_balance_analysis_list_guessed = list(data_nontest_trials_erotic_maxtrialnum, data_nontest_trials_nonerotic_maxtrialnum)

for(i in 1:length(data_for_balance_analysis_list_guessed)){
  data_for_balance_analysis = data_for_balance_analysis_list_guessed[[i]]
  
  balance_analysis_table_guessed_side[i, "N_side_left"] = sum(data_for_balance_analysis[,"guessed_side"] == "left")
  balance_analysis_table_guessed_side[i, "proportion_side_left"] = balance_analysis_table_guessed_side[i, "N_side_left"]/length(data_for_balance_analysis[,"guessed_side"] == "left" | data_for_balance_analysis[,"guessed_side"] == "right")
  balance_analysis_table_guessed_side[i, "CIlb_side_left"] = binom.test(x = balance_analysis_table_guessed_side[i, "N_side_left"], n = length(data_for_balance_analysis[,"guessed_side"]))$conf.int[1]
  balance_analysis_table_guessed_side[i, "CIub_side_left"] = binom.test(x = balance_analysis_table_guessed_side[i, "N_side_left"], n = length(data_for_balance_analysis[,"guessed_side"]))$conf.int[2]
  
}

############# target side

balance_analysis_table_target_side = as.data.frame(matrix(data = NA, nrow = 2, ncol = 5))
names(balance_analysis_table_target_side) = c("N_side_left", "proportion_side_left", "CIlb_side_left", "CIub_side_left", "reward_type")
balance_analysis_table_target_side[,"reward_type"] = c("erotic", "nonerotic")

data_for_balance_analysis_list_target = list(data_nontest_trials_erotic_maxtrialnum, data_nontest_trials_nonerotic_maxtrialnum)

for(i in 1:length(data_for_balance_analysis_list_target)){
  data_for_balance_analysis = data_for_balance_analysis_list_target[[i]]
  
  balance_analysis_table_target_side[i, "N_side_left"] = sum(data_for_balance_analysis[,"target_side"] == "left")
  balance_analysis_table_target_side[i, "proportion_side_left"] = balance_analysis_table_target_side[i, "N_side_left"]/length(data_for_balance_analysis[,"target_side"] == "left" | data_for_balance_analysis[,"target_side"] == "right")
  balance_analysis_table_target_side[i, "CIlb_side_left"] = binom.test(x = balance_analysis_table_target_side[i, "N_side_left"], n = length(data_for_balance_analysis[,"target_side"]))$conf.int[1]
  balance_analysis_table_target_side[i, "CIub_side_left"] = binom.test(x = balance_analysis_table_target_side[i, "N_side_left"], n = length(data_for_balance_analysis[,"target_side"]))$conf.int[2]
  
}

############# balance analysis plot

balance_analysis_table = rbind(balance_analysis_table_guessed_side, balance_analysis_table_target_side)
balance_analysis_table$guessed_or_target = rep(c("guessed", "target"), each = 2)

dodge <- position_dodge(width=0.2)

balance_analysis_table %>% 
  ggplot() + 
  aes(x = reward_type, y = proportion_side_left, color = guessed_or_target) +
  geom_point(position = dodge) +
  geom_errorbar(aes(ymin = CIlb_side_left, ymax = CIub_side_left), width = 0.1, position = dodge) +
  geom_hline(yintercept = 0.5, lty = "dashed") +
  ylim(c(0.46, 0.54))

