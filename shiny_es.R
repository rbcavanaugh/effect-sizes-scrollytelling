library(tidyverse)
library(brms)
library(here)
library(janitor)
library(SingleCaseES)
library(lme4)
library(brms)
library(bayesplot)
library(tidybayes)
library(bayestestR)
library(broom.mixed)

data_gen <- read.csv(here('data', 'data_sim_scd_8-11_unif_30.csv'))

################# shiny app data: #########################
ids = c(31, 34, 12, 15, 51, 56, 57, 84, 93, 99)

df.brm.30 <- data_gen %>%
  filter(condition == 1) %>% # treated words
  group_by(sub_id, condition, period) %>%
  mutate(sumCorr = sum(response), # number correct
         trials = 30, # number of targets
         phase = as.factor(phase)) %>%
  filter(item_id_crossed == 1) %>% # for distinct rows
  clean_names(case = "lower_camel")%>% ungroup() %>% # fix nmes
  select(subId, period, phase, slopeChange, sumCorr, trials)

df.brm.30 %>%
  filter(subId %in% ids) %>%
  mutate(subId = as.factor(subId)) %>%
  #filter(subId <=50) %>%
  ggplot(aes(x = period, y = sumCorr/30, color = subId, shape = phase)) +
  geom_point() +
  geom_line() + 
  #facet_wrap(~subId) + 
  geom_vline(aes(xintercept = 5.5), alpha = .5) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  scale_x_continuous(labels = NULL, breaks = NULL) +
  theme_grey(base_size = 12) +
  theme(legend.position = 'none') +
  ylab('Accuracy') +
  xlab(NULL)


# 35 = NR
# 95 = slope but no change
# 32 = slope and level
# 68 = level only
# 9 = minimimal baseline SD
# 48 = level and slope


shiny.df <- data_gen %>%
  filter(sub_id %in% ids) %>%
  filter(condition == 1) %>% # treated words
  group_by(sub_id, period) %>%
  mutate(sumCorr = sum(response), # number correct
         trials = 30, # number of targets
         phase = as.factor(phase)) %>%
  clean_names(case = "lower_camel")%>% ungroup() %>% # fix nmes
  select(sub_id = subId, session = period, phase, slopeChange, sumCorr, trials, response, item_id_crossed = itemIdCrossed)



# SMd, TAU, NAP

bl_var <- shiny.df %>% 
  filter(phase == 0) %>%
  group_by(sub_id, session) %>% 
  summarize(acc = mean(response)) %>%
  ungroup() %>%
  group_by(sub_id) %>%
  summarize(sd_baseline = sd(acc),
            baseline_perf = mean(acc))

df_summary <- shiny.df %>%
  group_by(sub_id, session) %>%
  summarize(mean_correct = mean(response),
            num_correct = sum(response)) %>%
  mutate(phase = ifelse(session <= 5, 0, 1)) %>%
  ungroup() %>%
  left_join(bl_var, by = 'sub_id')

df_smd <- df_summary %>%
  filter(sd_baseline != 0) 

df_tau <- df_summary %>%
  group_by(sub_id, phase) %>%
  summarize(slope = lm(num_correct~session)[[1]][[2]]) %>%
  filter(phase == 0) %>%
  select(-phase)

es_SMD <- batch_calc_ES(df_smd,
                        grouping = sub_id,
                        condition = phase,
                        outcome = num_correct,
                        improvement = 'increase',
                        ES = c('SMD', 'NAP', 'Tau', 'Tau-U'),
                        bias_correct = F,
                        session_number = session,
                        format = 'wide') %>% 
  clean_names() %>%
  select(sub_id, smd_est, nap_est, tau_u_est, tau_est) %>%
  left_join(df_tau, by = 'sub_id') %>%
  mutate(tau_u_est = ifelse(slope >.3, tau_u_est, tau_est)) %>%
  select(-tau_est, -slope)


df_smd_beeson_robey <- df_summary %>% 
  filter(sd_baseline != 0,
         session < 6 | session >14)

es_SMD_b_r <- batch_calc_ES(df_smd_beeson_robey,
                            grouping = sub_id,
                            condition = phase,
                            outcome = num_correct,
                            improvement = 'increase',
                            ES = c('SMD'),
                            bias_correct = F,
                            session_number = session,
                            format = 'wide') %>%
  clean_names() %>%
  select(sub_id, smd_br = smd_est)

effect_sizes_smd <- es_SMD %>%
  left_join(es_SMD_b_r, by = 'sub_id')



# PMG

# average correct at baseline
mean_baseline <- df_summary %>%
  select(sub_id, baseline_perf, sd_baseline) %>%
  distinct()

# performance at last treatment session
mean_15 <- df_summary %>%
  filter(session == 15) %>%
  select(sub_id, num_correct, session)

# calculate PMG
pmg <- mean_baseline %>%
  left_join(mean_15, by = 'sub_id') %>%
  mutate(trials = 30,
         mean_bl = baseline_perf*trials,
         gained = num_correct-mean_bl,
         pmg = (num_correct - mean_bl)/(trials-mean_bl),
         Quantile_baseline_performance = as.factor(ntile(mean_bl, 5)),
         Quantile_absolute_change = as.factor(ntile(gained, 5))) 


# join to main effect size dataframe
effect_sizes_smd_pmg <- effect_sizes_smd %>%
  left_join(pmg, by = 'sub_id') %>%
  select(sub_id, smd_est, smd_br, nap_est, tau_u_est, pmg, baseline_perf, sd_baseline, gained)



# GLMM


#split df into nested data frames by participant and condition
#split df into nested data frames by participant and condition
df.models <- shiny.df %>%
  mutate(target = as.factor(item_id_crossed),
         session2 = scale(session, center = T, scale = F)[,1],
         phase2 = ifelse(phase == 0, -1, 1))%>% # as.factor
  group_by(sub_id) %>%
  nest()

options(mc.cores = parallel::detectCores(logical = F))
# wiley and rapp modified by gilmore and meier
# session over all time points
fit_model_session <- function(data){
  mod <- glmer(response~phase2*session2 + 
                 (1 + session2|target), # random effects
               data = data,
               family = 'binomial',
               nAGQ = 0,# for model convergence with 500 models. not recommended typically run with nAGQ = 1 (default) with minimal difference in fixed effect estimates (r = .996)
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=100000))) 
  
  return(mod)
}

# run the model for each group
models_session <- df.models %>%
  mutate(mod = purrr::map(data, fit_model_session))


# unpack the models
models_coef_session <- models_session %>%
  mutate(tidy = map(mod, broom::tidy),
         glance = map(mod, broom::glance),
         n = map(data, nrow) %>% simplify(),
         session_bl = NA, bl_lower = NA, bl_upper = NA,
         method = 'session')%>%
  tidyr::unnest(c(tidy, glance)) %>%
  select(sub_id, term, estimate) %>%
  filter(term == 'session2' | term == 'phase2:session2') %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  clean_names() %>%
  mutate(tx_slope = exp(session2 + phase2_session2),
         slope_difference = exp(phase2_session2*2),
         avg_slope = exp(session2)) %>%
  select(sub_id, tx_slope, slope_difference, avg_slope)

# join back to previous ES ##################################

effect_sizes_smd_pmg_glm <- effect_sizes_smd_pmg %>%
  left_join(models_coef_session, by = 'sub_id') %>%
  rename(SMD = smd_est) 



##### BMEM



#model function
fit_brm <- function(data){
  mod <- update(test_mod, newdata = data)
  return(mod)
}

# test-model data
data <- shiny.df %>%
  mutate(phase = as.factor(phase),
         target = as.factor(item_id_crossed)) %>%
  filter(sub_id == 99)

# full models data
df.brm <- shiny.df %>%
  mutate(phase = as.factor(phase),
         target = as.factor(item_id_crossed))

test_mod <- brm(response ~ 0 + Intercept + session + phase + slopeChange + (1|target) +
                  (0+slopeChange|target) + (0+phase|target) + (0+session|target), 
                family = bernoulli(),
                data = data,
                cores = 4,
                chains = 4,
                inits = 'random',
                iter = 1000,
                control = list(adapt_delta = .95),
                warmup = 500,
                backend = 'cmdstan',
                prior = c( prior(normal(0, 3), class = b, coef = session),
                           prior(normal(0, 3), class = b, coef = phase1),
                           prior(normal(0, 3), class = b, coef = slopeChange),
                           prior(normal(0, 3), class = b, coef = Intercept),
                           prior(cauchy(0, 5), class = sd, coef = phase0, group = 'target'),
                           prior(cauchy(0, 5), class = sd, coef = phase1, group = 'target'),
                           prior(cauchy(0, 5), class = sd, coef = session, group = 'target'),
                           prior(cauchy(0, 5), class = sd, coef = slopeChange, group = 'target')),
                seed = 1000,
                silent = T)
summary(test_mod)


# nested data frames 30
df.brm.nest.30 <- df.brm %>%
  group_by(sub_id) %>%
  nest()

# run all 10 models
models_brm <- df.brm.nest.30 %>%
  mutate(mod = map(data, fit_brm))


options(dplyr.summarise.inform = FALSE) # for progress bar

# load these and run one at a time. VERY LARGE FILES
#load(file = here('data', 'mods500_30.Rdata'))

# 30 items
es_list.tx <- list()

for(i in 1:10){
  
  prediction_data = models_brm$data[[i]] %>%
    select(-response) %>%
    filter(session == 5 | session == 15)
  
  mod = models_brm$mod[[i]]
  sub_id = models_brm$sub_id[[i]]
  
  es <- add_fitted_draws(mod, newdata = prediction_data, pred = 'value', seed = 42) %>% # predict each word at 3 & 15
    ungroup() %>%
    mutate(time_point = ifelse(session == 5, 'entry', 'exit')) %>% # rename these time points
    select(time_point, target, value = .value, draw = .draw) %>% # select entry and exit
    group_by(draw, time_point) %>% # for each sample and time point
    summarize(num_corr = sum(value)) %>% # count the number of correct words predicted
    pivot_wider(names_from = time_point, values_from = num_corr) %>% # wider to substract
    mutate(effect_size = exit - entry) %>% # subtract
    ungroup() %>%  # remove grouping
    select(effect_size) %>% # only need one variable
    median_hdi(.width = .9) %>%
    mutate(sub_id = sub_id)
  
  es_list.tx[[i]] <- es
}

brms_all <- bind_rows(es_list.tx) %>%
  select(sub_id, effect_size)%>%
  ungroup()


es_list.tx2 <- list()
for(i in 1:10){
  
  with = models_brm$data[[i]] %>%
    filter(session == 15)
  
  without = models_brm$data[[i]] %>%
    filter(session == 15) %>%
    mutate(slope_change = 0,
           phase = as.factor(0))
  
  prediction_data = bind_rows(with, without)
  
  mod = models_brm$mod[[i]]
  sub_id = models_brm$sub_id[[i]]
  
  es <- add_fitted_draws(mod, newdata = prediction_data, pred = 'value', seed = 42) %>% # predict each word at 3 & 15
    ungroup() %>%
    mutate(time_point = ifelse(phase == 0, 'entry', 'exit')) %>% # rename these time points
    select(time_point, target, value = .value, draw = .draw) %>% # select entry and exit
    group_by(draw, time_point) %>% # for each sample and time point
    summarize(num_corr = sum(value)) %>% # count the number of correct words predicted
    pivot_wider(names_from = time_point, values_from = num_corr) %>% # wider to substract
    mutate(effect_size = exit - entry) %>% # subtract
    ungroup() %>%  # remove grouping
    select(effect_size) %>% # only need one variable
    median_hdi(.width = .9) %>%
    mutate(sub_id = sub_id)
  
  es_list.tx2[[i]] <- es

}

brms_2 <- bind_rows(es_list.tx2) %>%
  select(sub_id, effect_size_2 = effect_size)%>%
  ungroup()%>%
  left_join(brms_all, by = 'sub_id')



es_all <- effect_sizes_smd_pmg_glm %>%
  left_join(brms_2, by = 'sub_id') %>%
  select(sub_id, SMD, SMD_br = smd_br, pmg, tau_u = tau_u_est, nap = nap_est, glmm = tx_slope, brms = effect_size, brms2= effect_size_2, baseline_perf, sd_baseline, glmm_slope_difference = slope_difference, glmm_avg_slope = avg_slope ) 













write.csv(es_all, 'shiny_es.csv')
write.csv(df_summary, 'shiny_data.csv')
