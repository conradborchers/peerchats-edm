library(tidyverse)
library(lme4)

# Process Steps with multiple KCs
combine_kc_default <- function(row) {
  kc_columns <- grep("^kc_default", names(row), value = TRUE)
  kc_values <- row[kc_columns]
  kc_values <- kc_values[!is.na(kc_values)] %>% as.character()
  return(kc_values)
}

d <- read_csv('analysis-set-v2.csv') %>% 
  janitor::clean_names()

# Extract team role
d <- d %>% 
  mutate(student_role = case_when(
    str_detect(problem_name, 'solver') ~ 'solver',
    str_detect(problem_name, 'tutor') ~ 'tutor',
    str_detect(problem_name, 'apta') & substr(problem_name, nchar(problem_name), nchar(problem_name)) == 's' ~ 'solver',
    str_detect(problem_name, 'apta') & substr(problem_name, nchar(problem_name), nchar(problem_name)) == 't' ~ 'tutor',
    TRUE ~ 'single'
  )) 

# Retain collaborative session IDs, in this case, by referencing the solver
solver_sessions <- d %>% 
  filter(student_role == 'solver') %>% 
  count(session_id) %>% 
  pull(session_id)

# Descriptive collaboration statistics
length(solver_sessions)

d %>% 
  filter(session_id %in% solver_sessions) %>%
  mutate(tt = as.POSIXct(cf_tutor_event_time)) %>% 
  group_by(session_id) %>% 
  summarize(
    mm = max(tt, na.rm = TRUE) - min(tt, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  pull(mm) %>% 
  (function(v){v=v[!is.infinite(v)]}) %>% 
  sd() %>% 
  as.numeric() / 60

d %>% 
  filter(session_id %in% solver_sessions) %>% 
  group_by(session_id) %>% 
  summarize(n_msg = sum(!is.na(chat_msg))) %>% 
  ungroup() %>% 
  pull(n_msg) %>% 
  sd()

# Create AFM opportunity counts from log data
d_afm <- d %>%
  filter(attempt_at_step == 1) %>% 
    filter(step_name != 'done ButtonPressed') %>%
    filter((!is.na(kc_default)) | str_detect(step_name, 'solve')) %>%
  arrange(anon_student_id, problem_name, step_name, desc(time)) %>%
  distinct(anon_student_id, problem_name, step_name, .keep_all = TRUE) %>%
  mutate(outcome_bin = case_when(
    outcome == "CORRECT" ~ 1,
    outcome == "INCORRECT" ~ 0,
    outcome == "HINT" ~ 0
  )) %>%
  filter(!is.na(outcome_bin)) %>%
  mutate(kcs = apply(., 1, combine_kc_default)) %>%
  mutate(kc_length = kcs %>% map_int(length)) %>%
  filter(kc_length > 0) %>%
  select(session_id, transaction_id, anon_student_id, problem_name, step_name, time, kcs, student_role, cf_tool_event_time, outcome, outcome_bin)%>%
  unchop(kcs) %>%
  mutate(time = as.POSIXct(cf_tool_event_time)) %>%
  group_by(anon_student_id, kcs) %>%
  arrange(time) %>%
  reframe(transaction_id = transaction_id, problem_name = problem_name, time = time, session_id = session_id, n_opportunity = 1:n(), outcome_bin = outcome_bin) %>% 
  rename(kc_default = kcs)

# Create a predicted chat lable lookup table
d_labels <- d %>%
  filter(!is.na(predicted_label)) %>% 
  select(session_id, anon_student_id, problem_name, transaction_id, cf_tool_event_time, predicted_label) %>% 
  mutate(time = as.POSIXct(cf_tool_event_time))

join_this <- d %>% 
  select(session_id, transaction_id, student_role)
  
d_labels_join <- d_labels %>% 
  left_join(join_this, by=c('session_id', 'transaction_id')) %>% 
  select(session_id, transaction_id_msg = transaction_id, time, student_role, predicted_label)

# Annotate opportunities with the closest chat message type following the opportunity

dfs <- list()
i <- 0
for (sess in solver_sessions){
  d_afm_sess <- d_afm %>% 
    filter(session_id == sess)
  if (nrow(d_afm_sess)==0)
    next
  
  oppo_times <- d_afm_sess$time
  oppo_tids <- d_afm_sess$transaction_id
  d_labels_join_sess <- d_labels_join %>% 
    filter(session_id == sess)
  closest_times_indices <- sapply(d_labels_join_sess$time, function(x) {
    delta <- x - oppo_times
    delta <- delta[delta>=0]
    if (length(delta)==0)
      return(NA)
    closest_idx <- which.min(delta)
    return(closest_idx)
  })
  if(length(closest_times_indices)==0)
    next
  
  i <- i + 1
  if (sum(is.na(closest_times_indices))==length(closest_times_indices)){
    d_labels_join_sess['closest_time'] <- NA
    d_labels_join_sess['closest_tid'] <- NA
  } else {
    d_labels_join_sess['closest_time'] <- oppo_times[closest_times_indices]
    d_labels_join_sess['closest_tid'] <- oppo_tids[closest_times_indices]
  }
  dfs[[i]] <- d_labels_join_sess
}

d_labels_join_all <- do.call(rbind, dfs)

d_afm <- d_afm %>% 
  filter(kc_default != 'distribute-division') # 4 occurrences in whole data set

d_labels_join_all_final <- d_labels_join_all %>% 
  select(session_id, transaction_id = closest_tid, predicted_label)

d_labels_join_all_final

d_afm_labeled <- d_afm %>% 
  left_join(d_labels_join_all_final %>% distinct(transaction_id, .keep_all=TRUE), 
            by=c('session_id', 'transaction_id'))

# Then also do that for tutor and solver messages separately

# Tutor messages only:
dfs <- list()
i <- 0
for (sess in solver_sessions){
  d_afm_sess <- d_afm %>% 
    filter(session_id == sess)
  if (nrow(d_afm_sess)==0)
    next
  
  oppo_times <- d_afm_sess$time
  oppo_tids <- d_afm_sess$transaction_id
  d_labels_join_sess <- d_labels_join %>% 
    filter(session_id == sess) %>% 
    filter(student_role == 'tutor')
  closest_times_indices <- sapply(d_labels_join_sess$time, function(x) {
    delta <- x - oppo_times
    delta <- delta[delta>=0]
    if (length(delta)==0)
      return(NA)
    closest_idx <- which.min(delta)
    return(closest_idx)
  })
  if(length(closest_times_indices)==0)
    next
  
  i <- i + 1
  if (sum(is.na(closest_times_indices))==length(closest_times_indices)){
    d_labels_join_sess['closest_time'] <- NA
    d_labels_join_sess['closest_tid'] <- NA
  } else {
    d_labels_join_sess['closest_time'] <- oppo_times[closest_times_indices]
    d_labels_join_sess['closest_tid'] <- oppo_tids[closest_times_indices]
  }
  dfs[[i]] <- d_labels_join_sess
}

d_labels_join_all <- do.call(rbind, dfs)

d_labels_join_all_final <- d_labels_join_all %>% 
  select(session_id, transaction_id = closest_tid, predicted_label_tutor = predicted_label)

d_labels_join_all_final

d_afm_labeled <- d_afm_labeled %>% 
  left_join(d_labels_join_all_final %>% distinct(transaction_id, .keep_all=TRUE), 
            by=c('session_id', 'transaction_id'))

# Solver messages only:
dfs <- list()
i <- 0
for (sess in solver_sessions){
  d_afm_sess <- d_afm %>% 
    filter(session_id == sess)
  if (nrow(d_afm_sess)==0)
    next
  
  oppo_times <- d_afm_sess$time
  oppo_tids <- d_afm_sess$transaction_id
  d_labels_join_sess <- d_labels_join %>% 
    filter(session_id == sess) %>% 
    filter(student_role == 'solver')
  closest_times_indices <- sapply(d_labels_join_sess$time, function(x) {
    delta <- x - oppo_times
    delta <- delta[delta>=0]
    if (length(delta)==0)
      return(NA)
    closest_idx <- which.min(delta)
    return(closest_idx)
  })
  if(length(closest_times_indices)==0)
    next
  
  i <- i + 1
  if (sum(is.na(closest_times_indices))==length(closest_times_indices)){
    d_labels_join_sess['closest_time'] <- NA
    d_labels_join_sess['closest_tid'] <- NA
  } else {
    d_labels_join_sess['closest_time'] <- oppo_times[closest_times_indices]
    d_labels_join_sess['closest_tid'] <- oppo_tids[closest_times_indices]
  }
  dfs[[i]] <- d_labels_join_sess
}

d_labels_join_all <- do.call(rbind, dfs)

d_labels_join_all_final <- d_labels_join_all %>% 
  select(session_id, transaction_id = closest_tid, predicted_label_solver = predicted_label)

d_labels_join_all_final

d_afm_labeled <- d_afm_labeled %>% 
  left_join(d_labels_join_all_final %>% distinct(transaction_id, .keep_all=TRUE), 
            by=c('session_id', 'transaction_id'))

# Question 1: Do students in chats improve faster?

# Separate opportunities by whether Solvers were paired or not
d_afm_sep <- d_afm %>% 
  mutate(is_solver_session = session_id %in% solver_sessions) %>% 
  group_by(anon_student_id, kc_default, is_solver_session) %>%
  arrange(time) %>%
  reframe(transaction_id = transaction_id, problem_name = problem_name, time = time, session_id = session_id, 
          n_opportunity = 1:n(), outcome_bin = outcome_bin) %>% 
  select(transaction_id, anon_student_id, time, kc_default, is_solver_session, n_opportunity, outcome_bin) %>% 
  pivot_wider(names_from = is_solver_session, values_from = n_opportunity) %>% 
  rename(regular_opportunity = `FALSE`, paired_opportunity = `TRUE`) %>% 
  arrange(anon_student_id, time) %>% 
  group_by(anon_student_id, kc_default) %>% 
  fill(regular_opportunity, paired_opportunity, .direction = "down") %>% # keep respective other opportunity counts
  ungroup() %>% 
  replace_na(list(regular_opportunity = 0, paired_opportunity = 0)) %>% # Then fill with 0
  left_join(
    d_afm %>% select(transaction_id, kc_default, n_opportunity),
    by = c('transaction_id', 'kc_default')
  )

# Distribution of opportunities by student
d_afm_sep %>% 
  group_by(anon_student_id, kc_default) %>% 
  summarize(
    n_paired = max(paired_opportunity),
    n_unpaired = max(regular_opportunity)
  ) %>% 
  ungroup() %>% 
  group_by(anon_student_id) %>% 
  summarize(
    n_paired = sum(n_paired),
    n_unpaired = sum(n_unpaired)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = n_unpaired - n_paired)) +
  geom_density() +
  labs(x = "N Unpaired - N Paired Opportunities by Student", y = "Density") +
  theme_minimal()

m_null <- glmer(outcome_bin ~ (1 |anon_student_id) + kc_default + 
                  n_opportunity, d_afm_sep, family='binomial', nAGQ=0, verbose = 2)

m_ifa <- glmer(outcome_bin ~ (1 |anon_student_id) + kc_default + 
                 paired_opportunity + regular_opportunity, d_afm_sep, family='binomial', nAGQ=0, verbose = 2)

anova(m_null, m_ifa)

sjPlot::tab_model(m_ifa)

# Chat message opportunity distribution

d_tmp_tmp <- d_afm_labeled %>% 
  filter(session_id %in% solver_sessions)
d_afm_labeled$predicted_label %>% is.na %>% sum
d_afm_labeled$predicted_label %>% is.na %>% `!` %>%  sum

d_tmp_tmp %>% nrow
d_tmp_tmp$predicted_label %>% is.na %>% `!` %>%  sum
d_tmp_tmp$predicted_label %>% is.na %>%  sum

d_tmp_tmp$predicted_label_solver %>% is.na %>% `!` %>%  sum
d_tmp_tmp$predicted_label_tutor %>% is.na %>% `!` %>%  sum

d_tmp_tmp$predicted_label_solver %>% table()
d_tmp_tmp$predicted_label_tutor %>% table()

a = d_tmp_tmp$predicted_label_solver %>% is.na %>% `!` %>%  sum
b=nrow(d_tmp_tmp)
a/b

# Post-hoc analysis Are chat messages related to engagement?

d_engagement <- d %>% 
  group_by(anon_student_id) %>% 
  summarize(
    n_msg = sum(!is.na(predicted_label)),
    n_oppo = sum(attempt_at_step==1, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  select(n_msg, n_oppo) 

plot(d_engagement$n_msg, d_engagement$n_oppo)

cor.test(d_engagement$n_msg, d_engagement$n_oppo, method = 'spearman')

# RQ3: IFA by chat message type

d_ifa <- d_afm_labeled %>% 
  mutate(chat_type = case_when(
    predicted_label_tutor == 0 ~ 'tutor_minimal',
    predicted_label_tutor == 1 ~ 'tutor_facilitative',
    predicted_label_tutor == 2 ~ 'tutor_constructive',
    predicted_label_solver == 0 ~ 'solver_minimal',
    predicted_label_solver == 1 ~ 'solver_facilitative',
    predicted_label_solver == 2 ~ 'solver_constructive',
    is.na(predicted_label) | is.na(predicted_label_tutor) | is.na(predicted_label_solver) ~ 'nochat',
    TRUE ~ 'nochat'
  )) %>% 
  group_by(anon_student_id, kc_default, chat_type) %>%
  arrange(time) %>%
  reframe(transaction_id = transaction_id, problem_name = problem_name, time = time, session_id = session_id, 
          n_opportunity = 1:n(), outcome_bin = outcome_bin) %>% 
  select(transaction_id, anon_student_id, time, kc_default, chat_type, n_opportunity, outcome_bin) %>% 
  pivot_wider(names_from = chat_type, values_from = n_opportunity) %>% 
  arrange(anon_student_id, time) %>% 
  group_by(anon_student_id, kc_default) %>% 
  fill(nochat, tutor_minimal, tutor_facilitative, solver_minimal, solver_facilitative, solver_constructive, .direction = "down") %>% # keep respective other opportunity counts
  ungroup() %>% 
  replace_na(list(nochat = 0, tutor_minimal = 0, tutor_facilitative = 0, solver_minimal = 0, solver_facilitative = 0, solver_constructive = 0)) # Then fill with 0

# Need to shift opportunities one observation forward as learning will only show after the chat opportunity has been experienced
d_ifa <- d_ifa %>% 
  mutate(
    nochat = lag(nochat, 1),
    tutor_minimal = lag(tutor_minimal, 1),
    tutor_facilitative = lag(tutor_facilitative, 1),
    solver_minimal = lag(solver_minimal, 1),
    solver_facilitative = lag(solver_facilitative, 1),
    solver_constructive = lag(solver_constructive, 1)
  ) %>% 
  replace_na(list(nochat = 0, tutor_minimal = 0, tutor_facilitative = 0, solver_minimal = 0, solver_facilitative = 0, solver_constructive = 0)) %>% 
  left_join(
    d_afm %>% select(transaction_id, kc_default, n_opportunity),
    by = c('transaction_id', 'kc_default')
  )

# Intercorrelations of opportunity types on the student level
d_corr <- d_ifa %>% 
  group_by(anon_student_id, kc_default) %>% 
  summarize(
    nochat = max(nochat),
    tutor_minimal = max(tutor_minimal),
    tutor_facilitative = max(tutor_facilitative),
    solver_minimal = max(solver_minimal),
    solver_facilitative = max(solver_facilitative),
    solver_constructive = max(solver_constructive)
  ) %>% 
  ungroup() %>% 
  group_by(anon_student_id) %>% 
  summarize(
    nochat = sum(nochat),
    tutor_minimal = sum(tutor_minimal),
    tutor_facilitative = sum(tutor_facilitative),
    solver_minimal = sum(solver_minimal),
    solver_facilitative = sum(solver_facilitative),
    solver_constructive = sum(solver_constructive)
  ) %>% 
  ungroup()

cor_matrix <- d_corr %>% 
  select(nochat, tutor_minimal, tutor_facilitative, solver_minimal, solver_facilitative, solver_constructive)

cor_result <- psych::corr.test(cor_matrix)
cor_matrix_with_pvalues <- cor_result$r
p_values <- cor_result$p

cor_matrix_with_pvalues <- round(cor_matrix_with_pvalues, 2)

cor_matrix_with_pvalues
round(p_values, 3)

# IFA model comparisons via likelihood-ratio tests, null model being AFM
m_null <- glmer(outcome_bin ~ (1 |anon_student_id) + kc_default + 
                 n_opportunity, d_ifa, family='binomial', nAGQ=0, verbose = 2)

m_ifa <- glmer(outcome_bin ~ (1 |anon_student_id) + kc_default + 
                 nochat + tutor_minimal + tutor_facilitative + solver_minimal + 
                 solver_facilitative + solver_constructive, d_ifa, family='binomial', nAGQ=0, verbose = 2)

anova(m_null, m_ifa)

sjPlot::tab_model(m_ifa)

# Exploratory analysis of chats after correct and incorrect attempts
d_ifa <- d_afm_labeled %>% 
  mutate(chat_type = case_when(
    (!is.na(predicted_label_tutor)) & (lag(outcome_bin, 1)==1) ~ 'chat_postcorrect_tutor',
    (!is.na(predicted_label_tutor)) & (lag(outcome_bin, 1)==0) ~ 'chat_posterror_tutor',
    (!is.na(predicted_label_solver)) & (lag(outcome_bin, 1)==1) ~ 'chat_postcorrect_solver',
    (!is.na(predicted_label_solver)) & (lag(outcome_bin, 1)==0) ~ 'chat_posterror_solver',
    TRUE ~ 'nochat'
  )) %>% 
  group_by(anon_student_id, kc_default, chat_type) %>%
  arrange(time) %>%
  reframe(transaction_id = transaction_id, problem_name = problem_name, time = time, session_id = session_id, 
          n_opportunity = 1:n(), outcome_bin = outcome_bin) %>% 
  select(transaction_id, anon_student_id, time, kc_default, chat_type, n_opportunity, outcome_bin) %>% 
  pivot_wider(names_from = chat_type, values_from = n_opportunity) %>% 
  arrange(anon_student_id, time) %>% 
  group_by(anon_student_id, kc_default) %>% 
  fill(nochat, chat_postcorrect_tutor, chat_posterror_tutor,
       chat_posterror_solver, chat_postcorrect_solver,
       .direction = "down") %>% # keep respective other opportunity counts
  ungroup() %>% 
  replace_na(list(nochat = 0, chat_postcorrect_tutor = 0, chat_posterror_tutor = 0,
                  chat_posterror_solver = 0, chat_postcorrect_solver = 0
                  )) # Then fill with 0

d_ifa <- d_ifa %>% 
  mutate(
    nochat = lag(nochat, 1),
    chat_postcorrect_tutor = lag(chat_postcorrect_tutor, 1),
    chat_posterror_tutor = lag(chat_posterror_tutor, 1),
    chat_posterror_solver = lag(chat_posterror_solver, 1),
    chat_postcorrect_solver = lag(chat_postcorrect_solver, 1)
  ) %>% 
  replace_na(list(nochat = 0, chat_postcorrect_tutor = 0, chat_posterror_tutor = 0,
                  chat_posterror_solver = 0, chat_postcorrect_solver = 0)) %>% 
  left_join(
    d_afm %>% select(transaction_id, kc_default, n_opportunity),
    by = c('transaction_id', 'kc_default')
  )

m_null <- glmer(outcome_bin ~ (1 |anon_student_id) + kc_default + 
                  n_opportunity, d_ifa, family='binomial', nAGQ=0, verbose = 2)

m_ifa <- glmer(outcome_bin ~ (1 |anon_student_id) + kc_default + 
                 nochat + chat_postcorrect_tutor + chat_posterror_tutor+
                  chat_postcorrect_solver + chat_posterror_solver, 
                d_ifa, family='binomial', nAGQ=0, verbose = 2)

anova(m_null, m_ifa)

sjPlot::tab_model(m_ifa)

# Post-hoc Individualized learning rates comparison across tutor pairing
m_iafm <- glmer(outcome_bin ~ (1 + n_opportunity|anon_student_id) + kc_default*n_opportunity, d_ifa, family='binomial', nAGQ=0, verbose = 2)

# Most common tutor message grouping
student_group <- d_ifa %>% 
  mutate(grouping = case_when(
    tutor_facilitative == 0 & tutor_minimal == 0 ~ 'No Chats',
    tutor_facilitative == 1 & tutor_minimal == 0 ~ 'Facilitative',
    tutor_facilitative == 0 & tutor_minimal == 1 ~ 'Minimal',
    TRUE ~ 'other'
  )) %>% 
  count(anon_student_id, grouping) %>% 
  arrange(desc(n)) %>% 
  distinct(anon_student_id, grouping)

# Extract individualized learning rates for comparison
d_test <- ranef(m_iafm)$anon_student_id %>% 
  rownames_to_column('anon_student_id') %>% 
  select(anon_student_id, n_opportunity) %>% 
  tibble() %>% 
  inner_join(student_group, by = 'anon_student_id')

d_test %>% 
  group_by(grouping) %>% 
  summarize(m = mean(n_opportunity), n=n()) %>% 
  ungroup()

# Group-wise summary
summary_data <- d_test %>%
  group_by(grouping) %>%
    summarize(m = mean(n_opportunity),
              ci_lower = mean(n_opportunity) - qt(0.975, n() - 1) * (sd(n_opportunity) / sqrt(n())),
              ci_upper = mean(n_opportunity) + qt(0.975, n() - 1) * (sd(n_opportunity) / sqrt(n())),
              se = (sd(n_opportunity) / sqrt(n()))
              ) %>%
  ungroup()

# Perform post-hoc test (Tukey's HSD)
post_hoc_test <- TukeyHSD(aov(n_opportunity ~ grouping, data = d_test))

post_hoc_test

p <- ggplot(summary_data, aes(x = grouping, y = m)) +
  geom_point(position = position_dodge(width = 0.9), size = 3) + # Use geom_point instead of geom_bar
  geom_errorbar(data = summary_data, aes(ymin = ci_lower,#m-se,
                                         ymax = ci_upper),#m+se),
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(title = "Individualized Learning Rate by Tutor Style",
       x = "Tutor Style",
       y = "Mean iAFM Learning Rate") +
  theme_minimal()

pdf_width <- 14  # Width in inches
pdf_height <- pdf_width / 4  # Half the width to make it twice as wide as high

p
ggsave("apta-meancomp.pdf", p, width = pdf_width, height = pdf_height)

# Distribution of opportunities plot
d_plot <- d_ifa %>% 
  group_by(anon_student_id, kc_default) %>% 
  summarize(
    solver_facilitative = max(solver_facilitative),
    solver_minimal = max(solver_minimal),
    solver_constructive = max(solver_constructive),
    tutor_facilitative = max(tutor_facilitative),
    tutor_minimal = max(tutor_minimal)
  ) %>% 
  ungroup() %>% 
  group_by(anon_student_id) %>% 
  summarize(
    solver_facilitative = sum(solver_facilitative),
    solver_minimal = sum(solver_minimal),
    solver_constructive = sum(solver_constructive),
    tutor_facilitative = sum(tutor_facilitative),
    tutor_minimal = sum(tutor_minimal)
  ) %>% 
  ungroup() %>% 
  select(-solver_constructive)
  
p1 <- d_plot %>%
  group_by(solver_facilitative, solver_minimal) %>%
  mutate(tile_count = n()) %>% 
  ggplot(aes(x = solver_minimal, y = solver_facilitative)) +
  geom_tile(aes(fill = tile_count)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", labels = c(1, 50, 100, 150, 200)) +  # Customize the color scale
  labs(
    title = "Facilitative vs. Minimal Solver Opportunities",
    y = "Solver Facilitative Chat Opportunities",
    x = "Solver Minimal Chat Opportunities",
    fill = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid = element_blank()
  )

p2 <- d_plot %>%
  group_by(tutor_facilitative, tutor_minimal) %>%
  mutate(tile_count = n()) %>% 
  ggplot(aes(x = tutor_minimal, y = tutor_facilitative)) +
  geom_tile(aes(fill = tile_count)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", labels = c(1, 50, 100, 150, 200)) +  # Customize the color scale
  labs(
    title = "Facilitative vs. Minimal Tutor Opportunities",
    y = "Tutor Facilitative Chat Opportunities",
    x = "Tutor Minimal Chat Opportunities",
    fill = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid = element_blank()
  )

arranged_plots <- gridExtra::grid.arrange(p1, p2, ncol = 2)

pdf_width <- 14  # Width in inches
pdf_height <- pdf_width / 4  # Half the width to make it twice as wide as high

# Save the arranged plots as a high-res PDF
ggsave("apta-heatmap.pdf", arranged_plots, width = pdf_width, height = pdf_height)

# % descriptive statistics related to heatmap

d_plot <- d_ifa %>% 
  group_by(anon_student_id, kc_default) %>% 
  summarize(
    nochat = max(nochat),
    solver_facilitative = max(solver_facilitative),
    solver_minimal = max(solver_minimal),
    solver_constructive = max(solver_constructive),
    tutor_facilitative = max(tutor_facilitative),
    tutor_minimal = max(tutor_minimal)
  ) %>% 
  ungroup() %>% 
  group_by(anon_student_id) %>% 
  summarize(
    nochat = sum(nochat),
    solver_facilitative = sum(solver_facilitative),
    solver_minimal = sum(solver_minimal),
    solver_constructive = sum(solver_constructive),
    tutor_facilitative = sum(tutor_facilitative),
    tutor_minimal = sum(tutor_minimal)
  ) %>% 
  ungroup()

d_plot$nochat %>% sum()
sum((d_plot$solver_facilitative + d_plot$solver_minimal + d_plot$solver_constructive==0))
sum((d_plot$solver_facilitative + d_plot$solver_minimal + d_plot$solver_constructive==0))/nrow(d_plot)
sum((d_plot$tutor_facilitative + d_plot$tutor_minimal)==0)/nrow(d_plot)
sum(((d_plot$solver_facilitative + d_plot$solver_minimal + d_plot$solver_constructive + d_plot$tutor_facilitative + d_plot$tutor_minimal)==0))/nrow(d_plot)
