#Packages
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(Synth)
library(augsynth)
library(ggplot2)
library(ggtext)
library(Cairo)
library(geosphere)
library(tibble)
library(kableExtra)

#File Path
project_dir <- "C:/MYFILEPATH"

#Working Directory
setwd(project_dir)

#Load dataset
load("ZimServices.Rda")
gc()

CONST <- CONST %>%
  dplyr::mutate(clinics_outcome = clinics_outcome/(asqkm/100),
                roads_outcome = roads_outcome/1000,
                roads_outcome = roads_outcome/asqkm,
                opp = ifelse(zanupf == 0,1,0),
                opp = ifelse(is.na(zanupf),NA,opp))

CONST <- CONST %>%
  dplyr::arrange(month,constID) %>%
  group_by(month) %>%
  dplyr::mutate(t = ifelse(row_number() == 1,1,0)) %>%
  ungroup %>%
  dplyr::mutate(time = cumsum(t)) %>%
  dplyr::arrange(constID,month) %>%
  dplyr::rename(treat = clinic,
                treatO = clinico,
                placebo = plcb_clinic,
                placeboO = plcb_clinico)

CONST <- subset(CONST, select = -c(t))

#Prepare classic SCM
CONST <- CONST %>%
  arrange(constID,time)

#Identify treated constituencies
treat_info <- CONST %>%
  group_by(constID) %>%
  summarise(n_treat = sum(treatO > 0, na.rm = TRUE),
            first_treat = ifelse(any(treatO > 0),
                                     min(time[treatO > 0]),
                                     NA_real_),
            .groups = "drop")

#Remove multiply treated constituencies
single_treat <- treat_info %>%
  filter(n_treat == 1)

treated_units <- single_treat$constID

cat("Constituencies with exactly one treatment:",
    length(treated_units),
    "\n")

#Descriptive checks on parliamentary activity
activity <- CONST %>%
  group_by(constID) %>%
  summarise(totalQ = sum(pol_s + ser_s, na.rm = TRUE),
            .groups = "drop")

neverQ_units <- activity %>%
  filter(totalQ == 0) %>%
  pull(constID)

cat("Constituencies with no parliamentary activity:",
    length(neverQ_units),
    "\n")

#Identify never-treated constituencies
never_treated <- CONST %>%
  group_by(constID) %>%
  summarise(ever_treated = max(treatO, na.rm = TRUE),
            .groups = "drop") %>%
  filter(ever_treated == 0) %>%
  pull(constID)

cat("Never-treated donor candidates:",
    length(never_treated),
    "\n")

CONST$constID <- as.numeric(CONST$constID)

################
# EXAMPLE CASE #
################

treated_id <- treated_units[5] #constID: 552090088 (Hurungwe West)

#Donor pool
donor_units <- setdiff(never_treated,
                       treated_id)

treat_month <- treat_info %>%
  filter(constID == treated_id) %>%
  pull(first_treat)

#Subset data
scm_data <- CONST %>%
  filter(constID %in% 
           c(treated_id, donor_units))

scm_data <- as.data.frame(scm_data)

#Data preparation
dataprep.out <- dataprep(foo = scm_data,
                         predictors = c("roads_outcome",
                                        "u5mr_smooth",
                                        "diff",
                                        "opp"),
                         special.predictors = list(list("clinics_outcome",
                                                        1:(treat_month - 1),
                                                        "mean")),
                         dependent = "clinics_outcome",
                         unit.variable = "constID",
                         time.variable = "time",
                         treatment.identifier = treated_id,
                         controls.identifier = donor_units,
                         time.predictors.prior = 1:(treat_month -1),
                         time.optimize.ssr = 1:(treat_month - 1),
                         time.plot = 1:96)

synth.out <- synth(dataprep.out)

#Compute gaps
treated_path <- dataprep.out$Y1plot
synthetic_path <- dataprep.out$Y0plot %*% synth.out$solution.w

gap <- treated_path - synthetic_path

event_time <- (1:96) - treat_month

gap_df <- data.frame(constID = treated_id,
                     event_time = event_time,
                     gap = as.numeric(gap))

head(gap_df)
tail(gap_df)

#Gaps
plot(gap_df$event_time,
     gap_df$gap,
     type = "l")
abline(v = 0, lty = 2)

#Trends plot
plot_df <- data.frame(event_time = (1:96) - treat_month,
                      Treated = as.numeric(treated_path),
                      Synthetic = as.numeric(synthetic_path)) %>%
  pivot_longer(cols = c(Treated, Synthetic),
               names_to = "Series",
               values_to = "Clinics")

p.trends <- ggplot(plot_df,
                   aes(x = event_time,
                       y = Clinics,
                       color = Series,
                       linetype = Series))

pdf(file = file.path(project_dir, "Output", "SCM", "Clinics_SCM_Trends_552090088.pdf"))
  
p.trends +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  scale_color_manual(values = c("Treated" = "black",
                                "Synthetic" = "grey50")) +
  scale_linetype_manual(values = c("Treated" = "solid",
                                   "Synthetic" = "dashed")) +
  labs(x = "Months relative to service request",
       y = "Clinics per 100 square kilometers",
       color = NULL,
       linetype = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank()) 

dev.off()

#RMSPE
pre_gap <- gap[event_time < 0]
sqrt(mean(pre_gap^2))

######################
# AUTOMATED ANALYSIS #
######################

#Storage objects
scm_store <- list()
unit_summary <- list()
all_gaps <- list()

#Estimation loop
for (i in seq_along(treated_units)) {
  treated_id <- treated_units[i]
  
  treat_month <- treat_info %>%
    filter(constID == treated_id) %>%
    pull(first_treat) %>%
    first()
  
  if(is.na(treat_month) || treat_month <= 12) next
  
  #Donor pool
  donor_units <- setdiff(never_treated,
                         treated_id)
  
  #Subset data
  scm_data <- CONST %>%
    filter(constID %in% 
             c(treated_id, donor_units)) %>%
    as.data.frame()
  
  #Data preparation
  dataprep.out <- dataprep(foo = scm_data,
                           predictors = c("roads_outcome",
                                          "u5mr_smooth",
                                          "diff",
                                          "opp"),
                           special.predictors = list(list("clinics_outcome",
                                                          1:(treat_month - 1),
                                                          "mean")),
                           dependent = "clinics_outcome",
                           unit.variable = "constID",
                           time.variable = "time",
                           treatment.identifier = treated_id,
                           controls.identifier = donor_units,
                           time.predictors.prior = 1:(treat_month -1),
                           time.optimize.ssr = 1:(treat_month - 1),
                           time.plot = 1:96)
  
  synth.out <- tryCatch(synth(dataprep.out),
                        error = function(e) NULL)
  
  if(is.null(synth.out)) next
  
  scm_store[[as.character(treated_id)]] <- list(dataprep = dataprep.out,
                                                synth = synth.out,
                                                treat_month = treat_month)
  
  #Minimal RMSPE only
  gap <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
  event_time <- dataprep.out$tag$time.plot - treat_month
  
  pre_rmspe <- sqrt(mean(gap[event_time < 0]^2))
  
  unit_summary[[length(unit_summary) + 1]] <- data.frame(constID = treated_id,
                                                         treat_month = treat_month,
                                                         pre_rmspe = pre_rmspe)
  
  cat("Completed:", i, "of", length(treated_units), "\n")
  
}

#Unit-analysis loop
unit_summary <- bind_rows(unit_summary)

for (id in names(scm_store)) {
  
  obj <- scm_store[[id]]
  
  treated_id <- as.numeric(id)
  
  treated_path <- obj$dataprep$Y1plot
  synthetic_path <- obj$dataprep$Y0plot %*% obj$synth$solution.w
  
  treat_month <- obj$treat_month
  time_vector <- obj$dataprep$tag$time.plot
  event_time <- time_vector - treat_month
  
  gap <- treated_path - synthetic_path
  
  all_gaps[[id]] <- data.frame(constID = treated_id,
                               event_time = event_time,
                               gap = as.numeric(gap))
  
  #Trends plot
  plot_df <- data.frame(event_time = event_time,
                        Treated = as.numeric(treated_path),
                        Synthetic = as.numeric(synthetic_path)) %>%
    tidyr::pivot_longer(cols = c(Treated, Synthetic),
                        names_to = "Series",
                        values_to = "Clinics")
  
  p.trends <- ggplot(plot_df,
                     aes(x = event_time,
                       y = Clinics,
                       color = Series,
                       linetype = Series))
  
  file_name <- paste0("C:/Users/Admin/Dropbox/Manuscripts/Query Sessions/Service Provision/Output/SCM/Clinics_SCM_Trends_",
                      treated_id,
                      ".pdf")
  
  pdf(file = file_name)
  
  print(p.trends +
          geom_line(linewidth = 1.1) +
          geom_vline(xintercept = 0,
                     linetype = "dashed") +
          scale_color_manual(values = c("Treated" = "black",
                                        "Synthetic" = "grey50")) +
          scale_linetype_manual(values = c("Treated" = "solid",
                                           "Synthetic" = "dashed")) +
          labs(x = "Months relative to service request",
               y = "Clinics per 100 square kilometers",
               color = NULL,
               linetype = NULL) +
          theme(legend.position = "bottom",
                plot.background = element_rect(fill = "white", color = "white"),
                panel.background = element_rect(fill = "white",
                                                colour = "gray90"),
                panel.grid.major = element_line(linewidth = 0.5,
                                                colour = "gray90"),
                axis.ticks = element_blank()))
  
  dev.off()
  
  cat("Completed:", id, "of", length(scm_store), "\n")
  
}


###################
## Augmented SCM ##
###################

#Subset data
AUG <- CONST %>%
  filter(constID %in% 
           c(treated_units, never_treated)) %>%
  dplyr::arrange(constID,time) %>%
  group_by(constID) %>%
  mutate(tmonth = if (any(treatO > 0)) {
    min(time[treatO > 0])
  } else {
    Inf
  },
  treat_synth = 1 * (time >= tmonth)) %>%
  ungroup()

table(single_treat$first_treat)

#Partially pooled SCM w/o covariates
ppool_clinics <- multisynth(form = clinics_outcome ~ treat_synth,
                            unit = constID,
                            time = time,
                            data = AUG,
                            n_leads = 12,
                            n_lags = NULL,
                            nu = NULL)

print(ppool_clinics$nu)

ppool_clinics

ppool_clinics_sum <- summary(ppool_clinics)

#Global & Individual Balance + ATT Estimate
ppool_clinics_sum

# Construct diagnostics table
ascm_diag <- tibble(`Ridge parameter (ν)` = round(ppool_clinics$nu, 3),
                    `Global Improvement (%)` =
                      round(100 * (1 - ppool_clinics_sum$scaled_global_l2), 1),
                    `Individual Improvement (%)` =
                      round(100 * (1 - ppool_clinics_sum$scaled_ind_l2), 1))

# Export LaTeX table
kbl(ascm_diag,
    format = "latex",
    booktabs = TRUE,
    align = c("l", "c"),
    caption = "Balance Diagnostics for the Augmented Synthetic Control Analysis") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  save_kable("Output/SCM/ASCM_BalanceDiagnostics_Clinics.tex")

#Plotting the Pretreatment Balance + Estimated ATT
plot(ppool_clinics_sum)

#Plotting the ATT
plot(ppool_clinics_sum, levels = "Average")

#ggplot2() version
att_avg <- ppool_clinics_sum$att %>%
  filter(Level == "Average") %>%
  filter(!is.na(Time)) %>%
  arrange(Time)

p.msynth.NoCov <- ggplot(att_avg, aes(x = Time, 
                                      y = Estimate))

pdf(file = file.path(project_dir, "Output", "SCM", "Clinics_AugSynth_ATT_NoCov.pdf"))

p.msynth.NoCov +
  geom_ribbon(data = subset(att_avg, Time >= 0), # POST-TREATMENT ONLY confidence intervals
              aes(ymin = lower_bound, ymax = upper_bound),
              fill = "grey80",
              alpha = 0.5) +
  geom_line(linewidth = 1.1,
            color = "black") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             linewidth = 0.8) +
  geom_hline(yintercept = 0,
             color = "grey60",
             linewidth = 0.6) +
  scale_x_continuous(limits = c(-100,20), breaks = seq(-100,20, 20),
                     expand = expansion(mult = c(0, 0.0125))) +
  scale_y_continuous(limits = c(-3,3), breaks = seq(-3, 3, 1),
                     expand = expansion(mult = c(0.0125, 0.0125))) +
  labs(x = "Months relative to service request",
       y = "Average treatment effect of the treated") +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90"),
        panel.grid.major = element_line(linewidth = 0.5,
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

#Partially pooled SCM w/ covariates
colSums(is.na(AUG[, c("roads_outcome", "u5mr_smooth", "diff", "opp")]))

#Remove constituencies with vacancies that create NAs
bad_units <- AUG %>%
  group_by(constID) %>%
  summarise(across(c(roads_outcome, u5mr_smooth, diff, opp),
                   ~any(is.na(.)))) %>%
  filter(if_any(everything(), ~ . == TRUE)) %>%
  pull(constID)

AUG_clean <- AUG %>%
  filter(!constID %in% bad_units)

ppool_clinics_cov <- multisynth(form = clinics_outcome ~ treat_synth | 
                                  roads_outcome + u5mr_smooth + diff + opp,
                                unit = constID,
                                time = time,
                                data = AUG_clean,
                                n_leads = 12,
                                n_lags = NULL,
                                nu = NULL)

print(ppool_clinics_cov$nu)

ppool_clinics_cov

ppool_clinics_cov_sum <- summary(ppool_clinics_cov)

#Global & Individual Balance + ATT Estimate
ppool_clinics_cov_sum

# Construct diagnostics table
ascm_cov_diag <- tibble(`Ridge parameter (ν)` = round(ppool_clinics_cov$nu, 3),
                        `Global Improvement (%)` =
                          round(100 * (1 - ppool_clinics_cov_sum$scaled_global_l2), 1),
                        `Individual Improvement (%)` =
                          round(100 * (1 - ppool_clinics_cov_sum$scaled_ind_l2), 1))

# Export LaTeX table
kbl(ascm_cov_diag,
    format = "latex",
    booktabs = TRUE,
    align = c("l", "c"),
    caption = "Balance Diagnostics for the Augmented Synthetic Control Analysis") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  save_kable("Output/SCM/ASCM_BalanceDiagnosticsCov_Clinics.tex")

#Plotting the Pretreatment Balance + Estimated ATT
plot(ppool_clinics_cov_sum)

#Plotting the ATT
plot(ppool_clinics_cov_sum, levels = "Average")

#ggplot2() version
att_avg_cov <- ppool_clinics_cov_sum$att %>%
  filter(Level == "Average") %>%
  filter(!is.na(Time)) %>%
  arrange(Time)

p.msynth.Cov <- ggplot(att_avg_cov, aes(x = Time, 
                                        y = Estimate))

pdf(file = file.path(project_dir, "Output", "SCM", "Clinics_AugSynth_ATT_Cov.pdf"))

p.msynth.Cov +
  geom_ribbon(data = subset(att_avg, Time >= 0), # POST-TREATMENT ONLY confidence intervals
              aes(ymin = lower_bound, ymax = upper_bound),
              fill = "grey80",
              alpha = 0.5) +
  geom_line(linewidth = 1.1,
            color = "black") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             linewidth = 0.8) +
  geom_hline(yintercept = 0,
             color = "grey60",
             linewidth = 0.6) +
  scale_x_continuous(limits = c(-100,20), breaks = seq(-100,20, 20),
                     expand = expansion(mult = c(0, 0.0125))) +
  scale_y_continuous(limits = c(-3,3), breaks = seq(-3, 3, 1),
                     expand = expansion(mult = c(0.0125, 0.0125))) +
  labs(x = "Months relative to service request",
       y = "Average treatment effect of the treated") +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90"),
        panel.grid.major = element_line(linewidth = 0.5,
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

#Combined evaluation statistics table
ascm_diag2 <- ascm_diag %>%
  mutate(Model = "No covariates")

ascm_cov_diag2 <- ascm_cov_diag %>%
  mutate(Model = "Covariates")

ascm_combined <- bind_rows(ascm_diag2, ascm_cov_diag2)

ascm_combined <- ascm_combined %>%
  select(Model, everything())

kbl(ascm_combined,
    format = "latex",
    booktabs = TRUE,
    align = c("l", "l", "c"),
    caption = "Balance Diagnostics for the Synthetic Control Analysis: Clinics per 100 km²") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  save_kable("Output/SCM/ASCM_BalanceDiagnostics_Combined_Clinics.tex")