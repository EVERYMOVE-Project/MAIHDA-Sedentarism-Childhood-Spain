## Project: Childhood Inequities of Sedentarism
## Script: MAIHDA Data analysis
## Finalized: March 10 2026
## Edited: July 22 2026

# Load libraries----
library(haven)
library(tidyverse)
library(srvyr)
library(survey)
library(lme4)
library(readxl)
library(ggplot2)
library(clipr)
library(ggrepel)
library(segmented)
library(gt)
library(gtsummary)
library(sjPlot)
library(merTools)
library(pROC)

library(extrafont)

# Load data ----
dt <- get(load("joined_clean_maihda.RData"))

# Descriptive Table 1 ----
# Tabulate each individual characteristics
table(dt$sexo, dt$sedentarismo)
table(dt$edad_cat3, dt$sedentarismo)
table(dt$edad_cat, dt$sedentarismo)
table(dt$clase_2, dt$sedentarismo)
table(dt$urb_rur, dt$sedentarismo)
table(dt$NUTS1, dt$sedentarismo)
table(dt$survey2, dt$sedentarismo)
table(dt$sedentarismo)

table1 <- dt %>%
  ungroup() %>% 
  select(sexo, edad_cat3, urb_rur, clase_2, survey2, sedentarismo) %>%
  tbl_summary(
    label = list(
      sexo ~ "Sex",
      edad_cat3 ~ "Age Group",
      urb_rur ~ "Municipality Type",
      clase_2 ~ "Occupational Social Class",
      survey2 ~ "Pre-Post Inequality Inflection Point",
      sedentarismo ~ "Sedentarism"
    ),
    type = list(
      all_categorical() ~ "categorical"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)")
  ) %>%
  bold_labels() %>% 
  as_gt() %>%
  gt::tab_header(
    title = "Table 1. Descriptive Summary of Study Sample",
    subtitle = "Counts and percentages of variables used to define strata"
  )

table1

table1_sed <- dt %>%
  select(sedentarismo, sexo, edad_cat, clase_2, NUTS1,survey2) %>%
  tbl_summary(
    by = sedentarismo,
    label = list(
      sexo ~ "Sex",
      edad_cat ~ "Age Group",
      clase_2 ~ "Occupational Social Class",
      NUTS1 ~ "NUTS Region",
      survey2 ~ "Pre-Post"
    ),
    type = list(
      all_categorical() ~ "categorical"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_categorical() ~ c(0, 1))
  ) %>%
  add_overall %>% 
  bold_labels() %>% 
  as_gt() %>%
  gt::tab_header(
    title = "Table 1. Descriptive Summary of Study Sample by Sedentarism",
    subtitle = "Counts and percentages of variables used to define strata"
  )
table1_sed

# Inflection Point ----
rii_sedentarism_overall_clase <- get(load("~/UAH/PhD Documents/INEdatos/Analysis/Datasets/clase_tr/rii_sedentarism_overall_clase.RData"))
rii_sedentarism_overall_clase$encuesta <- as.numeric(rii_sedentarism_overall_clase$encuesta)

m0 <- lm(rii ~ encuesta, data = rii_sedentarism_overall_clase)

## fit segmented model
seg_m <- segmented(m0, seg.Z = ~encuesta, psi = 2011)

## plot inflection point
plot(rii ~ encuesta, data = rii_sedentarism_overall_clase)
plot(seg_m, add = TRUE, col = "red")
ggsave("Figures/inflection.png", width = 4000, height = 2200, dpi=300, units = "px")

# Database MAIHDA ####
# adapt category names before tables
dt <- dt %>%
  mutate(
    edad_cat3 = fct_recode(edad_cat3,
                           "6–9 years" = "6-9",
                           "10–12 years" = "10-12",
                           "13–15 years" = "13-15"),
    sexo = fct_recode(sexo,
                      "Boys" = "Male",
                      "Girls" = "Female")
  )

maihda2 <- dt %>% 
  select(factor2, sexo, edad_cat, clase_2, survey, survey2, sedentarismo, urb_rur, NUTS1)

maihda$survey <- factor(maihda$survey)

save(maihda2, file = "maihda2.RData")

# Load data ----
dt <- get(load("maihda2.RData"))

# Generate stratum ----
## Generate stratum ID
levels(dt$sexo)
levels(dt$edad_cat3)
levels(dt$edad_cat)
levels(dt$clase_2)
levels(dt$survey2)
levels(dt$NUTS1)
dt$NUTS1 <- relevel(dt$NUTS1, ref = "North-East")

# STRATUM 
## sexo:     1 = Male, 2 = Female
## edad_cat: 1 = 6-11, 2 = 12-15
## clase_2:  1 = Non-manual workers, Manual workers 
## NUTS1:    1 = Canary Islands, 2 = Centre, 3 = East, 4 = Madrid, 5 = North-East, 6 = North-West, 7 = South
## survey2:  1 = Post, 2 = Pre

## Need numeric type to create stratum ID
dt <- dt %>%
  mutate(
    sexo_num = as.numeric(sexo),
    edad_cat_num = as.numeric(edad_cat),
    clase_2_num = as.numeric(clase_2),
    survey_num = as.numeric(survey),
    survey2_num = as.numeric(survey2),
    NUTS1_num = as.numeric(NUTS1),
    sedentarismo2 = as.numeric(sedentarismo)
  )

# Convert 1/2 to 0/1
dt$sedentarismo2 <- dt$sedentarismo2 - 1

# STRATUM
## create new database with variables needed to construct stratum
dt <- dt %>% 
  mutate(
    stratum3 = 10000*sexo_num + 1000*edad_cat_num + 100*clase_2_num + 10*NUTS1_num + survey2_num
  )
dt$stratum3 <- as.factor(dt$stratum3)

# percent of sedentarism overall and per sex
prop.table(table(dt$sedentarismo))*100
prop.table(table(dt$sedentarismo, dt$sexo), 1)*100 

# Prevalence of sedentarism per strata
tab <- prop.table(table(dt$stratum3, dt$sedentarismo), 1)*100
tab_round <- round(tab, 1)
clipr::write_clip(tab_round)

## maihda database with stratum variable
save(dt, file = "maihda3_stratum.RData")

## Sort data by stratum
dt <- dt[order(dt$stratum4),]

## Generate a new variable which records stratum size
dt <- dt %>%
  group_by(stratum) %>%
  mutate(strataN = n())

dt <- dt %>%
  group_by(stratum4) %>%
  mutate(strataN = n())

# Fit model 0 ----
model0_3 <- glmer(sedentarismo ~ (1|stratum3), data = dt, family = "binomial")
tab_model(model0_3, show.se=T)

## Calculate the VPC
## approximated as the variance(stratum)/variance(stratum + 3.29)
tau2 <- as.numeric(VarCorr(model0_3)$stratum[1,1]) 
VPC0 <- tau2 / (tau2 + (pi^2 / 3)) 
VPC0_percent <- VPC0*100
VPC0_percent

# Fit model 1 ----
# STRATA
model1 <- glmer(sedentarismo ~ sexo + edad_cat + clase_2 + NUTS1 + survey2 +
                  (1|stratum3), data = dt, family = "binomial")
tab_model(model1, show.se=T)
VarCorr(model1)

## Calculate the VPC
tau2 <- as.numeric(VarCorr(model1)$stratum[1,1]) ## the between-stratum variance
tau2 
VPC1 <- tau2 / (tau2 + (pi^2 / 3))
VPC1
VPC1_percent <- VPC1*100
VPC1_percent

## Calculate the PCV
var_null <- as.numeric(VarCorr(model0)$stratum[1,1])
var_null <- as.numeric(VarCorr(model0_3)$stratum[1,1])
var_adj  <- as.numeric(VarCorr(model1)$stratum[1,1])

var_null
var_adj

PCV <- ((var_null - var_adj) / var_null)*100
PCV

# Extract and prepare model predictions and random effects ----
# Model 0 - total effect, prob scale
dt$m0_prob_total <- predict(model0, type="response") 

# Model 0 - fixed effect, prob scale
dt$m0_prob_fixed <- predict(model0, type ="response", re.form=NA) 

# Model 1 - total effect, log scale 
m1_log_total <- predictInterval(model1, which="full", level=0.95, type="linear.prediction", include.resid.var=FALSE)
dt$m1_log_total <- m1_log_total$fit

# Model 1 - total effect, CI, log scale 
dt$m1_log_total_upr <- m1_log_total$upr
dt$m1_log_total_lwr <- m1_log_total$lwr

# Model 1 - fixed effect, log scale
dt$m1_log_fixed <- predict(model1, re.form=NA)

# Model 1 - total effect, prob scale
m1_prob_total <- predictInterval(model1, level=0.95, which = "full", include.resid.var=FALSE, type = "probability")
m1_prob_total <- mutate(m1_prob_total, id=row_number())

# Model 1 - fixed effect, prob scale
m1_prob_fixed <- predictInterval(model1, level=0.95, which = "fixed", include.resid.var=FALSE, type = "probability")
m1_prob_fixed <- mutate(m1_prob_fixed, id=row_number())

m1_random <- predictInterval(model1, level=0.95, which = "random", include.resid.var=FALSE, type = "probability")
m1_random <- mutate(m1_random, id=row_number())

# variable for merging
dt$id <- seq.int(nrow(dt))

# merge total effect
dt2 <- merge(dt, m1_prob_total, by="id") %>%
  rename(m1fit_total=fit, m1upr_total=upr, m1lwr_total=lwr)

# merge fixed effect
dt2 <- merge(dt2, m1_prob_fixed, by="id") %>%
  rename(m1fit_fixed=fit, m1upr_fixed=upr, m1lwr_fixed=lwr)

# merge random effect
dt2 <- merge(dt2, m1_random, by="id") %>%
  rename(m1fit_random=fit, m1upr_random=upr, m1lwr_random=lwr)

# collapse to stratum level
stratum_level3 <- dt %>% 
  group_by(stratum3) %>% 
  summarise(
    across(c(sexo, edad_cat, clase_2, NUTS1, survey2), first),
    across(c(sedentarismo2, strataN,
             m0_prob_total, m0_prob_fixed,
             m1_log_total, m1_log_total_upr, m1_log_total_lwr,
             m1_log_fixed, m1fit_fixed, m1upr_fixed, m1lwr_fixed,
             m1fit_total, m1upr_total, m1lwr_total,
             m1fit_random, m1upr_random, m1lwr_random), mean)
  )

# just stratum variables
stratum_level3 <- dt %>% 
  group_by(stratum3) %>% 
  summarise(
    across(c(sexo, edad_cat, clase_2, NUTS1, survey2), first),
    across(c(sedentarismo2, strataN), mean)
  )

# convert to percentage
stratum_level3 <- stratum_level3 %>%
  mutate(across(
    c(m0_prob_total, m0_prob_fixed, 
      m1fit_fixed, m1upr_fixed, m1lwr_fixed, 
      m1fit_total, m1upr_total, m1lwr_total,
      m1fit_random, m1upr_random, m1lwr_random),
    ~ .x * 100
  ))

# create variable to represent percentage of sedentarismo
stratum_level3$sedentarismo2_p <- stratum_level3$sedentarismo2*100

# predict the stratum random effects and associated standard errors
m1SE <- REsim(model1)

save(stratum_level3, file = "stratum_level3.RData")
# reloaded/renamed to remove 3
stratum_level <- get(load("stratum_level3.RData"))
clipr::write_clip(stratum_level3)

# Table 2 ----
# Generate binary indicators for whether each stratum has more than X 
# individuals
stratum_level3$n500plus <- ifelse(stratum_level3$strataN>=500, 1,0)
stratum_level3$n450plus <- ifelse(stratum_level3$strataN>=450, 1,0)
stratum_level3$n400plus <- ifelse(stratum_level3$strataN>=400, 1,0)
stratum_level3$n350plus <- ifelse(stratum_level3$strataN>=350, 1,0)
stratum_level3$n300plus <- ifelse(stratum_level3$strataN>=300, 1,0)
stratum_level3$n250plus <- ifelse(stratum_level3$strataN>=250, 1,0)
stratum_level3$n200plus <- ifelse(stratum_level3$strataN>=200, 1,0)
stratum_level3$n175plus <- ifelse(stratum_level3$strataN>=175, 1,0)
stratum_level3$n150plus <- ifelse(stratum_level3$strataN>=150, 1,0)
stratum_level3$n100plus <- ifelse(stratum_level3$strataN>=100, 1,0)
stratum_level3$n90plus <- ifelse(stratum_level3$strataN>=90, 1,0)
stratum_level3$n80plus <- ifelse(stratum_level3$strataN>=80, 1,0)
stratum_level3$n70plus <- ifelse(stratum_level3$strataN>=70, 1,0)
stratum_level3$n60plus <- ifelse(stratum_level3$strataN>=60, 1,0)
stratum_level3$n50plus <- ifelse(stratum_level3$strataN>=50, 1,0)
stratum_level3$n40plus <- ifelse(stratum_level3$strataN>=40, 1,0)
stratum_level3$n30plus <- ifelse(stratum_level3$strataN>=30, 1,0)
stratum_level3$n20plus <- ifelse(stratum_level3$strataN>=20, 1,0)
stratum_level3$n10plus <- ifelse(stratum_level3$strataN>=10, 1,0)

# tabulate the binary indicators
table(stratum_level3$n500plus)
table(stratum_level3$n450plus)
table(stratum_level3$n400plus)
table(stratum_level3$n350plus)
table(stratum_level3$n300plus)
table(stratum_level3$n250plus)
table(stratum_level3$n200plus)
table(stratum_level3$n175plus)
table(stratum_level3$n150plus)
table(stratum_level3$n100plus)
table(stratum_level3$n90plus)
table(stratum_level3$n80plus)
table(stratum_level3$n70plus)
table(stratum_level3$n60plus)
table(stratum_level3$n50plus)
table(stratum_level3$n40plus)
table(stratum_level3$n30plus)
table(stratum_level3$n20plus)
table(stratum_level3$n10plus)

# Observed stratum-level means (prevalence of sedentarism)
observed_table <- dt2 %>%
  group_by(stratum) %>% 
  summarise(
    n = n(),
    cases = sum(sedentarismo == "Yes", na.rm = TRUE),
    prevalence = mean(sedentarismo == "Yes", na.rm = TRUE) 
  ) %>%
  mutate(prevalence_pct = prevalence * 100)
clipr::write_clip(observed_table)

# Overall dataframe stratum_level
stratum_level_pp <- dt2 %>%
  group_by(
    stratum, strataN, sexo, edad_cat, clase_2, NUTS1, survey2
    
  ) %>%
  summarise(
    observed_prevalence = mean(sedentarismo2, na.rm = TRUE)*100,
    
    m1fit_total = mean(m1fit_total, na.rm = TRUE)*100,
    m1upr_total = mean(m1upr_total, na.rm = TRUE)*100,
    m1lwr_total = mean(m1lwr_total, na.rm = TRUE)*100,
    
    
    m1fit_fixed      = mean(m1fit_fixed, na.rm = TRUE)*100,
    m1upr_fixed      = mean(m1upr_fixed, na.rm = TRUE)*100,
    m1lwr_fixed      = mean(m1lwr_fixed, na.rm = TRUE)*100,
    
    .groups = "drop"
  ) %>% 
  arrange(m1fit_total)

clipr::write_clip(stratum_level_pp)

head(stratum_level_pp)
tail(stratum_level_pp)

# Table 3 ----
# Create a table that includes all model estimates, including the Variance 
# Partitioning Coefficients (VPC)
tab_model(model0, model1, p.style="stars")

VPC0
VPC1
PCV

# model0 based on the fixed and random effects
AUC0 <- auc(dt2$sedentarismo2, dt2$m0_prob_total)
AUC0

# model0 based only on the fixed effects
AUC0f <- auc(dt2$sedentarismo2, dt2$m0_prob_fixed)
AUC0f

# model1 based on the fixed and random effects
AUC1 <- auc(dt2$sedentarismo2, dt2$m1fit_total)
AUC1

# model1 based only on the fixed effects
AUC1f <- auc(dt2$sedentarismo2, dt2$m1fit_fixed)
AUC1f

# Figure 2 ----
stratum_level <- stratum_level %>%
  mutate(rank1=rank(m1fit_total))

# Generate list of 6 highest and 6 lowest predicted stratum means (for Table 4)
stratum_level <- stratum_level[order(stratum_level$rank1),]
head(stratum_level)
tail(stratum_level)

# Plot the caterpillar plot of the predicted stratum means
top <- max(stratum_level$m1upr_total) + 1.5

ggplot(stratum_level, aes(x = rank1, y = m1fit_total)) +
  geom_pointrange(aes(ymin = m1lwr_total, ymax = m1upr_total), size = 0.5) +
  geom_text(
    aes(x = rank1, y = top, label = stratum),            
    vjust = -0.15,
    size = 3,
    angle = 90,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  labs(
    title = "Predicted Probability (%) of Sedentarism per Strata",
    x = "Stratum Rank",
    y = "Predicted Percent of Sedentarism",
  ) +
  theme_bw()

# updated graph
top <- max(stratum_level$m1fit_total) + 6

ggplot(stratum_level, aes(x = rank1, y = m1fit_total)) +
  geom_vline(
    xintercept = seq(min(stratum_level$rank1),
                     max(stratum_level$rank1),
                     by = 1),
    color = "grey90",
    linewidth = 0.3
  ) +
  # Confidence intervals
  geom_pointrange(
    aes(ymin = m1lwr_total, ymax = m1upr_total),
    color = "grey20",
    size = 0.4
  ) +
  
  # Point estimates
  geom_point(
    color = "#1F78B4",
    size = 2
  ) +
  
  geom_text(
    aes(x = rank1, y = top, label = stratum),            
    vjust = -0.15,
    size = 3,
    angle = 90,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  
  labs(
    title = "Predicted Probability of Sedentarism by Stratum (Stratum 3)",
    x = "Intersectional Stratum (lowest to highest risk)",
    y = "Predicted probability of sedentarism (%)",
    caption = "Stratum-level predicted probabilities; 95% confidence intervals"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9, hjust = 0),
    axis.text.x = element_text(size = 9),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("Figures/MAIHDA/caterpillarplot_pp_s3_16-04-26.png", width = 4000, height = 2200, dpi=300, units = "px")

# Figure 3 w/ simulation ----
## REsim estimates the stratum-level random effects and their uncertainty
m1SE <- REsim(model1) 
plotREsim(m1SE)

set.seed(1234)

# duplicate stratum data *1000
stratumsim <- rbind(stratum_level, 
                    stratum_level[rep(1:nrow(stratum_level),999),])

# approximate SE for the linear prediction on the logit scale
stratumsim$m1_total_SE <- (stratumsim$m1_log_total - stratumsim$m1_log_total_lwr)/1.96

# approximate predicted stratum percentages based on the regression 
# coefficients and the predicted stratum random effect, factoring in prediction uncertainty
stratumsim$m1_total_sim <- 100*invlogit(stratumsim$m1_log_total + 
                                            rnorm(nrow(stratumsim), mean=0, sd=stratumsim$m1_total_SE))

# approximate the predicted stratum percentages for fixed effect
stratumsim$m1_fixed_sim <- 100*invlogit(stratumsim$m1_log_fixed)

# calculate the difference in the predicted stratum percentages due to the predicted stratum effect
stratumsim$m1_diff_sim <- stratumsim$m1_total_sim - stratumsim$m1_fixed_sim

# sort the data by strata
stratumsim <- stratumsim[order(stratumsim$stratum),]

# collapse by strata
stratum_info <- stratumsim %>%
  distinct(stratum, sexo, edad_cat3, urb_rur, clase_2, survey2, strataN)

dt_maihda <- dt %>% 
  distinct(stratum, sexo, edad_cat, urb_rur, clase_2, survey2, strataN)

stratumsim_final <- stratumsim %>%
  group_by(stratum) %>%
  summarise(
    m1_diff_mean=mean(m1_diff_sim), 
    m1_diff_std=sd(m1_diff_sim)) %>%
  mutate(rank=rank(m1_diff_mean)) %>%
  mutate(hi=(m1_diff_mean + 1.96*m1_diff_std)) %>%
  mutate(lo=(m1_diff_mean - 1.96*m1_diff_std)) %>% 
  left_join(stratum_info, by = "stratum")

clipr::write_clip(stratumsim_final)

# plot of predicted stratum differences
ggplot(stratumsim_final, aes(x=rank, y=m1_diff_mean)) +
  geom_hline(yintercept=0, color="blue", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=lo, ymax=hi)) + 
  xlab("Stratum Rank") +
  ylab("Difference in predicted percent sedentarism due to interactions") +
  theme_bw()

# significant strata
stratumsim_final %>% filter(lo>0)
stratumsim_final %>% filter(hi<0)

# Filter only significant strata
sig_strata <- stratumsim_final %>%
  filter(lo > 0 | hi < 0) %>% ## stratums significant ONLY if the zero is completely excluded from the uncertainty level
  arrange(m1_diff_mean) %>%            
  mutate(rank = row_number())  

# new plot
top_y <- max(stratumsim_final$hi) + 0.5

ggplot(stratumsim_final, aes(x = rank, y = m1_diff_mean)) +
  geom_text(
    aes(
      x = rank, y = top_y, label = stratum),
    vjust = -0.15,
    size = 3,
    angle = 90
  ) +
  geom_vline(
    xintercept = seq(min(stratumsim_final$rank),
                     max(stratumsim_final$rank),
                     by = 1),
    color = "grey90",
    linewidth = 0.3
  ) +
  
  # Zero reference line (no residual intersectional effect)
  geom_hline(
    yintercept = 0,
    color = "firebrick",
    linewidth = 0.9,
    linetype = "solid"
  ) +
  
  # Confidence intervals
  geom_pointrange(
    aes(ymin = lo, ymax = hi),
    color = "grey20",
    linewidth = 0.4
  ) +
  
  # Point estimates
  geom_point(
    color = "#1F78B4",
    size = 2.3
  ) +
  
  labs(
    title = "Residual Intersectional Effects on Sedentarism",
    subtitle = "How much each stratum deviates from what would be expected based on additive effects alone.",
    x = "Intersectional strata (ranked by residual effect)",
    y = "Difference in predicted percentage points",
    caption = "Stratum-level residual effects; 95% confidence intervals.\nRed line indicates no residual intersectional effect."
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0),
    axis.text.x = element_text(size = 10),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("Figures/MAIHDA/caterpillarplot_re_sim2_10-03-26.png", width = 4000, height = 2200, dpi=300, units = "px")

lowest <- head(stratumsim2)
lowest
clipr::write_clip(lowest)
highest <- tail(stratumsim2)
highest
clipr::write_clip(highest)
clipr::write_clip(stratumsim2)

# Figure 3 w/o simulation ----
# collapse to stratum level
stratum_level3 <- aggregate(
  x = dt2[c("sedentarismo2", "strataN", "m0_prob_total", "m0_prob_fixed", "m1_log_fixed", 
            "m1fit_fixed", "m1upr_fixed", "m1lwr_fixed",  "m1fit_total", "m1upr_total", "m1lwr_total")],
  by = dt2[c("stratum")],
  FUN = mean
) 

# convert to percentage
stratum_level3 <- stratum_level3 %>%
  mutate(across(
    c(m0_prob_total, m0_prob_fixed, m1fit_fixed,
      m1upr_fixed, m1lwr_fixed, m1fit_total,
      m1upr_total, m1lwr_total),
    ~ .x * 100
  ))

# calculate standard errors for total and fixed effects
stratum_level3 <- stratum_level3 %>% 
  mutate(
    se_total = (m1upr_total - m1lwr_total) / (2*1.96), 
    se_fixed = (m1upr_fixed - m1lwr_fixed) / (2*1.96)
  )

# calculate the standard error of the difference (residual effects)
stratum_level3 <- stratum_level3 %>% 
  mutate(
    se_diff = sqrt(se_total^2 + se_fixed^2)
  )

# calculate the difference (residual effects)
stratum_level3 <- stratum_level3 %>% 
  mutate(
    dif_mean = m1fit_total - m1fit_fixed,
    dif_lwr = dif_mean - 1.96*se_diff,
    dif_upr = dif_mean + 1.96*se_diff
  )

# rank by total stratum probabilities
stratum_level3 <- stratum_level3 %>%
  mutate(rank1=rank(m1fit_total))

# rank by difference (residual effect on probability scale)
stratum_level3 <- stratum_level3 %>%
  mutate(rank2=rank(dif_mean))

# add stratum info
stratum_info2 <- stratum_level %>%
  distinct(stratum, sexo, edad_cat, NUTS1, clase_2, survey2)

stratum_level3 <- stratum_level3 %>%
  left_join(stratum_info2, by = "stratum")

clipr::write_clip(stratum_level3)

# list of 6 highest and 6 lowest predicted stratum means (for Table 4)
stratum_level3 <- stratum_level3[order(stratum_level3$rank1),]
head <- head(stratum_level3)
tail <- tail(stratum_level3)
clipr::write_clip(tail)

# plot predicted stratum probabilities
ggplot(stratum_level3, aes(x = rank1, y = m1fit_total)) +
  geom_point() +
  geom_pointrange(aes(ymin = m1lwr_total, ymax = m1upr_total)) +
  geom_text(
    aes(
      x = rank2,
      y = m1fit_total + 1,   # coloca el texto encima del error bar
      label = stratum
    ),
    angle = 90,
    vjust = 0,
    size = 3
  ) +
  ylab("Predicted Percent Sedentarism, Model 1") +
  xlab("Stratum Rank") +
  theme_bw()

# new plot
top_y <- max(stratum_level3$m1upr_total) + 1.5

ggplot(stratum_level3, aes(x = rank1, y = m1fit_total)) +
  geom_text(
    aes(
      x = rank1, y = top_y, label = stratum),
    vjust = -0.15,
    size = 3,
    angle = 90
  ) +
  geom_vline(
    xintercept = seq(min(stratum_level3$rank1),
                     max(stratum_level3$rank1),
                     by = 1),
    color = "grey90",
    linewidth = 0.3
  ) +
  
  # Confidence intervals
  geom_pointrange(
    aes(ymin = m1lwr_total, ymax = m1upr_total),
    color = "grey20",
    linewidth = 0.4
  ) +
  
  # Point estimates
  geom_point(
    color = "#1F78B4",
    size = 2.3
  ) +
  labs(
    title = "Predicted Probability of Sedentarism by Intersectional Stratum",
    x = "Intersectional Stratum (lowest to highest risk)",
    y = "Predicted probability of sedentarism (%)",
    caption = "Stratum-level predicted probabilities; 95% confidence intervals"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0),
    axis.text.x = element_text(size = 10),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("Figures/MAIHDA/caterpillarplot_pp.png", width = 4000, height = 2200, dpi=300, units = "px")

# residual effect plot
ggplot(stratum_level3, aes(x = rank2, y = dif_mean)) +
  geom_point() +
  geom_pointrange(aes(ymin = dif_lwr, ymax = dif_upr)) +
  geom_text(
    aes(
      x = rank2,
      y = dif_upr + 1,   # coloca el texto encima del error bar
      label = stratum
    ),
    angle = 90,
    vjust = 0,
    size = 3
  ) +
  ylab("Probability difference in total and fixed effect for sedentarism, Model 2") +
  xlab("Stratum Rank") +
  theme_bw()

# new plot
top_y <- max(stratum_level3$dif_upr) + 1

ggplot(stratum_level3, aes(x = rank2, y = dif_mean)) +
  geom_text(
    aes(
      x = rank2, y = top_y, label = stratum),
    vjust = -0.15,
    size = 3,
    angle = 90
  ) +
  geom_vline(
    xintercept = seq(min(stratum_level3$rank2),
                     max(stratum_level3$rank2),
                     by = 1),
    color = "grey90",
    linewidth = 0.3
  ) +
  
  # Zero reference line (no residual intersectional effect)
  geom_hline(
    yintercept = 0,
    color = "firebrick",
    linewidth = 0.9,
    linetype = "solid"
  ) +
  
  # Confidence intervals
  geom_pointrange(
    aes(ymin = dif_lwr, ymax = dif_upr),
    color = "grey20",
    linewidth = 0.4
  ) +
  
  # Point estimates
  geom_point(
    color = "#1F78B4",
    size = 2.3
  ) +
  
  labs(
    title = "Residual Intersectional Effects on Sedentarism no Simulation",
    subtitle = "How much each stratum deviates from what would be expected based on additive effects alone.",
    x = "Intersectional strata (ranked by residual effect)",
    y = "Difference in predicted percentage points",
    caption = "Stratum-level residual effects; 95% confidence intervals.\nRed line indicates no residual intersectional effect."
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0),
    axis.text.x = element_text(size = 10),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("Figures/MAIHDA/caterpillarplot_re.png", width = 4000, height = 2200, dpi=300, units = "px")

# list of 6 highest and 6 lowest stratum residual effects
stratum_level3 <- stratum_level3[order(stratum_level3$rank2),]
head(stratum_level3)
tail(stratum_level3)

### Figures for Stata ----

## load data from Stata
stata <- read_excel("stata_strata3.xlsx")

stata <- stata %>%
  mutate(
    sex_s    = ifelse(sexo == "Boys", "B", "G"),
    age_s    = ifelse(grepl("6", edad_cat), "6-11", "12-15"),
    class_s  = ifelse(grepl("Non", clase_2), "NMW", "MW"),
    region_s = recode(NUTS1,
                      "Canary Islands" = "CI",
                      "Centre"         = "Ctr",
                      "East"           = "E",
                      "Madrid"         = "Mad",
                      "North-East"     = "NE",
                      "North-West"     = "NW",
                      "South"          = "S"),
    label = paste(sex_s, age_s, class_s, region_s, survey2)
  )

## Predicted Probabilities
stata <- stata %>%
  group_by(stratum) %>%
  mutate(strataN = n())

ggplot(stata, aes(x = m1_prob_total_rank, y = m1_total_prob)) +
  geom_text(
    aes(
      x = m1_prob_total_rank, y = bot_y, label = stratum),
    vjust = -0.15,
    size = 3,
    angle = 90
  ) +
  
  geom_vline(
    xintercept = seq(min(stata$m1_prob_total_rank),
                     max(stata$m1_prob_total_rank),
                     by = 1),
    color = "grey92",
    linewidth = 0.3
  ) +
  
  # Confidence intervals
  geom_pointrange(
    aes(ymin = m1_total_lo, ymax = m1_total_hi),
    color = "grey20",
    linewidth = 0.4
  ) +
  
  # Point estimates
  geom_point(
    color = "blue4",
    size = 1.5,
    shape = 9
  ) +
  
  labs(
    title = "Predicted Probability of Sedentarism by Intersectional Stratum",
    subtitle = "Model 2",
    x = "Intersectional Stratum (lowest to highest risk)",
    y = "Predicted probability of sedentarism (%)",
    caption = "Stratum-level predicted probabilities; 95% confidence intervals%"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0),
    axis.text.x = element_text(size = 10),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("Figures/MAIHDA/caterpillarplot_pp_stata.png", width = 4000, height = 2200, dpi=300, units = "px")

## Residual effects
bot_y <- max(stata$m1_diff_lo) - 12

ggplot(stata, aes(x = m1_diff_rank, y = m1_diff)) +
  geom_text(
    aes(
      x = m1_diff_rank, y = bot_y, label = stratum),
    vjust = -0.15,
    size = 3,
    angle = 90
  ) +

  geom_vline(
    xintercept = seq(min(stata$m1_diff_rank),
                     max(stata$m1_diff_rank),
                     by = 1),
    color = "grey92",
    linewidth = 0.3
  ) +
  
  # Zero reference line (no residual intersectional effect)
  geom_hline(
    yintercept = 0,
    color = "red4",
    linewidth = 0.7,
    linetype = "solid"
  ) +
  
  # Confidence intervals
  geom_pointrange(
    aes(ymin = m1_diff_lo, ymax = m1_diff_hi),
    color = "grey20",
    linewidth = 0.4
  ) +
  
  # Point estimates
  geom_point(
    color = "blue4",
    size = 1.5,
    shape = 9
      ) +
  
  labs(
    title = "Residual Intersectional Effects on Sedentarism",
    subtitle = "How much each stratum deviates from what would be expected based on additive effects alone.",
    x = "Intersectional strata (ranked by residual effect)",
    y = "Difference in Predicted Percent Points",
    caption = "Stratum-level residual effects; 95% confidence intervals.\nRed line indicates no residual intersectional effect."
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0),
    axis.text.x = element_text(size = 10),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("Figures/MAIHDA/caterpillarplot_re_stata.png", width = 4000, height = 2200, dpi=300, units = "px")
