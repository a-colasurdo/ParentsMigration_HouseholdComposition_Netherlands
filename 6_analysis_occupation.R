#### SEPARATE ANALYSIS: SOCIAL CLASS (FATHER'S OCCUPATION AT BIRTH) ####

#### elite and lower middle class ####
# combine the two classes, as those with least individuals
data_logistic_elite_middle_10=data_lagged_reduced_10 %>%
  filter(occupation_father=="Elite" | occupation_father=="Lower middle class")

logit_model_elite_middle_10 = glm(
  move_10 ~ gender + lag_geographical_area + lag_age + I(lag_age^2) + birth_cohort + 
    migration_parents + own_mig_background +  lag_municipality_size + lag_duration +
    lag_parents_family + lag_partner_family + lag_sibling_family + 
    lag_grandparent_family + lag_children_family + lag_nonkin_family + lag_extended_family,
  data = data_logistic_elite_middle_10,
  family = binomial
)

# Extract clustered standard errors from the model
clustered_vcov =  vcovCL(logit_model_elite_middle_10, cluster = data_logistic_elite_middle_10$rp_id)
clustered_se = sqrt(diag(clustered_vcov))

stargazer(logit_model_elite_middle_10,
          type = "html",
          se = list(clustered_se),  # Use clustered standard errors
          title = "Internal Migration Alone: Log-Odds Coefficients with Clustered Standard Errors",
          dep.var.labels = "First Internal Migration Alone",
          model.numbers = FALSE,
          notes = c("Standard errors clustered by individual ID in parentheses"),
          notes.align = "l",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "logistic_elite_middle_10.html")

# skilled workers
data_logistic_skilled_10=data_lagged_reduced_10 %>%
  filter(occupation_father=="Skilled workers")

logit_model_skilled_10 = glm(
  move_10 ~ gender + lag_geographical_area + lag_age + I(lag_age^2) + birth_cohort + 
    migration_parents + own_mig_background +  lag_municipality_size + lag_duration +
    lag_parents_family + lag_partner_family + lag_sibling_family + 
    lag_grandparent_family + lag_children_family + lag_nonkin_family + lag_extended_family,
  data = data_logistic_skilled_10,
  family = binomial
)

# Extract clustered standard errors from the model
clustered_vcov =  vcovCL(logit_model_skilled_10, cluster = data_logistic_skilled_10$rp_id)
clustered_se = sqrt(diag(clustered_vcov))

stargazer(logit_model_skilled_10,
          type = "html",
          se = list(clustered_se),  # Use clustered standard errors
          title = "Internal Migration Alone: Log-Odds Coefficients with Clustered Standard Errors",
          dep.var.labels = "First Internal Migration Alone",
          model.numbers = FALSE,
          notes = c("Standard errors clustered by individual ID in parentheses"),
          notes.align = "l",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "logistic_skilled_10.html")

#### self-employed farmers ####
data_logistic_farmers_10=data_lagged_reduced_10 %>%
  filter(occupation_father=="Self-employed farmers")

logit_model_farmers_10 = glm(
  move_10 ~ gender + lag_geographical_area + lag_age + I(lag_age^2) + birth_cohort + 
    migration_parents + own_mig_background +  lag_municipality_size + lag_duration +
    lag_parents_family + lag_partner_family + lag_sibling_family + 
    lag_grandparent_family + lag_children_family + lag_nonkin_family + lag_extended_family,
  data = data_logistic_farmers_10,
  family = binomial
)

# Extract clustered standard errors from the model
clustered_vcov =  vcovCL(logit_model_farmers_10, cluster = data_logistic_farmers_10$rp_id)
clustered_se = sqrt(diag(clustered_vcov))

stargazer(logit_model_farmers_10,
          type = "html",
          se = list(clustered_se),  # Use clustered standard errors
          title = "Internal Migration Alone: Log-Odds Coefficients with Clustered Standard Errors",
          dep.var.labels = "First Internal Migration Alone",
          model.numbers = FALSE,
          notes = c("Standard errors clustered by individual ID in parentheses"),
          notes.align = "l",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "logistic_farmers_10.html")

#### unskilled workers ####
data_logistic_unskilled_10=data_lagged_reduced_10 %>%
  filter(occupation_father=="Unskilled workers")

logit_model_unskilled_10 = glm(
  move_10 ~ gender + lag_geographical_area + lag_age + I(lag_age^2) + birth_cohort + 
    migration_parents + own_mig_background +  lag_municipality_size + lag_duration +
    lag_parents_family + lag_partner_family + lag_sibling_family + 
    lag_grandparent_family + lag_children_family + lag_nonkin_family + lag_extended_family,
  data = data_logistic_unskilled_10,
  family = binomial
)

# Extract clustered standard errors from the model
clustered_vcov =  vcovCL(logit_model_unskilled_10, cluster = data_logistic_unskilled_10$rp_id)
clustered_se = sqrt(diag(clustered_vcov))

stargazer(logit_model_unskilled_10,
          type = "html",
          se = list(clustered_se),  # Use clustered standard errors
          title = "Internal Migration Alone: Log-Odds Coefficients with Clustered Standard Errors",
          dep.var.labels = "First Internal Migration Alone",
          model.numbers = FALSE,
          notes = c("Standard errors clustered by individual ID in parentheses"),
          notes.align = "l",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "logistic_unskilled_10.html")

#### unknown ####
data_logistic_unknown_10=data_lagged_reduced_10 %>%
  filter(occupation_father=="Unknown")

logit_model_unknown_10 = glm(
  move_10 ~ gender + lag_geographical_area + lag_age + I(lag_age^2) + birth_cohort + 
    migration_parents + own_mig_background +  lag_municipality_size + lag_duration +
    lag_parents_family + lag_partner_family + lag_sibling_family + 
    lag_grandparent_family + lag_children_family + lag_nonkin_family + lag_extended_family,
  data = data_logistic_unknown_10,
  family = binomial
)

# Extract clustered standard errors from the model
clustered_vcov =  vcovCL(logit_model_unknown_10, cluster = data_logistic_unknown_10$rp_id)
clustered_se = sqrt(diag(clustered_vcov))

stargazer(logit_model_unknown_10,
          type = "html",
          se = list(clustered_se),  # Use clustered standard errors
          title = "Internal Migration Alone: Log-Odds Coefficients with Clustered Standard Errors",
          dep.var.labels = "First Internal Migration Alone",
          model.numbers = FALSE,
          notes = c("Standard errors clustered by individual ID in parentheses"),
          notes.align = "l",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "logistic_unknown_10.html")



