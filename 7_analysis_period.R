#### SEPARATE ANALYSIS: BIRTH COHORT ####

#### 1850-1869 ####
# combine the first two cohorts
data_logistic_1850_1869=data_lagged_reduced_10 %>%
  filter(birth_cohort=="1850-1859" | birth_cohort=="1860-1869")

logit_model_1850_1869 = glm(
  move_10 ~ gender + lag_geographical_area + lag_age + I(lag_age^2) + occupation_father +
    migration_parents + own_mig_background +  lag_municipality_size + lag_duration +
    lag_parents_family + lag_partner_family + lag_sibling_family + 
    lag_grandparent_family + lag_children_family + lag_nonkin_family + lag_extended_family,
  data = data_logistic_1850_1869,
  family = binomial
)

# Extract clustered standard errors from the model
clustered_vcov =  vcovCL(logit_model_1850_1869, cluster = data_logistic_1850_1869$rp_id)
clustered_se = sqrt(diag(clustered_vcov))

stargazer(logit_model_1850_1869,
          type = "html",
          se = list(clustered_se),  # Use clustered standard errors
          title = "Internal Migration Alone: Log-Odds Coefficients with Clustered Standard Errors",
          dep.var.labels = "First Internal Migration Alone",
          model.numbers = FALSE,
          notes = c("Standard errors clustered by individual ID in parentheses"),
          notes.align = "l",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "logistic_1850_1869.html")


#### 1870-1879 ####
data_logistic_1870_1879=data_lagged_reduced_10 %>%
  filter(birth_cohort=="1870-1879")

logit_model_1870_1879 = glm(
  move_10 ~ gender + lag_geographical_area + lag_age + I(lag_age^2) + occupation_father +
    migration_parents + own_mig_background +  lag_municipality_size + lag_duration +
    lag_parents_family + lag_partner_family + lag_sibling_family + 
    lag_grandparent_family + lag_children_family + lag_nonkin_family + lag_extended_family,
  data = data_logistic_1870_1879,
  family = binomial
)

# Extract clustered standard errors from the model
clustered_vcov =  vcovCL(logit_model_1870_1879, cluster = data_logistic_1870_1879$rp_id)
clustered_se = sqrt(diag(clustered_vcov))

stargazer(logit_model_1870_1879,
          type = "html",
          se = list(clustered_se),  # Use clustered standard errors
          title = "Internal Migration Alone: Log-Odds Coefficients with Clustered Standard Errors",
          dep.var.labels = "First Internal Migration Alone",
          model.numbers = FALSE,
          notes = c("Standard errors clustered by individual ID in parentheses"),
          notes.align = "l",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "logistic_1870_1879.html")

# 1880-1889
data_logistic_1880_1889=data_lagged_reduced_10 %>%
  filter(birth_cohort=="1880-1889")

logit_model_1880_1889 = glm(
  move_10 ~ gender + lag_geographical_area + lag_age + I(lag_age^2) + occupation_father +
    migration_parents + own_mig_background +  lag_municipality_size + lag_duration +
    lag_parents_family + lag_partner_family + lag_sibling_family + 
    lag_grandparent_family + lag_children_family + lag_nonkin_family + lag_extended_family,
  data = data_logistic_1880_1889,
  family = binomial
)

# Extract clustered standard errors from the model
clustered_vcov =  vcovCL(logit_model_1880_1889, cluster = data_logistic_1880_1889$rp_id)
clustered_se = sqrt(diag(clustered_vcov))

stargazer(logit_model_1880_1889,
          type = "html",
          se = list(clustered_se),  # Use clustered standard errors
          title = "Internal Migration Alone: Log-Odds Coefficients with Clustered Standard Errors",
          dep.var.labels = "First Internal Migration Alone",
          model.numbers = FALSE,
          notes = c("Standard errors clustered by individual ID in parentheses"),
          notes.align = "l",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "logistic_1880_1889.html")
