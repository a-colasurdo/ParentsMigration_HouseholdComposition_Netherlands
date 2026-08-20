### SENSITIVITY CHECK - START OBSERVING MIGRATION AFTER DIFFERENT AGES ###
# age 12 (the one used for the main analyses and included in the main text)
# age 14
# age 16
# age 18

#### AGE 12 ####
data_12=data_lagged_reduced_10 %>%
  filter(age>=12, age<=50,
         birth_cohort %in% c("1850-1859","1860-1869","1870-1879","1880-1889")) %>%
  droplevels()


logit_model_12=glm(
  move_10 ~ gender + lag_geographical_area + lag_age + I(lag_age^2) + birth_cohort + 
    migration_parents + own_mig_background + occupation_father +  lag_municipality_size + lag_duration +
    lag_parents_family + lag_partner_family + lag_sibling_family + 
    lag_grandparent_family + lag_children_family + lag_nonkin_family + lag_extended_family,
  data=data_12,
  family = binomial
)

# Extract clustered standard errors from the model
clustered_vcov =  vcovCL(logit_model_12, cluster = data_12$rp_id)
clustered_se = sqrt(diag(clustered_vcov))

stargazer(logit_model_12,
          type = "html",
          se = list(clustered_se),  
          title = "Internal Migration Alone: Log-Odds Coefficients with Clustered Standard Errors",
          dep.var.labels = "First Internal Migration Alone",
          model.numbers = FALSE,
          notes = c("Standard errors clustered by individual ID in parentheses"),
          notes.align = "l",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "logistic_12.html")

#### AGE 14 ####
data_14=data_lagged_reduced_10%>%
  filter(age>=14, age<=50,
         birth_cohort %in% c("1850-1859","1860-1869","1870-1879","1880-1889")) %>%
  droplevels()


logit_model_14=glm(
  move_10 ~ gender + lag_geographical_area + lag_age + I(lag_age^2) + birth_cohort + 
    migration_parents + own_mig_background + occupation_father +  lag_municipality_size + lag_duration +
    lag_parents_family + lag_partner_family + lag_sibling_family + 
    lag_grandparent_family + lag_children_family + lag_nonkin_family + lag_extended_family,
  data=data_14,
  family = binomial
)

# Extract clustered standard errors from the model
clustered_vcov =  vcovCL(logit_model_14, cluster = data_14$rp_id)
clustered_se = sqrt(diag(clustered_vcov))

stargazer(logit_model_14,
          type = "html",
          se = list(clustered_se), 
          title = "Internal Migration Alone: Log-Odds Coefficients with Clustered Standard Errors",
          dep.var.labels = "First Internal Migration Alone",
          model.numbers = FALSE,
          notes = c("Standard errors clustered by individual ID in parentheses"),
          notes.align = "l",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "logistic_14.html")

#### AGE 16 ####
data_16=data_lagged_reduced_10%>%
  filter(age>=16, age<=50,
         birth_cohort %in% c("1850-1859","1860-1869","1870-1879","1880-1889")) %>%
  droplevels()


logit_model_16=glm(
  move_10 ~ gender + lag_geographical_area + lag_age + I(lag_age^2) + birth_cohort + 
    migration_parents + own_mig_background + occupation_father +  lag_municipality_size + lag_duration +
    lag_parents_family + lag_partner_family + lag_sibling_family + 
    lag_grandparent_family + lag_children_family + lag_nonkin_family + lag_extended_family,
  data=data_16,
  family = binomial
)

# Extract clustered standard errors from the model
clustered_vcov =  vcovCL(logit_model_16, cluster = data_16$rp_id)
clustered_se = sqrt(diag(clustered_vcov))

stargazer(logit_model_16,
          type = "html",
          se = list(clustered_se),  
          title = "Internal Migration Alone: Log-Odds Coefficients with Clustered Standard Errors",
          dep.var.labels = "First Internal Migration Alone",
          model.numbers = FALSE,
          notes = c("Standard errors clustered by individual ID in parentheses"),
          notes.align = "l",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "logistic_16.html")

#### AGE 18 ####
data_18=data_lagged_reduced_10 %>%
  filter(age>=18, age<=50,
         birth_cohort %in% c("1850-1859","1860-1869","1870-1879","1880-1889")) %>%
  droplevels()


logit_model_18=glm(
  move_10 ~ gender + lag_geographical_area + lag_age + I(lag_age^2) + birth_cohort + 
    migration_parents + own_mig_background + occupation_father +  lag_municipality_size + lag_duration +
    lag_parents_family + lag_partner_family + lag_sibling_family + 
    lag_grandparent_family + lag_children_family + lag_nonkin_family + lag_extended_family,
  data=data_18,
  family = binomial
)

# Extract clustered standard errors from the model
clustered_vcov =  vcovCL(logit_model_18, cluster = data_18$rp_id)
clustered_se = sqrt(diag(clustered_vcov))

stargazer(logit_model_18,
          type = "html",
          se = list(clustered_se),  
          title = "Internal Migration Alone: Log-Odds Coefficients with Clustered Standard Errors",
          dep.var.labels = "First Internal Migration Alone",
          model.numbers = FALSE,
          notes = c("Standard errors clustered by individual ID in parentheses"),
          notes.align = "l",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "logistic_18.html")
