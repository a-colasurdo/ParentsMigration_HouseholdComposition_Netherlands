#### PARENTS' MIGRATION BACKGROUND ####

# parents have migration background if they have experienced a move larger than 10 kilometers
# parents migration computed by comparing parents' municipality of residence and individual's first municipality of residence
data_parents_10=data_parents %>%
  dplyr::select(rp_id, municipality_HSN, year_birth, municipality_birth_father, year_birth_father, municipality_birth_mother, year_birth_mother) %>%
  filter(!is.na(municipality_HSN),
         !is.na(municipality_birth_father),
         !is.na(municipality_birth_mother)) %>%
  group_by(rp_id) %>%
  slice(1) %>%
  left_join(spatio_temporal %>%
              dplyr::select(municipality_HSN=municipality, latitude, longitude, amco=`Amsterdam code`),
            by="municipality_HSN") %>%
  left_join(spatio_temporal %>%
              dplyr::select(municipality_birth_father=municipality, latitude_birth_father=latitude, longitude_birth_father=longitude, amco_birth_father=`Amsterdam code`),
            by="municipality_birth_father") %>%
  left_join(spatio_temporal %>%
              dplyr::select(municipality_birth_mother=municipality, latitude_birth_mother=latitude, longitude_birth_mother=longitude, amco_birth_mother=`Amsterdam code`),
            by="municipality_birth_mother") %>%
  filter(!is.na(amco),
         !is.na(amco_birth_father),
         !is.na(amco_birth_mother)) %>%
  group_by(rp_id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    longitude = longitude / 1000,
    latitude = latitude / 1000,
    longitude_birth_father = longitude_birth_father / 1000,
    latitude_birth_father = latitude_birth_father / 1000,
    longitude_birth_mother = longitude_birth_mother / 1000,
    latitude_birth_mother = latitude_birth_mother / 1000,
    distance_father = pointDistance(
      cbind(longitude, latitude),
      cbind(longitude_birth_father, latitude_birth_father),
      lonlat = TRUE),
    distance_mother = pointDistance(
      cbind(longitude, latitude),
      cbind(longitude_birth_mother, latitude_birth_mother),
      lonlat = TRUE),
    distance_father=distance_father/1000,
    distance_mother=distance_mother/1000,
    migration_father=ifelse(distance_father>=10,1,0),
    migration_mother=ifelse(distance_mother>=10,1,0),
    migration_parents=case_when(migration_father==0 & migration_mother==0 ~ "none",
                                migration_father==1 & migration_mother==0 ~ "only father",
                                migration_father==0 & migration_mother==1 ~ "only mother",
                                migration_father==1 & migration_mother==1 ~ "both")) %>%
  dplyr::select(rp_id, migration_parents)

# compute distance of move, as change of municipality
# define migration as move made without parents larger than 10 kilomenters
person_address_years_10 = person_address_years %>%
  group_by(rp_id) %>%
  mutate(
    longitude = longitude / 1000,
    latitude = latitude / 1000,
    distance = pointDistance(
    cbind(longitude, latitude),
    cbind(lag(longitude), lag(latitude)),
    lonlat = TRUE)) %>%
  ungroup() %>%
  mutate(distance=distance/1000,
         distance_alone=case_when(move_alone==1 ~ distance,
                                  TRUE ~ 0),
         move_10=ifelse(distance_alone>=10,1,0),
         move_10_parent=ifelse(move_alone == 0 & distance >= 10, 1, 0))

# add parents migration background
person_address_years_10=person_address_years_10 %>%
  left_join(data_parents_10,
            by="rp_id") %>%
  filter(!is.na(migration_parents))

# add variable that has value 1 if people moved in the past with a parent (likely in childhood)
person_address_years_10 = person_address_years_10 %>%
  group_by(rp_id) %>%
  mutate(
    
    moved_with_parent = move_alone == 0 & move_10_parent == 1,
    
    year_first_move_parent = ifelse(
      any(moved_with_parent, na.rm = TRUE),
      min(year[moved_with_parent], na.rm = TRUE),
      NA_real_
    ),
    
    
    own_mig_background = ifelse(
      !is.na(year_first_move_parent) & year >= year_first_move_parent,
      1,
      0
    )
  ) %>%
  ungroup()

# add father occupation at birth, that remains constant over life
# proxy of social class
person_address_years_10=person_address_years_10 %>%
  left_join(data_occupation %>%
              group_by(rp_id)%>%
              slice(1)%>%
              ungroup()%>%
              dplyr::select(rp_id, occupation_father=HISCLASS_5_father), by="rp_id") 



# look at first move alone
# keep observation until person move alone
# all observations if the person never moved alone
data_first_move_alone_10 = person_address_years_10 %>%
  group_by(rp_id) %>%
  mutate(event_year = if (any(move_10 == 1, na.rm = TRUE)) min(year[move_10 == 1], na.rm = TRUE) else Inf) %>%
  filter(year <= event_year) %>%
  ungroup()

# we want to evaluate the association between likelihood to move and the characteristics in the household before the move
# we lag the variables that change over time: age, geographical area, household composition, size of the municipality, duration of residence
data_lagged_10=data_first_move_alone_10 %>%
  group_by(rp_id) %>%
  mutate(lag_age=lag(age),
         lag_geographical_area=lag(geographical_area),
         lag_mother_family=lag(mother_family),
         lag_father_family=lag(father_family),
         lag_parents_family=lag(parents_family),
         lag_partner_family=lag(partner_family),
         lag_sibling_family=lag(sibling_family),
         lag_grandparent_family=lag(grandparent_family),
         lag_children_family=lag(children_family),
         lag_nonkin_family=lag(nonkin_family),
         lag_extended_family=lag(extended_family),
         lag_municipality_size=lag(municipality_size),
         lag_duration=lag(duration)
  ) %>%
  ungroup()

# HISCLASS, standardized occupations
# 5 classes plus 'Unknown'
data_lagged_10=data_lagged_10 %>%
  mutate(occupation_father=case_when(
    occupation_father==1 ~ "Elite",
    occupation_father==2 ~ "Lower middle class",
    occupation_father==3 ~ "Skilled workers",
    occupation_father==4 ~ "Self-employed farmers",
    occupation_father==5 ~ "Unskilled workers",
    occupation_father==-1 ~ "Unknown"
  ))

data_lagged_10=data_lagged_10 %>%
  mutate(own_mig_background=as.factor(own_mig_background))

# change reference for variables parents' migration background and father's occupation at birth 
data_lagged_10$migration_parents=relevel(factor(data_lagged_10$migration_parents), ref="none")
data_lagged_10$occupation_father=relevel(factor(data_lagged_10$occupation_father), ref="Skilled workers")

# we observe individuals from age 12 to age 50, as that's the age of the most recent cohort at the end of the observation period
# have only age between 12 and 50, and look only at movements that happen after age 12
data_lagged_reduced_10=data_lagged_10 %>%
  filter(age>=12, age<=50)

data_lagged_reduced_10 = data_lagged_reduced_10 %>%
  group_by(rp_id) %>%
  arrange(year) %>%  
  mutate(row_num = row_number()) %>%
  mutate(
    
    first_move_10_row = if_else(
      any(move_10 == 1, na.rm = TRUE),
      min(row_num[move_10 == 1]),
      as.numeric(NA)
    )
  ) %>%
  filter(
    
    is.na(first_move_10_row) | row_num <= first_move_10_row
  ) %>%
  dplyr::select(-row_num, -first_move_10_row) %>%
  ungroup()%>%
  arrange(rp_id,year)

# family composition as binary variables
data_lagged_reduced_10=data_lagged_reduced_10 %>%
  mutate(lag_mother_family=as.factor(lag_mother_family),
         lag_father_family=as.factor(lag_father_family),
         lag_partner_family=as.factor(lag_partner_family),
         lag_sibling_family=as.factor(lag_sibling_family),
         lag_grandparent_family=as.factor(lag_grandparent_family),
         lag_children_family=as.factor(lag_children_family),
         lag_nonkin_family=as.factor(lag_nonkin_family),
         lag_extended_family=as.factor(lag_extended_family))

data_lagged_reduced_10=data_lagged_reduced_10 %>%
  mutate(own_mig_background=as.factor(own_mig_background))

# change reference categories for several variables
data_lagged_reduced_10$migration_parents=relevel(factor(data_lagged_reduced_10$migration_parents), ref="none")
data_lagged_reduced_10$occupation_father=relevel(factor(data_lagged_reduced_10$occupation_father), ref="Skilled workers")
data_lagged_reduced_10$lag_parents_family=as.factor(data_lagged_reduced_10$lag_parents_family)
data_lagged_reduced_10$lag_parents_family=relevel(factor(data_lagged_reduced_10$lag_parents_family), ref="none")
data_lagged_reduced_10$lag_municipality_size=relevel(factor(data_lagged_reduced_10$lag_municipality_size), ref="rural")

#### LOGISTIC MODEL ####

logit_model_10 = glm(
  move_10 ~ gender + lag_geographical_area + lag_age + I(lag_age^2) + birth_cohort + 
    migration_parents + own_mig_background + occupation_father +  lag_municipality_size + lag_duration +
    lag_parents_family + lag_partner_family + lag_sibling_family + 
    lag_grandparent_family + lag_children_family + lag_nonkin_family + lag_extended_family,
  data = data_lagged_reduced_10,
  family = binomial
)

# extract clustered standard errors from the model to account for multiple observations per individuals
clustered_vcov = vcovCL(logit_model_10, cluster = data_lagged_reduced_10$rp_id)
clustered_se = sqrt(diag(clustered_vcov))

stargazer(logit_model_10,
          type = "html",
          se = list(clustered_se),  # Use clustered standard errors
          title = "Internal Migration Alone: Log-Odds Coefficients with Clustered Standard Errors",
          dep.var.labels = "First Internal Migration Alone",
          model.numbers = FALSE,
          notes = c("Standard errors clustered by individual ID in parentheses."),
          notes.align = "l",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "logistic_10.html")

# add in the model interaction between gender and parents' migration background
logit_model_gender_10 = glm(
  move_10 ~ lag_geographical_area + lag_age + I(lag_age^2) + birth_cohort + 
    migration_parents * gender + own_mig_background + occupation_father +  lag_municipality_size + lag_duration +
    lag_parents_family + lag_partner_family + lag_sibling_family + 
    lag_grandparent_family + lag_children_family + lag_nonkin_family + lag_extended_family,
  data = data_lagged_reduced_10,
  family = binomial
)


# Get predictions with both migration_parents and gender
predictions_interaction = avg_predictions(logit_model_gender_10, 
                                           variables = c("migration_parents", "gender"),
                                           vcov = ~rp_id)

# Create and order labels
predictions_interaction$migration_parents_label = case_when(
  predictions_interaction$migration_parents == "both" ~ "Both Parents",
  predictions_interaction$migration_parents == "only mother" ~ "Only Mother",
  predictions_interaction$migration_parents == "only father" ~ "Only Father",
  predictions_interaction$migration_parents == "none" ~ "None"
)

predictions_interaction$migration_parents_label = factor(
  predictions_interaction$migration_parents_label, 
  levels = c("None", "Only Mother", "Only Father", "Both Parents")
)

predictions_interaction$gender_label = case_when(
  predictions_interaction$gender == "m" ~ "Male",
  predictions_interaction$gender == "v" ~ "Female",
  TRUE ~ as.character(predictions_interaction$gender)
)

# plot average predicted probability of moving for the first time within the Netherlands (>10 km) without parents after age 12
# by the parents’ migration background and the research person’s gender
predictions_interaction %>%
  ggplot(aes(x = migration_parents_label, 
             y = estimate, 
             shape = gender_label)) +
  geom_point(size = 4, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.1, size = 1,
                position = position_dodge(width = 0.3)) +
  coord_flip() +
  labs(
    x = "Parents' Migration Background",
    y = "Predicted Probability",
    shape = "Gender") +
  scale_shape_manual(values = c("Male" = 16, "Female" = 17)) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 16, face = "bold"),      
    axis.text = element_text(size = 12, face = "bold"),       
    legend.title = element_text(size = 14, face = "bold"),    
    legend.text = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 12, face = "bold")
  )

# add interaction between gender and household composition
logit_model_gender_household_10 = glm(
  move_10 ~ gender * (lag_parents_family + lag_partner_family + lag_sibling_family + 
                           lag_grandparent_family + lag_children_family + lag_nonkin_family + 
                           lag_extended_family) +
    lag_geographical_area + lag_age + I(lag_age^2) + birth_cohort + migration_parents + 
    own_mig_background + occupation_father + lag_municipality_size + lag_duration,
  data = data_lagged_reduced_10,
  family = binomial
)


# calculate marginal effects for all household composition variables, by gender
marg_effects_family_gender = avg_slopes(
  logit_model_gender_household_10, 
  variables = c("lag_parents_family", "lag_partner_family", "lag_sibling_family", 
                "lag_grandparent_family", "lag_children_family", "lag_nonkin_family", 
                "lag_extended_family"),
  by = "gender",  
  vcov = ~rp_id
)

# clean up the variable names
marg_effects_family_gender = marg_effects_family_gender %>%
  mutate(term = case_when(
    term == "lag_parents_family" & contrast == "only mother - none" ~ "Parents: Only Mother",
    term == "lag_parents_family" & contrast == "only father - none" ~ "Parents: Only Father", 
    term == "lag_parents_family" & contrast == "both - none" ~ "Parents: Both",
    term == "lag_partner_family" ~ "Spouse",
    term == "lag_sibling_family" ~ "Sibling",
    term == "lag_grandparent_family" ~ "Grandparent",
    term == "lag_children_family" ~ "Child",
    term == "lag_nonkin_family" ~ "Unrelated",
    term == "lag_extended_family" ~ "Extended",
    TRUE ~ term
  ))

marg_effects_family_gender$gender_label = case_when(
  marg_effects_family_gender$gender == "m" ~ "Male",
  marg_effects_family_gender$gender == "v" ~ "Female",
  TRUE ~ as.character(marg_effects_family_gender$gender)
)

marg_effects_family_gender$term = factor(
  marg_effects_family_gender$term,
  levels = c("Spouse", "Parents: Only Mother", "Parents: Only Father", "Parents: Both", 
             "Grandparent", "Child", "Sibling", "Extended", "Unrelated")
)

# plot marginal effects of household composition on the probability of moving for the first time within the Netherlands (>10 km) without parents after age 12
# by the research person’s gender
ggplot(marg_effects_family_gender, aes(x = term, y = estimate, shape = gender_label)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.1, size = 1, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1, alpha = 0.6) +
  coord_flip() +
  labs(
    x = "Household Member Presence", 
    y = "Marginal Effect",
    shape = "Gender"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 16, face = "bold"),      
    axis.text = element_text(size = 12, face = "bold"),       
    legend.title = element_text(size = 14, face = "bold"),    
    legend.text = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  scale_shape_manual(values = c("Male" = 16, "Female" = 17))  
