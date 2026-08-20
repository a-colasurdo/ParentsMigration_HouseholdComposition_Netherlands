#### MIGRATION AS ANY INTERMUNICIPAL MOVE ####
logit_model <- glm(
  move_alone ~ gender + lag_geographical_area + lag_age + I(lag_age^2) + birth_cohort + 
    migration_parents + own_mig_background + occupation_father +  lag_municipality_size + lag_duration +
    lag_parents_family + lag_partner_family + lag_sibling_family + 
    lag_grandparent_family + lag_children_family + lag_nonkin_family + lag_extended_family,
  data = data_lagged_reduced,
  family = binomial
)

# Extract clustered standard errors from the model
clustered_vcov<- vcovCL(logit_model, cluster = data_lagged_reduced$rp_id)
clustered_se<- sqrt(diag(clustered_vcov))

stargazer(logit_model,
          type = "html",
          se = list(clustered_se),  
          model.numbers = FALSE,
          notes = c("Standard errors clustered by individual ID in parentheses."),
          notes.align = "l",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "logistic_any_move.html")

#### MIGRATION AS INTERMUNICIPAL MOVE LARGER THAN 40 KILOMETERS ####
data_parents_40=data_parents %>%
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
    migration_father=ifelse(distance_father>=40,1,0),
    migration_mother=ifelse(distance_mother>=40,1,0),
    migration_parents=case_when(migration_father==0 & migration_mother==0 ~ "none",
                                migration_father==1 & migration_mother==0 ~ "only father",
                                migration_father==0 & migration_mother==1 ~ "only mother",
                                migration_father==1 & migration_mother==1 ~ "both")) %>%
  dplyr::select(rp_id, migration_parents)

# distance move
person_address_years_40 = person_address_years %>%
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
         move_40=ifelse(distance_alone>=40,1,0),
         move_40_parent=ifelse(move_alone == 0 & distance >= 40, 1, 0))

#add parents migration background
person_address_years_40=data_parents_40 %>%
  left_join(person_address_years_40,
            by="rp_id")

# variable that has value 1 if people moved in the past with a parent (likely in childhood)
person_address_years_40 = person_address_years_40 %>%
  group_by(rp_id) %>%
  mutate(
    
    moved_with_parent = move_alone == 0 & move_40_parent == 1,
    
    
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
person_address_years_40=person_address_years_40 %>%
  left_join(data_occupation %>%
              group_by(rp_id)%>%
              slice(1)%>%
              ungroup()%>%
              dplyr::select(rp_id, occupation_father=HISCLASS_5_father), by="rp_id") 



# look at first move alone
# keep observation until person move alone
# all observations if the person never moved alone
data_first_move_alone_40 = person_address_years_40 %>%
  group_by(rp_id) %>%
  mutate(event_year = if (any(move_40 == 1, na.rm = TRUE)) min(year[move_40 == 1], na.rm = TRUE) else Inf) %>%
  filter(year <= event_year) %>%
  ungroup()

# we want to evaluate the association between likelihood to move and the characteristics in the household before the move
# we lag the variables that change over time: age, geographical area, household composition, size of the municipality, duration of residence
data_lagged_40=data_first_move_alone_40 %>%
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

data_lagged_40=data_lagged_40 %>%
  mutate(occupation_father=case_when(
    occupation_father==1 ~ "Elite",
    occupation_father==2 ~ "Lower middle class",
    occupation_father==3 ~ "Skilled workers",
    occupation_father==4 ~ "Self-employed farmers",
    occupation_father==5 ~ "Unskilled workers",
    occupation_father==-1 ~ "Unknown"
  ))

data_lagged_40=data_lagged_40 %>%
  mutate(own_mig_background=as.factor(own_mig_background))

data_lagged_40$migration_parents=relevel(factor(data_lagged_40$migration_parents), ref="none")
data_lagged_40$occupation_father=relevel(factor(data_lagged_40$occupation_father), ref="Skilled workers")

# observe individuals between age 12 and 50, age of the most recent cohort at the end of the observation period
data_lagged_reduced_40=data_lagged_40 %>%
  filter(age>=12, age<=50)

data_lagged_reduced_40 = data_lagged_reduced_40 %>%
  group_by(rp_id) %>%
  arrange(year) %>%  # Sort by year
  mutate(row_num = row_number()) %>%
  mutate(
    # Find the row number of first move alone event
    first_move_40_row = if_else(
      any(move_40 == 1, na.rm = TRUE),
      min(row_num[move_40 == 1]),
      as.numeric(NA)
    )
  ) %>%
  filter(
    # Keep all rows up to and including the first move alone event
    is.na(first_move_40_row) | row_num <= first_move_40_row
  ) %>%
  dplyr::select(-row_num, -first_move_40_row) %>%
  ungroup()%>%
  arrange(rp_id,year)

# family composition as binary variables
data_lagged_reduced_40=data_lagged_reduced_40 %>%
  mutate(lag_mother_family=as.factor(lag_mother_family),
         lag_father_family=as.factor(lag_father_family),
         lag_partner_family=as.factor(lag_partner_family),
         lag_sibling_family=as.factor(lag_sibling_family),
         lag_grandparent_family=as.factor(lag_grandparent_family),
         lag_children_family=as.factor(lag_children_family),
         lag_nonkin_family=as.factor(lag_nonkin_family),
         lag_extended_family=as.factor(lag_extended_family))

data_lagged_reduced_40=data_lagged_reduced_40 %>%
  mutate(own_mig_background=as.factor(own_mig_background))

data_lagged_reduced_40$migration_parents=relevel(factor(data_lagged_reduced_40$migration_parents), ref="none")
data_lagged_reduced_40$occupation_father=relevel(factor(data_lagged_reduced_40$occupation_father), ref="Skilled workers")
data_lagged_reduced_40$lag_parents_family=as.factor(data_lagged_reduced_40$lag_parents_family)
data_lagged_reduced_40$lag_parents_family=relevel(factor(data_lagged_reduced_40$lag_parents_family), ref="none")
data_lagged_reduced_40$lag_municipality_size=relevel(factor(data_lagged_reduced_40$lag_municipality_size), ref="rural")

#### LOGISTIC MODEL ####

logit_model_40 <- glm(
  move_40 ~ gender + lag_geographical_area + lag_age + I(lag_age^2) + birth_cohort + 
    migration_parents + own_mig_background + occupation_father +  lag_municipality_size + lag_duration +
    lag_parents_family + lag_partner_family + lag_sibling_family + 
    lag_grandparent_family + lag_children_family + lag_nonkin_family + lag_extended_family,
  data = data_lagged_reduced_40,
  family = binomial
)

# Extract clustered standard errors from the model
clustered_vcov<- vcovCL(logit_model_40, cluster = data_lagged_reduced_40$rp_id)
clustered_se<- sqrt(diag(clustered_vcov))

stargazer(logit_model_40,
          type = "html",
          se = list(clustered_se), 
          model.numbers = FALSE,
          notes = c("Standard errors clustered by individual ID in parentheses"),
          notes.align = "l",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "logistic_40.html")

