#### NEGATIVE BINOMIAL MOVES LARGER THAN 10 KILOMETERS ####

# create variable that says how many time a person moved without parents between age 12 and 50
# for time-varying variables, keep values at age 12
person_years_negbin_10=person_address_years_10 %>%
  group_by(rp_id)%>%
  mutate(move_childhood=ifelse(any(own_mig_background==1),1,0))%>%
  ungroup() %>%
  filter(age>=12, age<=50)%>%
  group_by(rp_id)%>%
  mutate(lag_age=lag(age),
         lag_geographical_area=lag(geographical_area),
         lag_municipality_size=lag(municipality_size),
         lag_nuclear=lag(nuclear_family),
         lag_father=lag(father_family),
         lag_mother=lag(mother_family),
         lag_partner=lag(partner_family),
         lag_sibling=lag(sibling_family),
         lag_grandparent=lag(grandparent_family),
         lag_children=lag(children_family),
         lag_nonkin=lag(nonkin_family),
         lag_grandchild=lag(grandchild_family),
         lag_extended=lag(extended_family),
         total_moves=sum(move_10, na.rm=TRUE),
         first_area=first(geographical_area),
         first_municipality_size=first(municipality_size),
         first_nuclear=first(nuclear_family),
         first_father=first(father_family),
         first_mother=first(mother_family),
         first_partner=first(partner_family),
         first_sibling=first(sibling_family),
         first_grandparent=first(grandparent_family),
         first_children=first(children_family),
         first_nonkin=first(nonkin_family),
         first_grandchild=first(grandchild_family),
         first_extended=first(extended_family))%>%
  ungroup()

person_years_negbin_10=person_years_negbin_10 %>%
  mutate(occupation_father=case_when(
    occupation_father==1 ~ "Elite",
    occupation_father==2 ~ "Lower middle class",
    occupation_father==3 ~ "Skilled workers",
    occupation_father==4 ~ "Self-employed farmers",
    occupation_father==5 ~ "Unskilled workers",
    occupation_father==-1 ~ "Unknown"
  ))

person_years_negbin_10=person_years_negbin_10 %>%
  mutate(move_childhood=as.factor(move_childhood))

person_years_negbin_10$migration_parents=relevel(factor(person_years_negbin_10$migration_parents), ref="none")
person_years_negbin_10$occupation_father=relevel(factor(person_years_negbin_10$occupation_father), ref="Skilled workers")

person_years_negbin_10=person_years_negbin_10 %>%
  mutate(first_mother=as.factor(first_mother),
         first_father=as.factor(first_father),
         first_sibling=as.factor(first_sibling),
         first_grandparent=as.factor(first_grandparent),
         first_children=as.factor(first_children),
         first_nonkin=as.factor(first_nonkin),
         first_extended=as.factor(first_extended))

person_years_negbin_10$first_parents=as.factor(person_years_negbin_10$parents_family)
person_years_negbin_10$first_parents=relevel(factor(person_years_negbin_10$first_parents), ref="none")
person_years_negbin_10$first_municipality_size=relevel(factor(person_years_negbin_10$first_municipality_size), ref="rural")

# keep one row per individual
data_negbin_10=person_years_negbin_10 %>%
  group_by(rp_id)%>%
  slice(1)

# negative binomial model
nb_model_10 = glm.nb(
  total_moves ~ gender + migration_parents + move_childhood + birth_cohort + occupation_father + first_area + first_municipality_size + first_parents +first_sibling +
    first_grandparent + first_nonkin + first_extended,
  data = data_negbin_10
)

# add interaction between gender and parents' migration background
nb_model_gender = glm.nb(total_moves ~ first_area + first_municipality_size + gender*migration_parents + move_childhood + birth_cohort + occupation_father + first_parents +first_sibling +
                                first_grandparent + first_children + first_nonkin + first_extended,
                              data = data_negbin_10)

# get predictions for the interaction
pred_data = predictions(nb_model_gender, 
                         newdata = datagrid(gender = unique(data_negbin_10$gender),
                                            migration_parents = unique(data_negbin_10$migration_parents)))

# create proper labels for migration background 
pred_data$migration_parents_label = case_when(
  pred_data$migration_parents == "both" ~ "Both Parents",
  pred_data$migration_parents == "only mother" ~ "Only Mother",
  pred_data$migration_parents == "only father" ~ "Only Father",
  pred_data$migration_parents == "none" ~ "None"
)

pred_data$migration_parents_label <- factor(
  pred_data$migration_parents_label, 
  levels = c("None", "Only Mother", "Only Father", "Both Parents")
)

# create gender labels
pred_data$gender_label <- case_when(
  pred_data$gender == "m" ~ "Male",
  pred_data$gender == "v" ~ "Female",
  TRUE ~ as.character(pred_data$gender)
)

# plot expected number of moves within the Netherlands (>10 km) without parents after age 12
# by the parents’ migration background and the research person’s gender
pred_data %>%
  ggplot(aes(x = migration_parents_label, y = estimate, shape = gender_label))+
  geom_point(size = 4, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.3), width = 0.2, size = 1) +
  coord_flip() +
  labs(x = "Parents' Migration Background", 
       y = "Expected Number of Moves",
       shape = "Gender"
  ) +
  scale_shape_manual(values = c("Male" = 16, "Female" = 17)) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 16, face = "bold"),      
    axis.text = element_text(size = 12, face = "bold"),       
    legend.title = element_text(size = 14, face = "bold"),    
    legend.text = element_text(size = 12, face = "bold")
  )
