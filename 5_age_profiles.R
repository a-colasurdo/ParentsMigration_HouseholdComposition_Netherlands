# age profiles any internal moves
age_profiles = person_address_years %>%
  filter(birth_cohort %in% c("1850-1859","1860-1869","1870-1879","1880-1889"))%>%
  group_by(birth_cohort, lag_age=lag(age)) %>%
  summarise(
    moving_rate = mean(move, na.rm = TRUE),
    n_observations = n(),
    .groups = 'drop'
  )

col.plot=c("#ca0020","#f4a582","#92c5de","#0571b0")

# smoothed age profiles any internal moves
age_profiles %>%
  ggplot(aes(x = lag_age, y = moving_rate, color = birth_cohort)) +
  geom_smooth(span=.24, se=FALSE, size=2) +
  xlim(0,50)+
  labs(x = "Age",
    y = "Migration Rate",
    color = "Birth Cohort"
  ) +
  theme_classic() +
  scale_color_manual(values = col.plot) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),      
    axis.text = element_text(size = 14, face = "bold"),       
    legend.title = element_text(size = 14, face = "bold"),    
    legend.text = element_text(size = 12, face = "bold")      
  )

# age profiles internal moves without parents
age_profiles_alone = person_address_years %>%
  filter(birth_cohort %in% c("1850-1859","1860-1869","1870-1879","1880-1889"))%>%
  group_by(birth_cohort, lag_age=lag(age)) %>%
  mutate(move_alone=ifelse(is.na(move_alone),0,1)) %>%
  summarise(
    moving_rate = mean(move_alone, na.rm = TRUE),
    n_observations = n(),
    .groups = 'drop'
  )

# smoothed age profiles internal moves without parents
age_profiles_alone %>%
  ggplot(aes(x = lag_age, y = moving_rate, color = birth_cohort)) +
  geom_smooth(span=.24, se=FALSE, size=2) +
  xlim(0,60)+
  labs(x = "Age",
       y = "Migration Rate",
       color = "Birth Cohort"
  ) +
  theme_classic() +
  scale_color_manual(values = col.plot) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),      
    axis.text = element_text(size = 14, face = "bold"),       
    legend.title = element_text(size = 14, face = "bold"),    
    legend.text = element_text(size = 12, face = "bold")      
  )

# smoothed age distribution of first move without parents
data_lagged_reduced %>%
  filter(move_alone==1,
         birth_cohort %in% c("1850-1859","1860-1869","1870-1879","1880-1889"))%>%
  ggplot(aes(x=age, color=birth_cohort))+
  geom_density(size=2)+
  theme_classic() +
  scale_color_manual(values = col.plot) +
  labs(x = "Age",
       y = "Proportion",
       color = "Birth Cohort"
  )+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),      
    axis.text = element_text(size = 14, face = "bold"),       
    legend.title = element_text(size = 14, face = "bold"),    
    legend.text = element_text(size = 12, face = "bold")      
  )

