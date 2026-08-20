# compute minimum between year of death and end observation 
data_rp=data_rp%>%
  mutate(year_end=pmin(year_death,end_obs, na.rm = TRUE))

# one row for each year (and age)
person_years=data_rp%>%
  dplyr::select(rp_id, year_birth, year_marriage, year_end, gender)%>%
  filter(year_birth<1890)%>%
  rowwise()%>%
  mutate(year=list(seq(year_birth, year_end))) %>%
  unnest(year)%>%
  ungroup()%>%
  mutate(age=year-year_birth)

# order address by start date
data_address=data_address%>%
  group_by(rp_id, start_address)%>%
  arrange(rp_id,start_address)%>%
  ungroup()

data_address=data_address %>%
  filter(rp_id %in% c(person_years$rp_id))

data_address_new=data_address %>%
  group_by(rp_id)%>%
  mutate(end_address=lead(start_address)-1,
         end_address = if_else(is.na(end_address), 1940, end_address))%>%
  ungroup()

address_years = data_address_new %>%
  rowwise() %>%
  mutate(year = list(seq(start_address, end_address))) %>%
  unnest(year) %>%
  dplyr::select(rp_id, year, municipality_HSN, n_house)

person_address_years = person_years %>%
  left_join(address_years, by = c("rp_id", "year"))

person_address_years = person_address_years %>%
  distinct(rp_id, year, municipality_HSN, n_house, .keep_all = TRUE)

# add 10-years birth cohort
person_address_years=person_address_years %>%
  mutate(birth_cohort=cut(
    year_birth,
    breaks = seq(1850, 1940, by = 10),  
    right = FALSE,                      
    labels = paste(seq(1850, 1930, by = 10), seq(1859, 1939, by = 10), sep = "-"),  # Label each cohort
    include.lowest = TRUE                
  ))

# add province and geographical area
person_address_years=person_address_years %>%
  left_join(municipality %>% 
              group_by(standardised_HSN)%>%
              slice(1)%>%
              dplyr::select(municipality_HSN=standardised_HSN, province),
            by="municipality_HSN") %>%
  mutate(geographical_area=case_when(province %in% c("Drenthe","Friesland","Groningen") ~ "North",
                                     province %in% c("Gelderland","Overijssel") ~ "East",
                                     province %in% c("Noord-Brabant","Limburg") ~ "South",
                                     province %in% c("Noord-Holland","Zuid-Holland","Utrecht", "Zeeland") ~ "West",
                                     province %in% c("NULL", "Unknown") ~ NA))


# add household composition at each household
# each number is a different 'relationship with the RP'
# what each number represents is present in the data documentation
person_address_years=person_address_years %>%
  left_join(data_dynamic_household %>%
              dplyr::select(rp_id, n_house, relative_household),
            by=c("rp_id","n_house")) %>%
  mutate(size_household=lengths(relative_household))

person_address_years = person_address_years %>%
  mutate(nuclear_family=ifelse(size_household==1,1,0)) %>%
  mutate(father_family=lapply(relative_household, function(x) {
    if (any(x %in% c(11, 81, 148))) {
      return(1)  
    } else {
      return(0)  
    }
  })) %>%
  mutate(mother_family=lapply(relative_household, function(x) {
    if (any(x %in% c(21, 82, 149))) {
      return(1)  
    } else {
      return(0)  
    }
  })) %>%
  mutate(parents_family=case_when(
    mother_family==0 & father_family==0 ~ "none",
    mother_family==1 & father_family==0 ~ "only mother",
    mother_family==0 & father_family==1 ~ "only father",
    mother_family==1 & father_family==1 ~ "both")) %>%
  mutate(partner_family=lapply(relative_household, function(x) {
    if (any(x %in% c(2, 146, 110, 145, 147))) {
      return(1)  
    } else {
      return(0)  
    }
  })) %>%
  mutate(sibling_family=lapply(relative_household, function(x) {
    if (any(x %in% c(12,85,87,112,121,128,138,141,22,86,88,118,122,129,139,142))) {
      return(1)  
    } else {
      return(0)  
    }
  })) %>%
  mutate(grandparent_family=lapply(relative_household, function(x) {
    if (any(x %in% c(10,20))) {
      return(1)  
    } else {
      return(0)  
    }
  })) %>%
  mutate(children_family=lapply(relative_household, function(x) {
    if (any(x %in% c(3,5,8,111,114,123,4,6,9,116,124,133))) {
      return(1)  
    } else {
      return(0)  
    }
  })) %>%
  mutate(nonkin_family=lapply(relative_household, function(x) {
    if (any(x %in% c(90,91,92,93,94,95,96,97,98,99))) {
      return(1)  
    } else {
      return(0)  
    }
  })) %>%
  mutate(grandchild_family=lapply(relative_household, function(x) {
    if (any(x %in% c(30,40))) {
      return(1)  
    } else {
      return(0)  
    }
  })) %>%
  mutate(extended_family=lapply(relative_household, function(x) {
    if (any(x %in% c(13,14,15,16,17,18,23,24,25,26,27,28,51,52,53,54,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,83,84,131,132))) {
      return(1)  
    } else {
      return(0)  
    }
  }))

person_address_years=person_address_years %>%
  mutate(father_family=as.numeric(father_family),
         mother_family=as.numeric(mother_family),
         partner_family=as.numeric(partner_family),
         sibling_family=as.numeric(sibling_family),
         grandparent_family=as.numeric(grandparent_family),
         children_family=as.numeric(children_family),
         nonkin_family=as.numeric(nonkin_family),
         grandchild_family=as.numeric(grandchild_family),
         extended_family=as.numeric(extended_family))

# variable that states the size of the municipality
person_address_years=person_address_years %>% 
  left_join(population_2 %>%
              filter(!is.na(municipality_HSN))%>%
              dplyr::select(municipality_HSN, year, municipality_size),
            by=c("year","municipality_HSN"))

# add latitude and longitude of the municipality
person_address_years <- person_address_years %>%
  left_join(spatio_temporal %>%
              dplyr::select(amco = `Amsterdam code`, latitude, longitude, from, till),
            by = "amco") %>%
  filter(year >= from & year <= till) %>%
  dplyr::select(-from, -till)

# variable that says whether people are moving or not
# move defined as any change of municipality 
person_address_years = person_address_years %>%
  group_by(rp_id) %>%
  mutate(move = as.integer(municipality_HSN != lag(municipality_HSN, default = first(municipality_HSN)))) %>%
  mutate(move = replace(move, row_number() == 1, 0)) %>%
  ungroup()

# variable that says since how many years people are living in a certain municipality - duration of residence
person_address_years = person_address_years %>%
  group_by(rp_id) %>%
  mutate(
    spell_id = cumsum(move),
    duration = row_number() - match(first(spell_id), spell_id) + 1
  ) %>%
  group_by(rp_id, spell_id) %>%
  mutate(duration = row_number()) %>%
  ungroup() %>%
  dplyr::select(-spell_id)

# variable that states whether people are moving alone, with at least one parent, or not moving
person_address_years = person_address_years %>%
  group_by(rp_id) %>%
  mutate(move_alone = case_when(
    move==1 & father_family==0 & mother_family==0 ~ 1,
    move==1 & (father_family==1 & mother_family==1 | father_family==1 & mother_family==0 | father_family==0 & mother_family==1) ~ 0,
    move==0 ~ NA
  ))



