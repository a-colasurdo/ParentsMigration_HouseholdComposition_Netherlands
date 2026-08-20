# import dataset with general information on Research Persons (RP's)
BEVOP = read.dbf("U:/Cloud/HSN/HSN-lifecourse/Data/BEVOP.DBF")

data_rp=BEVOP %>%
  dplyr::select(rp_id=IDNR,
                n_rp=PERSNROP,
                n_rp_father=PERSOP_V,
                n_rp_mother=PERSOP_M,
                year_birth=GEBJAROP,
                year_death=OVLJAROP,
                year_marriage=HUWJAROP,
                year_obs_marriage=HOBJAROP,
                n_rp_partner=PERSNR_P,
                gender=SEX_OP,
                start_obs=OBSJAROP,
                end_obs=OBEJAROP)


# import dataset with information on RPs address
BEVADRES = read.dbf("U:/Cloud/HSN/HSN-lifecourse/Data/BEVADRES.DBF")

data_address=BEVADRES %>%
  dplyr::select(rp_id=IDNR,
                n_house=HUISHNR,
                id_house=IDNRHS,
                start_address=ADRESJAR,
                end_address=ADUITJAR,
                municipality=GEMEENTE)

# import excel file with standardized locations
placename_variations = read_csv("U:/Cloud/HSN/2024-05-20 - placename variations.csv")

# extract name municipalities from the address file, and transform to lowercase
municipality=data_address %>%
  dplyr::select(municipality) %>%
  mutate(municipality=tolower(municipality))

# standardize the name of the municipalities in the address file
municipality=municipality %>%
  left_join(placename_variations %>%
              rename(municipality=original),
            by="municipality")

# dataset with information on RPs + address
data_rp_address=data_address %>%
  left_join(data_rp,
            by="rp_id")


# import dataset with RPs relatives static information
BEVSTATP = read.dbf("U:/Cloud/HSN/HSN-lifecourse/Data/BEVSTATP.DBF")

data_relatives_static=BEVSTATP %>%
  dplyr::select(rp_id=IDNR,
                id_house=IDNRHS,
                id_relative=IDNRHSP,
                n_person=PERSNR,
                start_obs_relative=INSJAR,
                end_obs_relative=UITJAR,
                role_household=FUNCTIE,
                gender_relative=GESLACHT,
                year_birth_relative=GEBJAR,
                municipality_birth_relative=GEBGEM,
                year_death_relative=OVLJAR,
                municipality_death_relative=OVLGEM)


# import dataset with RPs relatives dynamic information
BEVDYNAP = read.dbf("U:/Cloud/HSN/HSN-lifecourse/Data/BEVDYNAP.DBF")

data_relatives_dynamic=BEVDYNAP %>%
  dplyr::select(rp_id=IDNR,
                n_house=HUISHNR,
                id_relative=IDNRHSP,
                dynamic_type=DYNATYPE,
                dynamic_text=DYNATEXT,
                start_obs_dynamic=INGJAR,
                end_obs_dynamic=UIGJAR,
                dynamic_code=DYNAINHD,
                dynamic_code_content=RELACODE)

# standardize the name of the municipalities related to place of birth and place of death of relatives
# transform to lowercase the name of the municipalities

municipality_relatives=data_relatives_static %>%
  mutate(municipality_birth_relative=stri_trans_tolower(municipality_birth_relative),
         municipality_death_relative=stri_trans_tolower(municipality_death_relative)) %>%
  left_join(placename_variations %>%
              dplyr::select(original, standardised_HSN) %>%
              rename(municipality_birth_relative=original,
                     municipality_birth_relative_HSN=standardised_HSN),
            by="municipality_birth_relative") %>%
  left_join(placename_variations %>%
              dplyr::select(original, standardised_HSN) %>%
              rename(municipality_death_relative=original,
                     municipality_death_relative_HSN=standardised_HSN),
            by="municipality_death_relative") %>%
  mutate(municipality_birth_relative_HSN=ifelse(municipality_birth_relative_HSN=="Unknown", NA, municipality_birth_relative_HSN),
         municipality_death_relative_HSN=ifelse(municipality_death_relative_HSN=="Unknown", NA, municipality_death_relative_HSN))

# add standardized location in the address file 
data_address=data_address  %>%
  cbind(municipality %>%
          dplyr::select(municipality_HSN=standardised_HSN))


# add information of parents year and location of birth and death to information of research person, keep one row for each research person
data_rp_relatives=data_rp %>%
  left_join(municipality_relatives %>%
              dplyr::select(rp_id,n_rp=n_person,municipality_birth=municipality_birth_relative_HSN,municipality_death=municipality_death_relative_HSN),
            by=c("rp_id","n_rp")) %>%
  group_by(rp_id) %>%
  filter(if(all((is.na(municipality_death)))) row_number() == 1 else !is.na(municipality_death)) %>%
  distinct(rp_id, .keep_all = TRUE) %>%
  left_join(municipality_relatives %>%
              dplyr::select(rp_id,n_rp_father=n_person,year_birth_father=year_birth_relative,municipality_birth_father=municipality_birth_relative_HSN,year_death_father=year_death_relative,municipality_death_father=municipality_death_relative_HSN),
            by=c("rp_id","n_rp_father")) %>%
  group_by(rp_id) %>%
  filter(if(all(is.na(municipality_death_father))) row_number() == 1 else !is.na(municipality_death_father)) %>%
  distinct(rp_id, .keep_all = TRUE) %>%
  left_join(municipality_relatives %>%
              dplyr::select(rp_id,n_rp_mother=n_person,year_birth_mother=year_birth_relative,municipality_birth_mother=municipality_birth_relative_HSN,year_death_mother=year_death_relative,municipality_death_mother=municipality_death_relative_HSN),
            by=c("rp_id","n_rp_mother")) %>%
  group_by(rp_id) %>%
  filter(if(all(is.na(municipality_death_mother))) row_number() == 1 else !is.na(municipality_death_mother)) %>%
  distinct(rp_id, .keep_all = TRUE)

# extract information on occupation, as relatives' dynamic information
data_occupation=data_relatives_dynamic %>%
  filter(dynamic_type %in% c(5,15)) %>%
  mutate(n_person=as.numeric(substr(as.character(id_relative), nchar(id_relative) - 1, nchar(id_relative)))) 

# add occupation research person, occupation father, occupation mother
data_occupation=data_rp %>%
  left_join(data_occupation %>%
              filter(n_house==1) %>%
              dplyr::select(rp_id, n_rp=n_person, occupation_rp=dynamic_code),
            by=c("rp_id","n_rp"))%>%
  left_join(data_occupation %>%
              filter(n_house==1) %>%
              dplyr::select(rp_id, n_rp_father=n_person, occupation_father=dynamic_code),
            by=c("rp_id","n_rp_father")) %>%
  left_join(data_occupation %>%
              filter(n_house==1) %>%
              dplyr::select(rp_id, n_rp_mother=n_person, occupation_mother=dynamic_code),
            by=c("rp_id","n_rp_mother"))

# import dataset HISCO codes for all the occupation observed in the HSN
HISCO_codes = read.csv("U:/Cloud/HSN/occupation/HSN_HISCO_release_2020_02.csv")

# standardize, adding HISCO, HISCLASS-5 for RPs, RPs father, RPs mother
data_occupation=data_occupation %>%
  mutate(occupation_rp=stri_trans_tolower(occupation_rp),
         occupation_father=stri_trans_tolower(occupation_father),
         occupation_mother=stri_trans_tolower(occupation_mother))%>%
  left_join(HISCO_codes %>%
              dplyr::select(Original, HISCO, HISCLASS_5) %>%
              rename(occupation_rp=Original,
                     HISCO_rp=HISCO,
                     HISCLASS_5_rp=HISCLASS_5),
            by="occupation_rp") %>%
  left_join(HISCO_codes %>%
              dplyr::select(Original, HISCO, HISCLASS_5) %>%
              rename(occupation_father=Original,
                     HISCO_father=HISCO,
                     HISCLASS_5_father=HISCLASS_5),
            by="occupation_father") %>%
  left_join(HISCO_codes %>%
              dplyr::select(Original, HISCO, HISCLASS_5) %>%
              rename(occupation_mother=Original,
                     HISCO_mother=HISCO,
                     HISCLASS_5_mother=HISCLASS_5),
            by="occupation_mother")


# replace with -1 (unknown) in HISCLASS_5 if NA
data_occupation=data_occupation %>%
  mutate(across(c(HISCLASS_5_rp, HISCLASS_5_father, HISCLASS_5_mother), ~replace_na(.x,-1)))

# for each research person, one row for each household
data_household = data_address %>%
  group_by(rp_id, n_house, id_house, municipality_HSN) %>%
  summarise(start_address = min(start_address, na.rm = TRUE),
            end_address = max(end_address, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(rp_id,n_house,start_address)

# for each household, the household composition
data_dynamic_household=data_relatives_dynamic %>%
  group_by(rp_id,n_house) %>%
  filter(dynamic_type==4) %>%
  summarise(relative_household=list(dynamic_code_content))

# for each research person, one row for each household and the household composition, with the size of the household
data_household=data_household %>%
  left_join(data_dynamic_household,
            by=c("rp_id","n_house")) %>%
  mutate(size_household=lengths(relative_household))

# unique dataset with municipality birth parents and individuals
data_parents=data_household %>%
  left_join(data_rp_relatives %>%
              dplyr::select(rp_id, gender, year_birth, year_death, municipality_birth, municipality_death,
                            year_birth_father, municipality_birth_father, year_death_father, 
                            municipality_death_father, year_birth_mother, municipality_birth_mother,
                            year_death_mother, municipality_death_mother),
            by="rp_id") 

# dataset with muncipality's population in each year
HDNG=read.csv("U:/Cloud/HSN/Data/HDNG_v4 (1).txt")

# population betwenn 1850 and 1940, our observation period
population=HDNG[HDNG$description=="Bevolking" & HDNG$year>1850 & HDNG$year<=1940,] %>% filter(!is.na(amco))

# some municipalities have information by sex
population_1 = population %>%
  filter(sex %in% c("M","V"), information == "totaal") %>%
  group_by(amco, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

population_2=population_1 %>%
  rbind(population %>%
          filter(sex=="_T", information=="totaal") %>%
          dplyr::select(amco, year, value)) %>%
  arrange(amco, year)

population_2=population_2 %>%
  filter(value!=0) %>%
  group_by(amco, year) %>%
  slice(1)

# classify municipalities based on their size
population_2=population_2 %>%
  mutate(municipality_size=case_when(value<10000 ~ "rural", value>=10000 & value<50000 ~ "medium", value>=50000 ~ "large"))

population_2=population_2 %>%
  left_join(municipality %>%
              group_by(standardised_HSN) %>%
              slice(1) %>%
              dplyr::select(amco=AMCO, municipality_HSN=standardised_HSN),
            by="amco")

add_1850 = population_2 %>%
  filter(year == 1851) %>%
  mutate(year = 1850)

population_2 = bind_rows(population_2, add_1850) %>%
  arrange(amco, year)

# import dataset with municipality's coordinates for different historical periods
spatio_temporal = read_excel("Data/DutchMunicipalities1812-2012Spatio-Temporal.xlsx")

spatio_temporal = spatio_temporal %>%
  filter(`till JJJJMMDD`>=18500101,
         `from JJJJMMDD`<=19401231) %>%
  rename(latitude=`latitude (dec.degrees)`,
         longitude=`longitude (dec.degrees)`,
         from=`from JJJJMMDD`,
         till=`till JJJJMMDD`) %>%
  mutate(from=substr(from,1,4),
         till=substr(till,1,4),
         from=as.numeric(from),
         till=as.numeric(till))

