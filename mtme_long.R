#######################################
# Longitudinal MTME model
#
# Export to Mplus long data
#
# Last modified: 07/11/2018
#
#######################################
#
# TO DO
#
# 1. Import data
# 2. Merge data
# 3. Export to Mplus
#
#######################################

#######################################
# 0. House keeping
#######################################

# clear working space
rm(list = ls())


# load packages
pkg <- c("haven", "tidyverse", "lavaan",
         "stringr", "MplusAutomation")
sapply(pkg, library, character.only = T)


# load custom fuctions
map(str_c("./functions/",
          list.files("./functions/")),
    source)




# Load data ---------------------------------------------------------------

# load cleaned IP data from original paper

load("./Data/ip7_clean.RData")
load("Data/ip7_extra.RData")
load("Data/ip8_clean.RData")
load("Data/ip9_clean.RData")

# rename variables
mtmmip7 <- mtmm_ip7 %>%
  rename_at(vars(matches("G")),
            funs(str_c(., "_7")))

mtmmip8 <- mtmm_ip8 %>%
  rename_at(vars(matches("G")),
            funs(str_c(., "_8"))) %>%
  dplyr::select(-newid)

mtmmip9 <- mtmm_ip9 %>%
  rename_at(vars(matches("G")),
            funs(str_c(., "_9"))) %>%
  dplyr::select(-newid)


# bring all the variables together
mtme_long <- mtmmip7 %>%
  full_join(mtmmip8, by = "pidp") %>%
  full_join(mtmmip9, by = "pidp") %>%
  left_join(ip7_extra, by = "pidp") %>%
  mutate(newid = 1:nrow(.))


# recode mode variables
mtme_long <- mtme_long %>%
  mutate(modew5 = as.numeric(modew5) - 1,
         modew8 = as.numeric(modew8) - 1,
         modew9 = as.numeric(modew9) - 1)




# Get new variables for R&R -----------------------------------------------



# see codebook

ip9 %>%
  dplyr::select(-ends_with("code")) %>%
  summarise_all(funs(attributes(.)$label)) %>%
  t() %>% as.data.frame() %>%
  mutate(var = row.names(.)) %>%
  tbl_df() %>%
  View()



# get IP wave 7-9 data

data_path <- list.files("./data/stata/",
                        pattern = "g_|h_|i_",
                        full.names = T)
raw_data <- map(data_path, read_dta)


# select only new variables
vars_want <- c("pidp", "deviceused", "netpuse", "health")

# clean variables
i <- 6
raw_data2 <- map(raw_data, function(x){
  out <- x %>%
    dplyr::select(matches(str_c(vars_want, collapse = "|"))) %>%
    rename_all(funs(str_remove(., "[g-i]_"))) %>%
    mutate(mobile = ifelse(deviceused > 1, 1, 0),
           net_day = ifelse(netpuse == 1, 1, 0),
           long_ill = ifelse(health == 1, 1, 0)) %>%
    dplyr::select(pidp, mobile, net_day, long_ill) %>%
    rename_at(vars(mobile, net_day, long_ill),
              funs(str_c(., "_", i + 1)))

  i <<- i + 1

  out

})

desc_tab(raw_data2[[1]]$mobile_7)
desc_tab(raw_data2[[2]]$mobile_8)
desc_tab(raw_data2[[3]]$mobile_9)


# put all data together
mtme_long_2 <- raw_data2 %>%
  reduce(full_join) %>%
  left_join(mtme_long, by = "pidp")




# save data to mplus
prepareMplusData(mtme_long, "./Data/mtme_long.dat")
prepareMplusData(mtme_long_2, "./Data/mtme_long_2.dat")


# save clean data
save(mtme_long, file = "./Data/mtme_long.Rdata")


