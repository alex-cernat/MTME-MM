
# OVERVIEW ----------------------------------------------------------------

# Do MTME in the Innovation Pannel waves 7-9 by mode design and mode
#
# Aims:
#
# 1. Import Mplus output - unconditional
#
# 2. Do graphs by mode design
#
# 3. Import posterior
#
# 4. Make table



# 00 Admin ----------------------------------------------------------------


# clear working space
rm(list = ls())

# load packages
pkg <- c("tidyverse", "MCMCpack", "MplusAutomation",
         "ggthemes", "gridExtra", "lavaan", "rhdf5")
sapply(pkg, library, character.only = T)


# load custom fuctions
map(str_c("./functions/",
          list.files("./functions/")),
    source)



# read data
load("./Data/mtme_long.Rdata")





# 1. Import Mplus output - unconditional ----------------------------------


path_mplus <- "./Mplus models/regression/"
files_mplus <- str_remove(list.files(path_mplus, pattern = "m0.+out"),
                          "\\.out")

# get parameters from all the models
out_par <- map(files_mplus, function(x) {
  mplus_bayes_out_read(x, path_mplus)
  })

# bring al parameters together
ip789_out <- map(out_par, function(x) x$res_std) %>%
  reduce(rbind) %>%
  tbl_df()




# 02. Cross-sectional graphs ----------------------------------------------


# Variance decomponsition -------------------------------------------------



traits <- "
Allow same race
Allow different race
Allow poorer countries
Good for economy
Culture enriched
Better place to live
"
traits <- stringr::str_trim(scan(text = traits, sep = "\n",
                                 what = "character"))

ip789_out$question <- traits[ip789_out$question]

save(ip789_out, file = "./Data/ip789_out.RData")


# graph overall
# graph by wave



###
res_std_long <- reshape2::melt(ip789_out[, c('median', "version",
                                             "question", "component",
                                             "wave")])

random_error <- res_std_long %>%
  filter(component != "Random") %>%
  group_by(question, version, wave) %>%
  dplyr::summarize(R2 = sum(value^2),
                   E2 = 1 - R2,
                   value = sqrt(E2),
                   component = "Random") %>%
  dplyr::select(question, version, component, value, wave)

res_std_long <- res_std_long %>%
  filter(component != "Random") %>%
  bind_rows(random_error)

res_std_long$question <- factor(res_std_long$question, levels = traits)
res_std_long$component <- dplyr::recode(res_std_long$component,
                                        "A" = "Acquiescence",
                                        "P11" = "Method (11pt)",
                                        "Random" = "Random error",
                                        "S" = "Social Desirability",
                                        "T" = "Trait") %>% as.factor %>%
  fct_relevel("Trait", "Acquiescence", "Social Desirability", "Method (11pt)")

names(res_std_long) <- c("Form", "Question", "Component",
                         "wave", "Variable", "std_coef")
res_std_long$Variance <- res_std_long$std_coef^2



# graph by wave and form
res_std_long %>%
  group_by(wave, Component) %>%
  summarise(Variance = mean(Variance)) %>%
  group_by(wave) %>%
  mutate(Variance = Variance/sum(Variance)) %>%
  ggplot(aes(wave, Variance, fill = Component )) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Accent") + theme_minimal(base_family = "Sans") +
  xlab("Wave") + ylab("Proportion of variance explained") +
  theme(plot.margin = unit(c(0.5, 2, 0.5, 0.5),"cm")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1"))

ggsave("./graphs/long_mtme.png", dpi = 500)


# get posterior


files_mplus2 <- str_remove(
  list.files(path_mplus, pattern = "m0.+_save.dat"),
                          "\\.dat")

out_save <- map(files_mplus2, function(x) {
  mplus_bayes_savedata_read(x, path_mplus)
})


mtme_extra <- mtme_long %>%
  dplyr::select(-matches("G.T."))


# add wave prefix to imputations
i <- 6
out_save2 <- map(out_save, function(x) {
  i <<- i + 1
  x %>%
    tbl_df() %>%
    rename_at(vars(matches("_p")),
              funs(str_replace_all(., "_p",
                                   str_c("_", i, "_p"))))
})


ip789_save <- reduce(out_save2, full_join, by = "NEWID") %>%
  tbl_df() %>%
  rename_all(funs(str_to_lower(.))) %>%
  full_join(mtme_extra, by = "newid")



# get impputed data
ip789_imp <- ip789_save %>%
  dplyr::select(newid, pidp, matches("_p"))

# median vars
ip789_sd <- ip789_save %>%
  dplyr::select(newid, pidp, matches("f2f"), matches("mode"),
                matches("standarddeviation")) %>%
  rename_at(vars(matches("standarddeviation")),
            funs(str_remove_all(., "\\.|standarddeviation")))

ip789_mean <- ip789_save %>%
  dplyr::select(newid, pidp, matches("f2f"), matches("mode"),
                matches("mean")) %>%
  rename_at(vars(matches("mean")),
            funs(str_remove_all(., "\\.|mean")))

mean(ip789_save$g1t1_7, na.rm = T)

out_table <- ip789_save %>%
  dplyr::select(matches("f2f"), matches("mode"),
         matches("s_"), matches("p11_"), matches("a_")) %>%
  dplyr::select(matches("f2f"), matches("mode"),
                matches("mean"), matches("standarddeviation")) %>%
  rename_all(funs(str_replace(., "standarddeviation", "sd"))) %>%
  mutate_at(vars(matches("mode")),
            funs(factor(., labels = c("SM", "MM")))) %>%
  mutate_at(vars(matches("f2f")),
            funs(factor(., labels = c("Web", "F2F"))))



out_table2 <- list(NULL)
out_table2[[1]] <- out_table %>%
  dplyr::select(f2f7, modew5, matches("_7")) %>%
  rename(f2f = f2f7, mode = modew5)
out_table2[[2]] <- out_table %>%
  dplyr::select(f2f8, modew8, matches("_8")) %>%
  rename(f2f = f2f8, mode = modew8)
out_table2[[3]] <- out_table %>%
  dplyr::select(f2f9, modew9, matches("_9")) %>%
  rename(f2f = f2f9, mode = modew9)



res1 <- map(out_table2, function(x)
  x %>%
  group_by(mode) %>%
  summarise_at(vars(matches("_")),
            funs(mean(., na.rm = T))))


res2 <- map(out_table2, function(x)
  x %>%
    group_by(f2f) %>%
    summarise_at(vars(matches("_")),
                 funs(mean(., na.rm = T))))

res <- reduce(res2, left_join) %>%
  tbl_df() %>%
  rename(mode = f2f) %>%
  rbind(reduce(res1, left_join)) %>%
  filter(!is.na(mode))

write.csv(res, file = "./graphs/desc_mtme.csv")

