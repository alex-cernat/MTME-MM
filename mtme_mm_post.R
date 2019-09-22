
# 00 Admin ----------------------------------------------------------------


# clear working space
rm(list = ls())

setwd(paste0("C:/Users/",
             Sys.getenv("USERNAME"),
             "/Dropbox (The University of Manchester)/Papers/Joe S/MTME-MM/"))

if(Sys.getenv("USERNAME") == "msassac6"){
  .libPaths(c(
    paste0(
      "C:/Users/",
      Sys.getenv("USERNAME"),
      "/Dropbox (The University of Manchester)/R/package"
    ),
    .libPaths()
  ))}



# load packages
pkg <- c("tidyverse", "MCMCpack", "MplusAutomation",
         "ggthemes", "gridExtra", "lavaan")
sapply(pkg, library, character.only = T)


# load custom fuctions
map(str_c("./functions/",
          list.files("./functions/")),
    source)



# read data
load("./Data/mtme_long.Rdata")




path_mplus <- "./Mplus models/regression/"
files_mplus <- str_remove(list.files(path_mplus, pattern = ".out"),
                          "\\.out")

files_mplus <- str_subset(files_mplus, "0")

out_par <- map(files_mplus, function(x) {
  mplus_bayes_out_read(x, path_mplus)
})


data_name <- str_c("Wave ", 7:9)

names(out_par) <- data_name


i <- 0
ip_mm_out <- map(out_par, function(x) {
  i <<- i + 1
  x$res_std %>%
    mutate(wave = data_name[i]) %>%
    rename(model = wave)
  }) %>%
  reduce(rbind) %>%
  tbl_df()

dim(ip_mm_out)


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

ip_mm_out$question <- traits[ip_mm_out$question]

save(ip_mm_out, file = "./Data/ip_mm_out.RData")


# graph overall
# graph by wave



###
res_std_long <- reshape2::melt(ip_mm_out[, c('median', "version",
                                             "question", "component",
                                             "model")])

random_error <- res_std_long %>%
  filter(component != "Random") %>%
  group_by(question, version, model) %>%
  dplyr::summarize(R2 = sum(value^2),
                   E2 = 1 - R2,
                   value = sqrt(E2),
                   component = "Random") %>%
  dplyr::select(question, version, component, value, model)

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
                         "model", "Variable", "std_coef")
res_std_long$Variance <- res_std_long$std_coef^2



# graph for all 3 waves

res_std_long %>%
  group_by(model, Component) %>%
  summarise(Variance = mean(Variance)) %>%
  group_by(model) %>%
  summarise(sum(Variance))



# mtme_decomp_overall <-
  res_std_long %>%
  group_by(model, Component) %>%
  summarise(Variance = mean(Variance)) %>%
    group_by(model) %>%
    mutate(Variance = Variance/sum(Variance)) %>%
  ggplot(aes(model, Variance, fill = Component )) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Accent") + theme_minimal(base_family = "Sans") +
  xlab("") + ylab("Proportion of variance explained") +
  theme(plot.margin = unit(c(0.5, 2, 0.5, 0.5),"cm")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1"))

ggsave("./graphs/long_mtme.png", dpi = 500)


