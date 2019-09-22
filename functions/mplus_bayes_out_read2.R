


# function to import Mplus output

mplus_bayes_out_read <- function(model_int,
                                 model_link,
                                 savedata = TRUE){

model_out <- read_file(str_c(model_link, model_int, ".out"))

model_label <- str_split(model_out, "Order of parameters saved\r\n\r\n +")[[1]][2]
model_label <- str_split(model_label, "\r\n\r\nDIAGRAM INFORMATION\r\n\r\n")[[1]][1]
model_label <- unlist(str_split(model_label, "\r\n +"))
model_label <- model_label[!str_detect(model_label,
                          "Convergence|Factor score")]


post_name <- str_extract(model_out, "BPARAMETERS = [:print:]+;")
post_name <- str_remove_all(post_name, "BPARAMETERS = |;")

# import posterior
model_post <- read.table(str_c(model_link, post_name))

names(model_post) <- model_label
model_post$iter  <- rep(1:(nrow(model_post)/4), 4)
model_post$chain <- as.factor(model_post$`Chain number`)

#discard burn=in & thin 2
model_post <- model_post[model_post$iter > nrow(model_post)/8,]
dim(model_post)

if (savedata == TRUE) {
save(model_post, file = str_c(model_link, model_int, ".rdata"))
}

# rename variables
res <- t(apply(model_post[, grep("(Parameter|STDYX)", names(model_post))], 2,
               function(x)
                 c(mean = mean(x),
                   sd = sd(x),
                   median = median(x),
                   mad = mad(x),
                   quantile(x, c(0.025, 0.05, 0.95, 0.975)))
))

options(digits = 3)
zapsmall(res)

res <- as.data.frame(res)
nm <- rownames(res)[grep("(STDYX,.+ BY G|STDYX,  G)", rownames(res))]

res_std <- res[nm,]
res_std <- res_std %>%
  mutate(rows = row.names(res[nm,])) %>%
  mutate(variable = gsub(".+(G.+)", "\\1", rows)) %>%
  separate(variable, into = c("variable", "wave"), sep = "_") %>%
  mutate(version = gsub("G([0-9])T[0-9]", "\\1", variable),
         question = as.numeric(gsub("G([0-9])T([0-9])", "\\2", variable)),
         component = gsub("STDYX,[ ]+(.+)[ ]+BY (G.+)", "\\1", rows),
         component = ifelse(str_detect(component, "STDYX,  G"),
                            "Random", component),
         component = str_remove(component, "_."),
         component = str_replace(component, "T.+", "T"))


# median make positive?
res_std$median <- abs(res_std$median)

out <- list(all_res = res, res_std = res_std)
}
