create_table_varcomps <- function(df_tab) {
  if("label" %in% names(df_tab)) df_tab <- df_tab[, names(df_tab) != "label"]
  
  df_tab %>% 
    mutate_each(funs(round(., digits = 4)), -lhs, -op, -rhs) %>%
    filter(op == "~~" & lhs == rhs &       # Select variances
             grepl("^[TASP]", lhs) &       # Select latent variables
             abs(se) > 1e-5) %>%             # Exclude fixed parameters
    datatable(., extensions = c("FixedHeader"), filter =list(position = 'top', clear = TRUE, plain = FALSE), style = "bootstrap", fillContainer = FALSE)
}

create_table_stdized <- function(df_tab) {
  if("label" %in% names(df_tab)) df_tab <- df_tab[, names(df_tab) != "label"]
  
  df_tab %>%
    mutate_each(funs(round(., digits = 4)), -lhs, -op, -rhs) %>%
    filter(op != "~1" & !(op == "~~" & grepl("^G", lhs)) & 
             abs(se) > 1e-6) %>% 
    datatable(., extensions = c("FixedHeader"), filter= list(position = 'top', clear = TRUE, plain = FALSE), 
              style = "bootstrap", fillContainer = FALSE)
  
}
  
  make_zero_to_one <- function(x) {
    x - min(x, na.rm = TRUE)
  }  

  
    
  ev_plot <- function(ev) {
    eigenvalue_negative <- ifelse(ev >= 0, "No", "Yes")
    evdf <- data.frame(x = seq_along(ev), ev = ev)
    ggplot(evdf, aes(x, ev)) +
      geom_bar(stat = "identity", aes(fill = eigenvalue_negative)) +
      scale_y_continuous("Eigenvalue") + xlab("") +
      scale_fill_manual(values = c("green", "red"))
  }
  

  
  
  
  
  scoring_func <- function(object, data.obs) {
    data.obs <- data.obs[, object@Data@ov.names[[1]]] %>% as.matrix
    
    lavmodel <- object@Model
    lavsamplestats <- object@SampleStats
    
    Sigma.hat <- lavaan:::computeSigmaHat(lavmodel = lavmodel)
    Sigma.hat.inv <- lapply(Sigma.hat, solve)
    VETA <- lavaan:::computeVETA(lavmodel = lavmodel, lavsamplestats = lavsamplestats)
    EETA <- lavaan:::computeEETA(lavmodel = lavmodel, lavsamplestats = lavsamplestats)
    EY <- lavaan:::computeEY(lavmodel = lavmodel, lavsamplestats = lavsamplestats)
    LAMBDA <- lavaan:::computeLAMBDA(lavmodel = lavmodel, remove.dummy.lv = FALSE)
    g <- 1
    FSC <- VETA[[g]] %*% t(LAMBDA[[g]]) %*% Sigma.hat.inv[[g]]
    
    RES <- sweep(data.obs, MARGIN = 2L, STATS = EY[[g]], FUN = "-")
    
    napats <- apply(RES, 2, function(x) 1*!is.na(x)) %>% apply(1, paste, collapse = "")
    napats_unique <- unique(napats)
    
    scores_pats <- list()
    
    for(ipat in seq_along(napats_unique)) {
      pat <- napats_unique[ipat]
      
      RES_sub <- RES[napats == pat, ,drop = FALSE]
      is_observed <- as.logical(as.numeric(strsplit(pat, "")[[1]]))
      
      RES_sub <- RES_sub[, is_observed, drop = FALSE]
      FSC_sub <- FSC[, is_observed, drop = FALSE]
      
      scores_pats[[ipat]] <- 
        sweep(RES_sub %*% t(FSC_sub), MARGIN = 2L, STATS = EETA[[g]], FUN = "+")
    } 
    FS <- Reduce(rbind, scores_pats)
    colnames(FS) <- object@pta$vnames$lv[[1]]
    
    FS
  }
  
  
  