## ----setup, include=FALSE----------------------------------------------------------
knitr::opts_chunk$set(echo = F)
knitr::opts_chunk$set(eval = T)
knitr::opts_chunk$set(warning = F)
knitr::opts_chunk$set(error = F)
knitr::opts_chunk$set(message = F)
knitr::opts_chunk$set(tidy = T)
knitr::opts_chunk$set(comment = ">")
knitr::opts_chunk$set(fig.align = "center")
library(tidyverse)


## ----------------------------------------------------------------------------------
# setup ------------------------------------------------------------------------

rm(list = ls())

proj_dir <- "~/R_Jobs/StatIterazione/progetto_iterazione"
if (normalizePath(getwd()) != normalizePath(proj_dir)) setwd(proj_dir)

library(tidyverse)
library(ggfortify)
library(equatiomatic)


# lettura e preparazione dei dati ----------------------------------------------

## crescita
setwd("pannelli/crescita")

# lettura dati
piante_filenames <- list.files(pattern = ".csv")
crescita <- lapply(piante_filenames, function(x) read.csv(x, header = T, sep = ","))
names(crescita) <- gsub(".csv", "", piante_filenames)
setwd(proj_dir)

# operazioni preliminari
for(name in names(crescita)){
  
  # formato POSIX
  crescita[[name]]$date <- as.POSIXlt(crescita[[name]]$date, format = "%Y-%m-%d")
  
  # settimana
  crescita[[name]]$week_aligned <- lubridate::week(crescita[[name]]$date)
  
  # codifica area
  if ("area" %in% names(crescita[[name]])) crescita[[name]]$area <- substr(crescita[[name]]$area, 0, 1)
  else crescita[[name]]$area <- "no_info"
  
  # covariata con nome pianta (per unire i dataframes)
  crescita[[name]] <- crescita[[name]] %>% mutate(pianta = name)
  
} 

crescita$tomatoes$number_fruits[is.na(crescita$tomatoes$number_fruits)] <- 0
crescita$ornamentals$week_aligned[crescita$ornamentals$week_aligned == 2] <- 53

#str(crescita)
#lapply(crescita, summary)
#lapply(crescita, function(df) sort(unique(df$week_aligned)))
# separare ornamentals e alfalfa?

## meteo
setwd("pannelli/meteo")

# lettura dati
meteo_filenames <- list.files(pattern = ".xlsx")
meteo <- lapply(meteo_filenames[-1], function(x) readxl::read_xlsx(x, col_names = F, skip = 3, guess_max = 1e5))
names(meteo) <- gsub(".xlsx", "", meteo_filenames[-1])

# unico file con nomi variabili
meteo$FL <- readxl::read_xlsx(meteo_filenames[1], skip = 3, guess_max = 1e5)
setwd(proj_dir)

# operazioni preliminari
names(meteo$FL)[1] <- "DataOra"
names(meteo$SH30) <- names(meteo$FL)
names(meteo$HSH60) <- names(meteo$SH20) <- names(meteo$SH60) <- names(meteo$FL)[-c(17:22)]

for(name in names(meteo)){
  
  # formato POSIX
  meteo[[name]]$DataOra <- as.POSIXlt(meteo[[name]]$DataOra, format = "%d/%m/%Y %H:%M")
  
  # covariata relativa all'esposizione alla luce (per unire i dataframes)
  meteo[[name]] <- meteo[[name]] %>% mutate(light = name)
  
  # riordina colonne in modo che siano coincidenti
  if(name %in% c("FL", "SH30")) meteo[[name]] <- meteo[[name]] %>% relocate(starts_with("Bfogl"), .after = light)
  
}
#str(meteo)
#sapply(meteo, summary)

# unione dei dataset (per ora covariate comuni)
meteo1 <- lapply(meteo, function(x) x[, names(meteo$SH60)]) %>% do.call(rbind, .)
#sapply(meteo1, function(x) mean(is.na(x)))
meteo1 <- na.omit(meteo1) # guardare meglio

# compressione su settimana (stessa scala di crescita)
# per ogni settimana e tipo di esposizione alla luce, media max e min
meteo_grp <- meteo1 %>%
  mutate(week_aligned = lubridate::week(meteo1$DataOra)) %>%
  relocate(week_aligned, .after = DataOra) %>%
  arrange(light, week_aligned) %>% 
  group_by(week_aligned, light)

avg <- meteo_grp %>% summarise_at(vars(ends_with("Media")), list(mean))
max <- meteo_grp %>% summarise_at(vars(ends_with("Max")), list(max)) 
min <- meteo_grp %>% summarise_at(vars(ends_with("Min")), list(min))

# dataset compresso
meteo_compresso <- Reduce(function(x,y) merge(x, y, by = c("week_aligned", "light")), list(avg, max, min)) %>% as_tibble()
#meteo_compresso <- avg
#names(meteo_compresso)

#str(meteo_compresso)
#summary(meteo_compresso)
#sum(is.na(meteo_compresso))
meteo_compresso$week_aligned[meteo_compresso$week_aligned == 1] <- 53
meteo_compresso$week_aligned[meteo_compresso$week_aligned == 2] <- 54




## ----------------------------------------------------------------------------------
# funzioni grafiche ------------------------------------------------------------

## funzione per visualizzare l'andamento di una variabile di interesse in una 
# qualsisasi pianta, stratificato per light
plot_crescita <- function(pianta = "tomatoes", y =  "height", time = "week_aligned", strata = "light"){
  
  plot <- crescita[[pianta]] %>% 
    select(.data[[strata]], .data[[time]], .data[[y]]) %>% 
    group_by(.data[[strata]], .data[[time]]) %>% 
    ggplot(aes_string(x = time, y = y, color = strata, shape = strata)) +
    geom_point() +
    geom_smooth(method = 'loess', formula = 'y ~ x', alpha = 0.2) +
    labs(x = "week", y = y, title = paste0("Pianta: ", pianta, "\nVariabile: ", y))
  
  plot
}  

# prove
#plot_crescita()
#plot_crescita("potatoes", "diameter1")

#str(crescita$tomatoes)
covs1 <- c("height", "diameter1", "diameter2", "number_leaves")
covs2 <- c("number_leaves", "number_flowers", "number_fruits")

p <- lapply(covs2, function(x) plot_crescita("tomatoes", x))
#ggpubr::ggarrange(plotlist = p, legend = "bottom", common.legend = T)


## ---- fig.width=8, fig.height=6----------------------------------------------------
p <- lapply(covs1, function(x) plot_crescita("tomatoes", x))
ggpubr::ggarrange(plotlist = p, legend = "bottom", common.legend = T)


## ----------------------------------------------------------------------------------
## funzione per visualizzare l'andamento di una misura metereologica di 
# interesse stratificato per light
plot_meteo <- function(misura = "TempSuoloMedia"){
  
  plot <- meteo_compresso %>% 
    select(week_aligned, light, starts_with(misura)) %>% 
    ggplot(aes(x = week_aligned, y = .data[[misura]], color = light)) +
    geom_point() +
    geom_line() + 
    labs(x = "week", y = "", title = misura)
  plot
}

# prove
#plot_meteo("TempSuoloMedia")


## ---- fig.width=8, fig.height=6----------------------------------------------------
covs <- c("TempSuoloMedia", "UmSuoloMedia", "RadSolMedia", "PrugMedia")
p <- lapply(covs, function(x) plot_meteo(x))
ggpubr::ggarrange(plotlist = p, legend = "bottom", common.legend = T)


## ---- fig.dim=c(10, 10)------------------------------------------------------------
## funzione per visualizzare la matrice di correlazione delle variabili
# di crescita per le diverse piante
plot_corr <- function(pianta = "tomatoes"){
  
  d <- crescita[[pianta]] %>% 
    select(!matches("date|week|light|area|plant|pianta"))
  c <- round(cor(d), 3)
  require(ggcorrplot)
  plot <- ggcorrplot(c, hc.order = T, type = "upper", outline.col = "white",
                     lab = T, ggtheme = ggplot2::theme_void()) +
    labs(title = paste0("Pianta: ", pianta)) +
    theme(legend.position = "none")
  plot
}  

#plot_corr()
corrplots <- lapply(setdiff(names(crescita), c("salad", "alfalfa")), plot_corr)
ggpubr::ggarrange(plotlist = corrplots, legend = "none", font.label = list(size=5))


## ---- echo = F---------------------------------------------------------------------
# strategia 1 ------------------------------------------------------------------

# pca scores e merge crescita
# aggiunge gli scores a un dataset
add_pca_scores <- function(df, k = 1){
  
  #pca <- princomp((df %>% select(-date, -week, -week_aligned, -light, -area, -plant, -pianta)), cor = T, scores = T)
  pca <- princomp((df %>% select(!matches("date|week|light|area|plant|pianta"))), cor = T, scores = T)
  v <- pca$sdev^2
  #k <- which(cumsum(v/sum(v)) > 0.8)[1]
  #print(cumsum(v/sum(v))[1:k])
  
  s <- pca$scores
  colnames(s) <- paste0("crescita_pca", 1:ncol(s))
  crescita_pca <- s[,1:k]
  
  out <- cbind(df, crescita_pca) %>% as_tibble()
  out <-  out %>% select(week_aligned, light, area, plant, pianta, crescita_pca)

  out
  
}

# ottengo dataframe unico con score della prima cp
crescita_ridotto <- crescita[setdiff(names(crescita), c("alfalfa", "ornamentals"))]
crescita_pca <- lapply(crescita_ridotto, add_pca_scores)
#names(crescita_pca)
#lapply(crescita_pca, names)
crescita_pca <- do.call(rbind, crescita_pca)
#str(crescita_pca)

# pca scores meteo con lag
#names(meteo_compresso)
pca2 <- princomp(meteo_compresso[,-c(1,2)], cor = T)
#summary(pca2)
# plot(pca2)
k <- 5
meteo_pca <- cbind(meteo_compresso[,c(1,2)], pca2$scores[,1:k]) %>% as_tibble()

nm1 <- paste0("meteo_pcascores_", 1:k)
nm2 <- paste0("meteo_pcascores_", 1:k, "_lag")
colnames(meteo_pca)[-c(1:2)] <- nm1

library(data.table)
meteo_pca <- data.table(meteo_pca)
meteo_pca[, (nm2) :=  shift(.SD), by=light, .SDcols = nm1]
meteo_pca <- as_tibble(na.omit(meteo_pca))

# unione dataframe
d <- merge(crescita_pca, meteo_pca, by = c("week_aligned", "light"), all.x =  T) %>%
  as_tibble() %>% 
  mutate_if(is.character, factor)
#d


## ---- fig.width=10-----------------------------------------------------------------
# andamento della variabile risposta
p1 <- d %>% 
  select(week_aligned, pianta, crescita_pca) %>% 
  group_by(week_aligned, pianta) %>% 
  summarise(response = mean(crescita_pca)) %>% 
  ggplot(aes(x = week_aligned, y = response, color = pianta)) +
  geom_line() +
  geom_point() +
  labs(x = "Settimana", y = "", title = "Andamento della variabile risposta")

p2 <- d %>% 
  select(week_aligned, light, crescita_pca) %>% 
  group_by(week_aligned, light) %>% 
  summarise(response = mean(crescita_pca)) %>% 
  ggplot(aes(x = week_aligned, y = response, color = light)) +
  geom_line() +
  geom_point() +
  labs(x = "Settimana", y = "", title = "Andamento della variabile risposta")

ggpubr::ggarrange(p1, p2)


## ---- echo = F---------------------------------------------------------------------
# modello 1 
form1 <- as.formula(paste0("crescita_pca ~ poly(week_aligned, 2):pianta + ",
                          paste0(names(d)[-c(1, 3, 4, 5, 6)], collapse = " + ")), )
#form1
m1 <- lm(form1, data = d)


## ----results='asis'----------------------------------------------------------------
#stargazer::stargazer(m1, header = F, style = "aer", title = "", type = "latex")
print(xtable::xtable(anova(m1), type = "html"), comment = F)


## ---- echo = F---------------------------------------------------------------------
#summary(m1)
#anova(m1)
autoplot(m1, label.size = 2, alpha = 0.5)


## ---- echo = F---------------------------------------------------------------------
# modello 2 
form2 <- as.formula(paste0("crescita_pca ~ poly(week_aligned, 2):pianta + light:pianta + ",
                           paste0(names(d)[-c(1, 3, 4, 5, 6)], collapse = " + ")))
#form2
m2 <- lm(form2, data = d)


## ----results='asis'----------------------------------------------------------------
#stargazer::stargazer(m2, header = F, style = "aer", title = "", type = "latex")
print(xtable::xtable(anova(m2), type = "html"), comment = F)


## ---- echo = F---------------------------------------------------------------------
#summary(m2)
#anova(m2)
autoplot(m2, label.size = 2, alpha = 0.5)


## ---- echo = F---------------------------------------------------------------------
# strategia 2 ------------------------------------------------------------------

# seleziona variabili per modellazione
prepare_df <- function(df, var1 = "diameter1", var2 = "diameter2", var3 = "height"){
  out <-  df %>%
    mutate(volume = .data[[var1]] * .data[[var2]] * .data[[var3]]) %>%
    mutate(volume = (volume-min(volume))/(max(volume)-min(volume))) %>% 
    select(week_aligned, light, area, plant, pianta, volume)
  out 
  
}

# ottengo dataframe unico con volume
crescita_ridotto1 <- crescita[setdiff(names(crescita), c("alfalfa", "ornamentals","salad"))]
crescita_vol <- lapply(crescita_ridotto1, prepare_df)
crescita_vol <- do.call(rbind, crescita_vol)
#str(crescita_vol)

# unione dataframe
d2 <- merge(crescita_vol, meteo_pca, by = c("week_aligned", "light"), all.x =  T) %>%
  as_tibble() %>% 
  mutate_if(is.character, factor)
#d2


## ---- echo = F---------------------------------------------------------------------
# strategia 3 ------------------------------------------------------------------

# compute and normalize response variables
prepare_df2 <- function(i){
  
  df <- crescita[[i]]
  df_name <- names(crescita)[i]
  
  normalise <- function(x) (x-min(x))/(max(x)-min(x))
  covs <- c("week_aligned", "light", "plant")
  
  if(df_name == "alfalfa"){
    
    df$height <- normalise(df$height)
    response <- "height"
    #if(length(unique(df$area)) < 2) covs <- setdiff(covs, "area")
    meteo_pca1 <- meteo_pca # alfalfa ha solo modalita FL e SH
    meteo_pca1$light <- ifelse(meteo_pca1$light == "FL", "FL", "SH")
    meteo_pca1 <- meteo_pca1 %>% 
      group_by(light, week_aligned) %>% 
      summarise_at(vars(starts_with("meteo")), list(mean))
    out <- merge(df[,c(covs, response)], meteo_pca1, by = c("week_aligned", "light"), all.x =  T) %>%
      as_tibble() %>% 
      mutate_if(is.character, factor)
    return(out)

  }

  else if (df_name == "salad"){

    df$surface <- df$diameter1 * df$diameter2
    df$surface <- normalise(df$surface)
    response <- "surface"
    #if(length(unique(df$area)) < 2) covs <- setdiff(covs, "area")
    out <- merge(df[,c(covs, response)], meteo_pca, by = c("week_aligned", "light"), all.x =  T) %>%
      as_tibble() %>% 
      mutate_if(is.character, factor)
    return(out)

  }

  else{
    
    df$surface <- df$diameter1 * df$diameter2
    df$volume <- df$height * df$surface
    df$volume <- normalise(df$volume)
    response <- "volume"
    #if(length(unique(df$area)) < 2) covs <- setdiff(covs, "area")
    out <- merge(df[,c(covs, response)], meteo_pca, by = c("week_aligned", "light"), all.x =  T) %>%
      as_tibble() %>% 
      mutate_if(is.character, factor)
    return(out)

  }
  
}

crescita2 <- lapply(seq_along(crescita), prepare_df2)
names(crescita2) <- names(crescita)
#str(crescita2) 

# get model for every plant
get_models <- function(i){
  
  df <- crescita2[[i]]
  df_name <- names(crescita2)[i]
  
  covs <- c("week_aligned", "light", "meteo_pcascores_1",
            "meteo_pcascores_2", "meteo_pcascores_3", "meteo_pcascores_4", 
            "meteo_pcascores_5", "meteo_pcascores_1_lag", "meteo_pcascores_2_lag",
            "meteo_pcascores_3_lag", "meteo_pcascores_4_lag", "meteo_pcascores_5_lag")
  #if("area" %in% names(df)) covs <- c(covs, "area")
  response <- "volume"
  response <- ifelse(df_name == "salad", "surface", response)
  response <- ifelse(df_name == "alfalfa", "height", response)
  
  form <- as.formula(paste0(response, " ~ poly(week_aligned, 2) + ", # nb l'andamento sembra ottimo anche con effetto lineare semplice
                            paste0(covs[-1], collapse = " + ")))
  model <- lm(form, data = df)
  model
  
}

models <- lapply(seq_along(crescita2), get_models)
names(models) <- names(crescita2)



extract_eq(models$tomatoes,
  wrap = TRUE,terms_per_line=2,
  operator_location = "start",
  intercept = "\\beta_0",
  greek = "\\beta",
  raw_tex = TRUE,
  use_coefs=FALSE,
  show_distribution =TRUE
)


## ----echo=FALSE--------------------------------------------------------------------
# print results for every model
print_results <- function(i){
  
  model <- models[[i]]
  df_name <- names(models)[i]
  
  cat("Dataset: ", df_name, "\n")
  cat("Variabile risposta: ", as.character(formula(model))[[2]], "\n")
  
  r <- summary(model)$adj.r.squared
  cat("R2 aggiustato: " , round(r, 4), "\n")
  
  a <- as.matrix(anova(model))
  cat("Variabile light: \n\tsignifcatività globale: \n")
  print(signif(a[grepl("light", rownames(a)), ], 4))
  
  s <- summary(model)$coefficients
  cat("\n\tStima dei coefficienti: \n")
  print(s[grepl("light", rownames(s)), ])
  cat("\n\n")
  
  return('')
  
}

#lapply(models, summary)
#lapply(models, anova)
sapply(seq_along(models), print_results)






## ----------------------------------------------------------------------------------
plot_crescita("ornamentals")


## ----------------------------------------------------------------------------------
# covariate
pianta <- "tomatoes"
# l'agronomo potrebbe fornirci:
# - a) le informazioni meteo in formato orario per la condizione di irraggiamento
#      in cui la pianta sta crescendo, per la settimana di interesse e quella precedente
# - b) le informazioni meteo per la settimana di interesse e quella precedente,
#      per tutte le condizioni di irraggiamento
meteo_new <- meteo_compresso %>%
  filter(week_aligned == 23 | week_aligned == 24) %>%
  select(-light) %>% 
  group_by(week_aligned) %>% 
  summarise(across(everything(), list(mean)))
set.seed(42)
meteo_new <- cbind(meteo_new[,1], apply(meteo_new[,-1], 2 , jitter))
names(meteo_new)[-1] <- names(meteo_compresso)[-c(1,2)]
meteo_new
# prendiamo questo come punto di partenza per le informazioni meteo fornite,
# magari scrivere una funzione che passa da info in formato originale a questo formato
get_meteo_new <- function(){
  ...
}


## ---- echo = F---------------------------------------------------------------------
# funzione per la scelta della condzione migliore di irraggiamento
choose_light <- function(pianta = "tomatoes", week = 24, meteo_new = meteo_new){
  
  # ottenendo $\widehat{volume}^{FL}$, $\widehat{volume}^{HSH60}$,
  #$\widehat{volume}^{SH30}$ e $\widehat{volume}^{SH60}$.
  
  #\begin{align*}
  #\widehat{volume}_{relativo} = \dfrac{\widehat{volume} - \min(\widehat{volume})}
  #{\max(\widehat{volume}) - \min(\widehat{volume})}
  #\end{align*}
  
  # covariates
  scores <- predict(pca2, meteo_new[,-1])[,1:k]
  scores_names <- names(crescita2[['tomatoes']])[-c(1:4)]
  mt <- c(scores[2,], scores[1,])
  nx <- c(week, "FL", mt)
  names(nx) <- c("week_aligned", "light", scores_names)
  nx <- as.data.frame(t(nx))
  nx[,-2] <- as.numeric(nx[,-2])
  nx <- nx[rep(1,4),]
  nx$light <- c("FL", "HSH60", "SH30", "SH60")
  nx$light <- as.factor(nx$light)
  rownames(nx) <- nx$light
  
  # predictions
  p <- predict(models[[pianta]], newdata = nx)
  out <- rbind(p, (p-min(p))/(max(p)-min(p)))
  rownames(out) <- c("Scala originale", "Scala relativa")
  
  # output
  cat("\nPrevisioni:")
  print(knitr::kable(out, digits = 2))
  cat("\nPer massimizzare la crescita della pianta, nella settimana di riferimento e tenendo conto\n delle condizioni metereologiche, si consiglia la condizione di irraggiamento", names(p)[which.max(p)])
  invisible(out)

}



## ---- echo = F---------------------------------------------------------------------
p <- choose_light(pianta = "tomatoes", week = 24, meteo_new = meteo_new)

choose_light(pianta = "melon", week = 24, meteo_new = meteo_new)


## ---- echo = F---------------------------------------------------------------------
# get model for every plant
get_models2 <- function(i){
  
  df <- crescita2[[i]]
  df_name <- names(crescita2)[i]
  
  covs <- c("week_aligned", "light")
  #if("area" %in% names(df)) covs <- c(covs, "area")
  response <- "volume"
  response <- ifelse(df_name == "salad", "surface", response)
  response <- ifelse(df_name == "alfalfa", "height", response)
  
  form <- as.formula(paste0(response, " ~ poly(week_aligned, 2):light + ",
                            paste0(covs[-1], collapse = " + ")))
  model <- lm(form, data = df)
  model
  
}

models2 <- lapply(seq_along(crescita2), get_models2)
names(models2) <- names(crescita2)

#lapply(models2, summary)
#lapply(models2, anova)
#sapply(seq_along(models2), print_results)

# funzione che visualizza le previsioni
explore_light <- function(pianta = "tomatoes"){
  
  # covariates
  nx <- as.data.frame(expand.grid(week_aligned = sort(unique(crescita2[[pianta]]$week_aligned)),
                    light = levels(crescita2[[pianta]]$light)))
  #if(!is.null(area)) nx$area <- as.factor(area)
  
  # predictions
  p <- predict(models2[[pianta]], newdata = nx, interval = "prediction")
  nx <- cbind(nx, p)
  pl <- ggplot(nx, aes(x = week_aligned, y = fit, shape = light, color = light, fill = light)) +
    geom_point() + geom_line() + 
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.3) +
    labs(x = "week", y = "growth prediction", title = paste0("Pianta: ", pianta))
  pl
  
}

#explore_light("tomatoes", area = 2)
#explore_light("melon", area = NULL)


## ---- echo = F---------------------------------------------------------------------
# riassunto
new_plant_info <- list(pianta = "tomatoes",
                       week = 24,
                       meteo_info = meteo_new)
new_plant_info2 <- list(pianta = "melon",
                       week = 24,
                       meteo_info = meteo_new)

helper <- function(npi){
  
  plot(explore_light(npi$pianta, new_plant_info$area))
  choose_light(npi$pianta, npi$week, npi$area, meteo_new = npi$meteo_info)
  
}

#helper(new_plant_info)
#helper(new_plant_info2)

