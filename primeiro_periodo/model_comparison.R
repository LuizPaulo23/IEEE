#' @title Comparando a distribuição das predições 
#' @author Luiz Paulo 

base::rm(list = ls())
pacman::p_load(tidyverse)
setwd("~/Github/Projetos/IEEE/primeiro_periodo/archive")

# Importando a predição dos modelos: 

random_forest = utils::read.csv("random_forest.csv") %>% glimpse()
random_forest_cv = utils::read.csv("random_forest_cv.csv") %>% glimpse()
extra_trees = utils::read.csv("predictions_ETR.csv") %>% glimpse()
xgb = utils::read.csv("XGB.csv") %>% glimpse()

# utils::write.csv(predict_final, "segundo_modelo.csv", row.names = FALSE)

# linear <- data.frame(price = lm_otimizado$price, set = 'model_linear')

rf <- data.frame(price = as.double(random_forest$price), set = 'RandomForest')
rf_cv <- data.frame(price = as.double(random_forest_cv$price), set = 'RandomForest_cv')
extra_trees <- data.frame(price = as.double(extra_trees$price), set = "Extra_trees")
xgb <- data.frame(price = as.double(xgb$price), set = "XGB")

combined_data <- dplyr::bind_rows(rf, rf_cv, extra_trees)

# rm(combined_data)
# Visualizando 

ggplot2::ggplot(combined_data, aes(x = price, fill = set)) +
          geom_histogram(position = "identity",
                         alpha = 0.5, 
                         bins = 30) +
          labs(title = "Distribuição de Preços",
               x = "",
               y = "Frequência", fill = "") +
          scale_fill_manual(values = c(
                                       "RandomForest" = "red", 
                                       "RandomForest_cv" = "blue",
                                       # "XGB" = "black", 
                                       "Extra_trees" = "black"))+
          theme_minimal()+
          theme(legend.position = "bottom")
