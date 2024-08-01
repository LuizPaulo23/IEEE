#' @title Comparando a distribuição das predições 
#' @author Luiz Paulo 

base::rm(list = ls())
pacman::p_load(tidyverse)
setwd("~/Github/Projetos/IEEE/primeiro_periodo/archive")

# Importando a predição dos modelos: 

random_forest = readxl::read_excel("predictions_nmodels.xlsx")
extra_trees = utils::read.csv("predictions_extra_trees.csv")

# utils::write.csv(random_forest, "predictions.csv", row.names = FALSE)

mean(random_forest$price)
mean(extra_trees$predicted_price)

r <- data.frame(price = random_forest$price, set = 'random_forest_R')
python <- data.frame(price = extra_trees$predicted_price, set = 'extra_trees_Python')

combined_data <- dplyr::bind_rows(r, python)


# Visualizando 

ggplot2::ggplot(combined_data, aes(x = price, fill = set)) +
          geom_histogram(position = "identity",
                         alpha = 0.5, 
                         bins = 30) +
          labs(title = "Distribuição de Preços",
               x = "",
               y = "Frequência", fill = "") +
          scale_fill_manual(values = c("random_forest_R" = "blue",
                                       "extra_trees_Python" = "red"))+
          theme_minimal()+
          theme(legend.position = "bottom")
