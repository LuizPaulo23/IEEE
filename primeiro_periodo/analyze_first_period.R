#' @title CIS IEEE UNB - Renting Houce Prices Prediction
#' @description  Criar modelos preditivos em relação ao preço de aluguel dos imóveis ofertados, visando a identificação de preços competitivos com base em ofertas semelhantes.
#' @author Luiz Paulo Tavares 

base::set.seed(123)
base::rm(list = ls())
grDevices::graphics.off()
base::options(scipen = 999)

setwd("~/Github/Projetos/IEEE/primeiro_periodo/archive")

# Dependências =================================================================

pacman::p_load(tidyverse, caret, 
               data.table, Metrics,DataExplorer, rsample, glmnet, stats)

# FUNÇÕES ======================================================================

# get_import: importa dataset de treino 

get_import <- function(db){
  
  data_raw = data.table::fread(paste0(db, ".csv")) %>% 
             janitor::clean_names()
  
  return(data_raw)
}


# get_remove_outliers: remove outliers usando a regra do intervalo interquartil

get_remove_outliers <- function(data) {
  
  numeric_cols <- data %>% 
                  dplyr::select(where(is.numeric)) %>% base::colnames()
  
  for (column in numeric_cols) {
    
    Q1 <- stats::quantile(data[[column]], 0.25, na.rm = TRUE)
    Q3 <- stats::quantile(data[[column]], 0.75, na.rm = TRUE)
    
    IQR <- Q3 - Q1
    
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    data <- data %>% dplyr::filter(data[[column]] >= lower_bound & data[[column]] <= upper_bound)
  }
  return(data)
}

# get_clean: limpa e organiza a base de treino via seleção manual 

get_clean <- function(){
  
  data_for_clean <- get_import(db = "train")
    
  # Seleção manual  
  
      data_clean = data_for_clean %>% 
                   dplyr::mutate(month = str_sub(last_scraped, start = 6, end = 7)) %>% 
                          select(month, 
                                 host_is_superhost, 
                                 latitude:number_of_reviews, 
                                 property_type, 
                                 price) %>% 
                          # Filtro para remover uma possível digitação errada 
                          filter(host_is_superhost != 3) %>%
                          # como numéricos e factor 
                          mutate(month = as.factor(month),
                                 host_is_superhost = as.factor(host_is_superhost),
                                 accommodates = as.numeric(accommodates), 
                                 minimum_nights = as.numeric(minimum_nights),
                                 property_type = as.factor(property_type),
                                 extra_people = as.numeric(gsub("[\\$,]", "", extra_people)), 
                                 price = as.numeric(gsub("[\\$,]", "", price))) %>%
                          # Valores do aluguel == 0 (no sense)
                          filter(price > 0) %>% 
                          # Retirado após testes 
                          select(-host_is_superhost, -property_type) %>%  na.omit()
  
      # Remover outliers automaticamente
  
  data_clean <- get_remove_outliers(data_clean)
  
  return(data_clean)
}


# run_model_select: Modelo LASSO para seleção de variáveis 

run_model_select <- function(db_clean, dist){
  
  # Validação Cruzada
  # db_clean = train
  
  n_matrix <- stats::model.matrix(price ~ ., data = db_clean)[,-1]
  
  cv_lasso <- glmnet::cv.glmnet(n_matrix, 
                                db_clean$price,
                                alpha = 1, 
                                family = dist)
  
  summary(cv_lasso)
  cat("Menor Lambda é: ", cv_lasso$lambda.min)
  
  # Ajustar o modelo com o melhor lambda 
  
  lasso_model <- glmnet(n_matrix,
                        db_clean$price, 
                        alpha = 1, 
                        family = dist,
                        lambda = cv_lasso$lambda.min)
  
  
  return(lasso_model)
  
}

# Chamando as FUNÇÕES ==========================================================

data_clean = get_clean()

data_split = rsample::initial_split(data = data_clean, prop = 0.75)
train = rsample::training(data_split)
test  = rsample::testing(data_split)

model_lasso = run_model_select(db_clean = train, dist = "gaussian")

# Import Dataset & EDA =========================================================

numeric_vars = train %>% dplyr::select(where(is.numeric)) 

# DataExplorer::create_report(train)

DataExplorer::plot_density(numeric_vars)
DataExplorer::plot_correlation(numeric_vars)

# Modelagem ====================================================================


# Resultados do LASSO: 

lasso_coefficients <- coef(model_lasso)

vip::vip(model_lasso, 
         lambda = model_lasso[5],
         num_features = 25) +
     geom_bar(stat = "identity", fill = "darkorange")+
     ggtitle("Variáveis mais relevantes")


# MODELAGEM \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Baseline: Modelo de regressão múltiplo =======================================

model_lm = stats::lm(formula = price ~., data = train)

# Random Forests ===============================================================

model_rf <- ranger::ranger(formula = price ~ ., 
                           data = train, 
                           num.trees = 200, 
                           mtry = sqrt(ncol(train) - 1), 
                           importance = 'impurity', 
                           min.node.size = 5, 
                           sample.fraction = 0.8, 
                           regularization.factor = 1, 
                           regularization.usedepth = F, 
                           num.threads = parallel::detectCores(), 
                           save.memory = T, 
                           oob.error = T, 
                           verbose = T)

# RF com validação cruzada: 

control <- caret::trainControl(method = "cv", number = 10)

# Predições: 

results = test %>%
          dplyr::mutate(lm = stats::predict(model_lm, newdata = test), 
                        random_forecast = stats::predict(model_rf, data = test)$predictions) %>% 
          dplyr::select(price, lm, random_forecast)

# métricas 

mae_value = mae(results$price, results$predicted_price) %>% print()
rmse_value = rmse(results$price, results$predicted_price) %>% print()
mape_value = mape(results$price, results$lm) %>% print()
mape_value = mape(results$price, results$random_forecast) %>% print()

#####################
# VALIDAÇÃO: 
# Visualizar as previsões
head(predictions_rf)


# performance::check_model(model_lm)
gtsummary::tbl_regression(model_lm)

vip::vip(model_lm)
performance::performance(model = model_lm)
equatiomatic::extract_eq(model = model_lm)
#####################


# library(ranger)
# library(dplyr)
# 
# # Número de folds para validação cruzada
# k <- 5
# 
# # Dividir os dados em k folds
# set.seed(123)
# folds <- createFolds(train$price, k = k, list = TRUE)
# 
# # Armazenar erros para cada fold
# errors <- data.frame(fold = 1:k, mse = numeric(k), rmse = numeric(k))
# 
# for (i in 1:k) {
#   
#   # Dados de treinamento e teste para o fold i
#   
#   train_fold <- train[-folds[[i]], ]
#   test_fold <- train[folds[[i]], ]
#   
#   # Ajustar o modelo Ranger
#   
#   model_rf <- ranger::ranger(formula = price ~ ., 
#                              data = train, 
#                              num.trees = 100, 
#                              mtry = sqrt(ncol(train) - 1), 
#                              importance = 'impurity', 
#                              min.node.size = 5, 
#                              sample.fraction = 0.8, 
#                              regularization.factor = 1, 
#                              regularization.usedepth = F, 
#                              num.threads = parallel::detectCores(), 
#                              save.memory = T, 
#                              oob.error = T, 
#                              verbose = T)
#   
#   # Predições no fold de teste
#   predictions_rf <- predict(model_rf, data = test_fold)$predictions
#   
#   # Calcular MSE e RMSE
#   mse <- mean((test_fold$price - predictions_rf)^2)
#   rmse <- sqrt(mse)
#   
#   errors[i, ] <- c(i, mse, rmse)
# }
# 
# # Resumir erros médios
# mean(errors$mse)
# mean(errors$rmse)
