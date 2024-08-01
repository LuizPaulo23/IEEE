#' @title CIS IEEE UNB - Renting Houce Prices Prediction
#' @description  Criar modelos preditivos em relação ao preço de aluguel dos imóveis ofertados, visando a identificação de preços competitivos com base em ofertas semelhantes.
#' @author Luiz Paulo Tavares 

base::set.seed(123)
base::rm(list = ls())
grDevices::graphics.off()
base::options(scipen = 999)

setwd("~/Github/Projetos/IEEE/primeiro_periodo/archive")

# Dependências =================================================================

pacman::p_load(tidyverse, data.table, 
               DataExplorer, naniar, 
               Metrics, vip, dlookr, caret, rsample, glmnet, stats)

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
                   dplyr::mutate(month = str_sub(last_scraped, start = 6, end = 7), 
                                 year = str_sub(last_scraped, start = 1, end = 4)) %>% 
                          select(month,
                                 year,
                                 host_is_superhost, 
                                 latitude:number_of_reviews, 
                                 property_type, 
                                 price) %>% 
                          # Filtro para remover uma possível digitação errada 
                          filter(host_is_superhost != 3) %>%
                          # como numéricos e factor 
                          mutate(month = as.factor(month),
                                 year = as.factor(year), 
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


# get_transformation: transformação dos dados, feature engine 

get_transformation <- function(transformation){

data_clean_transf = data_clean %>% 
                    dplyr::mutate_if(is.numeric, 
                                     ~dlookr::transform(., method = transformation))


return(data_clean_transf)

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

# get_metrics: retorna as mátricas de acurácia =================================

get_metrics <- function(actual, predicted, model_name) {
  
  cat("\nMetrics for", model_name, "model:\n")
  mae_value = Metrics::mae(actual, predicted)
  cat("MAE:", mae_value, "\n")
  
  rmse_value = Metrics::rmse(actual, predicted)
  cat("RMSE:", rmse_value, "\n")
  
  mse_value = Metrics::mse(actual, predicted)
  cat("MSE:", mse_value, "\n")
  
  mape_value = Metrics::mape(actual, predicted)
  cat("MAPE:", mape_value, "\n")
  
}

# Chamando as FUNÇÕES ==========================================================

data_clean_train = get_clean()

# getwd()
# writexl::write_xlsx(data_clean_train, "data_clean_train.xlsx")

# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# ISSO PODE DEMORAR!!!!
# RODAR LASSO APENAS EM CASO DE VERIFICAÇÃO NECESSÁRIA
# DESNECESSÁRIO NA PREDIÇÃO COM PIPELINE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

model_lasso = run_model_select(db_clean = data_clean_train, dist = "gaussian")

# Import Dataset & EDA =========================================================

numeric_vars = train %>% dplyr::select(where(is.numeric)) 

# DataExplorer::create_report(train)

DataExplorer::plot_density(numeric_vars)
DataExplorer::plot_correlation(numeric_vars)

# Modelagem ====================================================================

# Resultados do LASSO: 

lasso_coefficients <- coef(model_lasso) %>% print()

vip::vip(model_lasso, 
         lambda = model_lasso[5],
         num_features = 25) +
     geom_bar(stat = "identity", fill = "darkorange")+
     ggtitle("Variáveis mais relevantes")


# MODELAGEM \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# db = get_transformation(transformation)

data_split = rsample::initial_split(data = data_clean_train, prop = 0.75)
train = rsample::training(data_split)
test  = rsample::testing(data_split)

# Baseline: Modelo de regressão múltiplo =======================================

model_lm <- stats::lm(formula = price ~., data = train)

summary(model_lm)

stats::ks.test(model_lm$residuals,
               "pnorm",
               mean = mean(model_lm$residuals),
               sd = sd(model_lm$residuals))

colinearidade <- train %>% 
                 dplyr::mutate(residuals = model_lm$residuals) %>% 
                 dplyr::select(-month, - year) 

cor(colinearidade)
DataExplorer::plot_correlation(colinearidade)

# OTIMIZANDO O BASELINE ========================================================
# RETIRAR VARIÁVEIS NÃO SIGNIFICATIVAS E COM MULICOLINEARIDADE 

# vip::vip(model_lm)

# vip::vi_model(model_lasso)
# Usando o LASSO COMO PARÂMETRO DE SELEÇÃO DE VARIÁVEIS 

# data_clean_train <- data_clean_train %>% 
#                     dplyr::select(price, 
#                                   latitude,
#                                   longitude, 
#                                   bedrooms,
#                                   accommodates, 
#                                   year, 
#                                   number_of_reviews, 
#                                   minimum_nights)

# Modelo de regressão múltipica otimizado 


# model_lm <- stats::lm(formula = price ~., data = data_clean_train)

# summary(model_lm)

# stats::ks.test(model_lm$residuals,
#                "pnorm",
#                mean = mean(model_lm$residuals),
#                sd = sd(model_lm$residuals))
# 
# colinearidade <- data_clean_train %>% 
#                  dplyr::mutate(residuals = model_lm$residuals) 
# 
# cor(colinearidade)
# DataExplorer::plot_correlation(colinearidade)

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

# RF com validação cruzada =====================================================

control <- caret::trainControl(method = "cv", number = 10)

tune_grid <- expand.grid(mtry = sqrt(ncol(train) - 1),
                         splitrule = "extratrees",  # Critério de divisão para regressão
                         min.node.size = 5)

model_rf_cv <- train(price ~ ., 
                     data = train, 
                     method = "ranger",
                     trControl = control,
                     tuneGrid = tune_grid,
                     num.trees = 200)  

# Rede Neural ==================================================================

# control <- caret::trainControl(method = "cv", number = 1)
# 
# tune_grid <- base::expand.grid(size = seq(1, 10, by = 1),     # Tamanhos de 1 a 10 neurônios na camada oculta
#                               decay = c(0, 0.001, 0.01, 0.1))
# 
# model_nn_cv <- caret::train(price ~ ., 
#                             data = train, 
#                             method = "nnet", 
#                             trControl = control, 
#                             tuneGrid = tune_grid, 
#                             linout = TRUE, 
#                             maxit = 1,     # Número máximo de iterações
#                             trace = FALSE)   # Desativar saída de treino


# Predições: 

results = test %>%
          dplyr::mutate( 
                        random_forest = stats::predict(model_rf, data = test)$predictions,
                        random_forest_cv = stats::predict(model_rf_cv, newdata = test),
                        # model_nn_cv = stats::predict(model_nn_cv, newdata = test),
                        combined = (random_forest + random_forest_cv)/2) %>% 
          dplyr::select(price, random_forest, random_forest_cv, combined)

# métricas ====================================================================

# Métricas para cada modelo

# get_metrics(results$price, results$lm, "Linear Regression")
get_metrics(results$price, results$random_forest, "Random Forest")
get_metrics(results$price, results$random_forest_cv, "Random Forest with CV")
# get_metrics(results$price, results$model_nn_cv, "Neural Network with CV")
get_metrics(results$price, results$combined, "Combined")

mean(train$price)
mean(test$price)
mean(results$random_forest)
mean(results$random_forest_cv)
mean(results$combined)

# TESTE: PREDIÇÃO FORA DA AMOSTRA DE TREINO ======================================

data_test = get_import(db = "test")

DataExplorer::plot_intro(data = data_test)
naniar::gg_miss_var(data_test)

data_clean_test = data_test %>% 
                  dplyr::mutate(month = str_sub(last_scraped, start = 6, end = 7), 
                                year = str_sub(last_scraped, start = 1, end = 4)) %>% 
                  select(month,
                         year,
                         latitude:number_of_reviews) %>% 
                  mutate(month = as.factor(month),
                         year = as.factor(year), 
                         accommodates = as.numeric(accommodates), 
                         minimum_nights = as.numeric(minimum_nights),
                         extra_people = as.numeric(gsub("[\\$,]", "", extra_people)), 
                         beds = ifelse(is.na(beds), median(beds, na.rm = T), beds), 
                         bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm = T), bathrooms), 
                         bedrooms = ifelse(is.na(bedrooms), median(bedrooms, na.rm = T), bedrooms))


naniar::gg_miss_var(data_clean_test)
# writexl::write_xlsx(data_clean_test, "data_clean_test.xlsx")

predict_random_forest = stats::predict(model_rf, data = data_clean_test)$predictions

predict_random_forest <- predict_random_forest %>%  
                         base::as.data.frame() %>% 
                         dplyr::mutate(ID = data_clean_test$id) %>% 
                                       relocate(ID, .after = NULL) %>% 
                                       rename("price" = ".")

writexl::write_xlsx(predict_random_forest, "predictions.xlsx")
utils::write.csv(predict_random_forest, "predictions.csv", row.names = FALSE)

## FIM
