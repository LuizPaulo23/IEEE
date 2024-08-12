#' @title CIS IEEE UNB - CHURN
#' @author Luiz Paulo Tavares Gonçalves 

base::rm(list = ls())
grDevices::graphics.off()
base::options(scipen = 999)

base::set.seed(123)

# Ambiente de trabalho =========================================================

setwd("~/Github/Projetos/IEEE/second_period/db")

# Dependências =================================================================

pacman::p_load(tidyverse, data.table, 
               DataExplorer, naniar, 
               Metrics, vip, dlookr, caret, rsample, glmnet, stats)

# VAR GLOBAIS E CONSTANTES *\ 

THRESHOLD = 0.39

# FUNÇÕES ======================================================================

# get_import: importa dataset de treino ========================================

get_import <- function(db){
  
  data_raw = data.table::fread(paste0(db, ".csv")) %>% 
             janitor::clean_names()
  
  return(data_raw)
}

# get_split: divisão da base entre treino e teste ==============================

get_split <- function(data, pct_train){
  
  set.seed(123)
  data_split = rsample::initial_split(data = data, prop = pct_train)
  train = rsample::training(data_split)
  test  = rsample::testing(data_split)
  
  return(list(train, test))
  
}

# get_clean: limpando base de dados ============================================

get_var_select_manual <- function(db){
  
data_select = db %>%
              dplyr::select(-customer_id:-state,
                            -city,
                            -zip_code, 
                            -lat_long,
                            -paperless_billing, 
                            -payment_method, 
                            -cltv) 
  return(data_select)
  
}
 
# get_impute:imputar pela mediana ==============================================

get_impute <- function(data){
  
      data_impute = data %>%   
                    dplyr::mutate(across(where(is.numeric), ~ ifelse(is.na(.), 
                                                              median(., na.rm = TRUE), .)))
return(data_impute)
      
}
# get_encode: opção em encode: label e one_hot (one-hot encoding) ==============

get_encoding <- function(encode, data){
  
  if(encode == "label"){
    
  data_encoded = data %>% 
                 dplyr::mutate(across(where(is.character), as.factor)) 
    
  }else if(encode == "one_hot"){
    
  dummy_model = caret::dummyVars("~ .", data = data, fullRank = TRUE)
  data_encoded = stats::predict(dummy_model,
                                newdata = data) %>% as.data.frame()
  }else{
  
    stop("ERRO: OPÇÃO INVÁLIDA!")
}
  
  return(data_encoded)
  
}


# get_balance_dataset: balanceando o dataset ===================================

balance_dataset <- function(data, target_col, seed = 123) {

  # Configurar a semente para reprodutibilidade *\
  
  set.seed(seed) 
  
  yes_samples <- subset(data, get(target_col) == "Yes")
  no_samples <- subset(data, get(target_col) == "No")
  
  # Subamostrar a classe "No" para igualar o número de amostras da classe "Yes"
  
  no_samples_sub = no_samples[sample(nrow(no_samples), nrow(yes_samples)), ]
  balanced_data = base::rbind(yes_samples, no_samples_sub)
  balanced_data = balanced_data[sample(nrow(balanced_data)), ]
 
  return(balanced_data)
  
}

# get_confusionMatrix: Matriz de Confusão ===========================================================

get_confusionMatrix <- function(model, validation){
  
  predict_model = stats::predict(model, validation)
  cm = caret::confusionMatrix(predict_model, 
                              as.factor(class$churn_label))
  
  return(cm)
}

# get_f1_score: retorna o score F1

get_f1_score <- function(cm) {
  
  precision <- cm$byClass["Pos Pred Value"]
  recall <- cm$byClass["Sensitivity"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  return(f1_score)
  
}

# run_LASSO ====================================================================


get_var_select_lasso <- function(db_clean = data_clean, dist = "binomial"){

  n_matrix <- stats::model.matrix(churn_label ~ ., data = db_clean)[,-1]
  cv_lasso <- glmnet::cv.glmnet(n_matrix, db_clean$churn_label,alpha = 1, family = dist)

  summary(cv_lasso)
  cat("Menor Lambda é: ", cv_lasso$lambda.min)

  # Ajustar o modelo com o melhor lambda

  lasso_model <- glmnet::glmnet(n_matrix,db_clean$churn_label,
                                alpha = 1, family = dist,
                                lambda = cv_lasso$lambda.min)


  return(lasso_model)


}

# PIPE OF CLEAN ==========================================================

# import dataset *\

 balance = "yes"

 if(balance == "no"){

  data_train = get_import(db = "df_train")
  print(table(data_train$churn_label))

} else if(balance == "yes"){

  data_train = balance_dataset(get_import(db = "df_train"), target_col = "churn_label")
  print(table(data_train$churn_label))

} else{

  stop("ERRO: OPÇÃO INVÁLIDA")

}


# Configurar a semente para reprodutibilidade *\

# data_train = get_import(db = "df_train")
# n <- 1500
# 
# # Selecionar 1500 linhas aleatórias para cada grupo
# 
# yes_samples <- subset(data_train, churn_label == "Yes")
# no_samples <- subset(data_train, churn_label == "No")
# 
# yes_sampled <- yes_samples[sample(nrow(yes_samples), n), ]
# no_sampled <- no_samples[sample(nrow(no_samples), n), ]
# 
# # Juntar as amostras em um novo data.frame
# combined_samples <- rbind(yes_sampled, no_sampled)
# 
# # Verificar o resultado
# table(combined_samples$churn_label)
# 
# data_train = combined_samples

# Split *\

train = get_split(data = data_train, pct_train = 0.75)[[1]]
validation = get_split(data = data_train, pct_train = 0.75)[[2]] 

class = validation %>% dplyr::select(churn_label)

validation = get_split(data = data_train, pct_train = 0.75)[[2]] %>% 
             dplyr::select(-churn_label)  

# var select manual *\

train = get_var_select_manual(db = train)
validation = get_var_select_manual(db = validation)

# Imputação *\

train = get_impute(data = train)
validation = get_impute(data = validation)

# Encode *\

train = get_encoding(encode = "label", data = train)
validation = get_encoding(encode = "label", data = validation)

# writexl::write_xlsx(train, "train_balanceado.xlsx")
# writexl::write_xlsx(validation, "validation_balanceado.xlsx")

# Base de Teste \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

data_test_raw = get_import(db = "df_test")
data_test = get_var_select_manual(db = data_test_raw)
data_test = get_impute(data = data_test)
data_test = get_encoding(encode = "label", data = data_test)

# VAR SELECT LASSO *\ ==========================================================

# var_select = get_var_select_lasso(db_clean = train)
# 
# # Resultados do LASSO:
# 
# lasso_coefficients <- coef(var_select) %>% print()
# 
# vip::vip(var_select,
#          lambda = var_select[5],
#          num_features = 60) +
#     geom_bar(stat = "identity", fill = "darkorange")+
#     ggtitle("Variáveis mais relevantes")
# 
# train <- train %>% select(-gender,
#                           -longitude,
#                           -latitude,
#                           -total_charges,
#                           -monthly_charges)
# 
# validation <- validation %>% select(-gender,
#                                     -longitude,
#                                     -latitude,
#                                     -total_charges,
#                                     -monthly_charges)

# Modelagem ====================================================================
# BASELINE: Logit balanceado ===================================================

# logit_select_manual <- stats::glm(churn_label ~ dependents+tenure_months+contract, 
#                                   data = train, family = "binomial")

logit_model <- stats::glm(churn_label ~ ., data = train, family = "binomial")

# Classificação via logit 
# THRESHOLD = 0.99
# 0.43 = 0.8058691
# 0.44 = 0.8072976 
# 0.45 = 0.8096886 

predictions_prob <- stats::predict(logit_model, newdata = validation, type = "response")
predictions <- ifelse(predictions_prob > THRESHOLD , "Yes", "No")

# Maxtriz de confusão 

conf_matrix <- confusionMatrix(factor(predictions, levels = c("Yes", "No")),
                               factor(class$churn_label, levels = c("Yes", "No")))

print(conf_matrix)

conf_matrix <- table(Predicted = predictions, Actual = class$churn_label)
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(accuracy)

# Calcular o F1 Score

precision <- conf_matrix["Yes", "Yes"] / sum(conf_matrix[, "Yes"])
recall <- conf_matrix["Yes", "Yes"] / sum(conf_matrix["Yes", ])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Exibir métricas

cat("Acurácia:", accuracy, "\n")
cat("F1 Score:", f1_score, "\n")

# Classificando base de teste - fora da amostra 

predictions_prob <- stats::predict(logit_model, newdata = data_test, type = "response")

submit_prediction = data.frame("CustomerID" = data_test_raw$customer_id, 
                               "Churn Label" = ifelse(predictions_prob > THRESHOLD, "Yes", "No"))


getwd()
# utils::write.csv(submit_prediction , "model_logit_ultimo.csv", row.names = FALSE)

# Modelos de classificação \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Random Forrest *\

model_rf <- caret::train(churn_label  ~ .,
                         data = train, 
                         method ='rf', 
                         preProcess = c("center", "scale"),
                         trControl = trainControl(method ="cv", 
                                                  number = 10, 
                                                  allowParallel = TRUE, 

# Trees *\                                                  
                                                                                                    verboseIter = TRUE)) 
model_tree <- caret::train(churn_label ~ ., 
                           data = train, 
                           method = 'rpart',
                           preProcess = c("center", "scale"), 
                           trControl = trainControl(method = "cv", 
                                                    number = 10, 
                                                    allowParallel = TRUE, 
                                                    verboseIter = TRUE))
# Gradient Boosting *\

model_gbm <- caret::train(churn_label ~ .,
                           data = train,
                           method ='gbm',
                           verbose = FALSE,
                           trControl = trainControl(method = "cv",
                                                    number = 10,
                                                    allowParallel = TRUE,
                                                    verboseIter = TRUE))


# rede neural *\

model_nn <- caret::train(churn_label ~ .,
                         data = train,
                         method = 'nnet', 
                         trace = FALSE,    
                         # preProcess = c("center", "scale"),  
                         trControl = trainControl(method = "cv",   
                                                  number = 10,  
                                                  allowParallel = TRUE,  
                                                  verboseIter = TRUE)) 


# Linear Discriminant Analysis *\

model_lda <- caret::train(churn_label ~ ., 
                          data = train, 
                          method = 'lda',
                          preProcess = c("center", "scale"),
                          trControl = trainControl(method = "cv", 
                                                   number = 10, 
                                                   allowParallel = TRUE, 
                                                   verboseIter = TRUE)) 

# KNN *\

model_knn <- train(churn_label ~ .,
                  data = train,
                  method = "knn",
                  preProcess = c("center", "scale"),
                  trControl = trainControl(method = "cv",
                                           number = 10,
                                           allowParallel = TRUE,
                                           verboseIter = TRUE))

# Métricas de Acurácia 

rf = get_confusionMatrix(model = model_rf, validation) %>% print()
rf_f1 = get_f1_score(cm = rf) %>% print()

# tree = get_confusionMatrix(model = model_tree, validation) %>% print()
# tree_f1 = get_f1_score(cm = tree) %>% print()

gbm = get_confusionMatrix(model = model_gbm, validation) %>% print()
gbm_f1 = get_f1_score(cm = gbm) %>% print()

nn = get_confusionMatrix(model = model_nn, validation) %>% print()
nn_f1 = get_f1_score(cm = nn) %>% print()

lda = get_confusionMatrix(model = model_lda, validation) %>% print()
lda_f1 = get_f1_score(cm = lda) %>% print()

knn = get_confusionMatrix(model = model_knn, validation) %>% print()
knn_f1 = get_f1_score(cm = knn) %>% print()


model_compare <- data.frame(model = c('Random Forest',
                                      # 'Trees',
                                      'Gradient Boosting',
                                      "Rede Neural",
                                      'Linear Discriminant', 
                                      'KNN', 
                                      "Logit"),
                            accuracy = c(rf$overall[1],
                                         # tree$overall[1],
                                         gbm$overall[1],
                                         nn$overall[1],
                                         lda$overall[1], 
                                         knn$overall[1], 
                                         accuracy), 
                            F1 = c(rf_f1[1], 
                                   # tree_f1[1], 
                                   gbm_f1[1], 
                                   nn_f1[1], 
                                   lda_f1[1], 
                                   knn_f1[1], 
                                   f1_score)) %>% 
                    tidyr::pivot_longer(cols = accuracy:F1)


# Visualizando 

ggplot(model_compare) +
  aes(x = model, y = value, fill = name) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(value, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) + 
  ylim(0,1)+
  labs(y = "Métricas", x = "", 
       title = "Métricas ML", fill = "")+
  theme_minimal()+
  theme(legend.position = "bottom")
  

# Combinação de modelos 

submit_combined = data.frame("CustomerID" = data_test_raw$customer_id, 
                             Logit = ifelse(predictions_prob > THRESHOLD, "Yes", "No"), 
                             rf = predict(model_rf, newdata = data_test), 
                             # tree = predict(model_tree, newdata = data_test), 
                             gbm = predict(model_gbm, newdata = data_test), 
                             nn = predict(model_nn, newdata = data_test)
                             # lda = predict(model_lda, newdata = data_test), 
                             # knn = predict(model_knn, newdata = data_test)
                             )



# Função para encontrar a moda de um vetor, retornando "rf" em caso de empate bimodal
find_mode <- function(x) {
  # Conta a frequência de cada valor
  freq_table <- table(x)
  
  # Encontra o valor mais frequente
  max_freq <- max(freq_table)
  
  # Obtém todos os valores com a máxima frequência
  modes <- names(freq_table[freq_table == max_freq])
  
  # Se houver empate, retorna o valor da coluna 'rf', caso contrário, retorna a moda
  if (length(modes) > 1) {
    return(x[which(names(x) == "rf")])
  } else {
    return(modes)
  }
}

# Aplica a função find_mode a cada linha do data.frame
submit_combined$mode <- apply(submit_combined[, -1], 1, find_mode)


submit_combined <- submit_combined %>% 
  select(CustomerID,mode)

getwd()

utils::write.csv(submit_combined, "model_combined_ultimo2.csv", row.names = FALSE)
