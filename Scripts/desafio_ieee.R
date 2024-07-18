#' @title Desafio IEEE
#' @author Luiz Paulo Tavares Gonçalves 

# Definindo ambiente de trabalho ===============================================

base::rm(list = ls())
grDevices::graphics.off()
base::set.seed(123) 

setwd("~/Github/Projetos/IEEE/db")

# Dependências =================================================================

pacman::p_load(DataExplorer,
               rpart.plot,
               tidyverse,
               stargazer,
               glmnet,
               Amelia,
               GGally,
               broom,
               rpart,
               caret,
               knitr,
               nnet, 
               vip,
               car)


# CONSTANTES & VAR GLOBAIS 

THRESHOLD = 2
FUNCTIONS = "~/Github/Projetos/IEEE/functions/"

# Chamando funções =============================================================

files <- list.files(path = FUNCTIONS, 
                    pattern = "\\.R$", full.names = TRUE)

for (i in files) {
  
  source(i)
  
}

# import dataset & cleaning ====================================================

data_raw = readr::read_csv("Drugs.csv") %>% 
           janitor::clean_names() %>% 
           dplyr::distinct(id, .keep_all = TRUE)
        
# Questão 01 ===================================================================

# Verificando a presença de NA's 

Amelia::missmap(data_raw, 
                col = c('red','darkorange'),
                main ='Mapa de Valores Faltantes',
                legend = T) 

# DataExplorer::plot_intro(data = data_raw)

# get_pct_missing: Calculando a porcentagem de dados faltantes em cada variável

get_pct_missing(data_raw)

data_clean = data_raw %>% 
             # Retirando a variável income por ter 98.78% de NA's 
            dplyr::select(-income_usd) %>% 
            stats::na.omit() %>% # Excluindo NA's 
            dplyr::mutate(age_factor = case_when(age == "18-24" ~ 1, 
                                                 age == "25-34" ~ 2, 
                                                 age == "35-44" ~ 3, 
                                                 age == "45-54" ~ 4, 
                                                 age == "55-64" ~ 5, 
                                                 age == "65+" ~ 6))

# Questões de número 02 ========================================================

AGE <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")

ggplot2::ggplot(data_clean) +
         aes(x = as.factor(age_factor), 
             fill = age) + 
         geom_bar() +
         scale_x_discrete(labels = AGE) + 
         scale_fill_brewer(palette = "Oranges", 
                           direction = -1) +  
         labs(title = "Distribuição da variável Idade", 
              subtitle = "Variável Discreta",
              y = "Contagem",
              x = "",
              fill = "") + 
  theme_bw() +
  theme(legend.position = "bottom")

# Frequências 

freqs_age <- data_clean %>% 
             dplyr::group_by(age) %>% 
             dplyr::summarise(n = n()) %>% 
                    ungroup() %>% 
                    mutate(total = base::sum(n), 
                    pct = base::round((n/total)*100, THRESHOLD)) %>% select(-total) 


knitr::kable(freqs_age,
             caption = "Estatísticas Descrtitivas da variável Idade",  
             col.names = c("Idade", 
                           "Frequência Absoluta", 
                           "Frequência Relativa"),
             format = "markdown")

# Segmento por grupos de usuários de diferentes substâncias

seg_age <- data_clean %>% 
           dplyr::select(id, 
                         age, 
                         alcohol:vsa) %>% 
           tidyr::pivot_longer(cols = alcohol:vsa, 
                               values_to = "classification") %>% 
           dplyr::group_by(age, classification, name) %>% 
                  summarise(freq_abs = n()) %>% 
                  ungroup()


ggplot2::ggplot(seg_age) +
        aes(x = age, y = freq_abs, fill = classification) +
        geom_col(position = "fill") +
        scale_fill_brewer(palette = "Oranges", direction = -1) +
        labs(title = "Idade Segmentada por grupos de usuários e substância",
             y = "%",
             x = "",
             fill = "")+
        coord_flip() +
        theme_minimal()+
        theme(legend.position = "bottom")+
        facet_wrap(vars(name))

# Correlação entre idade e uso de substâncias \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# REPETIR O CÓDGIO PARA A NÚMERO 03 >> RELAÇÃO ENTRE ESCOLARIDADE E USO DE SUBSTÂNCIAS
# REPETIÇÃO DESNECESSÁRIO != BOAS PRÁTICAS & CLEAN CODE 
# CÓDIGO PODE SER REFATORADO 

unique(seg_age$age)

age_levels <- c("18-24", 
                "25-34", 
                "35-44", 
                "45-54", 
                "55-64", 
                "65+")


seg_age <- data_clean %>%
           dplyr::select(age, 
                         alcohol:vsa) %>%
                  mutate(age = factor(age,
                         levels = age_levels, 
                         ordered = TRUE)) %>%
                  mutate_if(is.character, as.factor)

# levels(seg_age$age)
# levels(seg_age$amphet)

# Correlações ==================================================================

cor_spearman <- function(var) {
  
  test <- cor.test(as.numeric(seg_age$age), 
                   as.numeric(var), method = "spearman")
  
  return(c(estimate = test$estimate, p.value = test$p.value))
  
}


cor_kendall <- function(var) {
  
  test <- cor.test(as.numeric(seg_age$age), 
                   as.numeric(var), method = "kendall")
  
  return(c(estimate = test$estimate, p.value = test$p.value))
}

# Calculando as correlações e p-valores ========================================

correlations_spearman <- base::do.call(rbind, lapply(seg_age[, -1], cor_spearman))
correlations_kendall <- base::do.call(rbind, lapply(seg_age[, -1], cor_kendall))


correlations <- base::data.frame(correlations_kendall,
                                 correlations_spearman) %>% 
  janitor::clean_names() %>% 
  dplyr::rename(Kendall = estimate_tau, 
                Spearman = estimate_rho, 
                "P-valor Kendall" = p_value, 
                "P-valor Spearman" = p_value_1) %>% 
  mutate("***Kendall" = ifelse(`P-valor Kendall` < 0.05,
                               "Significativo", "Não significativo"), 
         "***Spearman" = ifelse(`P-valor Spearman` < 0.05,
                                "Significativo", "Não significativo")) %>% 
  relocate(`***Kendall`, .after = `P-valor Kendall`) %>% 
  relocate(`***Spearman`, .after = `P-valor Spearman`)


knitr::kable(correlations, caption = "Relação entre Idade e uso de Substâncias")


# Questão 03 & 10 ===================================================================

# Ordenando o nível de escolaridade - do menor para o maior: 

education_levels <- c("Left school before 16 years", 
                      "Left school at 16 years", 
                      "Left school at 17 years", 
                      "Left school at 18 years", 
                      "Some college or university, no certificate or degree",
                      "Professional certificate/ diploma", 
                      "University degree", 
                      "Masters degree", 
                      "Master degree", 
                      "Doctorate degree")


knitr::kable(education_levels,col.names = "Nível",
               caption = "Nível Educacional - do menor para o maior")


seg_edu <- data_clean %>%
           dplyr::select(education, 
                         alcohol:vsa) %>%
                  mutate(education = factor(education,
                         levels = education_levels, 
                         ordered = TRUE)) %>%
                  mutate_if(is.character, as.factor)


# levels(seg_edu$education)
# levels(seg_edu$alcohol)

# Correlações ==================================================================

cor_spearman <- function(var) {
  
  test <- cor.test(as.numeric(seg_edu$education), 
                   as.numeric(var), method = "spearman")
  
  return(c(estimate = test$estimate, p.value = test$p.value))
  
}


cor_kendall <- function(var) {
  
  test <- cor.test(as.numeric(seg_edu$education), 
                   as.numeric(var), method = "kendall")
  
  return(c(estimate = test$estimate, p.value = test$p.value))
}

# Calculando as correlações e p-valores ========================================

correlations_spearman <- base::do.call(rbind, lapply(seg_edu[, -1], cor_spearman))
correlations_kendall <- base::do.call(rbind, lapply(seg_edu[, -1], cor_kendall))


correlations <- base::data.frame(correlations_kendall,
                                            correlations_spearman) %>% 
                           janitor::clean_names() %>% 
                           dplyr::rename(Kendall = estimate_tau, 
                                         Spearman = estimate_rho, 
                                         "P-valor Kendall" = p_value, 
                                         "P-valor Spearman" = p_value_1) %>% 
                                  mutate("***Kendall" = ifelse(`P-valor Kendall` < 0.05,
                                                               "Significativo", "Não significativo"), 
                                         "***Spearman" = ifelse(`P-valor Spearman` < 0.05,
                                                                "Significativo", "Não significativo")) %>% 
                                  relocate(`***Kendall`, .after = `P-valor Kendall`) %>% 
                                  relocate(`***Spearman`, .after = `P-valor Spearman`)


knitr::kable(correlations, caption = "Relação entre Educação e uso de Substâncias")

# Questão 04 ==================================================================

evid_gender = data_clean %>% 
              dplyr::select(id, gender, 
                            lsd, 
                            ecstasy,
                            ketamine, 
                            cannabis, 
                            mushrooms) %>% 
              tidyr::pivot_longer(lsd:mushrooms, 
                                  values_to = "classification") %>% 
              dplyr::mutate(uso = ifelse(classification == "CL0", 1,0),
                            classification = factor(classification), 
                            gender = factor(gender))
      


model_multinominal <- nnet::multinom(classification ~ gender, data = evid_gender)


odds_ratios <- base::exp(coef(model_multinominal))


knitr::kable(odds_ratios, 
       caption = "Coeficientes do Modelo Multinominal")

odds_ratios <- odds_ratios %>% 
               base::as.data.frame() %>% 
               janitor::clean_names() %>% 
               rename(Homem = gender_m, 
                      Mulher = intercept) %>% 
               mutate(classe = c("CL1", "CL2","CL3","CL4","CL5","CL6")) %>% 
               tidyr::pivot_longer(cols = Homem:Mulher)


ggplot(odds_ratios, 
       aes(classe, value, colour = name))+
  geom_point()+
  labs(title = "Coeficientes: Modelo Multinominal/Politômico",
       y = "", x = "", colour = "")+
  theme_bw()+
  theme(legend.position = "bottom")

# Questão 05 ===================================================================

impulse <- data_clean %>% 
            dplyr::select(id, 
                          impulsive, 
                          alcohol:vsa) %>% 
                    mutate(impulsive = as.numeric(impulsive)) %>% 
            stats::na.omit()

# score superior a zero == Impulsivo 

auto_classificam = table(impulse$impulsive > 0)


ggplot(impulse) +
  aes(x = impulsive) +
  geom_histogram(bins = 30L, fill = "darkorange") +
  labs(title = "Distribuição da Impulsividade", x = "", y = "") +
  theme_minimal() +
  geom_vline(xintercept = 0, 
             color = "black",
             linetype = "dashed") +
  annotate("text", 
           x = 2, y = 350,
           label = paste0(round((auto_classificam[2] / (auto_classificam[1] + auto_classificam[2]))*100, 2), "% se classificam como impulsivos"), 
           color = "black", angle = 0, vjust = 0)


# Correlação -------------------------------------------------------------------

cor_spearman <- impulse %>%
                dplyr::select(-id) %>% 
                       mutate(across(-impulsive, 
                                     ~ as.numeric(factor(.,
                                                        levels = c("CL0", "CL1", "CL2", "CL3", "CL4", "CL5", "CL6"),
                                                        labels = 0:6))))


cor_results <- cor_spearman %>%
               summarise(across(-impulsive, ~ cor.test(impulsive, ., method = "spearman")$estimate))

cor_results <- cor_results %>% 
               tidyr::pivot_longer(cols = alcohol:vsa) %>% 
               dplyr::rename("Substâncias" = "name",
                             "Correlação" = "value") 


knitr::kable(cor_results, 
             caption = "Correlação de Spearman")

# Questão 06 ===================================================================

six <- data.frame(var = c("id", "age", "gender", 
                          "education", "country", 
                          "ethnicity", "income_usd", "nscore", 
                          "escore", "oscore", "a_score", 
                          "cscore", "impulsive", "ss", 
                          "alcohol", 
                          "amphet", "amyl", "benzos", "caff", 
                          "cannabis", "choc", "coke", "crack", "ecstasy", 
                          "heroin", "ketamine", "legalh", "lsd", "meth", 
                          "mushrooms", "nicotine", "semer", "vsa"), 
                  qualitativa = c("sim","sim", "sim", 
                                  "sim", "sim", "sim", "não", "não", 
                                  "não", "não", "não", "não", "não", "não",
                                  "sim", 
                                  "sim", "sim", "sim", "sim", "sim", "sim", "sim", "sim", "sim", "sim", "sim", "sim", 
                                  "sim", "sim", "sim", "sim", "sim", "sim"), 
                  classe = c("nominal", "ordinal", "nominal", "ordinal", 
                             "nominal", "nominal", "contínua", "contínua", 
                             "contínua", "contínua", "contínua", "contínua", "contínua", 
                             "contínua", 
                             "ordinal", 
                             "ordinal", "ordinal", "ordinal", "ordinal", "ordinal", "ordinal", 
                             "ordinal", "ordinal", "ordinal", "ordinal", "ordinal", "ordinal", 
                             "ordinal", "ordinal", "ordinal", "ordinal", "ordinal", "ordinal"))

knitr::kable(six, 
             caption = "Estrutura dos dados")

# Questão 07 ===================================================================

# Criando um data frame com a classificação das drogas
drogas <- data.frame(
  droga = c("alcohol", "amphet", "amyl", "benzos", "caff", "cannabis", 
            "choc", "coke", "crack", "ecstasy", "heroin", "ketamine", 
            "legalh", "lsd", "meth", "mushrooms", "nicotine", "semer", "vsa"),
  status = c("lícita", "ilícita", "ilícita", "ilícita", "lícita", "ilícita", 
             "lícita", "ilícita", "ilícita", "ilícita", "ilícita", "ilícita", 
             "ilícita", "ilícita", "ilícita", "ilícita", "lícita", "ilícita", "ilícita"))

knitr::kable(drogas)

status <- data_clean %>% 
          dplyr::select(alcohol:vsa) %>% 
          tidyr::pivot_longer(cols = alcohol:vsa) %>% 
          dplyr::mutate(index = ifelse(name == "alcohol" | 
                                         name == "caff" |
                                            name == "choc" |
                                              name == "nicotine", "liberado", "proibido")) %>% 
          dplyr::group_by(name, value, index) %>% 
          dplyr::summarise(freq = n())

# Questão 08 ===================================================================

add_crack <- data_clean %>% 
             dplyr::mutate(user_crack = ifelse(crack == "CL0", 0,1)) %>% 
                    select(-id, -age_factor, -crack) %>% 
                    relocate(user_crack, .after = NULL) %>% 
                    mutate(age = factor(age), 
                           gender = factor(gender),
                           education = factor(education), 
                           country = factor(country), 
                           ethnicity = factor(ethnicity), 
                           alcohol = factor(alcohol), 
                           amphet = factor(amphet), 
                           amyl = factor(amyl), 
                           benzos = factor(benzos), 
                           caff = factor(caff), 
                           cannabis = factor(cannabis), 
                           choc = factor(choc), 
                           coke = factor(coke), 
                           ecstasy = factor(ecstasy), 
                           heroin = factor(heroin), 
                           ketamine = factor(ketamine), 
                           legalh = factor(legalh), 
                           lsd = factor(lsd), 
                           meth = factor(meth), 
                           mushrooms = factor(mushrooms), 
                           nicotine = factor(nicotine), 
                           semer = factor(semer), 
                           vsa = factor(vsa))


# Modelo LASSO para seleção de variáveis 

add_crack_matrix <- stats::model.matrix(user_crack ~ ., data = add_crack)[,-1]

cv_lasso <- glmnet::cv.glmnet(add_crack_matrix, 
                              add_crack$user_crack,
                              alpha = 1, family = "binomial")
summary(cv_lasso)
best_lambda <- cv_lasso$lambda.min

# Ajustar o modelo com o melhor lambda 

lasso_model <- glmnet(add_crack_matrix,
                      add_crack$user_crack, alpha = 1, 
                      family = "binomial", lambda = best_lambda)


vip(lasso_model, 
    lambda = best_lambda,
    num_features = 10) +
    geom_bar(stat = "identity", 
             fill = "darkorange")+
    ggtitle("Variáveis mais relevantes para explicar o uso de crack")
    
# Rodando LASSO {forma alternativa} ============================================
  
library(glmnet)  

# Use cross-validation to select the lambda that minimizes the mean cross-validated error.

exp = stats::model.matrix(user_crack ~ ., data = add_crack)[,-1]

# Fit Lasso Model

cv_lasso <- glmnet::cv.glmnet(y = add_crack$user_crack, x = exp, alpha = 1, family = "binomial") 

# Visualizando lambda 

plot(cv_lasso)

# Determine the Best Lambda

best_lambda <- cv_lasso$lambda.min


#  Re-fit the Lasso Model with the Selected Lambda

lasso_model <- glmnet::glmnet(y = add_crack$user_crack,
                              x = exp, 
                              lambda = best_lambda, alpha = 1, family = "binomial") 

#  Investigate the Coefficients

coef(lasso_model)

# graphics.off()

vip::vip(lasso_model, 
    lambda = best_lambda,
    num_features = 10) +
  geom_bar(stat = "identity", 
           fill = "darkorange")+
  ggtitle("Variáveis mais relevantes para explicar o uso de crack")

# Questão 09 ===================================================================

graphics.off()
scores = data_clean %>%
         dplyr::select(nscore, 
                       escore, 
                       oscore, 
                       a_score, 
                       cscore) %>% 
         stats::na.omit() 

for (i in 1:4) {
 
  # Executar o teste de Shapiro-Wilk e média aritmética
  print("***********************************")
  cat("Teste de Shapiro-Wilk para a variável: ", base::colnames(scores)[i], "\n")
  cat("Variável com média: ", base::mean(scores[[i]]), "\n")
  sw = stats::shapiro.test(scores[[i]])
  print(sw)
  
  
  if(sw["p.value"] < 0.05){
    cat("\n")
    cat("Variável não tem normalidade")
    cat("\n")
  } else{
    cat("\n")
    cat("Variável com distribuição normal")
    cat("\n")
}
  
  
  
}

GGally::ggpairs(scores, 
                title = "Matriz de correlação")             

# Questão 11 ===================================================================

db_model = data_clean %>% 
          dplyr::select(age, 
                        gender,
                        education, 
                        country,
                        ethnicity, 
                        alcohol) %>% 
          mutate(age = factor(age), 
                 gender = factor(gender),
                 education = factor(education), 
                 country = factor(country), 
                 ethnicity = factor(ethnicity), 
                 alcohol = ifelse(alcohol == "CL0", 0, 1), 
                 alcohol = as.factor(alcohol)) %>% 
          relocate(alcohol, .after = NULL)

    
 # Chamando a função de pré-processamento 
 
 data_pre_processing = get_pre_processing(db = db_model, 
                                          transformation = "zscore",
                                          pct_train = 0.75, 
                                          var_target = "alcohol")
    
# Definindo o controle de treino com validação cruzada de 10 vezes

train_control <- caret::trainControl(method = "cv", number = 10)

# Treinando o modelo

model <- caret::train(alcohol ~ ., 
                      data = data_pre_processing[["base_train"]], 
                      method = "rpart", 
                      trControl = train_control)

# Testar o modelo no conjunto de teste

predictions <- stats::predict(model, newdata = data_pre_processing[["base_test"]])
confusionMatrix(predictions, data_pre_processing[["base_test"]]$alcohol)

# Plotar a árvore de decisão

# final_model <- model$finalModel
# rpart.plot(final_model, box.palette="Blues")

# Questão 13 =====================================================================

country <- data_clean %>% 
  dplyr::select(country, alcohol:vsa) %>% 
  tidyr::pivot_longer(cols = alcohol:vsa) %>% 
  dplyr::mutate(country = trimws(country),
                country = ifelse(country == "EUA", "USA", country)) %>% 
  dplyr::filter(value != "CL0") %>% 
  group_by(country, name) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  top_n(3, n) %>% 
  arrange(country, desc(n))


ggplot(country) +
  aes(x = country, y = n, fill = name) +
  geom_col(position = "dodge2") +
  scale_fill_brewer(palette = "Paired", direction = -1) +
  xlim("UK", "USA", "Other", "Canada", "Australia", 
       "Republic of Ireland","New Zealand")+
  labs(y = "Número de CL0", x = "", fill = "", 
       title = "3 drogas mais usadas em cada país")+
  theme_minimal() +
  theme(legend.position = "bottom")+
  coord_flip()


# Menos usadas: 

country_min <- data_clean %>% 
  dplyr::select(country, alcohol:vsa) %>% 
  tidyr::pivot_longer(cols = alcohol:vsa) %>% 
  dplyr::mutate(country = trimws(country),
                country = ifelse(country == "EUA", "USA", country)) %>% 
  dplyr::filter(value != "CL0") %>% 
  group_by(country, name) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  slice_min(n, n = 3, with_ties = FALSE) %>% 
  arrange(country, n)


ggplot(country_min) +
  aes(x = country, y = n, fill = name) +
  geom_col(position = "dodge2") +
  scale_fill_brewer(palette = "Paired", direction = -1) +
  xlim("UK", "USA", "Other", "Canada", "Australia", 
       "Republic of Ireland","New Zealand")+
  labs(y = "Número de CL0", x = "", fill = "", 
       title = "3 drogas menos usadas em cada país")+
  theme_minimal() +
  theme(legend.position = "bottom")+
  coord_flip()
