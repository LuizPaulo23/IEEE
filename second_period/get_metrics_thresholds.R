#' @title get_metrics_thresholds: calcular as métricas de acordo com uma linha de acordo na probabilidade 
#' @author Luiz Paulo Tavares 

get_metrics_thresholds <- function(model, data_validation){

thresholds = base::seq(0.01, 1, by = 0.01)

# Inicialize vetores para armazenar as métricas

accuracy_values = base::numeric(length(thresholds))
f1_score_values = base::numeric(length(thresholds))


for (i in seq_along(thresholds)) {
  
  
  predictions_prob <- stats::predict(model, newdata = data_validation, type = "response")
  predictions <- ifelse(predictions_prob >  thresholds[i], "Yes", "No")
  
  # Matriz de confusão
  
  conf_matrix <- caret::confusionMatrix(factor(predictions, levels = c("Yes", "No")),
                                        factor(class$churn_label, levels = c("Yes", "No")))
  
  # Calcular a acurácia
  
  accuracy_values[i] <- conf_matrix$overall['Accuracy']
  
  # Calcular o F1 Score
  
  f1_score_values[i] <- conf_matrix$byClass['F1']
  
}


results_df <- base::data.frame(k = thresholds,
                               accuracy = accuracy_values,
                               f1_score = f1_score_values)


} 


metricas = get_metrics_thresholds(model = logit_model, data_validation = validation)

# Plotar as métricas

max_accuracy_threshold <- metricas %>% filter(accuracy == max(accuracy)) %>% print()
max_f1_score_threshold <- metricas %>% filter(f1_score == max(f1_score, na.rm = T)) %>% print()

# balanceado = 
  
  ggplot(metricas, aes(x = k)) +
  geom_line(aes(y = accuracy,
                color = "Acurácia")) +
  geom_line(aes(y = f1_score,
                color = "F1 Score")) +
  scale_color_manual(values = c("Acurácia" = "black", "F1 Score" = "darkorange")) +
  geom_vline(xintercept = max_accuracy_threshold$k,
             linetype = "dashed",
             color = "black",
             size = 0.8) +
  geom_vline(xintercept = max_f1_score_threshold$k,
             linetype = "dashed",
             color = "darkorange", size = 0.8) +
  labs(title = "Modelo balanceado: Métricas de Acurácia e F1 Score em Função do Threshold do modelo logit",
       x = "Threshold",
       y = "Métrica",
       color = "") +
  ylim(0,1)+
  theme_bw()+
  theme(legend.position = "bottom")

# library(patchwork)
# balanceado
# desbalanceado
# 
# combined_plot <- balanceado / desbalanceado
# 
# # Exibir o gráfico combinado
# print(combined_plot)
