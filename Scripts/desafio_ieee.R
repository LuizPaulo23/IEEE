#' @title Desafio IEEE
#' @author Luiz Paulo Tavares Gonçalves 

# Definindo ambiente de trabalho ===============================================

base::rm(list = ls())
grDevices::graphics.off()
setwd("~/Github/Projetos/IEEE/db")

# Dependências =================================================================

pacman::p_load(DataExplorer, 
               tidyverse,
               stargazer,
               Amelia,
               GGally,
               broom,
               knitr,
               nnet)


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
            stats::na.omit() %>% 
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
           dplyr::group_by(age, classification) %>% 
                  summarise(freq_abs = n()) %>% 
                  ungroup()

# Visualizando 

ggplot2::ggplot(seg_age) +
         aes(x = age, y = freq_abs, fill = classification) +
         geom_col(position = "dodge") +
         scale_fill_brewer(palette = "Oranges", 
                           direction = -1) +
         labs(title = "Distribuição da variável Idade Segmentada por grupos de usuários",
              y = "Frequência Absoluta",
              x = "",
              fill = "")+
         theme_bw()+
         theme(legend.position = "bottom")


# Questão 03 ===================================================================

evid_educ = data_clean %>% 
             dplyr::select(id, 
                           education, 
                           alcohol:vsa) %>% 
             tidyr::pivot_longer(cols = alcohol:vsa, 
                                 values_to = "classification") %>% 
             dplyr::mutate(index = ifelse(classification == "CL0", 1, 0))


model_multinominal <- nnet::multinom(factor(index) ~ factor(education),
                               data = evid_educ)


summary(model_multinominal)
coeficientes <- broom::tidy(model_multinominal, conf.int = T) # Salvando os coeficientes 



ggplot(coeficientes, aes(term, estimate))+
  geom_point()+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
  labs(y = "Coeficientes com intervalo de confiança",x="",
       title = "Modelo Politômico - Intercepto, Coeficientes")+
  coord_flip()+
  theme_bw()

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


