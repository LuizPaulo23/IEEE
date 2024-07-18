#' @title Split, Pré-Processamento e Imputação 
#' @author Luiz Paulo Tavares 

get_pre_processing <- function(db, transformation, pct_train, var_target){
  
  base::set.seed(123)
  
  # Divisao entre base de Treino e Teste
  
  data_split <- rsample::initial_split(data = db,
                                       prop = pct_train)
  
  bases <- list(base_train = rsample::training(data_split),
                base_test  = rsample::testing(data_split))
  
# Pré processando Treino e Teste
# Loop para aplicar as transformações em treino e teste
  
  for(i in seq_along(bases)) {
    bases[[i]] <- bases[[i]] %>%
                  dplyr::select(-dplyr::all_of(var_target)) %>%
                  # Imputação pré-definida: mediana *\\
                  # Na presença de dados faltantes imputa de forma automática 
                  dplyr::mutate_if(is.numeric,
                                   zoo::na.aggregate,
                                   FUN = median,
                                   na.rm = TRUE) %>%
                  # Transformação dos dados c("zscore", "minmax", "log", "log+1", "sqrt", "Box-Cox", "Yeo-Johnson")
                   dplyr::mutate_if(is.numeric, 
                                    ~dlookr::transform(., method = transformation)) %>%
                  dplyr::bind_cols(bases[[i]][dplyr::all_of(var_target)]) # %>%
                  # Janitor provocando bug na selecão da variável alvo
                  # janitor::clean_names()
    # Salvando bases pré-processadas
    switch(names(bases)[i],
           base_train = base_train <- base::data.frame(bases[[i]]),
           base_test = base_test <- base::data.frame(bases[[i]]))
    
  }
  
  return(bases)
  
}






