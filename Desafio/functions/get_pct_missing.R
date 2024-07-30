#' @title Contagem de missings 
#' @author Luiz Paulo Tavares Gonçalves 

get_pct_missing <- function(df) {
  
  missing_test <- base::sapply(df, function(x) {
    
    sum(is.na(x)) / length(x) * 100
    
  })
  
  missing_pct = missing_test %>% 
                base::as.data.frame() %>% 
                dplyr::arrange(desc(.)) %>% 
                dplyr::rename("%" = ".") 
  
  table_missing = knitr::kable(missing_pct,
                               caption = "Porcentagem de NA's nas variáveis do Dataset",  
                               col.names = c("Variáveis do Dataset", 
                                             "Porcentagem"),
                               format = "markdown")
  
  return(table_missing)
}

