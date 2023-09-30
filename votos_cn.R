library(tidyverse)
library(lubridate)
library(congressbr)

## Função para Câmara dos Deputados ----
votos_camara <- function(ano, barra_progresso = TRUE, quietly = FALSE) {
  votacoes <- cham_plenary_bills(year = ano) %>% 
    rowwise() %>% 
    mutate(bill_name = str_replace_all(bill_name, " =>.*", ""), # conserta o nome dos requerimentos
           bill_type = str_split(bill_name, " ") %>% 
             `[[`(1) %>% `[`(1),
           bill_number = str_split(bill_name, "/") %>% 
             `[[`(1) %>% `[`(1) %>% 
             str_replace_all("\\D", "") %>% # apaga não-numéricos
             as.numeric(), 
           bill_year = str_split(bill_name, "/") %>% 
             `[[`(1) %>% `[`(2) %>% str_replace_all("\\D", "") %>% 
             as.numeric() %>% str_extract("\\d{4}")) %>% 
    distinct(bill_id,  .keep_all = TRUE) %>% 
    filter(bill_type != "REQ")
  
  votos <- list()
  
  for (i in 1:nrow(votacoes)) {
    votacao <- cham_votes(type = as.character(votacoes$bill_type[i]),
                          number = as.numeric(votacoes$bill_number[i]),
                          year = as.numeric(votacoes$bill_year[i]))
    votos[[i]] <- votacao
    if (barra_progresso == TRUE) setTxtProgressBar(txtProgressBar(min = 0, max = nrow(votacoes), style = 3), i)
    if ((i %% 20) == 0) Sys.sleep(5) # esperar 5s a cada 20 buscas
  }
  
  detalhes_votos <- bind_rows(votos) %>% 
    mutate(decision_date = dmy(decision_date),
           decision_time = hm(decision_time))
  
  write.csv2(detalhes_votos, paste0("detalhes_votos_", ano, ".csv"), row.names = FALSE)
  
  if (quietly == FALSE) cat(paste0("\n\nAs votações foram salvas no diretório\n", getwd(), "\n\n"))
  return(detalhes_votos)
}

## Votos por período ----
votos_periodo <- function(ano_inicio, ano_fim) {
  legis_lista <- list()
  
  for (i in ano_inicio:ano_fim) {
    legis_ano <- votos_camara(i, barra_progresso = FALSE, quietly = TRUE)
    legis_lista[[i]] <- legis_ano
    setTxtProgressBar(txtProgressBar(min = ano_inicio, max = ano_fim, style = 3), i)
  }
  
  legislatura <- bind_rows(legis_lista)
  
  write.csv2(legislatura, paste0("legislatura_", ano_inicio ,"_a_", ano_fim, ".csv"), row.names = FALSE)
  
  cat(paste0("\n\nAs votações do período foram salvas no diretório\n", getwd(), "\n\n"))
  return(legislatura)
}

## Exemplos ----
# Votações em 2019
votos_2019 <- votos_camara(2019)

# 55a Legislatura (2015 - 2018)
legislatura_55 <- votos_periodo(2015, 2018)


