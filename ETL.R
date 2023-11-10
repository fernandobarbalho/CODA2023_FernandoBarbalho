library(siconfiBD)

###Dados do IBGE

#Função que busca dados da API do IBGE, trata os dados baixados e entrega um dataframe com as populações dos municípios
#brasileiros para o ano de 2022

gera_tabela_ibge_municipios<- function(){
  # Load required libraries
  library(httr)
  library(jsonlite)
  library(janitor)
  library(tidyverse)

  # API endpoint URL
  api_url <- "https://apisidra.ibge.gov.br/values/t/4714/n6/all/v/all/p/all/d/v614%202"




  data_list <- fromJSON(api_url)


  names_df<- data_list[1,]
  data_list <- data_list[-1,]

  names(data_list) <- names_df

  data_list <- janitor::clean_names(data_list)

  ibge_municipios<-
    data_list %>%
    mutate(valor =as.numeric(valor)) %>%
    select(municipio_codigo,
           municipio,
           variavel,
           valor,
           ano) %>%
    pivot_wider(id_cols = c(municipio_codigo, municipio), names_from = variavel, values_from = valor) %>%
    separate(municipio, into = c("municipio", "uf"), sep = " - ")


  ibge_municipios<- janitor::clean_names(ibge_municipios)


}


#chama a função com a geração do dataframe
ibge2022<-
gera_tabela_ibge_municipios()


#Cria arquivo csv com os dados do dataframe
ibge2022 %>%
  readr::write_csv("data/censo_pop_municipios_2022.csv")

#novas cargas
siconfiBD::setup_siconfi("nice-diorama-306223")

funcoes<-
siconfiBD::get_distinct_function()

desp_saude_educacao<- siconfiBD::get_perc_function_exp_municipality(
  year = 2022,
  gov_function = c("Saúde","Educação"),
  expense_stage =  "Despesas Liquidadas"
)

desp_saude_educacao %>%
  readr::write_csv("data/saude_educacao_municipios.csv")
