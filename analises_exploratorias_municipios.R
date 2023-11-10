library(tidyverse)

#Carrega os dados disponívies na pasta data


ibge2022 <- read_csv("data/censo_pop_municipios_2022.csv")


#Estatísticas globais

#COntagem de número de municípios brasileiros
NROW(ibge2022) #NROW é a função que conta as linhas de um dataframe

#População brasileira a partir da soma das populações dos municípios

sum(ibge2022$populacao_residente) #Sum é a função que soma valores presentes em um conjunto de dados numéricos.

#Uma comparação com a população brasielira em 1970
https://pt.wikipedia.org/wiki/Pra_frente_Brasil_(can%C3%A7%C3%A3o)


#Média da população dos municípios
mean(ibge2022$populacao_residente) #mean é a função que calcula a média de um conjunto de dados numéricos

#Mediana da população dos municípios

#Mediana é a medida que indica o valor que divide um conjunto ordenado de dados numérios em duas metades iguais

#Mediana para conjuntos de número ímpar de elementos

#Exemplo para o conjunto abaixo com cinco elementos, a mediana é 7 já que antes e depois do 7 formam-se dois sub-conjuntos de dois elementos

conjunto_impar<- c(1,3,7,9,11)

#Mediana para conjuntos de número par de elementos
#Para um número par de elementos, a mediana do conjunto é dada pela média pelos números que ocupam as duas posições centrais do conjunto.
#No caso do exemplo abaixo de um conjunto de seis elementos, as posições centrais são 3 e 4. Os números ocupados por essas duas posiçõe são 9 e 11
#A média calculada para 9 e 11 é 10. Logo, a mediana do cojunto abaixo é 10

conjunto_par<- c(1,3,9,11,14,20)


#median é a função que calcula a mediana de um conjunto de dados numéricos


median(conjunto_impar)

median(conjunto_par)


#Voltando ao problema original de cálculo da mediana da população residente teremos
median(ibge2022$populacao_residente)

#Agora é sua vez. Calcule:

#Área do Brasil a partir da soma das áreas dos municípios

#Média da área dos municípios

#Mediana da área dos municípios


#Agora um pouco de ordenamento
#As dez cidades mais populosas do Brasil

#A função slice_max particiona um dataset tendo como critério as n ocorrências de maior valor de uma variável informada
#No caso abaixo a variável usada como referência é população residente e a quantidade de ocorrência é 10

ibge2022 %>%
  slice_max(order_by = populacao_residente, n=10)



#As dez cidades menos populosas do Brasil

#A função slice_min particiona um dataset tendo como critério as n ocorrências de menor valor de uma variável informada
#No caso abaixo a variável usada como referência é população residente e a quantidade de ocorrência é 10

ibge2022 %>%
  slice_min(order_by = populacao_residente, n=10)


#Agora é sua vez. busque:

#Os dez municípios com maior área no  Brasil

#Os dez municípios com menor área no  Brasil


#uma sumarização por estado dos dados de população de municípios
#A função summarise permite que se faça diversas operações de resumo de dados, tais como: contagem, soma ou razão
#Os resumos de dados podem ser feitos por agrupamento tendo como refeência uma ou mais variáveis categóricas
ibge2022 %>%
  summarise(populacao_residente_uf = sum(populacao_residente), #somatória de valores
            area_uf = sum(area_da_unidade_territorial),
            densidade_demografica_uf = populacao_residente_uf/area_uf , #cálculo de razão
            numero_municipios_uf = n(), #contagem
            media_populacao_uf = mean(populacao_residente), #média
            .by = uf) %>%
  arrange(desc(populacao_residente_uf))

#Agora é sua vez: experimente trocar a varíavel usada como referência para o ordenamento. E também mudar a ordem

#mais epráticas E ANÁLISES