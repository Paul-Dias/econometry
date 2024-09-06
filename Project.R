#Trabalho de econometria

if(!require(dplyr)) install.packages("dplyr")
library(dplyr) 

install.packages("tidyverse")
library(tidyverse)
library(readr)
library(readxl)

dados <- read_csv("path to file")

#validação dos tipos de dados, todos dever ser dbl
spec(dados)

#Visão geral

View(dados)

summary(dados)

# Calcular a correlação
correlacoes <- cor(dados %>% select(numero_de_prisoes_efetuadas, 
                                    ocorrencias_de_porte_de_entorpecentes,
                                    ocorrencias_de_trafico_de_entorpecentes,
                                    ocorrencias_de_apreensao_de_entorpecentes,
                                    ocorrencias_de_porte_ilegal_de_arma,
                                    numero_de_armas_de_fogo_apreendidas,
                                    numero_de_flagrantes_lavrados,
                                    numero_de_infratores_apreendidos_em_flagrante,
                                    numero_de_infratores_apreendidos_por_mandado,
                                    numero_de_pessoas_presas_em_flagrante,
                                    numero_de_pessoas_presas_por_mandado,
                                    numero_de_veiculos_recuperados,
                                    total_de_inqueritos_policiais_instaurados))

print(correlacoes)

# Gráfico de dispersão entre número de prisões e ocorrências de tráfico
ggplot(dados, aes(x = ocorrencias_de_trafico_de_entorpecentes, y = numero_de_prisoes_efetuadas)) +
  geom_point() +
  labs(title = "Relação entre ocorrências de tráfico de entorpecentes e número de prisões",
       x = "Ocorrências de tráfico de entorpecentes",
       y = "Número de prisões efetuadas")

# Gráfico de dispersão entre número de prisões e ocorrências de porte ilegal de arma
ggplot(dados, aes(x = ocorrencias_de_porte_ilegal_de_arma, y = numero_de_prisoes_efetuadas)) +
  geom_point() +
  labs(title = "Relação entre ocorrências de porte ilegal de arma e número de prisões",
       x = "Ocorrências de porte ilegal de arma",
       y = "Número de prisões efetuadas")

if(!require(skimr)) install.packages("skimr")
library(skimr)
skim(dados)



 #Regressão da relação alvo do projeto
  modelo <- lm(numero_de_prisoes_efetuadas ~ ocorrencias_de_porte_de_entorpecentes + ocorrencias_de_porte_ilegal_de_arma + produto_interno_bruto_a_precos_correntes, data = dados)
  summary(modelo)


#Análises de regressão secundárias

modelo_pib <- lm(numero_de_prisoes_efetuadas ~ produto_interno_bruto_a_precos_correntes, data = dados)
summary(modelo_pib)


modelo_pib_veiculos <- lm(numero_de_prisoes_efetuadas ~ produto_interno_bruto_a_precos_correntes + numero_de_veiculos_recuperados, data = dados)
summary(modelo_pib_veiculos)

#Modelo de regressão com todas as variáveis
modelo_completo <- lm(numero_de_prisoes_efetuadas ~ ocorrencias_de_porte_de_entorpecentes + ocorrencias_de_trafico_de_entorpecentes + ocorrencias_de_apreensao_de_entorpecentes + ocorrencias_de_porte_ilegal_de_arma + numero_de_armas_de_fogo_apreendidas + numero_de_flagrantes_lavrados + numero_de_infratores_apreendidos_em_flagrante + numero_de_infratores_apreendidos_por_mandado + numero_de_pessoas_presas_em_flagrante + numero_de_pessoas_presas_por_mandado + numero_de_veiculos_recuperados + total_de_inqueritos_policiais_instaurados + produto_interno_bruto_a_precos_correntes, data = dados)
summary(modelo_completo)


