source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #
library(tidyr)
##Gráfico da Paraíba:
Dados_Paraíba$`ICMS-ST` <- Dados_Paraíba$`ICMS-ST` / 1e9
Dados_Paraíba$ICMS <- Dados_Paraíba$ICMS / 1e9

Dados_Paraíba_Longos <- pivot_longer(Dados_Paraíba, cols = c("ICMS", "ICMS-ST"), names_to = "Imposto", values_to = "Valor")

ggplot(Dados_Paraíba_Longos) +
  aes(x = Ano, y = Valor, group = Imposto, colour = Imposto) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Imposto", labels = c("ICMS", "ICMS-ST")) +
  scale_y_continuous(breaks = seq(1, 8, by = 1)) +
  scale_x_continuous(breaks = 2012:2022) +
  labs(x = "Ano", y = "Valor (em bilhões)") +
  theme_estat()
ggsave("grafico_paraiba.pdf", width = 158, height = 93, units = "mm")


##Gráfico do DF:
Dados_DF$`ICMS-ST` <- Dados_DF$`ICMS-ST` / 1e6
Dados_DF$ICMS <- Dados_DF$ICMS / 1e6

Dados_DF_Longos <- pivot_longer(Dados_DF, cols = c("ICMS", "ICMS-ST"), names_to = "Imposto", values_to = "Valor")    

ggplot(Dados_DF_Longos) +
  aes(x = Ano, y = Valor, group = Imposto, colour = Imposto) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Imposto", labels = c("ICMS", "ICMS-ST")) +
  scale_y_continuous(breaks = seq(1, 8, by = 1)) +
  scale_x_continuous(breaks = 2012:2022) +
  labs(x = "Ano", y = "Valor (em milhões)") +
  theme_estat()
ggsave("grafico_df.pdf", width = 158, height = 93, units = "mm")
