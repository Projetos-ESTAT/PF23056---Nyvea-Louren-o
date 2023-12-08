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


library(tidyverse)
# install.packages('rnaturalearth')
library(rnaturalearth)
# install.packages('devtools')
# devtools::install_github("AndySouth/rnaturalearthhires")


# Banco
library(readxl)
df <- read_excel("banco/Pesquisa-TesedeDoutoradorespostas.xlsx")
View(df)


# Tema ESTAT

cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")



# 1 - UF


BRA <- ne_states(
  country = 'Brazil',
  returnclass = 'sf'
)


plot(BRA)

count(df, df$`Qual UF você representa?`)



siglas <- c('AC', 'AP','BA', 'DF', 'ES', 'GO', 'MA', 'MG', 'RR', 'RS', 'TO')
cont <- c(1,1,1,2,2,2,1,1,1,1,1)
lat <- c(-8.77	, 1.41, -13.29	, -15.83	, -19.19	, -15.98	, -5.42	, -18.10	, 1.99	, -30.17	, -9.46	)
long <- c(-70.55, -51.77, -41.71, -47.86, -40.34, -49.86, -45.44, -44.38, -61.33, -53.50, -48.26)
dat <- data.frame(siglas, cont, lat, long)
view(dat)



ggplot()+
  geom_sf(data = BRA)+
  geom_text(data = dat, fontface = 'bold', size = 3, colour = '#A11D21',
            aes(x = long, y = lat), label = cont)+
  theme_estat()
# ggsave("mapaUF.pdf", width = 158, height = 93, units = "mm")

quadro_resumo <- df %>% 
  group_by(df$) %>% # caso mais de uma categoria
  summarize(Média = round(mean(coffee),2),
            `Desvio Padrão` = round(sd(coffee),2),
            `Variância` = round(var(coffee),2),
            `Mínimo` = round(min(coffee),2),
            `1º Quartil` = round(quantile(coffee, probs = .25),2),
            Mediana = round(quantile(coffee, probs = .5),2),
            `3º Quartil` = round(quantile(coffee, probs = .75),2),
            `Máximo` = round(max(coffee),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) # adicionar mais mutate(...) se tiver mais categorias

xtable::xtable(quadro_resumo)


# 2 Qual a legislação --------------

view(df)
unique(df$`Qual a legislação que versa sobre Substituição Tributária na sua Unidade Federada?`)

# Todas as respostas são diferentes e não sei o que significa nenhuma

# 3 Quanto tempo trabalha ------------------


theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}


df$'Há quanto tempo trabalha (numerico)' <- gsub("[^0-9.-]", "", df$`Há quanto tempo trabalha com Substituição Tributária?`)
df$`Há quanto tempo trabalha (numerico)` <- case_when(
  df$`Há quanto tempo trabalha (numerico)`== '.' ~ 7.5,
  T ~ as.numeric(df$`Há quanto tempo trabalha (numerico)`)
)



ggplot(df) +
  aes(x=factor(""), y=df$`Há quanto tempo trabalha (numerico)`) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Tempo de trabalho (em anos)")+
  theme_estat()
# ggsave("box_idade.pdf", width = 158, height = 93, units = "mm")

quadro_resumo <- df %>% 
  summarize(Média = round(mean(df$`Há quanto tempo trabalha (numerico)`),2),
            `Desvio Padrão` = round(sd(df$`Há quanto tempo trabalha (numerico)`),2),
            `Variância` = round(var(df$`Há quanto tempo trabalha (numerico)`),2),
            `Mínimo` = round(min(df$`Há quanto tempo trabalha (numerico)`),2),
            `1º Quartil` = round(quantile(df$`Há quanto tempo trabalha (numerico)`, probs = .25),2),
            Mediana = round(quantile(df$`Há quanto tempo trabalha (numerico)`, probs = .5),2),
            `3º Quartil` = round(quantile(df$`Há quanto tempo trabalha (numerico)`, probs = .75),2),
            `Máximo` = round(max(df$`Há quanto tempo trabalha (numerico)`),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) # adicionar mais mutate(...) se tiver mais categorias

xtable::xtable(quadro_resumo)



# 4 Medidas adotadas na sua UF


# 5 Você acha que os estados estavam preparados?


df$'Os estados estavam preparados?' <- c(rep('Não', 14))

df$`Os estados estavam preparados?`[5] = 'Em parte'

view(df)

library(scales)
library(tidyverse)



classes <- df %>%
  filter(!is.na(`Os estados estavam preparados?`)) %>%
  count(`Os estados estavam preparados?`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )



ggplot(classes) +
  aes(x = fct_reorder(`Os estados estavam preparados?`, n, .desc=F), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  labs(x = "Opinião", y = "Frequência") +
  theme_estat()+
  scale_y_continuous(limits = c(0,15))
ggsave("5_colunas.pdf", width = 158, height = 93, units = "mm")




# 6  Em sua opinião, houve esvaziamento? -----------------

colnames(df)[8] = 'Em sua opinião, houve esvaziamento?'


classes <- df %>%
  filter(!is.na(df$`Em sua opinião, houve esvaziamento?`)) %>%
  count(df$`Em sua opinião, houve esvaziamento?`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )



colnames(classes)[1] = 'A'
ggplot(classes) +
  aes(x = fct_reorder(classes$A, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  labs(x = "Opinião", y = "Frequência") +
  scale_y_continuous(limits = c(0,15))+
  theme_estat()
# ggsave("6_colunas.pdf", width = 158, height = 93, units = "mm")



# 7  Justifique a anterior -----------------



# 8  Há a cobrança complementar do ICMS? -----------------

colnames(df)[10] = 'Há cobrança complementar?'

classes <- df %>%
  filter(!is.na(df$`Há cobrança complementar?`)) %>%
  count(df$`Há cobrança complementar?`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

colnames(classes)[1] = 'A'

ggplot(classes) +
  aes(x = fct_reorder(classes$A, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  labs(x = "Há a cobrança complementar?", y = "Frequência") +
  scale_y_continuous(limits = c(0,15))+
  theme_estat()
# ggsave("8_colunas.pdf", width = 158, height = 93, units = "mm")


# 9  Se sim qual a legislação? -----------------
# 10  [12] "Na sua opinião, a substituição tributária  é um mecanismo operacional relevante na arrecadação do ICMS?" -----------------
# 11 Justifique a anterior ---------------
# 11 Justifique a anterior ---------------