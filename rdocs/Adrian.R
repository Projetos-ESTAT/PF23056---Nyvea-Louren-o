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

install.packages('rnaturalearth')
library(rnaturalearth)
install.packages('devtools')
devtools::install_github("AndySouth/rnaturalearthhires")

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
  geom_text(data = dat, fontface = 'bold', size = 3, colour = '#a11d21',
            aes(x = long, y = lat), label = cont)+
  theme_estat()
ggsave("mapaUF.pdf", width = 158, height = 93, units = "mm")



cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")





