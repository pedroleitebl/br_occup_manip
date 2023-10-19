# 1 - Packages 

library(here) # helps to define a directory 
library(dplyr) # fundamental in r to some functions (if, else, mutate)
library(readr) # read docs in r, .xls, .csv etc.
library(stringr) # package to work with strings 

# 2 - Upload 
perfil_ocupacional = read.csv(here("data/perfil_ocupacional.csv"), header = T)

# This database is a list os CBO codes (5 to 6 digits) from basedosdados.org. It cotains great area, activity
# and summary descriptions. 

# 3 - RCA with all occupations

perfil_ocupacional$cbo2d = substr(perfil_ocupacional$cbo_2002, start = 1, stop = 2) 

# Here, i create a new variable to capture the two first digits of CBO. This identifies the main subgroup,
# thus the managerial occupations (12, 13 and 14)

x = perfil_ocupacional %>%  
  filter(cbo2d == "14" | cbo2d == "12" | cbo2d == "13")  %>% 
  pull(descricao_atividade) %>% 
  table()%>%
  as.data.frame() %>%
  arrange(desc(Freq)) 

# This create a frequency table of summary descprition

y = perfil_ocupacional %>% 
  pull(descricao_atividade) %>% 
  table()%>%
  as.data.frame() %>%
  arrange(desc(Freq))

# Agora crio uma tabela de frequência das descrições de todos os cargos. 

total_freq =  sum(y$Freq)
palavras_filtradas = y[y$. %in% x$., ]
freq_cbo = sum(palavras_filtradas$Freq) 
participacao_cbo = freq_cbo / total_freq
freq_esperada = participacao_cbo * total_freq 
x$rca = palavras_filtradas$Freq/freq_esperada

# The last lines represent the RCA calculos of executives, directors and managers
