library(tidyverse)
library(rvest)
library(stringr)

avaliacoes <- read_csv("data/3-avaliacao-humana/avaliacoes-20180912.csv")

glimpse(avaliacoes)

avaliacoes <- avaliacoes %>% rename(id.reclamacao = id)
glimpse(avaliacoes)

avaliacoes <- avaliacoes %>% select(id.reclamacao, avaliacao) %>% 
              group_by(id.reclamacao) %>% 
              summarise(insatisfacao = median(avaliacao), 
                        avaliadores = n(),
                        range.avaliacoes = (max(avaliacao) - min(avaliacao)))

#quantas avaliações tem discordancia de avaliação maior que 2? Será que devemos confiar nessas avaliações?
avaliacoes %>% filter(range.avaliacoes > 2) 


reclamacoes.avaliadas <- read_csv("data/1-reclamacoes-selecionadas/20180910-reclamacoes-selecionadas.csv")

reclamacoes <- left_join(reclamacoes.avaliadas, avaliacoes, 
                         by = c("id" = "id.reclamacao"))

glimpse(reclamacoes)

reclamacoes %>%  write_csv("data/3-avaliacao-humana/reclamacoes-avaliadas-20180924.csv")
