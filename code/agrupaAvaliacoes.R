library(readr)
library(dplyr)

# gera a lista de nomes de arquivos .csv gerados pelos alunos
dirs <- c("20180911", "20180912")
files <- unlist(lapply(dirs, function(x) list.files(pattern = "*.csv", 
                    path = paste("data/3-avaliacao-humana/raw/", x, sep = ""), full.names = T)))

# captura os nomes das colunas no arquivo original bom base no primeiro arquivo de avaliação
cnames <- colnames(read_csv(files[1]))

# lê e agrupa os data.frames por linha
dff <- lapply(files, function(x) read_csv(x, col_names = cnames, col_types = "cccccnnnnc")) %>% bind_rows()
glimpse(dff)

# remove linha do cabeçalho duplicada
dff <- dff %>% filter(!orgao %in% cnames)
nrow(dff)

# remove linhas sem avaliações ou matrícula
avaliacoes <- dff %>% select(id, matricula, avaliacao) %>% filter(!is.na(matricula) & !is.na(avaliacao))
nrow(avaliacoes)
glimpse(avaliacoes)

# Existem avaliações distintas de uma mesma matrícula para uma mesma reclamação?
avaliacoes %>% distinct() %>% group_by(id, matricula) %>% 
  summarise(avaliacao_diff = n()) %>% filter(avaliacao_diff > 1)
filter(avaliacoes, matricula == 2016044387, id == 24)

# remove avalição duplicada optando pela maior 
avaliacoes <- avaliacoes %>% distinct() %>% group_by(id, matricula) %>% 
  summarise(avaliacao = max(avaliacao))

# Avalia novamente avaliações duplicadas
avaliacoes %>% distinct() %>% group_by(id, matricula) %>% 
  summarise(avaliacao_diff = n()) %>% filter(avaliacao_diff > 1) %>% nrow()

# Escreve dataframe em arquivo
write.csv(avaliacoes, file = "data/3-avaliacao-humana/avaliacoes-20180912.csv", row.names = F)