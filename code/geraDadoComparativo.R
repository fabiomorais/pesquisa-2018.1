library(tidyverse)

# Lendo arquivo de avaliação humana para dataframe selecionando colunas: id e insatisfacao
avaliacao_humana <- read_csv("data/3-avaliacao-humana/reclamacoes-avaliadas-20180924.csv") %>% 
  select(id, insatisfacao)
glimpse(avaliacao_humana)

# Lendo arquivo de avaliação automática para dataframe selecionando colunas: id, op30 e sent
avaliacao_automatica <- read_csv("data/5-sentimentos/sentimento.csv") %>% 
  select(id, sentimento_op30, sentimento_sent)
glimpse(avaliacao_automatica)

# lista as primeiras ocorrências no dataframe
head(avaliacao_humana)
head(avaliacao_automatica)

# imprime um sumário dos dados de insatisfação humana
summary(avaliacao_humana$insatisfacao)

# normaliza os dados de avaliação humana em uma escala de 0..1
avaliacao_humana <- avaliacao_humana %>% 
  mutate(val_humana = (insatisfacao - min(insatisfacao, na.rm = T)) / 
                      (max(insatisfacao, na.rm = T) - min(insatisfacao, na.rm = T))) %>% 
  select(-insatisfacao)

# normaliza os dados de sentimento automático em uma escala de 0..1 invertida (0 + negativo e 1 + positivo)
avaliacao_automatica <- avaliacao_automatica %>% 
  mutate(val_op30 = (sentimento_op30 - min(sentimento_op30, na.rm = T)) / 
                    (max(sentimento_op30, na.rm = T) - min(sentimento_op30, na.rm = T))) %>% 
  mutate(val_sent = (sentimento_sent - min(sentimento_sent, na.rm = T)) / 
                    (max(sentimento_sent, na.rm = T) - min(sentimento_sent, na.rm = T))) %>% 
  select(id, val_op30, val_sent)

# inverte a escala de positividade, para0 = a mais positivo e 1 = mais negativo
avaliacao_automatica <- avaliacao_automatica %>% 
  mutate(val_op30 = 1 - val_op30, val_sent = 1 - val_sent)
glimpse(avaliacao_automatica)

head(avaliacao_humana)
head(avaliacao_automatica)

# realiza a junção entre dataframes pela coluna id 
resultado <- full_join(avaliacao_automatica, avaliacao_humana, by = "id") %>% 
  filter(!is.na(val_humana))

# renomeia as colunas do dataframe
resultado <- resultado %>% rename(op30 = val_op30, sent = val_sent, humana = val_humana)

# transforma o formato do dataframe para auxiliar a visualização
res_trans <- resultado %>% gather("tipo", "valor", 2:4) %>% arrange(id)

# plot de pontos das avaliações por tipo
ggplot(res_trans, aes(tipo, valor)) +
  geom_point()

# Agrupa a mediana e media de valores por tipo
dff <- res_trans %>% group_by(tipo) %>% 
  summarise(mediana = median(valor),
            media = mean(valor))

# Gráfico das medianas por tipo de avaliação
ggplot(dff, aes(tipo, mediana)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0, 1, 0.05))

# Calcula o erro absoluto para a avaliação humana em relação as avaliações auto.
resultado <- resultado %>% group_by(id) %>% 
  mutate(erro_op30 = op30 - humana,
         erro_sent = sent - humana)

# Outras visualizações sobre o erro que podem ser realizadas
# Estudem boxplot
# Estudem intervalo de confiança



