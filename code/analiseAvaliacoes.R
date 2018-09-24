library(dplyr)
library(ggplot2)
library(scales)

dff <- read_csv("data/3-avaliacao-humana/avaliacoes-20180912.csv")
head(dff)

# Sumário das avaliações
summary(dff$avaliacao)

# Percentual de avaliaçãoes por avaliação (1-5)
percentuais <- dff %>% mutate(totall = n()) %>% group_by(avaliacao) %>%
  summarise(total = n()/first(totall))

# Plot de barras com cores para o data frame percentuais
p <- ggplot(percentuais, aes(as.factor(avaliacao), total, fill = as.factor(avaliacao))) + 
  geom_bar(stat = "identity") + 
  scale_fill_hue("Valor da\navaliação") +
  xlab("Avaliação") + ylab("Percentual") + 
  scale_y_continuous(labels = percent, 
                     limits = c(0, 0.5))
p

percentuais <- percentuais %>% 
  mutate(tipo = ifelse(avaliacao > 3, "Muito negativo", ifelse(avaliacao < 3, "Pouco negativo", "Normal")))

ggplot(percentuais, aes(avaliacao, total, fill = tipo)) + 
  geom_bar(stat = "identity") +
  xlab("Avaliçãoes") +
  ylab("Percentual") + 
  scale_y_continuous(labels = percent, limits = c(0, 0.5)) +
  scale_fill_hue("Grau negatividade") + 
  theme_bw(base_size = 10)

  # Qual a divergência entre o mesmo grupo?
dff %>% group_by(grupo, id) %>% 
  summarise(diff = max(avaliacao) - min(avaliacao)) %>% 
  ungroup() %>% group_by(grupo) %>% 
  summarise(max = max(diff), media = mean(diff), min = min(diff))
