library(dplyr)

# Dataframe 1 exemplo
exemplo_1 <- data.frame(exemplo = 1, metrica = letters[1:10], valor = sample(1:100, 10))

# Dataframe 2 exemplo
exemplo_2 <- data.frame(exemplo = 2, metrica = letters[1:10], valor = sample(1:10, 10))

# Dois dataframes com diferentes escalas (1 a 100) e (1 a 10)
# normalizar em escala de 0 a 1 = (valor - min) / (max - min)
exemplo_1_normalizado = exemplo_1 %>% mutate(valor_normalizado = (valor - min(valor)) / (max(valor) - min(valor)))
exemplo_2_normalizado = exemplo_2 %>% mutate(valor_normalizado = (valor - min(valor)) / (max(valor) - min(valor)))

# O maior valor dos dataframes normalizados é igual a 1 e o menor é igual a zero
summary(exemplo_1_normalizado$valor_normalizado)
summary(exemplo_2_normalizado$valor_normalizado)

a <- data.frame(nome = c("ana", "pedro", "jose"), 
                nota = c(10, 3, 7))
a %>% mutate(normalizado = (nota - min(nota))/(max(nota)-min(nota)))
