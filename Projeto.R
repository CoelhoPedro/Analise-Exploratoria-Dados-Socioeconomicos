install.packages("mice")
install.packages("ggthemes")
library(mice)
library(ggplot2)
library(lattice)
library(ggthemes)
library(dplyr)

dados = read.csv("dataset.csv")

View(dados)

# Renomeando as variáveis
names(dados) = c("Country", "Year", "LifeLadder", "PIBPerCapta", "SocialSupport", "HealthyLifeExpectancy", "Freedom", "Generosity", "CorruptionPerceptions", "PositiveAffects", "NegativeAffects")

str(dados)
summary(dados)

unique(dados$Country)

# utilizando o pacote mice para fazer a imputação de valores NA
md.pattern(dados[,c("PIBPerCapta", "SocialSupport", "HealthyLifeExpectancy", "Freedom", "Generosity", "CorruptionPerceptions", "PositiveAffects", "NegativeAffects")])
dados_limpos = mice(dados, m=5, maxit=50)

summary(dados_limpos)
dados_limpos$imp$PIBPerCapta
dados_limpos = complete(dados_limpos, 3)

View(dados_limpos)
summary(dados_limpos)
str(dados_limpos)

# Fazendo o boxplot para as variáveis numéricas
col_names_numeric = c("Year", "LifeLadder", "PIBPerCapta", "SocialSupport", "HealthyLifeExpectancy", "Freedom", "Generosity", "CorruptionPerceptions", "PositiveAffects", "NegativeAffects") 

lapply(col_names_numeric, function(x){
  ggplot(dados_limpos, aes_string(x)) +
    geom_boxplot() +
    ggtitle(paste("Boxplot de ",x))
})

# Verificando a correlação entre as variáveis
metodos <- c("pearson", "spearman")

cors <- lapply(metodos, function(method) 
  (cor(dados_limpos[, col_names_numeric], method = method)))

plot.cors <- function(x, labs){
  diag(x) <- 0.0 
  plot( levelplot(x, 
                  main = paste("Plot de Correlação usando Método", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}

# Mapa de Correlação
Map(plot.cors, cors, metodos)

# Pelo mapa de correlação podemos ver que o PIB per capta possui uma forte
# relação com a Expectativa de vida, o que nos dá uma intuição para a pergunta 1

ggplot(dados_limpos, aes(PIBPerCapta, HealthyLifeExpectancy)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "PIB per Capta X Expectativa de Vida") +
  theme_pander()

?cor
cor(dados_limpos$PIBPerCapta, dados_limpos$HealthyLifeExpectancy, method = "pearson")
cor(dados_limpos$PIBPerCapta, dados_limpos$HealthyLifeExpectancy, method = "spearman")

# O resultado da correlação é bem alto, portanto as variáveis possuem
# Uma forte correlação positiva

# Para a pergunta número 2, vamos conferir a relação entre a escada da vida e
# a conscientização do público sobre a corrupção.
# O mapa de correlação indica que possivelmente temos uma correlação negativa,
# Vamos conferir a fundo

ggplot(dados_limpos, aes(LifeLadder, CorruptionPerceptions)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Escada da Vida X Percepção Sobre a Corrupção") +
  theme_pander()

cor(dados_limpos$LifeLadder, dados_limpos$CorruptionPerceptions, method = "pearson")
cor(dados_limpos$LifeLadder, dados_limpos$CorruptionPerceptions, method = "spearman")

# Vemos que a escada da vida e a conscientização do público sobre a corrupção
# possuem uma fraca correlação negativa

# Para a pergunta 3, vamos checar a relação entre a escada de vida e a média
# de felicidade. O mapa de correlação indica algum nível de correlação
# positiva. Vamos conferir

ggplot(dados_limpos, aes(LifeLadder, PositiveAffects)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Escada da Vida X Felicidade") +
  theme_pander()

cor(dados_limpos$LifeLadder, dados_limpos$PositiveAffects, method = "pearson")
cor(dados_limpos$LifeLadder, dados_limpos$PositiveAffects, method = "spearman")

# Vemos que a escada da vida e a felicidade média geral possuem uma
# correlação positiva moderada

# A pergunta 4 questiona se o país com o menor índice de suporte social
# tem maior percepção de corrupção.

dados_limpos_tibble = as_tibble(dados_limpos)

suporte_por_pais = dados_limpos_tibble %>% select(Country, SocialSupport) %>%
  group_by(Country) %>%
  summarise(media_suporte_social = mean(SocialSupport)) %>%
  arrange(media_suporte_social)

?arrange

View(suporte_por_pais)

ggplot(suporte_por_pais[1:5,], aes(x = as.factor(Country), y = media_suporte_social),
       colour = as.factor(Country)) + geom_col() +
  labs(title = "5 Países com os menores índices de suporte racial") +
  theme_pander()

# O país com o menor índice de suporte racial é a República da África Central

corrupcao_por_pais = dados_limpos_tibble %>% select(Country, CorruptionPerceptions) %>%
  group_by(Country) %>%
  summarise(media_percepcao_corrupcao = mean(CorruptionPerceptions)) %>%
  arrange(desc(media_percepcao_corrupcao))

View(corrupcao_por_pais)

print(corrupcao_por_pais, n = 55)

# A República da África Central não é o país com a maior percepção de corrupção.
# Na verdade, de 166 países, ele esta na posição número 54. Isso pode ser
# averiguado vendo a correlação entre o índice de suporte racial e a
# percepção de corrupção.

ggplot(dados_limpos, aes(SocialSupport, CorruptionPerceptions)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Suporte Social X Percepção de Corrupção") +
  theme_pander()

cor(dados_limpos$SocialSupport, dados_limpos$CorruptionPerceptions, method = "pearson")
cor(dados_limpos$SocialSupport, dados_limpos$CorruptionPerceptions, method = "spearman")

# Conforme esperávamos, o suporte social e a percepção de corrupção possuem uma
# fraca correlação negativa, o que explica a situação da República da África Central
# em relação a essas duas variáveis

# A pergunta 5 nos indaga se pessoas generosas são mais felizes.
# O mapa de correlações indica uma correlação fraca.

ggplot(dados_limpos, aes(Generosity, PositiveAffects)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Generosidade X Felicidade") +
  theme_pander()

cor(dados_limpos$Generosity, dados_limpos$PositiveAffects, method = "pearson")
cor(dados_limpos$Generosity, dados_limpos$PositiveAffects, method = "spearman")

# A generosidade possui uma correlação positiva fraca com a felicidade