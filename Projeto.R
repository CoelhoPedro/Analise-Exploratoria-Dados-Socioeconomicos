install.packages("mice")
install.packages("ggthemes")
library(mice)
library(ggplot2)
library(lattice)
library(ggthemes)

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

View(dados_limpos$data)
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
# Uma forte correlação