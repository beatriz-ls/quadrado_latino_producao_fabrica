if (!require("ggplot2")) install.packages("ggplot2")
if (!require("lmtest")) install.packages("lmtest")
if (!require("lsmeans")) install.packages("lsmeans")
if (!require("multcomp")) install.packages("multcomp")
if (!require("multcompView")) install.packages("multcompView")

library(readxl)
estudo <- read_excel("producao_fabrica.xlsx")
View(estudo)

estudo$Ordem <- as.factor(estudo$Ordem)
estudo$Operador <- as.factor(estudo$Operador)
estudo$Metodo <- as.factor(estudo$Metodo)
str(estudo)

modelo <- aov(Producao ~ Ordem+Operador+Metodo , data = estudo)

####Pressupostos####
#Residuos e modelo ajustado
plot(modelo) #grafico
shapiro.test(residuals(modelo)) # Teste de normalidade W = 0.91416, p-value = 0.1358

# Teste de homogeneidade
bartlett.test(Producao ~ Metodo, data = estudo) # k-squared = 1.1998, df = 3, p-value = 0.7531
bartlett.test(Producao ~ Operador, data = estudo) # k-squared = 0.94942, df = 3, p-value = 0.8135
bartlett.test(Producao ~ Ordem, data = estudo) #k-squared = 2.5497, df = 3, p-value = 0.4664

#Grafico de interacao:
library(ggplot2)

# Gráfico de interação: Ordem x Método
ggplot(estudo, aes(x = Ordem, y = Producao, color = Metodo, group = Metodo)) +
  geom_line() +
  geom_point() +
  labs(title = "Interação: Ordem x Método",
       x = "Ordem", y = "Produção") +
  theme_minimal()

# Gráfico de interação: Operador x Método
ggplot(estudo, aes(x = Operador, y = Producao, color = Metodo, group = Metodo)) +
  geom_line() +
  geom_point() +
  labs(title = "Grafico de Interação: Operador x Método",
       x = "Operador", y = "Produção") +
  theme_minimal()

#Análise: Se as linhas não forem paralelas, isso sugere que há interação entre os fatores.

####Modelo com e sem interacao####

#Um sem interação (modelo aditivo).
#Outro com interação (modelo completo).
#Depois, compare os modelos usando um teste de significância, como 
#o ANOVA entre modelos ou o ajuste do R^2.

# Modelo aditivo (sem interação)
modelo_aditivo <- aov(Producao ~ Ordem + Operador + Metodo, data = estudo)

# Modelo com interação (com interação entre os fatores)
modelo_interacao <- aov(Producao ~ Ordem + Operador + Metodo + Ordem:Operador + Ordem:Metodo, data = estudo)

# Comparação dos modelos
anova(modelo_aditivo, modelo_interacao)#Se a interação não for significativa (p), você pode assumir que não há interação relevante entre os fatores. 
#Caso contrário, a interação precisa ser incluída no modelo.

# Ajuste o modelo completo com interações
modelo_completo <- aov(Producao ~ Ordem * Operador * Metodo, data = estudo)

# Sumário do modelo
summary(modelo_completo) #Verifique os valores p para os termos de interação, como Ordem:Operador, Ordem:Método, ou Operador:Método. Se os valores p forem maiores que 0,05, a interação pode ser considerada insignificante.

# Resíduos do modelo aditivo
residuos <- residuals(modelo_aditivo)

ggplot(estudo, aes(x = Metodo, y = Producao, colour = Metodo, fill = Metodo)) +
  geom_boxplot(alpha = 0.5) +
  stat_boxplot(geom = "errorbar") +
  geom_point()

ggplot(estudo, aes(x = Operador, y = Producao, colour = Operador, fill = Operador)) +
  geom_boxplot(alpha = 0.5) +
  stat_boxplot(geom = "errorbar") +
  geom_point()

ggplot(estudo, aes(x = Ordem, y = Producao, colour = Ordem, fill = Ordem)) +
  geom_boxplot(alpha = 0.5) +
  stat_boxplot(geom = "errorbar") +
  geom_point()

###Anova para modelo aditivo####

#Teste de Durbin-Watson
library(lmtest)

# Teste de Durbin-Watson para o modelo
dw_test <- dwtest(modelo_aditivo)
print(dw_test)

#estatistica de avaliação: se DW estiver proximo de 2, ausencia de autocorrelaçao, DW < 2 autocorrelaçao positiva ou > 2 negativa e p < 0.05 Rejeita-se a hipótese nula de ausência de autocorrelação.

#### Aplicacao da avaliacao do qaudrado latino ####
tukey <- TukeyHSD(modelo_aditivo, "Metodo")
print(tukey)

# Gráfico do teste de Tukey
plot(tukey)

# O teste de Tukey avalia todas as comparações entre os métodos (A, B, C, D).
# Intervalos de confiança que não incluem zero: Indicam diferenças significativas entre os métodos.