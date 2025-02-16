library(gamlss)
library(tidymodels)
library(caret)
library(car)

# Leitura dos dados
dados <- read.csv("alb_homes.csv", header = TRUE)

# Análise descritiva dos dados
summary(dados)
nrow(dados) # 3025 linhas
ncol(dados) # 15 colunas

sum(!complete.cases(dados)) # Há 5 observações com dados faltantes

# Verifica as características das observações com dados faltantes
dados[!complete.cases(dados),]
dados$yearbuilt[1389] <- 1992 # (2019 - 27) Com a idade da edificação conseguimos calcular o ano de construção

# Remove as observações com dados faltantes
dados = na.omit(dados)


# Transforma as variáveis qualitativas
dados$cooling    = as.factor(dados$cooling)
dados$esdistrict = as.factor(dados$esdistrict)
dados$msdistrict = as.factor(dados$msdistrict)
dados$hsdistrict = as.factor(dados$hsdistrict)
# dados$condition  = as.factor(dados$condition)
dados$censustract  = as.factor(dados$censustract)

# library(corrplot)
# corrplot(cor(dados, method = "spearman"), method = 'color', order = 'alphabet')

# Verificamos nomenclatura dos níveis e reordenamos das piores para melhores condições
unique(dados$condition)
dados$condition <- factor(dados$condition, levels = c("Substandard","Poor","Fair","Average","Good","Excellent"))
# Verificamos as relações entre níveis de distritos escolares e censitários
table(dados$hsdistrict,dados$msdistrict)
table(dados$esdistrict,dados$msdistrict)
table(dados$censustract,dados$hsdistrict)

# Verificamos correlação perfeita e excluímos uma das variáveis
cor(dados$yearbuilt,dados$age)
dados <- dados[,-1]

# verificamos a assimetria dos valores das casas
hist(dados$totalvalue, breaks="Scott", main="", ylab = "Frequência absoluta", xlab="Valor da residência em dólares", freq = F); box()




# busca pelos melhores modelos para valores reais positivos
mod0_best = fitDist(dados$totalvalue, type = "realplus", try.gamlss = TRUE)

mod0_logno = gamlss(formula = totalvalue ~ 1, sigma.formula = totalvalue ~ 1, data = dados, family = LOGNO(), method = RS(50))
mod0_gamma = gamlss(formula = totalvalue ~ 1, sigma.formula = totalvalue ~ 1, data = dados, family = GA(), method = RS(50))

hist(dados$totalvalue, breaks="Scott", main="", ylab = "Frequência relativa", xlab="Valor da residência em dólares", freq = F); box()

curve(dGA(x, mu = exp(mod0_gamma$mu.coefficients), sigma = exp(mod0_gamma$sigma.coefficients)), n = 10000,
      from = min(dados$totalvalue), to = max(dados$totalvalue), add=T, col = "blue", lwd = 1)
curve(dLOGNO(x, mu = mod0_logno$mu.coefficients, sigma = exp(mod0_logno$sigma.coefficients)), n = 10000,
      from = min(dados$totalvalue), to = max(dados$totalvalue), add=T, col = "red", lwd = 1)
curve(dGB2(x, mu = exp(mod0_best$mu.coefficients), sigma = exp(mod0_best$sigma.coefficients),
           nu = exp(mod0_best$nu.coefficients), tau = exp(mod0_best$tau.coefficients)),
      n = 10000, from = min(dados$totalvalue), to = max(dados$totalvalue), add=T, col = "darkgreen", lwd = 1)

legend("topright", legend=c("Gama", "Log-Normal", "GB2"), title = "Família",
       col=c("blue", "red", "darkgreen"), lty=1, cex=0.8)




# Modelo LogNormal
modelo = gamlss(formula = totalvalue ~ ., sigma.formula = totalvalue ~ ., data = dados, family = LOGNO(), method = RS(50))
summary(modelo)

# Modelo gamma
modelo = gamlss(formula = totalvalue ~ ., sigma.formula = totalvalue ~ ., data = dados, family = GA(), method = RS(50))
summary(modelo)

# GB2
modelo = gamlss(formula = totalvalue ~ ., sigma.formula = totalvalue ~ ., nu.formula = totalvalue ~ .,
                tau.formula = totalvalue ~ ., data = dados, family = GB2(), method = RS(50))
summary(modelo)


###########################################################
par(mfrow = c(2, 2))
# Modelo gamma
modeloGA = gamlss(formula = totalvalue ~ ., sigma.formula = totalvalue ~ ., data = dados, family = GA(), method = RS(50))
summary(modeloGA)
res_padrão <- residuals(modeloGA)
qqPlot(res_padrão, pch = 18, col = "black", main = "Gama")
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "red")


# GB
modeloGB = gamlss(formula = totalvalue ~ .,
                  sigma.formula = totalvalue ~.,
                  nu.formula = totalvalue ~ 1,
                  tau.formula = totalvalue ~ 1,
                  family = GB2(mu.link = "log", sigma.link = "log", nu.link = "log", tau.link = "log"), data = dados, method = RS(50))
summary(modeloGB)
res_padrão <- residuals(modeloGB)
qqPlot(res_padrão, pch = 18, col = "black", main = "Beta Generalizada")
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "red")

plot(modeloGB)


### Log normal

modeloLOGNO = gamlss(formula = totalvalue ~ . - halfbath, 
                     sigma.formula = totalvalue ~ . - cooling - fullbath - halfbath - esdistrict - condition, 
                     data = dados, family = LOGNO(mu.link = "identity", sigma.link = "log"), method = RS(50))
modeloLOGNO = gamlss(formula = totalvalue ~ ., 
                     sigma.formula = totalvalue ~ ., 
                     data = dados, family = LOGNO(mu.link = "identity", sigma.link = "log"), method = RS(50))
summary(modeloLOGNO)

res_padrão <- residuals(modeloLOGNO)
qqPlot(res_padrão, pch = 18, col = "black", main = "Log Normal")
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "red")

plot(modeloLOGNO)


## Gama generalizada

modeloGG = gamlss(formula = totalvalue ~ ., 
                  sigma.formula = totalvalue ~ ., 
                  nu.formula = totalvalue ~ 1, 
                  data = dados, family = GG(), method = RS(50))
summary(modeloGG)

res_padrão <- residuals(modeloGG)
qqPlot(res_padrão, pch = 18, col = "black", main = "Gama Generalizada")
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "red")



dados_sim <- dados
dados_sim <- dados_sim[,-7]

## Gráfico de envelope para m2
# Número de simulações
B <- 100
rq <- resid(modeloGB)
rqo <- sort(rq)
# Simulações
n <- 3021
mrq <- matrix(0, B, n)
for (b in 1:B) {
  dados_sim$ysim <-  predict(modeloGB, type = "response", simulation = TRUE, nsim = 1)
  m2s <- gamlss(formula = ysim ~ . - halfbath - bedroom , 
                sigma.formula = ysim ~ . - cooling - halfbath - fullbath - esdistrict - condition , 
                nu.formula = ysim ~ 1,
                tau.formula = ysim ~ 1, 
                data = dados_sim, 
                family = GB2(mu.link = "log",sigma.link = "log"), 
                method = RS(250))
  rqs <- resid(m2s)
  mrq[b,] <- rqs
}




## Seleção de variáveis com base no valor-p
modelo0 = gamlss(formula = totalvalue ~ .,
                 sigma.formula = totalvalue ~.,
                 nu.formula = totalvalue ~ .,
                 tau.formula = totalvalue ~.,
                 family = GB2(mu.link = "log", sigma.link = "log", nu.link = "log", tau.link = "log"), data = dados, method = RS(50))
summary(modelo0)

modelo1 = gamlss(formula = totalvalue ~ . - cooling - condition,
                sigma.formula = totalvalue ~. - condition,
                nu.formula = totalvalue ~ .,
                tau.formula = totalvalue ~. - halfbath - lotsize - condition,
                family = GB2(mu.link = "log", sigma.link = "log", nu.link = "log", tau.link = "log"), data = dados, method = RS(50))
summary(modelo1)

modelo2= gamlss(formula = totalvalue ~ . - cooling - condition,
                sigma.formula = totalvalue ~. - condition,
                nu.formula = totalvalue ~ . - age,
                tau.formula = totalvalue ~. - halfbath - lotsize - condition - fullbath,
                family = GB2(mu.link = "log", sigma.link = "log", nu.link = "log", tau.link = "log"), data = dados, method = RS(50))
summary(modelo2)

modelo3 = gamlss(formula = totalvalue ~ . - cooling - condition,
                sigma.formula = totalvalue ~. - condition - finsqft,
                nu.formula = totalvalue ~ . - age,
                tau.formula = totalvalue ~. - halfbath - lotsize - condition - fullbath,
                family = GB2(mu.link = "log", sigma.link = "log", nu.link = "log", tau.link = "log"), data = dados, method = RS(50))
summary(modelo3)

## StepGAICALL.B
modelo_step <- stepGAICAll.B(modelo3) # Retira hsdistrict
modelo_step$anova

modelo4 = gamlss(formula = totalvalue ~ . - cooling - condition - hsdistrict,
                 sigma.formula = totalvalue ~. - condition - finsqft - hsdistrict,
                 nu.formula = totalvalue ~ . - age - hsdistrict,
                 tau.formula = totalvalue ~. - halfbath - lotsize - condition - fullbath - hsdistrict,
                 family = GB2(mu.link = "log", sigma.link = "log", nu.link = "log", tau.link = "log"), data = dados, method = RS(50))
summary(modelo4)

## StepGAICALL.A
modelo_stepA <- stepGAICAll.A(modelo3) # Sem modificação, modelo continua o mesmo
modelo_stepA$anova

# Variando as funções de ligação
modelo5 = gamlss(formula = totalvalue ~ . - cooling - condition - hsdistrict,
                 sigma.formula = totalvalue ~. - condition - finsqft - hsdistrict,
                 nu.formula = totalvalue ~ . - age - hsdistrict,
                 tau.formula = totalvalue ~. - halfbath - lotsize - condition - fullbath - hsdistrict,
                 family = GB2(mu.link = "log", sigma.link = "identity", nu.link = "log", tau.link = "log"), data = dados, method = RS(50))
summary(modelo5)

modelo6 = gamlss(formula = totalvalue ~ . - cooling - condition - hsdistrict,
                 sigma.formula = totalvalue ~. - condition - finsqft - hsdistrict,
                 nu.formula = totalvalue ~ . - age - hsdistrict,
                 tau.formula = totalvalue ~. - halfbath - lotsize - condition - fullbath - hsdistrict,
                 family = GB2(mu.link = "log", sigma.link = "log", nu.link = "identity", tau.link = "log"), data = dados, method = RS(50))
summary(modelo6)

modelo7 = gamlss(formula = totalvalue ~ . - cooling - condition - hsdistrict,
                 sigma.formula = totalvalue ~. - condition - finsqft - hsdistrict,
                 nu.formula = totalvalue ~ . - age - hsdistrict,
                 tau.formula = totalvalue ~. - halfbath - lotsize - condition - fullbath - hsdistrict,
                 family = GB2(mu.link = "log", sigma.link = "log", nu.link = "log", tau.link = "identity"), data = dados, method = RS(50))
summary(modelo7)

# Variando a quantidade de parâmetros
modelo8 = gamlss(formula = totalvalue ~ . - cooling - condition - hsdistrict,
                 sigma.formula = totalvalue ~. - condition - finsqft - hsdistrict,
                 nu.formula = totalvalue ~ . - age - hsdistrict,
                 family = GB2(mu.link = "log", sigma.link = "log", nu.link = "log"), data = dados, method = RS(50))
summary(modelo8)


modelo9 = gamlss(formula = totalvalue ~ . - cooling - condition - hsdistrict,
                 sigma.formula = totalvalue ~. - condition - finsqft - hsdistrict,
                 family = GB2(mu.link = "log", sigma.link = "log"), data = dados, method = RS(50))
summary(modelo9)
modelo_stepB <- stepGAICAll.B(modelo9) ## StepGAICALL.B
modelo_stepB$anova
modelo_stepA <- stepGAICAll.A(modelo9) ## StepGAICALL.A
modelo_stepA$anova


modelo10 = gamlss(formula = totalvalue ~ . - cooling - condition - hsdistrict,
                 family = GB2(mu.link = "log"), data = dados, method = RS(50))
summary(modelo10)
modelo_stepB <- stepGAICAll.B(modelo9) ## StepGAICALL.B
modelo_stepB$anova
modelo_stepA <- stepGAICAll.A(modelo9) ## StepGAICALL.A
modelo_stepA$anova


### Iniciando pelo modelo só com mu
modelo00 = gamlss(formula = totalvalue ~ .,
                 sigma.formula = totalvalue ~ 1,
                 nu.formula = totalvalue ~ 1,
                 tau.formula = totalvalue ~ 1,
                 family = GB2(mu.link = "log", sigma.link = "log", nu.link = "log", tau.link = "log"), data = dados, method = RS(50))
summary(modelo00)


modelo01 = gamlss(formula = totalvalue ~ . - bedroom - halfbath,
                  sigma.formula = totalvalue ~ 1,
                  nu.formula = totalvalue ~ 1,
                  tau.formula = totalvalue ~ 1,
                  family = GB2(mu.link = "log", sigma.link = "log", nu.link = "log", tau.link = "log"), data = dados, method = RS(50))
summary(modelo01)

modelo02 = gamlss(formula = totalvalue ~ . - bedroom - halfbath,
                  sigma.formula = totalvalue ~.,
                  nu.formula = totalvalue ~ 1,
                  tau.formula = totalvalue ~ 1,
                  family = GB2(mu.link = "log", sigma.link = "log", nu.link = "log", tau.link = "log"), data = dados, method = RS(50))
summary(modelo02)

modelo03 = gamlss(formula = totalvalue ~ . - bedroom - halfbath,
                  sigma.formula = totalvalue ~. - cooling - fullbath - halfbath - condition,
                  nu.formula = totalvalue ~ 1,
                  tau.formula = totalvalue ~ 1,
                  family = GB2(mu.link = "log", sigma.link = "log", nu.link = "log", tau.link = "log"), data = dados, method = RS(150))
summary(modelo03)

modelo_stepB <- stepGAICAll.B(modelo03) ## StepGAICALL.B
modelo_stepB$anova
modelo_stepA <- stepGAICAll.A(modelo03) ## StepGAICALL.A, remove hsdistrict
modelo_stepA$anova

modelo04 = gamlss(formula = totalvalue ~ . - bedroom - halfbath - hsdistrict,
                  sigma.formula = totalvalue ~. - cooling - fullbath - halfbath - condition - hsdistrict,
                  nu.formula = totalvalue ~ 1,
                  tau.formula = totalvalue ~ 1,
                  family = GB2(mu.link = "log", sigma.link = "log", nu.link = "log", tau.link = "log"), data = dados, method = RS(50))
summary(modelo04)



### QQplot para GB2
par(mfrow = c(2, 2))
# 4 parametros
res_padrão <- residuals(modelo4)
qqPlot(res_padrão, pch = 18, col = "black", main = "GB2")
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "red")

# 2 parametros
res_padrão <- residuals(modelo04)
qqPlot(res_padrão, pch = 18, col = "black", main = "GB2 com nu e tau constantes")
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "red")

# 1 parametro
res_padrão <- residuals(modelo01)
qqPlot(res_padrão, pch = 18, col = "black", main = "GB2 com sigma, nu e tau constantes")
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "red")

# Resumo das medidas dos resíduos
plot(modelo4)
plot(modelo04)
plot(modelo01)