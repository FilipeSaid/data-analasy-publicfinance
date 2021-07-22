require (foreign)
library ()
____________________________________________________________________________________________________________________________________________________________________________
#Para ler os dados
read.csv ("D:/Filipe M Dias/Mestrado/Mestrado/Dissertação mestrado/Desenvolvimento dissertação/Base da pesquisa/base r.csv",  header= TRUE, sep = ";", dec= ",")

#Inserir os dados
attach (read.csv ("D:/Filipe M Dias/Mestrado/Mestrado/Dissertação mestrado/Desenvolvimento dissertação/Base da pesquisa/base r.csv",  header= TRUE, sep = ";",dec= ","))

#Dar nome a base dados não tratados sem divida
base_ori <- read.csv ("D:/Filipe M Dias/Mestrado/Mestrado/Dissertação mestrado/Desenvolvimento dissertação/Base da pesquisa/base r.csv", header= TRUE, sep = ";", dec= ",")

#Dar nome a base tratada pelo census x13 efeito macro fiscal e divida
base <- read.csv ("D:/Filipe M Dias/Mestrado/Mestrado/Dissertação mestrado/Desenvolvimento dissertação/Base da pesquisa/basex13.csv", header= TRUE, sep = ";", dec= ".")

#Dar nome a base  de efeitos macros e fiscais e divida
based <- read.csv ("D:/Filipe M Dias/Mestrado/Mestrado/Dissertação mestrado/Desenvolvimento dissertação/Base da pesquisa/based.csv", header= TRUE, sep = ";", dec= ",")

#Dar nome a base com juros nominais
based <- read.csv ("D:/Filipe M Dias/Mestrado/Mestrado/Dissertação mestrado/Desenvolvimento dissertação/Base da pesquisa/basex13jur.csv", header= TRUE, sep = ";", dec= ".")

__________________________________________________________________________________________________________________________________________________________________________________

*Ver dados básicos da série
summary (base)

*Definir como númerico
as.numeric (base$DBGG)
as.numeric (base$TC)
as.numeric (base$RGC)
as.numeric (base$DGC)
as.numeric (base$PIB)
as.numeric (base$IPCA)

*sem a função attatch tem que usar

lm (PIB ~ TC, data= base)
summary (lm (PIB ~ IPCA, data=base))

*criar uma segunda variável para o IPCA para poder usar log
base$IPCA2 <- ((base$IPCA)/100)+ 1

*função de regressão linear
summary (lm (log (PIB) ~ log (IPCA2) + log (DBGG) + log(TC) + log (RGC) + log (DGC), data=base))

*Tornar em serie temporal
base <- ts(base, start=2002, freq=12)
based <- ts(based, start=2002, freq=12)
base_ori <- ts(base_ori, start=2002, freq=12)

#criar a variável do efeito fiscal na dívida pública/PIB
fiscal <- (exp(base[,2])- exp(base[,6]))/exp(base[,5])
#criar variável do efeito macroeconômico no Div/PIB
var.perc.pib <- base[,5]/lag(base[,5], -1) - 1
macro <- (1 + (base[,3]/100))/(1+(base[,1]/100)*(1+(var.perc.pib/100)))


*plotar gráfico para ver, sazonalidade, tendência, ciclos etc
plot(based, col='black', main='Dados do modelo VAR e SVAR', xlab='Ano')

*plotar a RLGCL
plot(base [,5], col='blue', main='Dados RLGCL', xlab='Ano')

*para plotar serie temporal (PIB)

plot(base[,2], col='blue', main='IPCA de 2002-2017', xlab='Ano', ylab='IPCA')

*para plotar ts (DGC,RGC)

plot(base[,6],base[,2], col='blue', main=' Despesas e receitas do governo', xlab='RGC', ylab='DGC')

*Inserir linha no gráfico

abline (lm (base[,4] ~ base[,5], data=base))
 
*testando estacionariedade (trend) da serie para dois lags

df1 <- summary(ur.df(base[, "PIBR01L"], type = "trend", lags = 2))
df2 <- summary(ur.df(base[, "TCR"], type = "none", lags = 2))
df3 <- summary(ur.df(base[, "IPCA"], type = "drift", lags = 2))
df4 <- summary(ur.df(base[, "DGCL"], type = "drift", lags = 2))
df5 <- summary(ur.df(base[, "RLGCL"], type = "drift", lags = 2))
df6 <- summary(ur.df(base[, "DP"], type = "drift", lags = 2))
df6 <- summary(ur.df(base[, "SELIC"], type = "drift", lags = 2))
*Vendo os resultados para a estacionariedade
df1
df2
df3
df4
df5

*tendência, sazonalidade e erro das variaveis
decomp <- decompose (base[, "IPCA"], type= 'multiplicative')
decomp1 <- decompose (base[, "PIBR01L"], type= 'additive')
decomp2 <- decompose (base[, "TCR"], type= 'additive')
decomp3 <- decompose (base[, "RLGCL"], type= 'additive')
decomp4 <- decompose (base[, "DGCL"], type= 'additive')
decomp5 <- decompose (base[, "SELIC"], type= 'additive')

*plotando o grafico de tendencia, sazonalidade e erro das variaveis
plot (decomp)
plot (decomp1)
plot (decomp2)
plot (decomp3)
plot (decomp4)
plot (decomp5)

*Análise Holt winters (estabelecer uma esquação com indicadores de tendência, sazonalidade e erro das variaveis)
PIBs <- base[,'PIBR01L']
plot (PIBs)
ajuste_com_sazonalidade <- HoltWinters(PIBs)
ajuste_com_sazonalidade
plot (PIBs, xlab= 'tempo', ylab='PIB', main='')
lines (fitted(ajuste_com_sazonalidade)[,1], lwd=1, col= 'red')
legend (2002, 13, c('PIB', ' Ajuste HW'), lwd=c(1,2), col=c('black','red'), bty='n')

*Para o SELIC HW
SELICs <- base[,'SELIC']
ajuste_com_sazonalidade1 <- HoltWinters(SELICs, seasonal= c('multiplicative'))
plot (SELICs, xlab= 'tempo', ylab='PIB', main='')
lines (fitted(ajuste_com_sazonalidade1)[,1], lwd=1, col= 'red')
legend (2005, 2, c('SELIC', ' Ajuste HW'), lwd=c(1,2), col=c('black','red'), bty='n')

*Construir uma previsão para a variavel PIBRA01L para os proximos 10 meses, com IC de 95

library ('forecast')
library ('rlang')
ajuste_prev <- forecast(ajuste_com_sazonalidade, h=10, level= 95)
ajuste_prev
plot (ajuste_prev)

*Construir uma previsão para a variavel SELIC para os proximos 10 meses, com IC de 95
library ('forecast')
library ('rlang')
ajuste_prev1 <- forecast(ajuste_com_sazonalidade1, h=10, level= 95)
ajuste_prev1
plot(ajuste_prev1, main = "", xlab= "Tempo", ylab= "Dados")

#Gráficos com dessazonalizado
#IPCA
plot(base_ori[,2], col='black', main='IPCA variável original e dessazonalizada', xlab='Tempo', ylab='IPCA')
lines(base[,1],col='blue', main='IPCA variável original e dessazonalizada')
legend (2005, 2, c('IPCA_dess', ' IPCA_ori'), lwd=c(1,2), col=c('blue','black'), bty='n')
#PIBR01l
plot(base_ori[,1], col='black', main='IPCA variável original e dessazonalizada', xlab='Tempo', ylab='PIBR01L')
lines(base[,5],col='blue', main='PIBR01L variável original e dessazonalizada')
legend (2002, 13.2, c('PIBR01L_dess', ' PIBR01L_ori'), lwd=c(1,2), col=c('blue','black'), bty='n')
#SELIC
plot(base_ori[,3], col='black', main='SELIC variável original e dessazonalizada', xlab='Tempo', ylab='SELIC')
lines(base[,3],col='blue', main='SELIC variável original e dessazonalizada')
legend (2005, 2, c('PIBR01L_dess', ' PIBR01L_ori'), lwd=c(1,2), col=c('blue','black'), bty='n')
#DGCL
plot(base_ori[,4], col='black', main='DGCL variável original e dessazonalizada', xlab='Tempo', ylab='SELIC')
lines(base[,2],col='blue', main='DGCL variável original e dessazonalizada')
legend (2005, 12, c('DGCL_dess', ' DGCL_ori'), lwd=c(1,2), col=c('blue','black'), bty='n')
#RLGCL
plot(base_ori[,5], col='black', main='RLGCL variável original e dessazonalizada', xlab='Tempo', ylab='RLGCL')
lines(base[,6],col='blue', main='RLGCL variável original e dessazonalizada')
legend (2002, 11.5, c('RLGCL_dess', ' RLGCL_ori'), lwd=c(1,2), col=c('blue','black'), bty='n')
#TCR
plot(base_ori[,6], col='black', main='TCR variável original e dessazonalizada', xlab='Tempo', ylab='TCR')
lines(base[,4],col='blue', main='TCR variável original e dessazonalizada')
legend (2002, 17, c('TCR_dess', ' TCR_ori'), lwd=c(1,2), col=c('blue','black'), bty='n')
___________________________________________________________________________________________________________________________________________________
##carregar pacote para análise de var
library ("vars")

##Para testar a defasagem a ser utilizada pelo VAR
VARselect(base, lag.max = 12, type = "both")
 * O programa sugere utilizar defasagem de 2 (lag) para todos os 4 (AIC, BIC, HQ, FPE) testes realizados
 *Segundo a literatura a melhor defasagem a ser testada para variáveis mensais é de 12
 *Utilizar IPCA, no lugar do IPCAR é melhor para o caso do número de lags

##Estimando o Var de todas
p1ct <- VAR(base, p = 2, type = "both")
p2ct <- VAR(based, p = 1, type = "both")
p1ct1 <- SVAR(p2ct, p = 1, Amat= Amat, Bmat= Bmat)

##Função impulso resposta para o VAR
abc <- irf(p1ct)
plot (abc)

##Estimando de cada uma isolada o VAR
summary(p1ct, equation = "PIBR01L")

#Vendo a estabilidade do modelo
rp1ct <- roots(p1ct, modulus= TRUE)
plot(rp1ct)

#Projetando uma lm para dívida
lmmodel <- lm(base ~ lag(base, 1:12) + lag(based, 1:12))
summary(lmmodel)

#colocando as NAs e criando a matriz Amat para o modelo Svar de 7 variaveis
Amat <- diag(7)
Amat[2,1] <- NA
Amat[3,1] <- NA
Amat[4,1] <- NA
Amat[5,1] <- NA
Amat[6,1] <- NA
Amat[7,1] <- NA
Amat[3,2] <- NA
Amat[4,2] <- NA
Amat[5,2] <- NA
Amat[6,2] <- NA
Amat[7,2] <- NA
Amat[4,3] <- NA
Amat[5,3] <- NA
Amat[6,3] <- NA
Amat[7,3] <- NA
Amat[5,4] <- NA
Amat[6,4] <- NA
Amat[7,4] <- NA
Amat[6,5] <- NA
Amat[7,5] <- NA
Amat[7,6] <- NA

#colocando as NAs e criando a matriz Bmat para o modelo Svar de 7 variaveis
Bmat <- diag(7)
Bmat[1,1] <- NA
Bmat[2,2] <- NA
Bmat[3,3] <- NA
Bmat[4,4] <- NA
Bmat[5,5] <- NA
Bmat[6,6] <- NA
Bmat[7,7] <- NA


#estimando o SVAR
svarmodel <- SVAR(p1ct, Amat= Amat,Bmat= NULL, estmethod= "direct")
summary(svarmodel)

#modelo de impulso resposta para o SVAR
abd <- irf(svarmodel)
plot (abd)

_________________________________________________________________________________________________________________________________________________
##Teste de Granger para dívida pública, efeito monetário e fiscal

##carregando a base
divida <- read.csv ("D:/Filipe M Dias/Mestrado/Mestrado/Dissertação mestrado/Desenvolvimento dissertação/Base da pesquisa/divida ef e m.csv", header= TRUE, sep = ";", dec= ",")

#carregando pacote vars
library(vars)
divida <- ts(divida, start=2002, freq=12)

#Rodando um var para divida
varselect <- VARselect(divida,lag.max=20, type=c("const"),season=NULL)

## Efeito monetário (M) não granger causa efeito fiscal (EF)
 
granger1 <- grangertest(EF ~ M, order=13, data=divida)

##Quando a hipótese nula é que o Efeito monetário (M) não granger causa o efeito fiscal (EF).

## efeito fiscal (EF) não granger causa Efeito monetário (M)
 
granger2 <- grangertest(M ~ EF, order=13, data=divida)

*regeita as duas hipóteses de que efeito macroeconômico causa fiscal e vice versa

__________________________________________________________________________________________________________________________________
###Para uma economia fechada sem o governo
#colocando as NAs base[,1:5]
Amat <- diag(5)
Amat[3,1] <- NA
Amat[3,2] <- NA
Amat[4,1] <- NA
Amat[4,2] <- NA
Amat[4,3] <- NA
Amat[5,1] <- NA
Amat[5,2] <- NA
Amat[5,3] <- NA
Amat[5,4] <- NA
Amat[1,3] <- NA
Amat[2,3] <- NA
Amat[1,4] <- NA
Amat[2,4] <- NA
Amat[1,5] <- NA
Amat[2,5] <- NA

###rodando o modelo
##Adequando a base
base <- base[,1:5]
##rodando o VAR e o SVAR
p1ct <- VAR(base, p = 2, type = "both")
svarmodel <- SVAR(p1ct, Amat= Amat,Bmat= NULL, estmethod= "direct")
summary(svarmodel)
##Vendo o resultado no Impulso resposta
shockdespesa <- irf(p1ct, impulse = "DGCL", n.ahead = 24)
plot(shockdespesa)
shockdespesa <- irf(p1ct, impulse = "DGCL", n.ahead = 24)
plot(shockdespesa)

##Vendo impulso resposta para PIB e despesa
shockpibdesp <- irf(p1ct, impulse = "PIBR01L", response= "DGCL", n.ahead = 24)
plot(shockpibdesp)

____________________________________________________________________________________________________________________________________________________________
#criando dois periodos de tempo para a base
based2 <- (ts(based, start= c(2014,1), end= c(2017, 12), freq= 12))
based1 <- (ts(based, start= c(2002,1), end= c(2013, 12), freq= 12))

#testando a defasagem
VARselect(based1, lag.max = 12, type = "both")
VARselect(based2, lag.max = 12, type = "both")

#rodando um VAR
first <- VAR(based1, p = 1, type = "both")
second <- VAR(based1, p = 5, type = "both")

#vendo os resultados do VAR
summary(first)
summary(second)

#rodando um SVAR
firstm <- SVAR(first, Amat= Amat, Bmat= Bmat)
secondm <- SVAR(second, Amat= Amat, Bmat= Bmat)

#vendo os resultados do SVAR
firstm
secondm
________________________________________________________________________________________________________________________________
#Testando Granger 2 a 2
library(vars)
varselect <- VARselect(based,lag.max=12, type=c("const"),season=NULL)
varselect

##Dividindo a base em dois a dois
base12 <- based[,1:2]
base13 <- based[,c(1,3)]
base14 <- based[,c(1,4)]
base15 <- based[,c(1,5)]
base16 <- based[,c(1,6)]
base17 <- based[,c(1,7)]

base23 <- based[,2:3]
base24 <- based[,c(2,4)]
base25 <- based[,c(2,5)]
base26 <- based[,c(2,6)]
base27 <- based[,c(1,7)]

base34 <- based[,3:4]
base35 <- based[,c(3,5)]
base36 <- based[,c(3,6)]
base37 <- based[,c(3,7)]

base45 <- based[,4:5]
base46 <- based[,c(4,6)]
base47 <- based[,c(4,7)]

base56 <- based[,5:6]
base57 <- based[,c(5,7)]

base67 <- based[,6:7]

## Testanto número de defasagens
varselect <- VARselect(based,lag.max=12, type=c("const"),season=NULL)
varselect12 <- VARselect(base12, lag.max=12, type=c("const"),season=NULL)
varselect13 <- VARselect(base13, lag.max=12, type=c("const"),season=NULL)
varselect14 <- VARselect(base14, lag.max=12, type=c("const"),season=NULL)
varselect15 <- VARselect(base15, lag.max=12, type=c("const"),season=NULL)
varselect16 <- VARselect(base16, lag.max=12, type=c("const"),season=NULL)
varselect17 <- VARselect(base17, lag.max=12, type=c("const"),season=NULL)

varselect23 <- VARselect(base23, lag.max=12, type=c("const"),season=NULL)
varselect24 <- VARselect(base24, lag.max=12, type=c("const"),season=NULL)
varselect25 <- VARselect(base25, lag.max=12, type=c("const"),season=NULL)
varselect26 <- VARselect(base26, lag.max=12, type=c("const"),season=NULL)
varselect27 <- VARselect(base27, lag.max=12, type=c("const"),season=NULL)

varselect34 <- VARselect(base34, lag.max=12, type=c("const"),season=NULL)
varselect35 <- VARselect(base35, lag.max=12, type=c("const"),season=NULL)
varselect36 <- VARselect(base36, lag.max=12, type=c("const"),season=NULL)
varselect37 <- VARselect(base37, lag.max=12, type=c("const"),season=NULL)

varselect45 <- VARselect(base45, lag.max=12, type=c("const"),season=NULL)
varselect46 <- VARselect(base46, lag.max=12, type=c("const"),season=NULL)
varselect47 <- VARselect(base47, lag.max=12, type=c("const"),season=NULL)

varselect56 <- VARselect(base56, lag.max=12, type=c("const"),season=NULL)
varselect57 <- VARselect(base57, lag.max=12, type=c("const"),season=NULL)

varselect67 <- VARselect(base67, lag.max=12, type=c("const"),season=NULL)

## Teste Granger 2 a 2 teste
library(lmtest)
granger12 <- grangertest(based[,1] ~ based[,2], order=9, data=based)
granger13 <- grangertest(based[,1] ~ based[,3], order=5, data=based)
granger14 <- grangertest(based[,1] ~ based[,4], order=5, data=based)
granger15 <- grangertest(based[,1] ~ based[,5], order=5, data=based)
granger16 <- grangertest(based[,1] ~ based[,6], order=7, data=based)
granger17 <- grangertest(based[,1] ~ based[,7], order=5, data=based)
granger21 <- grangertest(based[,2] ~ based[,1], order=9, data=based)
granger31 <- grangertest(based[,3] ~ based[,1], order=5, data=based)
granger41 <- grangertest(based[,4] ~ based[,1], order=5, data=based)
granger51 <- grangertest(based[,5] ~ based[,1], order=5, data=based)
granger61 <- grangertest(based[,6] ~ based[,1], order=7, data=based)
granger71 <- grangertest(based[,7] ~ based[,1], order=5, data=based)

granger23 <- grangertest(based[,2] ~ based[,3], order=6, data=based)
granger24 <- grangertest(based[,2] ~ based[,4], order=11, data=based)
granger25 <- grangertest(based[,2] ~ based[,5], order=6, data=based)
granger26 <- grangertest(based[,2] ~ based[,6], order=6, data=based)
granger27 <- grangertest(based[,2] ~ based[,7], order=5, data=based)
granger32 <- grangertest(based[,3] ~ based[,2], order=6, data=based)
granger42 <- grangertest(based[,4] ~ based[,2], order=11, data=based)
granger52 <- grangertest(based[,5] ~ based[,2], order=6, data=based)
granger62 <- grangertest(based[,6] ~ based[,2], order=6, data=based)
granger172 <- grangertest(based[,7] ~ based[,2], order=5, data=based)

granger34 <- grangertest(based[,3] ~ based[,4], order=10, data=based)
granger35 <- grangertest(based[,3] ~ based[,5], order=10, data=based)
granger36 <- grangertest(based[,3] ~ based[,6], order=4, data=based)
granger37 <- grangertest(based[,3] ~ based[,7], order=10, data=based)
granger43 <- grangertest(based[,4] ~ based[,3], order=10, data=based)
granger53 <- grangertest(based[,5] ~ based[,3], order=10, data=based)
granger63 <- grangertest(based[,6] ~ based[,3], order=4, data=based)
granger73 <- grangertest(based[,7] ~ based[,3], order=10, data=based)

granger45 <- grangertest(based[,4] ~ based[,5], order=8, data=based)
granger46 <- grangertest(based[,4] ~ based[,6], order=1, data=based)
granger47 <- grangertest(based[,4] ~ based[,7], order=7, data=based)
granger54 <- grangertest(based[,5] ~ based[,4], order=8, data=based)
granger64 <- grangertest(based[,6] ~ based[,4], order=1, data=based)
granger74 <- grangertest(based[,7] ~ based[,4], order=7, data=based)

granger56 <- grangertest(based[,5] ~ based[,6], order=2, data=based)
granger57 <- grangertest(based[,5] ~ based[,7], order=8, data=based)
granger65 <- grangertest(based[,6] ~ based[,5], order=2, data=based)
granger75 <- grangertest(based[,7] ~ based[,5], order=8, data=based)

granger67 <- grangertest(based[,6] ~ based[,7], order=2, data=based)
granger76 <- grangertest(based[,7] ~ based[,6], order=2, data=based)
