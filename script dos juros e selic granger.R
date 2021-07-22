#Dar nome a base tratada pelo census x13 efeito macro fiscal e divida
base <- read.csv ("D:/Filipe M Dias/Mestrado/Mestrado/Dissertação mestrado/Desenvolvimento dissertação/Base da pesquisa/juroseselic.csv", header= TRUE, sep = ";", dec= ",")
head(base)
#carregando pacote vars
library(vars)
base <- ts(base, start=2002, freq=12)

#salvando em outro formato
write.table(base, "D:/Filipe M Dias/Mestrado/Mestrado/Dissertação mestrado/Desenvolvimento dissertação/Base da pesquisa/juroseselic2.dat")

#Rodando um var para divida
library(vars)
varselect <- VARselect(base,lag.max=20, type=c("const"),season=NULL)
varselect

## Efeito monetário (M) não granger causa efeito fiscal (EF)
library(lmtest)
granger1 <- grangertest(EF ~ M, order=7, data=base)
granger2 <- grangertest(M ~ EF, order=7, data=base)
granger3 <- grangertest(EF ~ M, order=2, data=base)
granger4 <- grangertest(M ~ EF, order=2, data=base)
