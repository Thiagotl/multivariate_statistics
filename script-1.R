##### Testes para 2 vetores de médias
### Teste para amostras emparelhadas
# Primiera Questão

antes<-matrix(c(8.9, 7.5, 7.4, 5.6, 5.0, 6.5, 8.0, 7.5, 4.4, 4.3, 4.8, 4.3, 4.8, 5.1, 5.3, 5.0),ncol=2)
antes

depois<-matrix(c(8.3, 7.0, 8.4, 7.8, 7.7, 6.2, 7.1, 7.8, 7.8, 8.7, 8.7, 8.1, 6.6, 6.0, 6.6, 7.8),ncol=2)
depois

####utilizando as expressões
d<-depois-antes
d
dm<-colMeans(d)
dm
sdmat<-cov(d)
sdmat
n<-16
p<-2
v<-n-1
prob<-0.95
###Estatística do teste
t2<-n*t(dm)%*%solve(sdmat)%*%dm
t2
###Região de rejeição
RR<-((v*p)/(v+1-p))*qf(prob,p,v+1-p)
RR
### como t2<RR não rejeitamos H0


###usando a função já implementada

library(rrcov)
T2.test(d)


# 2 questão





