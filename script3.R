#####Analise Discriminante
##### Calculos auxiliares para fazer sem o pacote
dados<-read.table("disc.txt",h=T)
dados

####separando os dados
p1<-subset(dados, Pop == "F")
p1


p2<-subset(dados, Pop == "P")
p2

#### medias de cada classificação
m1<-colMeans(p1[,1:2])
m1

m2<-colMeans(p2[,1:2])
m2

#### Matrizes de variancia e covariancia de cada grupo
s1<-cov(p1[,1:2])
s1

s2<-cov(p2[,1:2])
s2

#####matriz de variancias combinadas
sp<-round(((17-1)*s1+(13-1)*s2)/(17+13-2),2)
sp

####inversa de sp
spinv<-solve(sp)
spinv


##### calculo de a
a<-spinv%*%(m1-m2)
a

#### regra de classifica??o
0.5*(t(a)%*%m1+t(a)%*%m2)

#### Assim classificaremos uma obervacao x
#### na populacao 1, solo de floresta
#### se -0.14(x1)-0.009(x2)>=-3.90

###o 11 esta classificado incorretamente
-0.14*17.9-0.009*191>=-3.90

###o 19 esta classificado incorretamente
-0.14*11.6-0.009*242>=-3.90

###o 20 esta classificado incorretamente
-0.14*2.5-0.009*300.9>=-3.90

###o 23 esta classificado incorretamente
-0.14*6.5-0.009*74.9>=-3.90


###classificando [2    150]
-0.14*2-0.009*150

-0.14*2-0.009*150>=-3.90
###classifica em Floresta