#####Analise Discriminante
##### Calculos auxiliares para fazer sem o pacote


dados <- data.frame(
  Cliente = 1:20,
  Gasto = c(250, 270, 245, 260, 255, 240, 275, 265, 250, 260, 
            520, 530, 510, 540, 525, 515, 535, 550, 530, 545),
  Frequencia = c(3, 2, 3, 4, 3, 2, 4, 3, 2, 3,
                 8, 9, 7, 8, 7, 9, 8, 9, 8, 7),
  Classe = factor(c(rep("Regular", 10), rep("Premium", 10)))
)


dados

####separando os dados
p1<-subset(dados, Classe == "Regular")
p1


p2<-subset(dados, Classe == "Premium")
p2

#### medias de cada classificação
m1<-colMeans(p1[,2:3])
m1

m2<-colMeans(p2[,2:3])
m2

#### Matrizes de variancia e covariancia de cada grupo
s1<-cov(p1[,2:3])
s1

s2<-cov(p2[,2:3])
s2

#####matriz de variancias combinadas
sp<-round(((10-1)*s1+(10-1)*s2)/(10+10-2),2)
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
#### na populacao 1, Regular
#### se  -1.88(x1)-0.33(x2)>=-740.326


## Classificando [400, 5]

-1.88*400-0.33*5>=-740.326
# cliente classificado como Premium


## Segunda questão

mu1<-c(10,12)
mu2<-c(13,10)

sigma<-matrix (c(1, 3,
                 3, 16),
              nrow = 2, byrow = TRUE)

x<-c(9.32, 12.34)

sigma_inv<-solve(sigma)

a <- sigma_inv %*% (mu1 - mu2)


soma_mus <- mu1 + mu2

#regra de classificação
c_term <- -0.5 * (t(soma_mus) %*% a)

# -7.71(x1)+1.57(x2) >= 71.43  ?????

sum(a*x)





