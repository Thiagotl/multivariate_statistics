dados4 <- data.frame(Ca = c(2.3,2.5,1.8,3.4,1.8,3.7,1.4,1.5,2.8,1.4,1.8,1.9,2.8), 
                     Mg = c(1.7,2.5,2.1,2.5,1.1,1.4,0.7,0.6,2.2,0.8,0.6,1.7,0.8), 
                     SB = c(4.1,5.1,4.1,6.1,3.0,5.2,2.2,2.2,5.1,2.3,2.5,3.7,3.7),  
                     t=c(7.5,7.4,6.4,8.4,6.5,8.5,4.7,4.0,9.0,3.7,7.2,6.0,5.9))
library(MVar.pt)


####### Colocando type = 1 faz pela matriz de var e cov
####### Colocando type = 2 faz pela matriz de cor

Resp1 <- CCA(dados4[1:2],dados4[3:4] , type = 1, test = "Bartlett", sign = 0.05)
Resp1

dados2<-read.table("vendedores.txt",h=T)
dados2



Resp1 <- CCA(dados2[1:3],dados2[4:7] , type = 2, test = "Bartlett", sign = 0.05)


print("Matriz com autovalores (variancias) dos pares cononicos U e V:");
round(Resp1$var.UV,3)

print("Matriz de Correlacao dos pares cononicos U e V:"); round(Resp1$corr.UV,3)
### O primeiro par tem 0.99 de corelação entre as variaveis canonicas
### Como estratégia para contratação de novos vendedores, a empresa pode submeter
## cada candidato aos 4 testes psicologicos e, com base nas notas obtidas, calcular seu escore
## na variável V1. A classificação seria então de acordo com esses escores. Como a correlação
## com U1 é alta e positiva, candidatos com maiores escores seriam mais aptos ao emprego.

print("Matriz dos Coeficientes canonicos do grupo X:"); round(Resp1$coef.X,3)

print("Matriz dos Coeficientes canonicos do grupo Y:"); round(Resp1$coef.Y,3)

print("Matriz das Correlacoes entre as variaveis canonicas
      e as variaveis originais do grupo X:"); round(Resp1$corr.X,3)

print("Matriz das Correlacoes entre as variaveis canonicas
      e as variaveis originais do grupo Y:"); round(Resp1$corr.Y,3)

print("Matriz com os scores do grupo X:"); round(Resp1$score.X,3)

print("Matriz com os scores do grupo Y:"); round(Resp1$score.Y,3)

print("Testes de significancia dos pares canonicos:"); Resp1$sigtest



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











### TEstaremos todas as observacoes do exercicio
### para verificar se a regra classifica corretamente

library(MVar.pt)
dados
data  = dados[,1:2] # dados a serem classificados
data
class = dados[,3]   # classe dos dados
class

prior = c(1,1)/2 # probabilidade a priori das classes
prior

Res <- DA(data, class, type = "lda", validation = "learning", 
          method = "mle", prior = prior, testing = NA)
Res
print("Tabela de confusao:"); Res$confusion
####Classificou um de F em P
####CLassificou 3 de P em F

print("Proporcao global de acerto:"); 1 - Res$error.rate
print("Probabilidade das classes:"); Res$prior


print("Nomes das classes:"); Res$class.names
print("Numero de classes:"); Res$num.class

print("Numero de observacoes corretas:"); Res$num.correct
print("Matriz com os resultados da classificacao:"); Res$results



#####outra função que permite testar um vetor
library(MASS)
lin<-lda(dados[,1:2],method="mle", dados$Pop,  prior = c(1,1)/2)
testar<-matrix(c(2,150))
testar
predict(lin,t(testar))$class



#### Exemplo com  Mais que duas populações
data(iris) # conjunto de dados
iris
data  = iris[,1:4] # dados a serem classificados
class = iris[,5]   # classe dos dados
prior = c(1,1,1)/3 # probabilidade a priori das classes
prior
Res <- DA(data, class, type = "lda", validation = "learning", 
          method = "mle", prior = prior, testing = NA)

print("Tabela de confusao:"); Res$confusion
print("Proporcao global de acerto:"); 1 - Res$error.rate
print("Probabilidade das classes:"); Res$prior
print("Nomes das classes:"); Res$class.names
print("Numero de classes:"); Res$num.class
print("Numero de observacoes corretas:"); Res$num.correct
print("Matriz com os resultados da classificacao:"); Res$results


######Analise de Correspondencia
##Analise de dados quali
data(DataFreq) # conjunto de dados de frequencia


data <- DataFreq[,2:ncol(DataFreq)]
data
rownames(data) <- as.character(t(DataFreq[1:nrow(DataFreq),1]))

####se os dados já estão em forma de frequencia, usamos "f"
Resp <- CA(data = data, "f") # realiza CA

print("Existe dependencia entre as linhas e as colunas?"); Resp$depdata

### O numero de coordenadas principais
### min{(nlinhas-1);(ncolunas-1)}
print("Numero de coordenadas principais:"); Resp$numcood

print("Coordenadas principais das Linhas:"); round(Resp$mtxX,2)

print("Coordenadas principais das Colunas:"); round(Resp$mtxY,2)

print("Inercias das componentes principais:"); round(Resp$mtxAutvlr,2)

###Graficos
Tit = c("Scree-plot","Observacoes", "Variaveis", "Observacoes/Variaveis")

Plot.CA(Resp, titles = Tit, xlabel = NA, ylabel = NA,
        color = TRUE, linlab = rownames(data), casc = FALSE)


####dados qualitativos
data(DataQuali) # conjunto de dados qualitativos

data2 <- DataQuali[,2:ncol(DataQuali)]
data2
rownames(data2) <- as.character(t(DataQuali[1:nrow(DataQuali),1]))

res <- CA(data2, "c") # realiza CA

titulo <- c("","","Grafico das Variaveis")




Plot.CA(res,  titles=titulo, 
        color = T, casc = F)

