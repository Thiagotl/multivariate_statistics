dados <- data.frame(
  local = c("nascente", "nascente", "nascente", "nascente",
            "rural", "rural", "rural", "rural",
            "urbana", "urbana", "urbana", "urbana"),
  ph = c(4.5, 3.5, 4.5, 3.5, 5.8, 5.5, 5.0, 4.5, 6.5, 7.0, 6.8, 6.0),
  N = c(1.2, 1.3, 1.2, 1.3, 1.2, 1.1, 1.3, 1.2, 2.1, 1.8, 1.5, 1.5),
  Oxig = c(0.5, 0.5, 0.6, 0.7, 0.5, 0.4, 0.5, 0.3, 0.4, 0.5, 0.5, 0.4),
  M_Org = c(5.5, 5.2, 5.3, 5.4, 5.2, 5.4, 5.5, 5.8, 8.2, 8.3, 8.4, 8.5)
)


library(FactoMineR)
library(factoextra)

###fazendo a análise
###o pacote faz utilizando a matriz de correlação
### por enquanto não queremos graficos. Deixaremos graph=F
res.pca<-PCA(dados[,-1], graph=F)


###Obtendo a explicação (lembre-se que esta sendo feito pela matriz de cor)
value<-get_eig(res.pca)
value

####correlação dos CP com as var. originais
res.pca$var$cor

####gráfico screeplot
fviz_eig(res.pca, addlabels = T, ylim=c(0,90))

### Grafico dos CP
fviz_pca_var(res.pca)

### Biplot (CP e Individuos)
fviz_pca_biplot(res.pca,addEllipses = T)

fviz_pca_biplot(res.pca,
                label = "var",             # mostra apenas as variáveis
                habillage = dados$local,   # colorir por local
                addEllipses = TRUE,        # elipses por grupo
                palette = "jco",           # paleta de cores opcional
                repel = TRUE)   


res.pca <- PCA(dados, quali.sup = 1, graph = FALSE)  # primeira coluna = 'local'

# Agora o biplot com elipses por grupo funciona
fviz_pca_biplot(res.pca,
                label = "var",
                habillage = 1,         # '1' porque 'local' é a 1ª coluna
                addEllipses = TRUE,
                palette = "jco",
                repel = TRUE)
