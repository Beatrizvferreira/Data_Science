
path <- "C:/users/beeat/OneDrive/Área de Trabalho/Estatística com R/Data_Science/googleplaystore.csv"
dados <- read.csv(file = path, stringsAsFactors = FALSE)

library("tidyverse")

#install.packages("dplyr"): Se necessário
#install.packages("ggplot2"): Se necessário

library("dplyr")
library("ggplot2")

View(dados)
glimpse(dados)
str(dados)
head(dados) #6 primerios registros
tail(dados) #6 últimos registros

# App: Nome do aplicativo disponível
# Category: Categoria
# Rating: Avaliação dos usuários
# Review: Quantidade de vistos
# Size: Tamanho 
# Type: Se é pagou ou free
# Price: Preço
# Content.Rating: Categoria da pessoa que avaliou 
# Genres: Gênero específico do app
# Last.Updated: Data da última atualização
# Current.Ver: Versão atual 
# Android.Ver: Versão do android

hist(dados$Rating) # Gerou um histograma
table(dados$Rating) #Gerou uma tabela de frequência na qual observamos que o mínimo é 1
# e o máximo é 19. No entanto, as avaliações vão de 1 a 5, e este ponto destoante é
# um registro incorreto.

#Melhoraremos o gráfico
hist(dados$Rating,xlim=c(1,5))

# --------------------------------------------------

# FUNÇÃO GGPLOT 

# --------------------------------------------------

# Gera um histograma utilizando a função ggplot
rating.Histogram <- ggplot(data = dados) + geom_histogram(mapping = aes(x = Rating), na.rm = TRUE, breaks = seq(1,5)) 
                     + xlim(c(1,5)) 
rating.Histogram


###################################################
# EXERCICIO
###################################################


# 1) Tendo uma base de dados chamada notas contendo as colunas: cod_aluno, materia e nota_aluno, e que
# e ssa base de dados já passou pelo processo de limpeza, ou seja, dados inconsistentes já foram eliminados
# e as notas vão de 0 a 10. Elabore um histograma com  5 bins e outro histograma com 10 breaks de 0 a 10.
# Utilizando o pacote ggplot2.


#Inicialmente vamos gerar a base de dados. 

cod_aluno = c(1:2)
lista_materias = c("Português", "Matemática", "História", "Ciências")

tabela_notas <- data.frame()

for (i in cod_aluno){
  for (materia in lista_materias){
    nova_linha <- data.frame(cod_aluno = i, materia = materia, nota = round(runif(1, min=0,max=10),1))
    tabela_notas <- rbind(tabela_notas, nova_linha)
  
  }
}

tabela_notas

histograma_5bins <- ggplot(data = tabela_notas) + geom_histogram(mapping = aes(x = nota), bins = 5) 
histograma_5bins #  "bins" é usado para controlar o número de intervalos em um histograma

histograma_10breaks <- ggplot(data = tabela_notas) + geom_histogram(mapping = aes(x = nota), na.rm=TRUE, breaks = seq(0,10)) 
histograma_10breaks #"breaks" é usado para controlar os pontos de corte em uma curva de densidade.
