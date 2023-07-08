
path <- "C:/users/beeat/OneDrive/Área de Trabalho/Estatística com R/Data_Science/Arquivos/googleplaystore.csv"
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

histograma_10breaks <- ggplot(data = tabela_notas) + geom_histogram(mapping = aes(x = nota), na.rm=TRUE, breaks = seq(0,10,1)) 
histograma_10breaks #"breaks" é usado para controlar os pontos de corte em uma curva de densidade.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                         Gráficos de barras
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Para plotar um gráfico de barras, basta usar:

ggplot(data = dados) + geom_bar(mapping = aes(x = Category), stat = "count") + coord_flip() 

# Stat: Na função de barras irá fazer uma contagem, agrupando por categorias
# Coord_flip: Troca os eixos (x <-> y)

###############
# Ordenar os dados no gráfico

# Criaremos um data frame com a frequência
categoryfreq <- data.frame(table(dados$Category))

# Utilizamos a função "reorder" para ordenar de acordo com a tabela criada anteriormente
ggplot(data = categoryfreq) + geom_bar(mapping = aes(x = reorder(Var1,Freq), y=Freq), stat = "identity") + coord_flip()

# reorder(Var1,Freq): Se desejar deixar em ordem crescente, basta colocar -Freq 

###############
# Selecionar os top 10

category.top10 <- categoryfreq[(order(-categoryfreq$Freq)),]
category.top10 <- category.top10[1:10,]

###############
# Sujeiras em nossa base de dados

# Em alguns casos, temos informações irrelevante ou erradas em nossa base de dados. 
# Se o volume for baixo, podemos simplesmente retirá-la.

dados_2 <- dados %>%  dplyr::filter(Category != "1.9")


###############
# Campos vazios

# Quando temos campos vazios, não conseguimos utilizar a função min e max, como mostrado abaixo:
min(dados_2$Rating)
max(dados_2$Rating)

# Para sabermos o volume de campos vazios neste campo da base fazemos:
dados_2 %>% filter(is.na(Rating)) %>% count() # Seleciona todos os dados que tem NA
summary(dados_2$Rating) 

# Para corrigir esta ausência de dados, podemos recorrer a métodos estatísticos ou a origem dos dados
# Neste caso, utilizamos a média para corrigir as informações faltantes

dados_2 %>% filter(is.na(Rating)) %>% group_by(Category) %>%  count() #%>% 
            #summarise(media = mean(Rating)) 

# Calcula a média em cada categoria
mean.category <- dados_2 %>% filter(!is.na(Rating)) %>% group_by(Category) %>%
  summarise(media = mean(Rating)) 

# Vamos criar um laço para substituir as médias de cada categoria na base dados_2
for (i in 1:nrow(dados_2)){
  if(is.na(dados_2[i,"Rating"])){
    dados_2[i,"newRating"] <- mean.category[mean.category$Category == dados_2[i,"Category"],"media"]
  }else{
    dados_2[i,"newRating"] <- dados_2[i,"Rating"]
  }}

# Problema resolvido! Não há campos vazios na coluna newRating
summary(dados_2$newRating)
min(dados_2$newRating)
max(dados_2$newRating)

# A função if_else executa a primeira condição se for verdadeira, caso contrário executa o segundo comando
if_else(1 > 1, "verdade", "falso") 

dados_2 <- dados_2 %>% mutate(rating_class = if_else(newRating < 2, "ruim", 
                                          if_else(newRating > 4, "boa", "regular"))) 

ggplot(dados_2) + geom_bar(mapping = aes(x = rating_class), stat = "count")










