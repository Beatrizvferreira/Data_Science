####################################################
# Tarefa para casa:
####################################################


# carregar a base tips conforme abaixo
# ajustar uma árvore de regressão para classificar
# o valor do percentual da gorjeta conforme as variáveis disponíveis

library("reshape2")
library("tidyverse")
library("rpart")
library("viridis")
library("Rmisc")

data(tips)

tips %>% head
glimpse(tips)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ENTENDENDO OS DADOS   ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Total_bill: Gasto total da mesa
# tip : Total de gorjeta
# Sex: Sexo do garçom/garçonete
# Smoker: Se é uma mesa de fumante
# Dia: Dia da semana
# Time: Período do dia (Almoço ou Jantar)
# Size: Quantidade de pessoas na mesa


# 1) construir o percentual de gorjeta

tips <- tips %>% mutate(percent_tip = (tip / ( total_bill - tip ))) %>%
  mutate(percent_tip = round(percent_tip,2))

tips %>%head

boxplot(tips$percent_tip)

ggplot(tips, aes(x = "", y = percent_tip)) +
    geom_boxplot(fill = viridis_pal(begin=1)(0.15)) +
    labs(title = "Boxplot da variável pct_tip") +
    theme_minimal()

# Retirar o outlier:
tips <- tips %>% filter(percent_tip < 1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Etapa descritiva
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Quais variáveis parecem explicar o problema?

tgc <- Rmisc::summarySE(tips, measurevar = "percent_tip" , groupvars = c("sex"))
maxN <- max(tgc$N)

ggplot(data = tgc) + geom_bar(mapping = aes(x = tgc[,"sex"],
                                            y = max(tgc[,"percent_tip"])*N/maxN,
                                            fill = as.factor(tgc[,"sex"])),
                              position = "identity", stat = "identity",
                              alpha = 0.5) + scale_fill_viridis_d(direction = -1, begin = .85, end = .95) + 
                     geom_errorbar(aes(x = tgc[,"sex"], 
                                       y = tgc[,"percent_tip"], 
                                       ymin = tgc[,"percent_tip"] + qnorm(.025)*se, 
                                       ymax = tgc[,"percent_tip"] + qnorm(.975)*se, colour = '1'), width = .5) +
                      scale_color_viridis_d( direction = -1, begin = 0, end = .25 ) + guides(color = "none") +
                      labs(x = "Sexo", y="Percentual de gorjeta") +
  scale_y_continuous(sec.axis = sec_axis(~ . *maxN/max(tgc[,"percent_tip"]), 
                                         name = "Frequência", 
                                         labels = scales::number)) +  guides(fill = "none") +
  geom_line(aes(x = tgc[,"sex"], y = tgc[,"percent_tip"]), colour = '1', group = '1') +
  geom_point(aes(x = tgc[,"sex"], y = tgc[,"percent_tip"] ), colour = '1', group = '1')

# 2) treinar a árvore (não incluir o valor da gorjeta como explicativa)

dados_treinamento <- tips[, !colnames(tips) %in% c("tip","total_bill")]
head(tips)

tree <- rpart::rpart(percent_tip ~ .,
              data = dados_treinamento,
              xval=10,
              control = rpart.control( cp = 0, minsplit = 2, maxdepth = 30))

plota_arvore <- function(arvore_){
  paleta <- scales::viridis_pal(begin=.75, end=1)(20)
  plot <- rpart.plot::rpart.plot(arvore_,
                                 box.palette = paleta)
}

plota_arvore(tree)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   R^2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

metricas <- function(p_var, y_var){
  SQE <- sum((y_var - p_var)**2)
  
  # Cálculo do SSE (Sum of Squares Total)
  SST <- sum((y_var - mean(y_var))**2)
  
  # Cálculo do R-quadrado
  R_squared <- 1 - SQE/SST
  
  # Imprimindo os resultados
  cat("SQE: ", SQE, "QME : ", SQE/length(y_var), "\n")
  cat("SST: ", SST, "QMT: ", SST/length(y_var), "\n")
  cat("R-quadrado: ", R_squared, "\n")
}

metricas(predict(tree,dados_treinamento), dados_treinamento$percent_tip)

tab_cp <- rpart::printcp(tree)
cp_min <- tab_cp[which.min(tab_cp[,"xerror"]), "CP"]
cp_min

dados_treinamento

set.seed(123)
arvore_tunada <- rpart::rpart(percent_tip ~ .,
                              data=dados_treinamento,
                              xval=20,
                              control = rpart.control(cp = cp_min, 
                                                      minsplit = 2,
                                                      maxdepth = 30)
)
metricas(predict(arvore_tunada, tips), tips$percent_tip)

#Visualizar a árvore tunada:
plota_arvore(arvore_tunada)
# Paleta de cores



