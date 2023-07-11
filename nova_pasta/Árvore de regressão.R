########################
# Instalação de pacotes
pacotes <- c( 
  'tidyverse',    # Pacote básico de datawrangling
  'rpart',        # Biblioteca de árvores
  'rpart.plot',   # Conjunto com Rpart, plota a parvore
  'gtools',       # funções auxiliares como quantcut,
  'Rmisc',        # carrega a função sumarySE para a descritiva
  'scales',       # importa paletas de cores
  'viridis',      # Escalas 'viridis' para o ggplot2
  'caret',        # Funções úteis para machine learning
  'AMR',
  'randomForest',
  'fastDummies',
  'rattle',
  'xgboost',
  'ggpubr'
  
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

########################## Funções de apoio ####################################

descritiva <- function(var){
  # Sumariza a taxa de sobreviventes por categoria da variável em análise
  tgc <- Rmisc::summarySE(tmp, measurevar="survived", groupvars=c(var))
  
  ggplot(tgc) + 
    # Plota o gráfico de barras com as frequências
    geom_bar(aes(x=tgc[,var], weight=N/891, fill=as.factor(tgc[,var]))) + 
    # Plota as barras de erro
    geom_errorbar(aes(x=tgc[,var], y=survived, ymin=survived-se, ymax=survived+se, colour='1'), width=.1) +
    # Plota as médias de cada grupo
    geom_point(aes(x=tgc[,var], y=survived, colour='1', group='1')) +
    # Plota as linhas que conectam as médias
    geom_line(aes(x=tgc[,var], y=survived, colour='1', group='1')) +
    # Escala de cores do gráfico de médias
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    # Escala de cores do gráfico de barras
    scale_fill_viridis_d(direction = -1, begin=.85, end=.95) +
    # Estética mais 'leve' do gráfico
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    # Remove a legenda
    theme(legend.position = "none") +
    # Rótulo dos eixos
    xlab(var) + ylab("Taxa de sobreviventes") + 
    # Marcas do eixo secundário
    scale_y_continuous(sec.axis = sec_axis(~.*891, name = "Frequencia"), labels = scales::percent)
}


descritiva2 <- function(var, resp, df) {
  # Sumariza a taxa de sobreviventes por categoria da variável em análise
  tgc <- Rmisc::summarySE(df, measurevar = resp, groupvars = c(var))
  maxN <- max(tgc$N)
  
  # Gráfico de barras
  p <- ggplot(tgc) +
    geom_bar(aes(x = tgc[,var], 
                 y = max(tgc[,resp])*N/maxN, 
                 fill = as.factor(tgc[,var])), 
             position = "identity", stat = "identity", 
             alpha = 0.5) +
    scale_fill_viridis_d(direction = -1, begin = .85, end = .95)
  
  # Gráfico de linhas
  p <- p +
    geom_line(aes(x = tgc[,var], y = tgc[,resp]), colour = '1', group = '1') +
    geom_point(aes(x = tgc[,var], y = tgc[,resp] ), colour = '1', group = '1') +
    geom_errorbar(aes(x = tgc[,var], 
                      y = tgc[,resp], 
                      ymin = tgc[,resp] + qnorm(.025)*se, 
                      ymax = tgc[,resp] + qnorm(.975)*se, colour = '1'), width = .5) +
    
    #geom_point(aes(x = tgc[,var], y = tgc[,resp] - tgc[,ep]*qnorm(.975)), colour = '1', group = '1') +
    scale_color_viridis_d(direction = -1, begin = 0, end = .25)
  
  # Ajuste dos eixos
  p <- p +
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey"),
          axis.text = element_text(size = 14),  # Tamanho da fonte dos números dos eixos
          axis.title = element_text(size = 16),  # Tamanho da fonte dos títulos dos eixos
          legend.position = "none") +
    xlab(var) + ylab("Barras")
  
  p <- p +
    scale_y_continuous(sec.axis = sec_axis(~ . *maxN/max(tgc[,resp]), name = "Frequencia", labels = scales::number)) +
    ylab(resp) +
    # Limite do eixo vertical esquerdo
    coord_cartesian(ylim = c(min(tgc[,resp]) - 0.02, max(tgc[,resp]) + 0.02))
  
  return(p)
}




################################################################################
################################################################################
################################################################################
################################################################################
################################################################################




set.seed(2360873) # define a semente (seed) do gerador de números aleatórios

# O gerador de números aleatórios é um algoritmo usado para gerar sequências de 
# números aparentemente aleatórios, mas que são determinísticos e repetíveis 
# quando a mesma semente é usada.

x <- seq(0,1, length.out=1000) # Sequência de valores entre 0 e 1

vetor <- c(0,10,-10) # São as entradas de a, b e c, respectivamente, de um equação de segundo grau
# Descrita por: a + b*x + c*(x**2)

y <- vetor[1] + vetor[2]*x + vetor[3]*x^2 + rnorm(length(x), mean=0, sd=.1) # Usando a combinação 
# linear de x e adicionando um ruído, cuja média é zero e o desvio padrão é 0.1.

df <- data.frame(x, y) # Cria uma tabela de dados

#Cria um gráfico de pontos
p0 <- ggplot(df, aes(x,y)) + 
  geom_point(aes(colour='Observado')) +
  scale_color_viridis(discrete=TRUE, begin=0, end=.85, name = "Valor") +
  theme(legend.position="bottom",
        legend.spacing.x = unit(0, 'cm'))
p0


# -----------------------------------------------------

# 1.1 Construíndo a árvore

# -----------------------------------------------------

# Ajustando o modelo de árvore de decisão.
# y é a variável resposta e x a preditora
tree <- rpart(y ~ x,
              data = df,
              control = rpart.control(maxdepth = 2, cp = 0))

# Valor predito
df["p"] = predict(tree, df)
df$p %>% tail # investiga a previsão

df['r'] = df$y - df$p

# Plotando a árvore
plota_arvore <- function(arvore_){
  # paleta
  paleta <- scales::viridis_pal(begin=.75, end=1)(20)
  #definir a figura
  plot <- rpart.plot::rpart.plot(arvore_,
                                 box.palette = paleta) # Paleta de cores
}
plota_arvore(tree)



# ------------------------------------------------------

# 1.2 Calculando indicadores de avaliação

# -----------------------------------------------------


# Cálculo do SQE (Sum of Squares Error)
# -------------------------------------

metricas <- function(df_in, p_var, y_var){
  SSE <- sum((df_in[y_var] - df_in[p_var])^2)
  n <- dim(df_in)[1]
  # MSE <- SSE/n
  
  # Cálculo do SST (Sum of Squares Total)
  # -------------------------------------
  
  SST <- sum((df_in[y_var] - (df_in[y_var] %>% sum)/n)**2)
  # Var <- SST/n
  
  # Cálculo do R^2
  # ---------------
  
  R_squared <- paste0(round(1 - (SSE/SST),2)*100, "%")
  print(R_squared)
  # R^2 = 0: O modelo não fornece nenhuma informação sobre a variável de resposta e provavelmente não está ajustando bem os dados.
  # R^2 = 1: O modelo ajusta perfeitamente os dados e pode ser considerado um ajuste ideal.
  # R^2 /in (0.1): Indica que uma parte da variabilidade da variável resposta é explicada pelas variáveis preditoras.        
}


metricas(df, "p" , "y")

# ------------------------------------------------------

# 1.3. Análise gráfica

# -----------------------------------------------------

# Função para fazer plot x vs y com cor em z
scatterplot_color <- function(data, x_var, y_var, r_var) {
  ggplot(data) +
    geom_point(aes(x = !!sym(x_var), y = !!sym(y_var), color = !!sym(r_var))) +
    theme(legend.position="bottom") +
    ggtitle("Scatterplot") +
    scale_color_viridis_c()
}

scatterplot_color(df, "p", "y", "r")


# Valores esperados e observados
grafico_x_obs_esp <- function(df_in, x_var, y_var, p_var, r_var){
  arvore_vs_E <- ggplot(df_in) + 
    geom_point(alpha=.7, size=.5, aes(!!sym(x_var),!!sym(y_var), color=!!sym(r_var))) +
    scale_color_viridis_c() +
    geom_path(aes(!!sym(x_var),!!sym(p_var))) + #Ploting
    theme_bw() +
    theme(legend.position="bottom") +
    # guides(colour = guide_legend(label.position = "bottom")) +
    labs(title="Valores observados vs esperados")
  arvore_vs_E
}

graf1 <- grafico_x_obs_esp(df, "x", "y", "p", "r")
graf1

# Gráfico de resíduos
graf2 <- scatterplot_color(df, "x", "r", "r")
graf2


# Consolidar 2 gráficos em uma figura
analise_grafica <- function(df_in, x_var, y_var, p_var, r_var){
  # Gráfico 1: x vs y colorido por resíduos + valores esperados
  gr1 <- grafico_x_obs_esp(df_in, x_var, y_var, p_var, r_var)
  # Gráfico2 de x vs resíduos colorido por resíduos
  gr2 <- scatterplot_color(df, x_var, r_var, r_var)
  # gr2
  # painel com os dois gráficos
  ggpubr::ggarrange(gr1, gr2,
                    # labels = c("A", "B"),
                    ncol = 2, nrow = 1)
}
# painel com os dois gráficos
analise_grafica(df, "x", "y", "p", "r")




#######################################
### Parte 2: Tunando a árvore
###
### Passos:
###    1) Treinar uma árvore sem travas
###    2) Observar a complexidade dos caminhos da árvore (cp)
###    3) Escolher o CP que otimiza o erro dessa árvore
###    4) Avaliar a árvore tunada

# -----------------------------------------------------

# 2.1 treinar a árvore sem travas

# -----------------------------------------------------

# Deixaremos a nossa árvore ser feliz. Ela irá crescer o quando puder.
tree_hm <- rpart(y~x, 
                 data=df,
                 xval=10,
                 control = rpart.control(cp = 0, 
                                         minsplit = 2,
                                         maxdepth = 30)
)

df['p_hm'] = predict(tree_hm, df)
df$p %>% tail # investigar a previsão
df['r_hm'] = df$y - df$p_hm

# -----------------------------------------------------

# 2.2 avaliar a árvore hm

# -----------------------------------------------------

metricas(df, "p_hm", "y")
scatterplot_color(df, "p_hm", "y", "r_hm")
analise_grafica(df, "x", "y", "p_hm", "r_hm")

#
#
# NÃO TENTE PLOTAR ESTA ÁRVORE
#
#


# -----------------------------------------------------

# 2.3 Complexidade dos caminhos

# -----------------------------------------------------

tab_cp <- rpart::printcp(tree_hm) # Cria a tabela de complexidade, que mostra a estatística
# relacionada a complexidade do modelo para diferentes valores de custo-complexidade (cp)
rpart::plotcp(tree_hm) #plota o gráfico



# ----------------------------------------------------- 

# 2.4 Escolher o caminho que otimiza a impureza no cross validation

# -----------------------------------------------------

tab_cp[which.min(tab_cp[,'xerror']),] #Pegamos o menor erro

cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']
cp_min

#montamos a árvore tunada agora, com o custo de complexidade com erro mínimo
tree_tune <- rpart(y~x, 
                   data=df,
                   xval=0,
                   control = rpart.control(cp = cp_min, 
                                           maxdepth = 30)
)

# Valores preditos
df['p_tune'] = predict(tree_tune, df)
df$p %>% tail # investigar a previsão
df['r_tune'] = df$y - df$p_tune


# ----------------------------------------------------- 

## 2.5) Avaliar a árvore tunada

# ----------------------------------------------------- 

metricas(df, "p_tune", "y")
scatterplot_color(df, "p_tune", "y", "r_tune")
analise_grafica(df, "x", "y", "p_tune", "r_tune")

