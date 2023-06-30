# Definindo o projeto do curso
# ----------------------------------------------
# Pergunta: O que afeta a qualidade do ar? Como?
# ----------------------------------------------
# install.packages("Ecdat") # se necessário

library("Ecdat")

data(Airq) #carregando o banco de dados do pacote
names(Airq) #exibe os nomes das variáveis

#Descrevendo as variáveis":
# airq: índice de qualidade do ar (quanto menor, melhor)
# vala: valor das empresas nas cidades (milhares de dólares)
# rain: quatidade de chuva (em polegadas)
# coas: posição costeiras da cidade (sim ou não)
# dens: densidade populacional (milhas quadrada)
# medi: renda média per capita (dólares)

# Análise descritiva ou exploratória
# ----------------------------------------------

summary(Airq) #sumário das variáveis

# As variáveis podem ser contínuas ou categóricas (divididas em categorias)
# A variável resposta é a qualidade do air (airq)

plot(airq~vala, data=Airq)

########################### Criando um modelo estatístico ########################### 
# y (resposta) ~ x (explicativa) 
# y (crescimento da planta) ~ x (quantidade de adubo)
# y = x1 + x2 + x3 
# airq ~ vala + coas + rain

# ---------------------------------------------
### termos: 

# anova (variável contínua ~ de uma variável categórica)
# regressão (variável contínua ~ variável contínua)
# regressão múltipla (variável contínua ~ variáveis contínuas ou não)
# ---------------------------------------------

#Montando o modelo  - Regressão linear (duas varíaveis contínuas)
ml <- lm(airq~vala, data=Airq) # lm (modelo linear)
summary(ml) # alguns dados podem não ser lineares

####################################################################

# p-valor indica a significancia do modelo ou da variável
# se p-valor for menor (<) 0.05 a variável é significativa
# se p-valor for maior que (>=) 0.05 não existe o efeito esperado

####################################################################

# A variável "vala" não influenciou a qualidade do ar nas cidades ("airq")
# A variável é significativa ou não, não tem pouco ou muita significância

plot(airq~coas, data=Airq)
m2 <- lm(airq~coas, data=Airq)
summary(m2)

# Sim! a variável costeira da cidade influência a qualidade do ar das cidades
# As cidades costeiras apresentam uma melhor qualidade do ar

m3 <- lm(airq~medi, data=Airq)
summary(m3)
plot(airq~medi, data=Airq)

# A variável renda média não afetou a qualidade do ar

m4 <- lm(airq~rain, data=Airq)
summary(m4)

# A variável quantidade de chuva não afetou a qualidade do ar

m5 <- lm(airq~dens,data=Airq)
summary(m5)

# Não existe um efeito da densidade populacional da qualidade do ar

####################################################################

# As retas de modelos não significativos são opcionais nos gráficos

####################################################################

#Plotando retas

plot(airq~medi, data=Airq)

# A curva da reta é descrita por: y = a+b*x
# a <- intercepto, onde a reta vai tocar na eixo y
# b <- é a inclinação da reta

curve(9.936e+01+5.638e-04*x,add=TRUE) #add é para plotar no mesmo gráfico

#Melhorando o gráfico

plot(airq~medi, data=Airq,xlab="Renda média per capita", 
     ylab = "Qualidade do ar",pch=1, col="blue",cex=1.5, main= "Renda média = 2010")
curve(9.936e+01+5.638e-04*x,add=TRUE,col="darkblue",lwd=1,lty=2)

#lwd - largura da linha
#lty - tipo da linha
#cex - tamanho dos marcadores
#pch - tipo de marcador

plot(airq~coas, data=Airq, xlab="Cidade costeira", ylab="Qualidade do ar", col="lightblue",
     ylim=c(50,170), cex.lab=1.3, main="Análise da qualidade do ar")

# ylim - define os valores dos eixo
# cex.lab - tamanho dos títulos dos eixos

####################################################################

#Exercícios

####################################################################

# 1. Repita tudo o que foi feito em aula para as variáveos airq e dens

m5 <- lm(airq~dens, data=Airq)
summary(m5)
plot(airq~dens, data=Airq, xlab = "Densidade populacional (milhas quadrada)", ylab = "Qualidade do ar",
     col="blue",pch=16, cex =1.2, cex.lab=1, ylim=c(50,170), main="Densidade populacional x Qualidade do ar")
curve(1.054e+02+(-3.857e-04 )*x, add=TRUE, col="red", lwd=2, lty=2)

####################################################################

# 1. Plote um gráfico com as variáveis airq e vala
# 2.Adicione nesse gráfico os títulos dos eixos x e y, com os argumentos xlab e ylab
# 3. Modifique a cor e o formato dos pontos com o argumento col e pch
# 4. Por fim, aumente o tamanho da fonte do eixo com o argumento cex.lab

plot(airq~vala,data=Airq, ylab="Qualidade do ar", 
     xlab="Valor das empresas nas cidades (milhares de dólares)",
     col="red",pch=16,cex.lab=1.2)

# Após fazer o gráfico, faça a reta, que será plotada, utilizando os valores do summary(m1)
# Nessa reta, modifique a cor com o argumento col, o tipo de linha com lty e a espessura com o lwd.

m1 = lm(airq~vala,data=Airq)
summary(m1)
curve(96.451419+0.001969*x,add=TRUE, col="darkblue", lty=4, lwd=2)

####################################################################
####################################################################
####################################################################

# REGRESSÃO MÚLTIPLA

mRM1 <- lm(airq~vala+coas, data=Airq)
summary(mRM1)

# Existe uma efeito da posição costeira e do valor das empresas na qualidade do ar

#----------------------------

# GRÁFICO DE REFRESSÃO MÚLTIPLA

plot(airq~vala,data=Airq, xlab = "Valor das empresas ($)", ylab="Qualidade do ar")
curve(1.171e+02+(1.999e-03)*x,add=TRUE, col="darkblue", lwd=1.5) #cidade não costeira
curve(1.171e+02+(1.999e-03)*x+(-2.968e+01),add=TRUE, col="darkblue", lty=2, lwd=1.5) #cidade costeira
legend("bottomright",c("Não costeiras","Costeiras"),lty=c(1,2),col=c("darkblue","darkblue"),cex=0.8, bty="n") #topright

# bty - tira a caixa da legenda
# A qualidade do ar das cidades é afetada tanto pelo valor das empresas quanto pela posição costeiras das cidades.
# Quanto maior o valor das empresas, pior a qualidade do ar. Além disso, as cidades não-costeiras
# apresentam qualidade do ar pior do que as cidades costeiras.


mRM2 <- lm(airq~vala+coas+dens,data=Airq)
summary(mRM2)

# Apesar da dens não ser significativa, não posso tirá-la de imediato do nosso modelo,
# porque apesar da variável sozinha não ser significativa, ela pode exercer um efeito
# indireto sobre as demais.

# Abaixo vamos verificar se há prejuízo retirá-la do modelo:

## CONSTRASTES DE MODELOS 
# ----------------------------------------------

# comparar um modelo completo com um modelo sem a variável em questão
modelocompleto <- lm(airq~vala+coas+dens,data=Airq)
summary(modelocompleto)
modeloincompleto <- lm(airq~vala+coas,data=Airq)
summary(modeloincompleto)

# Os dois modelos são iguais?
# Se p>0.05 não existe diferença entre os modelos, então eu continuo com o modelo
# mais simples: se p<0.05 os modelos são diferentes e a variável não deve ser retirada do modelo

anova(modelocompleto,modeloincompleto)

# Como é 0.57 que é maior que 0.5, então a variável densidade não está trazendo muita informaçáo para
# o nosso modelo, sendo asism podemos tirá-la do nosso modelo sem prejuízo, isto é, ela não é significativa


#######################################################################

# EXERCÍCIO

#######################################################################

# 1. Analisando o seguinte modelo completo, com todas as variáveis:

modelocompleto <- lm(airq ~ vala + coas + dens, data= Airq)

# Qual variável não significativa poderá ser removida do modelo após o contraste?

summary(modelocompleto)

modeloincompleto1 <- lm(airq ~ vala + coas, data=Airq)
modeloincompleto2 <- lm(airq ~ vala + dens, data=Airq)
modeloincompleto3 <- lm(airq ~ coas + dens, data=Airq)

anova(modelocompleto,modeloincompleto1) # p-valor = 0.57
anova(modelocompleto,modeloincompleto2) # p-valor = 0.004
anova(modelocompleto,modeloincompleto3) # p-valor = 0.39

# A variável não significativa que poderá ser removida do modelo é densidade

#---------------------------------------------------------

# Conclusão

# O que afeta a qualidade do ar nas cidades?

# As variáveis que afetaram foram: (a) o valor das empresas e (b) a posição costeira
# das cidades. Quanto maior o valor das empresas, pior a qualidado do ar. Além disso,
# notamos que cidades costeiras apresentam uma melhor qualidade do ar.
