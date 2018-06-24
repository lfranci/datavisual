##___________________________________________##
## MINICURSO - VISUALIZACAO DE DADOS COM O R ##
## Luciana Franci - lucianafranci@gmail.com  ##
## Laboratorio de Ecologia Vegetal, UFPR     ##
## 01-02 de marco de 2018                    ##
##___________________________________________##

# Nota: defina o diretorio
##______________________________________________________________________________________

# 1. Instalando os pacotes----
if (!require("reshape2"))
  install.packages("reshape2")

if (!require("ggplot2"))
  install.packages("ggplot2")

if (!require("cowplot"))
  install.packages("cowplot")

if (!require("RColorBrewer"))
  install.packages("RColorBrewer")

if (!require("gridExtra"))
  install.packages("gridExtra")

if (!require("Rmisc"))
  install.packages("Rmisc")

if (!require("vegan"))
  install.packages("vegan")

if (!require("scales"))
  install.packages("scales")

if (!require("ggpubr"))
  install.packages("ggpubr")

## _____________________________________________________________________________________________     
# 2. Histogramas----
data(iris)
summary(iris)
View(iris)

#R basics
# Uso basico dos parametros graficos do R----
# Vamos definir os parametros do grafico (fonte, tamanho de fonte, margens...). Tudo que for definido em par, sera usado para os demais graficos. Tambem e possivel modificar alguns parametros, como as cores, na propria funcao do grafico

help(par)

#Definindo os parametros
par(cex = 1.5,               #tamanho de todas as fontes do grafico
    cex.lab = 1,             #tamanho dos rotulos dos eixos
    cex.main = 1.5,          #tamanho do titulo
    family = "sans",         #mudar a fonte, por exemplo, 'serif' equivale a Times New Roman; 'sans' equivale a Arial
    bg = "white")            #modificar cor do fundo (default e white)

# Histograma
help(hist)
#importante: 
#breaks: o default usa a regra de Sturges
#freq: T = contagem; F = probabilidade
#probability: pode ser util para plotar curvas de probabilidades, como para testes de normalidade

# Distribuicao de largura de sepala para a especie Iris setosa----
hist_setosa <- hist(subset(iris$Sepal.Width, iris$Species == "setosa"),   #selecionando apenas a especies 'setosa' do conjunto de dados
                    main = substitute(paste(italic('Iris'), sp.)),        #colocando apenas "Iris" em italico e sp sem italico
                    xlab = "Largura de sepala",                           #rotulo do eixo x
                    ylab = "Frequencia",                                  #rotulo do eixo y
                    col = "cadetblue1")                                   #cor

# Adicionando a distribuicao de largura de sepala para a especie Iris setosa----
hist_setosa <- hist(subset(iris$Sepal.Width, iris$Species == "versicolor"),
                    xlab = "Largura de sepala",
                    ylab = "Frequencia",
                    col = rgb(0, 1, 0, 0.5),                                 #cores transparentes: rgb(red, green, blue, alpha (transparencia); ou seja a funcao rgb faz uma mistura de cores
                    add = TRUE)                                              #add: adiciona pontos, curvas, barras ao grafico plotado anteriormente

# Adicionando a distribuicao de largura de sepala para a especie Iris virginica----
hist_setosa <- hist(subset(iris$Sepal.Width, iris$Species == "virginica"),
                    xlab = "Largura de sepala",
                    ylab = "Frequencia",
                    col = rgb(1, 0, 0, 1),
                    add = TRUE)

##______________________________________________________________________________________

# ggplot2----
library(ggplot2)

hist_iris <- ggplot(data = iris, aes(x = Sepal.Width)) +                       #dados e aesthetics, podemos colocar mais aesthetics abaixo
  geom_histogram(color="black", aes(fill = Species), alpha = 0.5, bins = 13) + #tipo de grafico: geom_histogram; bins = numero de classes; alpha indica 0.5 de transparencia
  labs(x = "Largura de sepala (cm)", y = "Densidade", fill = "Iris") +         #rotulos dos eixos x e y e da legenda (fill)
  theme(axis.text = element_text(size = 14),                                   #tamanho da fonte dos eixos
        axis.title = element_text(size = 14,face = "bold"),                    #tamanho da fonte dos rotulos dos eixos e em negrito (face = "bold")
        legend.text = element_text(size = 14),                                 #tamanho da fonte da legenda
        legend.title = element_text(size = 14, face = "bold"))                 #tamanho da fonte do totulo da legenda
hist_iris


#Agora melhorando o grafico, colocando uma especie por vez----
hist_iris2 <- ggplot(data = iris, aes(x = Sepal.Width, fill = Species)) +        #fill indica qual grupo sera usado para separar as categorias do grafico, inclusive as cores serao plotadas usando esta funcao
  geom_histogram(data = subset(iris, iris$Species == "setosa"), 
                 aes(fill = Species), color = "black", alpha = 0.5, bins = 13) + #fazendo um subset para plotar apenas a especie setosa; color = "black" indica a cor do contorno das barras;
  geom_histogram(data = subset(iris, iris$Species == "versicolor"), 
                 aes(fill = Species), color = "black", alpha = 0.5, bins = 13) + #fazendo o mesmo para a especie versicolor
  geom_histogram(data = subset(iris, iris$Species == "virginica"), 
                 aes(fill = Species), color = "black", alpha = 0.5, bins = 13) +    #o mesmo para a especie virginica
  labs(x = "Largura de sepala (cm)", y = "Densidade", fill = "Iris") +
  theme(axis.text=element_text(size = 14), 
        axis.title=element_text(size = 14,face = "bold"),
        legend.text=element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        panel.grid = element_blank(),                                             #removendo os grids
        #panel.background = element_blank(),                                      #esta funcao serve para deixar o fundo branco, mas como usei a funcao panel.background, nao precisaremos dela
        panel.background = element_rect(fill = "white", colour = "white"),        #serve para indicar as cores do painel de fundo do grafico, fill = "white" indica fundo branco, colour = "white" indica que nao quero um contorno colorido em volta do grafico
        axis.line = element_line(colour = "black"))                               #indica as cores do eixos, como indicamos em panel.background que colour = "white", sera mostrado apenas as linhas dos eixos x e y e nao uma moldura emvolta do grafico
hist_iris2

#salvando o grafico pronto para o manuscrito
png("histograma_iris.tiff", width = 12, height = 15, units = "cm", res = 400)
hist_iris2
dev.off()

# 3. Graficos de dispersao----
data(iris)
help("geom_point")
#----
disp_iris <-  ggplot(data=iris, aes(x =Sepal.Length, y = Sepal.Width, color = Species, shape = Species)) +
  geom_point(aes(shape = Species), size = 3, alpha = 0.7) + 
  labs(x = "Comprimento de sepala (cm)", y = "Largura de sepala (cm)", shape = "Iris", color = "Iris") + 
  theme(axis.text=element_text(size = 14), 
        axis.title=element_text(size = 14, face = "bold"),
        legend.text=element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.key = element_rect(fill = "white", colour = "white"),                #remover a cor atras do simbolo da legenda
        legend.position = "bottom",                                                 #colocando a legenda embaixo do grafico
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black")) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))                   #mudando as cores
disp_iris

#Pacote com paletas de cores prontas
library(RColorBrewer)

disp_iris2 <-  ggplot(data=iris, aes(x =Sepal.Length, y = Sepal.Width, color = Species, shape = Species)) +
  geom_point(aes(shape = Species), size = 3, alpha = 0.7) + 
  labs(x = "Comprimento de sepala (cm)", y = "Largura de sepala (cm)", shape = "Iris", color = "Iris") + 
  theme(axis.text=element_text(size = 14), 
        axis.title=element_text(size = 14, face = "bold"),
        legend.text=element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.position = "bottom",
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black")) +
  scale_color_brewer(palette = "Set2")                                       #mudando as cores usando uma paleta de cores prontas
disp_iris2

#Pacotes alternativos
if(!require("wesanderson")) install.packages("wesanderson")
library(wesanderson)
data(iris)


disp_iris3 <-  ggplot(data=iris, aes(x =Sepal.Length, y = Sepal.Width, color = Species, shape = Species)) +
  geom_point(aes(shape = Species), size = 4, alpha = 0.7) + 
  labs(x = "Comprimento de sepala (cm)", y = "Largura de sepala (cm)", shape = "Iris", color = "Iris") + 
  theme(axis.text=element_text(size = 14), 
        axis.title=element_text(size = 14, face = "bold"),
        legend.text=element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.position = c(0.85,0.9),                                          #colocando a legenda dentro do grafico
        legend.background = element_rect(fill = "white", 
                                         colour = "black"),                    #colocando uma borda preta em volta da legenda
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black")) +
  scale_color_manual(values = wes_palette(n = 3, name="FantasticFox"))         #mudando as cores usando uma paleta de cores prontas
disp_iris3

##______________________________________________________________________________________

# 4. Grafico de barras----

#Grafico de barras agrupado----
#A tabela de dados para graficos de barras e um pouco diferente. Precisamos indicar nas linhas ao que se refere cada valor e nao no cabecalho

library(reshape2)
iris2 <- melt(iris, id.vars="Species") #a funcao melt do pacote reshape faz examente o que precisamos para deixar a tabela pronta para o grafico de barras


#library(ggplot2)
#vamos fazer um grafico indicando as medias das medidas florais por especie de Iris
help(geom_bar)
#----
ggplot(data = iris2, aes(x = Species, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + #position = "dodge" indica que as barras serao agrupadas
  scale_fill_brewer(palette = "Set1",
                    name = "",
                    breaks = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                    labels  =c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width")) +
  labs(x = "Especies", y = "Medida (cm)") +
  theme(axis.text=element_text(size = 14), 
        axis.title=element_text(size = 14,face = "bold"),
        legend.text=element_text(size = 14))

#Mas grafico de barras precisa ter desvios ou erros padroes
#Primeiro precisamos criar um objeto com as medidas dos erros padroes

library("Rmisc") #para a funcao summarySE abaixo

erro_iris <- summarySE(iris2, measurevar = "value", groupvars = c("Species","variable"))
View(erro_iris)

#----
ggplot(data = erro_iris, aes(x = Species, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
                width=.2,
                position = position_dodge(0.9)) +                        #para centralizar a barra de erros nas barras de medias, vai por tentativa e erro
  scale_fill_brewer(palette = "Set1",
                    name = "",
                    breaks = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                    labels  =c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width")) +
  labs(x = "Especies", y = "Medida (cm)") +
  theme(axis.text=element_text(size = 14), 
        axis.title=element_text(size = 14, face = "bold"),
        legend.text=element_text(size = 14),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"), 
        axis.line = element_line(colour = "black"))

#Grafico de barras empilhadas----
#Criando um conjunto de dados de frutas em diferentes condicoes de nitrogenio
# create a dataset
fruta = c(rep("maca" , 3) , rep("manga" , 3) , rep("banana" , 3) , rep("pessego" , 3) )
condicao = rep(c("normal" , "stress" , "nitrogenio") , 4)
valor = abs(rnorm(12 , 0 , 15)) #abs serve para computador valores absolutos; rnorm gera dados com distribuicao normal
dados = data.frame(fruta, condicao, valor)

head(dados)

#grafico de barras agrupadas, valor no y, fruta no x, condicao nas cores----
ggplot(dados, aes(y = valor, x = fruta, fill = condicao)) + 
  geom_bar(position = "dodge", stat = "identity")

#barras empilhadas
#com valores absolutos----
ggplot(dados, aes(y = valor, x = fruta, fill = condicao)) + 
  geom_bar(stat = "identity")

#com proporcao----
ggplot(dados, aes(y = valor, x = fruta, fill = condicao)) + 
  geom_bar(stat = "identity", position = "fill")

#separando em facetas----
ggplot(dados, aes(y = valor, x = fruta, fill = condicao)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~condicao)

#Grafico de barras - exemplo----

#vamos usar os dados riqueza_relativa.csv e abundancia_relativa.csv
ri.rel <- read.csv("riqueza_relativa.csv", header = T, sep = ";")
ab.rel <- read.csv("abundancia_relativa.csv", header = T, sep = ";")

#Queremos fazer uma prancha com os graficos e queremos que as cores das barras sejam coincidam nos graficos

#Entao vamos unir os nomes dos taxon que ha nos dois data frames
dd1 <- union(ab.rel$taxon_plot, ri.rel$taxon_plot1)
#Agora vamos criar um objeto com nomes de cores
dd.col1 = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD")
#E vamos chamar essas cores com os nomes dos taxons, ou seja, cada taxon vai ter uma cor associada
names(dd.col1)  <- dd1
dd.col1

require(ggplot2)
require(RColorBrewer)

#Primeiro faremos o grafico para abundancia----
abund_rel <- ggplot(ab.rel, aes(x = taxon_plot,
                                y = relative, 
                                fill = taxon_plot)) + 
  geom_bar(stat="identity") + 
  labs(y = "Abundancia (1 ha)") +
  scale_fill_manual(values = dd.col1) +
  theme(legend.title = element_blank(),
        legend.text=element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text = element_text(size = 15))
abund_rel


#Agora o para riqueza----
rich_rel <- ggplot(ri.rel, aes(x = taxon_plot1,
                               y = relative, 
                               fill = taxon_plot1)) + 
  geom_bar(stat="identity") + 
  labs(y = "Riqueza (1 ha)") +
  scale_fill_manual(values = dd.col1) +
  theme(legend.title = element_blank(),
        legend.text=element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text = element_text(size = 15))
rich_rel


#Agora que temos os graficos prontos, vamos fazer o painel----
#Painel plot
library(cowplot)
library(gridExtra)

#Nao temos a abundancia para todas as especies que temos a riqueza, entao vamos salvar os nomes da legenda de riqueza em um objeto para podermos usa-las no painel
legenda_ri_ab <- get_legend(rich_rel)

#Agora faremos o painel usando a funcao plot_grid do pacote gridExtra
ri_ab <- plot_grid(rich_rel  + theme(legend.position="none"),    #nao quero que coloque a legenda deste grafico pois temos um objeto com as legendas
                      abund_rel + theme(legend.position="none"), #nao quero que coloque a legenda deste grafico pois temos um objeto com as legendas
                      align = 'hv',                              #como alinhas os graficos, pedi para alinhar horizontal e verticalmente
                      labels = c("(a)", "(b)"),                  #quero chamar os graficos de a e b
                      hjust = -0.2,                              #ajustando a posicao dos rotulos a e b - tentativa e erro
                      vjust = 1.1,                               #ajustando a posicao dos rotulos a e b - tentativa e erro
                      nrow = 1,                                  #quero os graficos em uma unica linha
                      ncol = 2,                                  #e em duas colunas, ou seja, um grafico ao lado do outro
                      scale = 1)                                 #ajustando as margens do painel
ri_ab 

#agora vamos colocar a legenda e ajustar o painel
plot_grid(ri_ab,                     #painel com os dois graficos
          legenda_ri_ab,             #objeto com as legendas
          ncol = 2,                  #numero de colunas, ou seja uma coluna para grafico1 e uma para legenda1
          rel_heights = c(1, 1),     #altura das colunas, ou seja, quero que tenham a mesma proporcao
          rel_widths = c(3,1))       #largura das colunas, ou seja quero que a primeira coluna ocupe 3 vezes mais espa?o que a segunda

##______________________________________________________________________________________
# 5. Box plot----
#Para o boxplot usaremos a tabela salva em iris2

ggplot(data = iris2, aes(x = Species, y = value, fill = variable)) + 
  stat_boxplot(geom ='errorbar') +                                         #colocando uma barrinha de final no "fio de bigode", coloco este antes do boxplot para a linha ficar por tr?s da caixa
  geom_boxplot(notch = T) +                                                #notch = T indica que queremos entalhe nas caixas
    scale_fill_brewer(palette = "Set1",
                    name="",
                    breaks = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                    labels = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width")) +
  labs(x = "Especies", y = "Medida (cm)") +
  theme(axis.text=element_text(size = 14), 
        axis.title=element_text(size = 14,face = "bold"),
        legend.text=element_text(size = 14),
        legend.key = element_rect(fill = "white", colour = "white"), 
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"))
#A mensagem 'notch went outside hinges. Try setting notch=FALSE' indica que o entalhe passou dos limites da caixa. Isso pode ser porque os dados tem distribuicao deslocada (skewed) ou o n e baixo.

#Como indicar quais medidas diferem----

library(ggpubr)

virg <- subset(iris2, iris2$Species == "virginica")

compare_means(value ~ variable,  data = virg,
              ref.group = ".all.", method = "t.test")
#----
ggplot(data = virg, aes(x = variable, y = value, fill = variable), facet.by = "variable", short.panel.labs = FALSE) + 
  stat_boxplot(geom ='errorbar') +  
  geom_boxplot(notch = F) +
  scale_fill_brewer(palette = "Set1",
                    name="",
                    breaks = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                    labels = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width")) +
  labs(x = "Medida da flor", y = "Tamanho (cm)") +
  theme(axis.text=element_text(size = 14), 
        axis.title=element_text(size = 14,face = "bold"),
        legend.text=element_text(size = 14),
        legend.key = element_rect(fill = "white", colour = "white"), 
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black")) +
  rotate_x_text(angle = 45) +                                                     #girando os rotulos do eixo x para facilitar a visualizacao
  stat_compare_means(method = "anova", label.y = 9) +                             #colocando um valor global de p (ANOVA)
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")  #aqui sera testado todo mundo contra todo mundo               

##______________________________________________________________________________________

# 6. Grafico de linhas----
#Vamos usar os dados do R ChickWeight, o qual contem pesos de galinhas e tipo de dieta
data(ChickWeight)
View(ChickWeight)
summary(ChickWeight)
#----
ggplot(data = ChickWeight, aes(x = Time, y = weight, color = Diet, group = Chick)) +
  geom_line(linetype = 1, size = 1) +                  #linetype indica o tipo da linha (1 para continua e 2 para tracejada - existem outros tipos, e so ir testando ou procurar no help da funcao), size indica a espessura da linha
  labs(x = "Dias", y = "Peso (g)", color = "Dieta") +
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14))+
  theme_bw()                                           #colocando um grid com linhas suaves no fundo para facilitar a visualizacao
##______________________________________________________________________________________

# 7. Como inserir mais de um eixo----
#Vamos usar como exemplo a construcao de um grafico de barras e linhas. Dados clima_curitiba.csv, contem as medias de temperatura e precipitacao por mes na cidade de Curitiba

clima <- read.csv("clima_curitiba.csv", header = T, sep = ";")
summary(clima)

#os niveis da coluna mes esta fora de ordem
levels(clima$mes)
clima$mes <- factor(clima$mes, levels = c("Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"))

#----

library(ggplot2)


clima_cwb <- ggplot(clima) +
  geom_bar(aes(x = mes, y = precip, fill = precip), stat="identity") +
  geom_line(aes(x = mes, y = temp_media*10, group = 1, colour = " "),size = 1.5) + #precisa modificar a escala do eixo y, aqui multipliquei por 10
  scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(~./10, name = "Temperatura (ºC)")) +         #divindo o eixo y por 10 para voltar a escala original
  scale_x_discrete(expand = c(0, 0)) +
  labs(x = "Mes", y = "Precipitacao (mm)", color = "Temperatura", fill = "Precipitacao") +
  rotate_x_text(angle = 45) +
  scale_fill_gradient2(low = "white", mid = "white", high = "dodgerblue4") +
  scale_color_manual(values = "green")
clima_cwb
##______________________________________________________________________________________

# 8. Regressao linear e regressao logistica----
data(iris)
#Regressao linear----
#primeiro vamos fazer a regressao para termos os valores de R^2 e P
#para facilitar a vida, vamos salvar os dados de Iris versicolor em um objeto:
iversi <- subset(iris, iris$Species == "versicolor")
lm_iversi <- lm(Sepal.Width ~ Sepal.Length, data = iversi) #nao testamos as premissas porque e so aprender a graficar
summary(lm_iversi)

#A funcao a baixo sera usada para montar a equacao da regressao linear
equation = function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 2),
                  b = round(coef(x)[2], digits = 2),
                  r2 = round(summary(x)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}

#----
ggplot(data = iversi, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(shape = Species), 
             size = 3, alpha = 0.7, color = "#E69F00", shape = 17) + 
  geom_smooth(aes(fill = Species),color = "black",alpha =.1, method = lm,                                             #adicionando a linha de uma regressao linear
              se = TRUE, #colocando so a linha sem intervalo de confianca, se = TRUE plota o intervalo de confianca
              fullrange = TRUE) +                                       #fullrange = TRUE coloca a linha da regressao em toda a extensao do grafico
  labs(x = "Comprimento de sepala (cm)", y = "Largura de sepala (cm)", 
       shape = "Iris", color = "Iris") + 
  theme(axis.text=element_text(size = 14), 
        axis.title=element_text(size = 14, face = "bold"),
        legend.text=element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.position = c(0.1, 0.9),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black")) +
  annotate("text", x = 6.5, y = 2, label = equation(lm_iversi), parse = TRUE, size = 5) + #parse = T serve para colocar a notacao como uma formula e nao como aparece no texto
  scale_fill_manual(values = "blue")


#Colocando varias linhas de regressao no mesmo grafico----
iris_lm <- ggplot(data = iris, aes(x =Sepal.Length, y = Sepal.Width, color = Species, shape = Species)) +
  geom_point(aes(shape = Species), size = 3, alpha = 0.7) + 
  geom_smooth(aes(fill= Species),method = lm,                                       #adicionando a linha de uma regressao linear
              se = TRUE,                                        #colocando so a linha sem intervalo de confianca, se = TRUE plot o intervalo de confianca
              fullrange = FALSE) +                                #fullrange = TRUE coloca a linha da regressao em toda a extensao do grafico
  labs(x = "Comprimento de sepala (cm)", y = "Largura de sepala (cm)", shape = "Iris", color = "Iris") + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.position = c(0.1, 0.9),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black")) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) #mudando as cores
iris_lm

#Colocando as especies em facetas diferentes----
iris_lm +
  theme(legend.position = "none") + #nao precisamos de legendas
  facet_grid(.~Species)

#Regressao logistica----
#Vamos usar a planilha de dados "dados_logistico.csv"
dados_log <- read.csv("dados_logistico.csv", header = T, sep = ";")

summary(dados_log)
str(dados_log)

mod_log <- glm(binario ~ medida1 + medida2 + grupo, data = dados_log, family = "binomial") 
summary(mod_log)

#----
ggplot(data = dados_log, aes(x = medida1, y = binario, color = grupo)) +
  geom_point(aes(shape = grupo), size = 3, alpha = 0.7, 
             position=position_jitter(height=0.03, width=0)) +                #colocar uma variacao aleatoria nos dados para facilitar a visualizacao
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se  = FALSE, size = 2) +
  labs(x = "Medida 1", y = "Binario", shape = "Grupo", color = "Grupo") +
  theme(axis.text=element_text(size = 14), 
        axis.title=element_text(size = 14, face = "bold"),
        legend.text=element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.position = "right",
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black"))
##______________________________________________________________________________________

# 9. Biplot e triplot----

#Biplot - PCA----
library(vegan)
iris_pca <- rda(iris[, 1:4], scale = T)
summary(iris_pca)

colvector <- c("red", "green", "blue") #vetor contendo as cores que usaremos

library(scales) #para colocar transparencia

par(cex = 1.5)
biplot(iris_pca, type = c("points", "points"), xlab = "PC1 (73%)", ylab = "PC2 (23%)", scaling = 3, col = c("white", "black"),choices = c(1,2))
with(iris, points(iris_pca, display = "sites", col = colvector[Species],
                     scaling = 3, pch = 21, bg =  alpha(colvector[Species], 0.5)))
text(iris_pca, display = "species", scaling = 3, cex = 0.9, col = "black")
with(iris, legend("bottomright", legend = levels(Species), bty = "n",
                     col = colvector, pch = 21, pt.bg = alpha(colvector, 0.5)))

#Triplot - CCA----
data(mite)
help(mite)
View(mite)
data(mite.env)
View(mite.env)
summary(mite.env)

mite.cca <- cca(mite, mite.env[,c(1,2)])
summary(mite.cca)

colvector2 <- c("darkorange", "navy") #vetor de cores

plot(mite.cca, scaling = 3, display = c("sp", "wa", "cn"), type = "n")
with(mite.env, points(mite.cca, display = "sites", col = colvector2[Topo], scaling = 3, pch = 21, bg = alpha(colvector2[Topo], 0.5)))
text(mite.cca, display = "species", scaling = 3, cex = 0.9, col = "black")
text(mite.cca, display = "cn", scaling = 3, cex = 0.9, col = "chartreuse4")
with(mite.env, legend("bottomleft", legend = c("Blanket", "Hummock"), bty = "n",
                       col = colvector2, pch = 21, pt.bg = alpha(colvector2, 0.5)))
