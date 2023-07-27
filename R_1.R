#------------------------------------------------------------------------------#
# Leonardo Reis                                                                #
#                                                                              #
# Nesta primeira análise, ajustarei uma árvore de regressão para classificar   #
# o valor do percentual da gorjeta conforme a base de dados que                #
# estão na liv 'reshape2'                                                      #
# Dados de um restaurante: total da conta; gorjeta; gênero; fumante; dia;      #
# horário; e qtd. pessoas                                                      #
#                                                                              #
# Pergunta interpretativa: quais mesas o garçom deve priorizar para ter as     #
# melhores gorjetas?                                                           #
#------------------------------------------------------------------------------#


library(tidyverse)
library(rpart)
library(rpart.plot)
library(gtools)
library("reshape2")

# Função usada pra criar gráfico
descritiva2 <- function(var, resp, df) {
  # Sumariza a variável resposta por categoria da variável em análise
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
    scale_y_continuous(sec.axis = sec_axis(~ . *maxN/max(tgc[,resp]),
                                           name = "Frequencia",
                                           labels = scales::number)) +
    ylab(resp) +
    # Limite do eixo vertical esquerdo
    coord_cartesian(ylim = c(0, #min(tgc[,resp]) - 0.02,
                             max(tgc[,resp] + qnorm(.975)*tgc$se) + 0.02))

  return(p)
}

data(tips)

tips %>% head

# Adicionando a transformação dos valores em percentuais em uma nova coluna 'pct_tip'
tips$pct_tip <- tips$tip/(tips$total_bill - tips$tip)

# Analisando os dados com o box plot - ggplot2
ggplot(tips, aes(x = "", y = pct_tip)) +
  geom_boxplot() +
  labs(title = " Boxplot da variável pct_tip") +
  theme_minimal()

# Eliminando o outlier (valor atípico):
# filtrar valores de gorjeta menores 1 (100%)
tips <- tips[tips$pct_tip<1,]

# Visualizando após o filtro
ggplot(tips, aes(x = "", y = pct_tip)) +
  geom_boxplot() +
  labs(title = " Boxplot da variável pct_tip") +
  theme_minimal()

# Etapa descritiva:
# Tentando entender a influência que cada variável exerce no percentual da gorjeta,
# usando a função criada 'descritiva2'
descritiva2("sex", "pct_tip", tips)
descritiva2("smoker", "pct_tip", tips)
descritiva2("day", "pct_tip", tips)
descritiva2("size", "pct_tip", tips)
tips$total_bill_cat <- quantcut(tips$total_bill, 5)
descritiva2("total_bill_cat", "pct_tip", tips)

# Treino da árvore
set.seed(123)
arvore_reg <- rpart::rpart(pct_tip ~  sex
                           + smoker
                           + day
                           + time
                           + size,
                           data=tips,
                           xval=10,
                           control = rpart.control(cp = 0,
                                                   minsplit = 2,
                                                   maxdepth = 2)
)

# Criando uma paleta de cores para usar na árvore
paleta <- scales::viridis_pal(begin=.75, end=1)(20)

#Visualizando a árvore
plot <- rpart.plot::rpart.plot(arvore_reg,
                               box.palette = paleta)


# Avaliando a árvore (R-quad)
# função auxiliar de avaliação
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

# Calculando o R-quadrado
metricas(predict(arvore_reg, tips), tips$pct_tip)


# Otimizando a árvore
set.seed(123)
arvore_grande <- rpart::rpart(pct_tip ~  sex
                              + smoker
                              + day
                              + time
                              + size,
                              data=tips,
                              xval=10,
                              control = rpart.control(cp = 0,
                                                      minsplit = 2,
                                                      maxdepth = 30)
)

# Calculando o R-quadrado
metricas(predict(arvore_grande, tips), tips$pct_tip)

# Achando o hiperparâmetro ótimo para o CP:
# Esta linha guarda todos os valores de CP para cada folha da árvore grande
tab_cp <- rpart::printcp(arvore_grande)
# Aqui uma visualização gráfica do CP vs erro em validação cruzada
rpart::plotcp(arvore_grande)

# Comando para pegar o melhor CP na validação cruzada
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']
cp_min

set.seed(123)
arvore_tunada <- rpart::rpart(pct_tip ~  sex
                              + smoker
                              + day
                              + time
                              + size,
                              data=tips,
                              xval=20,
                              control = rpart.control(cp = cp_min,
                                                      minsplit = 2,
                                                      maxdepth = 30)
)
metricas(predict(arvore_tunada, tips), tips$pct_tip)

#Visualizando a árvore tunada:
plot <- rpart.plot::rpart.plot(arvore_tunada,
                               box.palette = paleta)

