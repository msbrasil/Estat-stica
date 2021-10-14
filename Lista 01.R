###############################################################################
#Sejam os dados a seguir, já ordenados do menor para o maior (rol) de 50      #
#observações, em decibéis, de nível de ruído  de tráfego em certo cruzamento. #
#Pede-se:                                                                     #
###############################################################################
dados <- c(52.0, 55.9, 56.7, 59.4, 60.2, 
           61.0, 62.1, 63.8, 65.7, 67.9, 
           54.4, 55.9, 56.8, 59.4, 60.3, 
           61.4, 62.6, 64.0, 66.2, 68.2, 
           54.5, 56.2, 57.2, 59.5, 60.5, 
           61.7, 62.7, 64.6, 66.8, 68.9, 
           55.7, 56.4, 57.6, 59.8, 60.6, 
           61.8, 63.1, 64.8, 67.0, 69.4, 
           55.8, 56.4, 58.9, 60.0, 60.8, 
           62.0, 63.6, 64.9, 67.1, 77.1)


################################################
#(A) Calcular média, moda e mediana para o rol #
################################################
media <- mean(dados)
media

#para calcular a moda é necessário usar uma função própria
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
  }

#este método é melhor para séries grandes e e multimodal.
moda <- subset(table(dados), table(dados) == max(table (dados)))
moda

mediana <- median(dados)
mediana
####################################################################################
#B) Compare as 3 medidas do item (A) e faça a descrição sucinta dessas observações #
####################################################################################
amplitude <- range(dados) #amplitude total
amplitude
77.1 - 55.2

desvio_med <- sum(abs(dados-media)/length(dados)) #desvio médio
desvio_med 

IQR(dados, type=4) #amplitude interquartílica 

var(dados) #variância

sd(dados) #desvio padrão



'''
Posição Central: O conjunto de dados apresentou média de 61.386 db, mediana de 
60.9 e três modas que são os valores 55.9, 56.4 e 59.4.
variabilidade: A amplitude total do cojunto é de 21.9. O desvio médio é de 3.76544.
A amplitude interqualítica é de 7.3. A variância dos dados é de 23.20368 e o 
desvio padrão apresentado foi de 4.81702.

'''

############################
#(C) calcule Q1, Q3 e C80. #
############################

#argumento type = 4 é para selecionar o método de cálculo.

quartil1 <- quantile(dados, 0.25, type = 4)
quartil1

quartil3 <- quantile(dados, 0.75, type = 4)
quartil3

percentil80 <- quantile(dados, 0.80, type = 4)
percentil80

############################################################################
#(D) construir tabela de distribuição de frequências agrupadas em classes. #
############################################################################

library(dplyr) #pacote necessário para criar tabela de freq

ni <- table(cut(dados,                      #input
                breaks = seq(52, 80,by=4),  #classes
                right=FALSE))               #intervalo aberto na direita

tab_frequencia<- rbind(ni,                        #frequencias por intervalo
                       p_fi = 100*prop.table(ni)) #Frequencias relativas

tab_frequencia <- as.data.frame(t(cbind(tab_frequencia,     #Construcao da tabela
                                c(sum(tab_frequencia[1,]),  #soma das freq. absolutas
                                sum(tab_frequencia[2,])))), #soma das freq. relativas
                                row.names=c(colnames(tab_frequencia),
                                            "Total")) 

tab_frequencia <- transform(tab_frequencia,p_fi=round(p_fi,digits=2))
tab_frequencia
#########################################################################
#(E) calcule a média moda e mediana para os dados agrupados em classes. #
#########################################################################

#################################
#(F) compare os itens (A) e (E) #
#################################


#G) Comparando as três medidas de Ⓔ o que você pode dizer a respeito dessa distribuição.

#H) plote o histrograma e compare com g)

#I) calcular todas as medidas de dispersão












