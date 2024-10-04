#Exercicio PRECOS modelo dos bons somente com as vars X´s numericas####################

#1. Carregando Pacotes#####
library(readr)
library(openxlsx) #biblioteca para escrever arquivo em excel
library(haven)
library(readxl)
library(tidyverse)
library(yardstick) #biblioteca para calcular medidas de erro
library(lmtest) # calcula o teste de homogeneidade de variancia
library(car) # calcula vif
library(ggraph)
library(plotly)
library(ggstance)
library(jtools)
library(olsrr)
library(PerformanceAnalytics)
library(correlation)
library(fastDummies)
library(lazyeval) # para a analise de interacao

#2. Carregando os dados#####
# fazendo a leitura do arquivo ja em R que estao na pasta dados
precos <- readRDS("dados/precos.rds")

# nesse momento pegando apenas as variaveis numericas para brincar
precos <- precos %>% 
  select(-Heating_QC, -Season_Sold)

#Como ja sabemos o que acontece, vamos direto ao ponto#####
#3. Transformar variaveis X´s muito assimetricas#####
# isso pode ajudar a normalizar os residuos

# primeiro vou aplicar o log de todas as variaveis X´s
precos_log <- precos %>% 
  mutate(Basement_Area_log = log10(Basement_Area+1),
         Lot_Area_log = log10(Lot_Area+1),
         Gr_Liv_Area_log = log10(Gr_Liv_Area+1),
         Garage_Area_log = log10(Garage_Area+1),
         Deck_Porch_Area_log = log10(Deck_Porch_Area+1),
         Age_Sold_log = log10(Age_Sold+2),
         Bedroom_AbvGr_log = log10(Bedroom_AbvGr+1),
         Total_Bathroom_log = log10(Total_Bathroom+1))

# desenhando todos os boxplots de uma vez
precos_log %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value, y = 1)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free")

#4. Fazer o modelo utilizando todas as variaveis##########
# modelo completo
model_preco_log_completo <- lm(SalePrice~ Basement_Area_log +
                                 Lot_Area_log + Gr_Liv_Area_log + 
                                 Garage_Area_log + Deck_Porch_Area_log + 
                                 Age_Sold_log + Bedroom_AbvGr_log + 
                                 Total_Bathroom_log, data=precos_log)
summary(model_preco_log_completo)
# bati o olho, apenas por curiosidade e agora vou tentar pelo
# stepwise usando o AIC e p-valor como criterio de escolha

#regressao por stepwise usando AIC
model_preco_log_step_AIC <- step(model_preco_log_completo, direction = "both")
summary(model_preco_log_step_AIC)

#regressao por stepwise usando p-valor
model_preco_log_step_pvalor<- step(model_preco_log_completo, k = 3.841459)
summary(model_preco_log_step_pvalor)

#5. Fazer Analise de Residuo do modelo##########
# model_preco_log_step_AIC e model_preco_log_step_pvalor
# nesse caso tanto faz qual objeto usar, porque sao iguais

#5a. Predito X Residuo ##########
# plotando o ajustado X residuo 
plot(fitted(model_preco_log_step_pvalor),residuals(model_preco_log_step_pvalor),xlab="Valores Ajustados",ylab="Resíduos")
abline(h=0)

olsrr::ols_plot_resid_fit(model_preco_log_step_pvalor)
# teste de normalidade do residuo 
# h0: os dados sao normais
# h1: os dados nao sao normais
shapiro.test(model_preco_log_step_pvalor$residuals)

# ferrou tudo aqui, continua nao sendo normal e nao funcionou
#Continuamos sem modelo!! ##########
# as taticas que falamos ate agora ainda nao funcionaram


#Vamos conversar sobre variaveis categoricas##########
# depois voltamos para o modelo usando todas as variaveis
# inclusive usando mais uma tatica de categorizar as numericas assimetricas
# no entanto, tambem não quer dizer que vai funcionar

#Trabalhando com 1 variavel X e categorica##########
# vou trazer a base novamente, com outro nome, para trazer as vars
# categoricas que nao existem mais na tabela oficial precos
precos1 <- readRDS("dados/precos.rds")

# vamos comecar acreditando que soh existe a variavel Heating_QC
# fazer um grafico de barras para ver as frequencias
ggplot(precos1, aes(x = Heating_QC)) +
  geom_bar()

# ja me gerou desconforto porque Po aparece muito pouco
# fazer um calculo das medias por Heating_QC
# e verificar o volume de linhas por classe
sum_heating_qc <- precos1 %>% 
  group_by(Heating_QC) %>% 
  summarise(n = n(),
            mediaSalePrice = mean(SalePrice))
sum_heating_qc

# a frequencia eh muito pequena tanto no Po como no Fa
# eu nao me sinto segura, entao vou deixar apenas 3 classes
# Ex, TA e Outros (FA + Gd + Po)
precos1 <- precos1 %>% 
  mutate(Heating = ifelse(Heating_QC == "Ex", "Ex", 
                          ifelse(Heating_QC == "TA", "TA", "Outros")))

# vamos analisar a nova variavel
ggplot(precos1, aes(x = Heating)) +
  geom_bar()
sum_heating <- precos1 %>% 
  group_by(Heating) %>% 
  summarise(n = n(),
            mediaSalePrice = mean(SalePrice))
sum_heating

# vamos trabalhar com variavel Dummie
precos1_dummie <- fastDummies::dummy_columns(.data = precos1,
                                      select_columns = "Heating",
                                      remove_selected_columns = F,
                                      remove_most_frequent_dummy = F)

modelo_dummie <- lm(SalePrice ~ Heating_Ex + Heating_TA, data=precos1_dummie)
summary(modelo_dummie)

#criando o ychapeu
precos1_dummie <- precos1_dummie %>% 
  mutate(ychapeu_dummie = modelo_dummie$fitted.values)

#Visualizando a nossa obra de arte##########
arte_precos_d <- ggplot()
arte_precos_d <- arte_precos_d + geom_point(data=precos1_dummie, aes(x=Heating, y=SalePrice), color = "black" )
arte_precos_d
arte_precos_d <- arte_precos_d + geom_point(data=precos1_dummie, aes(x=Heating, y=ychapeu_dummie, color="Modelo dummie")) 
arte_precos_d

#Provando que os chutes nada mais sao do que medias##########
# repara que nada mais eh do que um chute na media de cada categoria
# vamos provar

# y chapeu quando eh Ex
predict(object = modelo_dummie,
        data.frame(Heating_Ex = 1, Heating_TA = 0))

# y chapeu quando eh TA
predict(object = modelo_dummie,
        data.frame(Heating_Ex = 0, Heating_TA = 1))

# y chapeu quando eh Outros
predict(object = modelo_dummie,
        data.frame(Heating_Ex = 0, Heating_TA = 0))

#veja as medias ja calculadas
#sum_heating <- precos1 %>% 
#  group_by(Heating) %>% 
#  summarise(n = n(),
#            mediaSalePrice = mean(SalePrice))
sum_heating

#Tabalhando com uma X categorica e outra numerica##########
# nesse caso aqui vamos brincar com a Heating e a Gr_Liv_Area

sp<-ggplot(precos1_dummie, aes(x=Gr_Liv_Area, y=SalePrice, color=Heating)) + geom_point()
sp

# Veja, sera que nao vale faz uma reta para cada cor?
modelo_dummie_num <- lm(SalePrice ~ Heating_Ex + Heating_TA + Gr_Liv_Area, data=precos1_dummie)
summary(modelo_dummie_num)
# veja que interessante, ele esta dando que a reta verde (outros - quem nao se colocou no modelo)
# nao tem diferenca siginificativa da reta azul (TA)
# ou seja, na presenca de Gr_Liv_Area eles se tornam medias iguais

# vamos fazer um teste!

# tirando a variavel dummie TA
# note que o Outros se tornara insignificante
modelo_dummie_num1 <- lm(SalePrice ~ Heating_Ex + Heating_Outros + Gr_Liv_Area, data=precos1_dummie)
summary(modelo_dummie_num1)

# agora vamos tirar Ex, note que as duas se tornarao significativas
modelo_dummie_num2 <- lm(SalePrice ~ Heating_TA + Heating_Outros + Gr_Liv_Area, data=precos1_dummie)
summary(modelo_dummie_num2)

# com parametros praticamente iguais e os intervalos se cruzam
confint(modelo_dummie_num2, level = 0.95)
plot_summs(modelo_dummie_num2, colors = "#4400FF")

# repare que no modelo em que tiramos Outros
summary(modelo_dummie_num)
# vemos que TA eh nao significativo, ou seja, tem media igual ao que foi tirado (Outros)
# Sendo assim, repare que os intervalos dos parametros de Heating_EX e Heating_TA nao se cruzam
# e o intervalo para Heating_TA contempla o zero
confint(modelo_dummie_num, level = 0.95)
plot_summs(modelo_dummie_num, colors = "#4400FF")

#isso quer dizer entao que essa classe pode agrupar TA com Outros
# vamos fazer isso

precos1 <- precos1 %>% 
  mutate(Heating_model = ifelse(Heating == "Ex", "Ex", "Outros"))

#plotando 
sp1<-ggplot(precos1, aes(x=Gr_Liv_Area, y=SalePrice, color=Heating_model)) + geom_point()
sp1

# vamos trabalhar com variavel Dummie
precos1_dummies <- dummy_columns(.data = precos1,
                                select_columns = "Heating_model",
                                remove_selected_columns = F,
                                remove_most_frequent_dummy = F)

modelo_dummie_num3 <- lm(SalePrice ~ Heating_model_Ex + Gr_Liv_Area, data=precos1_dummies)
summary(modelo_dummie_num3)

#criando o ychapeu
precos1_dummies <- precos1_dummies %>% 
  mutate(ychapeu_dummies = modelo_dummie_num3$fitted.values,
         ychapeu_heating_ex = 9289.174 + 44250.788*1 + 99.330*Gr_Liv_Area,
         ychapeu_heating_outros = 9289.174 + 44250.788*0 + 99.330*Gr_Liv_Area)

#Visualizando o conceito de trabalhar com X numerica e categorica##########
arte_precos_d_n <- ggplot()
arte_precos_d_n <- arte_precos_d_n + geom_point(data=precos1_dummies, aes(x=Gr_Liv_Area, y=SalePrice, color=Heating_model))
arte_precos_d_n
arte_precos_d_n <- arte_precos_d_n + geom_point(data=precos1_dummies, aes(x=Gr_Liv_Area, y=ychapeu_heating_ex, color="Modelo dummie ex")) 
arte_precos_d_n
arte_precos_d_n <- arte_precos_d_n + geom_point(data=precos1_dummies, aes(x=Gr_Liv_Area, y=ychapeu_heating_outros, color="Modelo dummie outros")) 
arte_precos_d_n

# reparem que as retas ficam paralelas (porque so muda o b0)
# ou seja, notamos que vale a pena fazer "um modelo para cada" 
# quando existe diferenca nas medias de Y conforme o nivel da variavel X categorica muda
# caso contrario nao tem porque colocar duas retas (que passariam no mesmo lugar)

#Vamos conversar sobre interacao entre variaveis categoricas##########
#para isso vamos considerar as duas variaveis categoricas
# Heating_QC e Season_Sold

# Heating_QC nos ja discutimos, Relembrando:
# Heating_QC variavel original que tinha classe com pouca observacao
# Heating variavel agrupada por conta do motivo descrito acima
# Heating_model variavel agrupada conforme sugestao do modelo 
# com a presenca de outra variavel
# sendo assim, vou voltar a trabalhar com a Heating, para ver o que acontece
# na presença de Season
precos2 <- precos1 %>% 
  mutate(Season_Sold = as.character(Season_Sold))

# explorando um pouco de Season_Sold
ggplot(precos2, aes(x = Season_Sold)) +
  geom_bar()
# esta uma distribuicao super ok, nao vou mexer em nada
# so criar a dummie e testar no modelo
# mas antes quero ver o que vai acontecer, quero entender a média da Y pela Season
# sera que essas distribuicoes sao diferentes
# se for, otimo, se nao for, pode ser que a variavel nao seja tao sensual

# essa eh a distribuicao de SalePrice
ggplot(data = precos2, aes(x = SalePrice)) +
  geom_density() + ggtitle('SalePrice')
# sera que ela muda por Season_Sold?
ggplot(data = precos2, aes(x = SalePrice, color=Season_Sold, fill=Season_Sold)) +
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.2) 
# vou fazer um boxplot por Seaon_Sold tambem
ggplot(data = precos2, aes(x = SalePrice, fill=Season_Sold)) +
  geom_boxplot()

# vou criar as duas dummies, tanto a da Season como da Heating
precos2_dummies <-  dummy_columns(.data = precos2,
                                  select_columns = "Season_Sold",
                                  remove_selected_columns = F,
                                  remove_most_frequent_dummy = F)

precos2_dummies <-  dummy_columns(.data = precos2_dummies,
                                  select_columns = "Heating",
                                  remove_selected_columns = F,
                                  remove_most_frequent_dummy = F)

# vou testar apenas com a Season_Sold
modelo_dummie_num4 <- lm(SalePrice ~ Season_Sold_1 +
                           Season_Sold_2 + Season_Sold_3, data=precos2_dummies)
summary(modelo_dummie_num4)
# repare que as Season_Sold = 1 e 3 e 4, tem medias iguais na variavel Y
# por isso podem sem agrupadas

# sera que isso muda na presenca de Heating
# entao Heating e Season juntas
modelo_dummie_num5 <- lm(SalePrice ~ Heating_Ex + 
                           Heating_TA + Season_Sold_1 +
                           Season_Sold_2 + Season_Sold_3, data=precos2_dummies)
summary(modelo_dummie_num5)
# nao mudam!! as decisoes se mantiveram, 
# precisamos agrupar Season_Sold_1 com a 3 e a 4
# apenas Season_Sold_2 tem media diferente 
precos2_dummies <- precos2_dummies %>% 
  mutate(Season_model = ifelse(Season_Sold == 2, "Season_2", "Season_Outros"))

#plotando 
sp2<-ggplot(precos2_dummies, aes(x=Gr_Liv_Area, y=SalePrice, color=Season_model)) + geom_point()
sp2

# vamos trabalhar com variavel Dummie
precos2_dummies <- dummy_columns(.data = precos2_dummies,
                                 select_columns = "Season_model",
                                 remove_selected_columns = F,
                                 remove_most_frequent_dummy = F)

modelo_dummie_num6 <- lm(SalePrice ~ Heating_Ex + 
                           Heating_TA + Season_Sold_2, data=precos2_dummies)
summary(modelo_dummie_num6)

# todo mundo importante!!!
# mas sera que se criarmos a combinacao delas, tambem se torna importante?

precos2_dummies <- precos2_dummies %>% 
  mutate(interacao = str_replace_all(paste("Heating_",Heating,"_",Season_model)," ",""))

# vamos para a regressao com a interacao
# criando as varias dummies da interacao
precos2_dummies <- dummy_columns(.data = precos2_dummies,
                                select_columns = "interacao",
                                remove_selected_columns = F,
                                remove_most_frequent_dummy = F)

#plotando 
sp3<-ggplot(precos2_dummies, aes(x=Gr_Liv_Area, y=SalePrice, color=interacao)) + geom_point()
sp3

#colocando todas mais a interacao no modelo
modelo_dummie_num7 <- lm(SalePrice ~ Heating_Ex + 
                           Heating_TA + Season_Sold_2 + 
                           interacao_Heating_Ex_Season_Outros + 
                           interacao_Heating_Outros_Season_Outros +
                           interacao_Heating_Outros_Season_2 + 
                           interacao_Heating_TA_Season_Outros +
                           interacao_Heating_TA_Season_2, data=precos2_dummies)
summary(modelo_dummie_num7)

# nem todas sao importantes, isso quer dizer que soh importa
# o relacionamento de Heating_Ex com Season_Outros
# sendo assim, vamos criar uma nova interacao
# e depois tratar as outras variaveis!

precos2_dummies <- precos2_dummies %>% 
  mutate(interacao2 = ifelse(interacao == "Heating_Ex_Season_Outros", "interacao_Heating_Ex_Season_Outros", "interacao_Outra"))

# vamos para a regressao com a interacao
# criando as varias dummies da interacao
precos2_dummies <- dummy_columns(.data = precos2_dummies,
                                 select_columns = "interacao2",
                                 remove_selected_columns = F,
                                 remove_most_frequent_dummy = F)

#vamos verificar o modelo
modelo_dummie_num8 <- lm(SalePrice ~ Heating_Ex + 
                           Heating_TA + Season_Sold_2 + 
                           interacao2_interacao_Heating_Ex_Season_Outros, data=precos2_dummies)
summary(modelo_dummie_num8)

# repare que agora nao precisamos mais de Season_Sold_2
# entao basta usarmos a Heating_model (que ja esta Ex e Outros)
# e nao adicionar Season_Sold (que estaria contemplada apenas na interacao)

# preciso criar as dummies do Heating_model
precos2_dummies <- dummy_columns(.data = precos2_dummies,
                                 select_columns = "Heating_model",
                                 remove_selected_columns = F,
                                 remove_most_frequent_dummy = F)


modelo_dummie_num9 <- lm(SalePrice ~ Heating_model_Ex +  
                           interacao2_interacao_Heating_Ex_Season_Outros + Gr_Liv_Area, data=precos2_dummies)
summary(modelo_dummie_num9)

# equacao do modelo
#9694.53 + 39339.38*Heating_model_Ex + 
#7130.29*interacao2_interacao_Heating_Ex_Season_Outros 
#99.03*Gr_Liv_Area

# eh isso que siginifica que variavel categoria e interacao entre variaveis
#Dica para Detectar interação entre variáveis################################
# essa função fará o gráfico da interacao para voce

interaction.plot.ggplot2 <- function(response, predictor, group, data){
  
  l_response <- lazy(response)
  l_predictor <- lazy(predictor)
  l_group <- lazy(group)
  
  interaction <- data %>%
    select_(.dots = list(l_predictor, l_group, l_response)) %>%
    group_by_(.dots = list(l_predictor, l_group)) %>%
    summarise_(
      .dots = setNames(list(interp(~mean(response), response = l_response)), "average")
    )
  
  p <- ggplot(interaction, aes_string(x=expr_text(predictor), y="average", colour=expr_text(group), group=expr_text(group))) +
    geom_line()
  
  print(p)
}


interaction.plot.ggplot2(SalePrice, Heating, Season_Sold, precos2_dummies)

# se as retas se cruzam, pode ser interessante testar essa interacao

# para fechar essa discussao, vamos fazer a analise 
# de residuo do modelo com interacao


# plotando o ajustado X residuo 
plot(fitted(modelo_dummie_num9),residuals(modelo_dummie_num9),xlab="Valores Ajustados",ylab="Resíduos")
abline(h=0)

# teste de normalidade do residuo 
# h0: os dados sao normais
# h1: os dados nao sao normais
shapiro.test(modelo_dummie_num9$residuals)

 # Se lascou todo e ainda nao resolveu o problema
# isso eh ciencia de dados!!!
# E ai? curtindo de brincar de detetive?
#Continuamos sem modelo!! ##########

# quesito curiosidade, vamos ver os erros?
#criando respostas
# relembrando que 
# modelo_dummie_num3 = (SalePrice ~ Heating_model_Ex + Gr_Liv_Area)
# modelo_dummie_num6 = (SalePrice ~ Heating_Ex + Heating_TA + Season_Sold_2)
# modelo_dummie_num9 = (SalePrice ~ Heating_model_Ex + interacao2_interacao_Heating_Ex_Season_Outros + Gr_Liv_Area)
tentativas <- precos1_dummies %>% 
  mutate(modelo_dummie_num3 = modelo_dummie_num3$fitted.values,
         modelo_dummie_num6 = modelo_dummie_num6$fitted.values,
         modelo_dummie_num9 = modelo_dummie_num9$fitted.values)
# erro modelo_dummie_num3 = (SalePrice ~ Heating_model_Ex + Gr_Liv_Area)
rmse(tentativas, truth = SalePrice, estimate = modelo_dummie_num3)
# erro modelo_dummie_num6 = (SalePrice ~ Heating_Ex + Heating_TA + Season_Sold_2)
rmse(tentativas, truth = SalePrice, estimate = modelo_dummie_num6)
# erro modelo_dummie_num9 = (SalePrice ~ Heating_model_Ex + interacao2_interacao_Heating_Ex_Season_Outros + Gr_Liv_Area)
rmse(tentativas, truth = SalePrice, estimate = modelo_dummie_num9)


# brinquem agora com todas as variaveis mais as categoricas
# nao quer dizer que vai funcionar, mas agora voces ja sabem trabalhar com as duas
