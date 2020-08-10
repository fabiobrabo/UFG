#Trabalho UFG - DataSUS
library(magrittr)
library(dplyr)
library(readxl)
library(tidyverse)
library(caret)
library(leaps)
library(car)
library(Hmisc)

#Carregando as tabelas
Internacoes_Doencas <- read.delim("~/Downloads/UFG/resumo.txt")
Tipo_Domicilio <- read_excel("tabela1134.xlsx")
#Dist_Moradores <- read_excel("tabela1378.xlsx")
Total_Morador <- read_excel("totalmorador.xlsx")
Tipo_Residencia <- read_excel("tabela1394-1.xlsx")
Tipo_Esgoto <- read_excel("tabela1394-2.xlsx")
Uso_Sanitario <- read_excel("tabela1394.xlsx")
IDH <- read_excel("tabela1722.xlsx")
Prop_Urbano_Rural <- read_excel("tabela3175-1.xlsx")
Prop_Homem_Mulher <- read_excel("tabela3175-2.xlsx")
Faixa_Etaria <- read_excel("tabela3175-3.xlsx")
Etnia <- read_excel("tabela3175.xlsx")
Alfabetizacao <- read_excel("tabela3213.xlsx")
Descarte_Lixo <- read_excel("tabela3218-2.xlsx")
Tinham_Energia <- read_excel("tabela3218-3.xlsx")
Acesso_Agua <- read_excel("tabela3218.xlsx")
Renda_Mensal_Dom_Per_Capita <- read_excel("tabela3261.xlsx")
Renda_Mensal_Dom_Nominal <- read_excel("tabela3268.xlsx")
codigo_municipio <- read_excel("codigo_municipio.xlsx")
Not_Dengue <- read_excel("Not_Dengue_10.xlsx")

#Juntando as tabelas
tabelao <- bind_cols(Tipo_Domicilio,Tipo_Residencia,Tipo_Esgoto,Uso_Sanitario,Prop_Urbano_Rural,Prop_Homem_Mulher,Faixa_Etaria,Etnia,Alfabetizacao,Descarte_Lixo,Tinham_Energia,Acesso_Agua,Renda_Mensal_Dom_Per_Capita,Renda_Mensal_Dom_Nominal,codigo_municipio,Total_Morador)

#Analisando as Notificacoes
summary(Not_Dengue)
boxplot(Not_Dengue$Total)
boxplot(log10(Not_Dengue$Total))

#Analisando as Internações
summary(Internacoes_Doencas)
boxplot(Internacoes_Doencas)

#Parece que os campos Sexo, Numero de Filhos e Idade está inconsistente, vamos retirar!
#Sumarizar apenas o ano, retirando os meses
as.data.frame(Internacoes_Doencas)

#cria as variaveis
doencas <- Internacoes_Doencas %>%
  mutate(
    TOTAL = QTD,
    Tripanossomíase = DIAG_PRINC %in% c("B56" ,"B560" ,"B561" ,"B569" ,"B57" , "B570", "B571", "B572", "B573", "B574", "B575")*QTD,
    Dengue = DIAG_PRINC %in% c("A90" , "A91")*QTD,
    Dracunculíase = DIAG_PRINC %in% c("B72")*QTD,
    Equinococose = DIAG_PRINC %in% c("B67","B670","B671","B672","B673","B674","B675","B676","B677","B678","B679")*QTD,
    Leishmaniose = DIAG_PRINC %in% c("B55", "B550", "B551", "B552", "B559")*QTD,
    Hanseníase = DIAG_PRINC %in% c("A30", "A300", "A301", "A302", "A303", "A304", "A305", "A306", "A307", "A308", "A309", "B92")*QTD,
    Micoses = DIAG_PRINC %in% c("B350","B351","B352","B353","B354","B355","B356","B357","B358","B359","B360","B361","B362","B363","B364","B365","B366","B367","B368","B369","B370","B371","B372","B373","B374","B375","B376","B377","B378","B379","B380","B381","B382","B383","B384","B385","B386","B387","B388","B389","B390","B391","B392","B393","B394","B395","B396","B397","B398","B399","B400","B401","B402","B403","B404","B405","B406","B407","B408","B409","B410","B411","B412","B413","B414","B415","B416","B417","B418","B419","B420","B421","B422","B423","B424","B425","B426","B427","B428","B429","B430","B431","B432","B433","B434","B435","B436","B437","B438","B439","B440","B441","B442","B443","B444","B445","B446","B447","B448","B449","B450","B451","B452","B453","B454","B455","B456","B457","B458","B459","B460","B461","B462","B463","B464","B465","B466","B467","B468","B469","B470","B471","B472","B473","B474","B475","B476","B477","B478","B479","B480","B481","B482","B483","B484","B485","B486","B487","B488","B489","B490","B491","B492","B493","B494","B495","B496","B497","B498","B499","B35","B36","B37","B38","B39","B40","B41","B42","B43","B44","B45","B46","B47","B48","B49")*QTD,
    Oncocercose = DIAG_PRINC %in% c("B73")*QTD,
    Raiva = DIAG_PRINC %in% c("A82","A820","A821","A829")*QTD,
    Esquistossomose = DIAG_PRINC %in% c("B65","B650","B651","B652","B653","B658","B659")*QTD,
    Tracoma = DIAG_PRINC %in% c( "A71" , "A710" , "A711" , "A719")*QTD,
    Leptospirose = DIAG_PRINC %in% c("A27","A270","A278","A279")*QTD,
    Helmintíases = DIAG_PRINC %in% c("B68","B680","B681","B689","B69","B690","B691","B698","B699","B70","B700","B701","B71","B710","B711","B718","B719","B75","B77","B770","B778","B779","B78","B780","B781","B787","B789","B79","B80","B81","B810","B811","B812","B813","B814","B818","B82","B820","B829","B83","B830","B831","B832","B833","B834","B838","B839")*QTD,
    Ancilostomíase = DIAG_PRINC %in% c("B76","B760","B761","B768","B769")*QTD
  )


#soma as variaveis
doencas <- doencas %>%
  group_by(MUNIC_RES)%>%
  summarise(
    TOTAL = sum(TOTAL),
    Tripanossomíase = sum(Tripanossomíase),
    Dengue = sum(Dengue),
    Dracunculíase = sum(Dracunculíase),
    Equinococose = sum(Equinococose),
    Leishmaniose = sum(Leishmaniose),
    Hanseníase = sum(Hanseníase),
    Micoses = sum(Micoses),
    Oncocercose = sum(Oncocercose),
    Raiva = sum(Raiva),
    Esquistossomose = sum(Esquistossomose),
    Tracoma = sum(Tracoma),
    Leptospirose = sum(Leptospirose),
    Helmintíases = sum(Helmintíases),
    Ancilostomíase = sum(Ancilostomíase)
  )

#Cruzando tabelas
#tabelao<-left_join(x=tabelao,y=doencas,by=c("cod_datasus"="MUNIC_RES"))

#Cruzando as Notificacoes
tabelao<-left_join(x=tabelao,y=Not_Dengue,by=c("cod_datasus"="Cod"))

#tabelao <- tabelao %>%
#  mutate(Incidencia = log10((Dengue/Total)*100000))

tabelao <- tabelao %>%
  mutate(Incidencia = log10((Dengue/Total)*100000))

#tabela2 <- tabelao[,c(-7,-8,-14,-15,-23,-24,-28,-29,-32,-33,-36,-37,-52,-53,-60,-61,-63,-64,-67,-68,-71,-72,-80,-81,-90,-91,-101,-102,-103,-104,-105,-106,-107,-108,-109,-110,-111,-112,-113,-114,-115,-116,-117,-118,-119,-120,-121,-122)]
tabela2 <- tabelao[,c(-7,-8,-14,-15,-23,-24,-28,-29,-32,-33,-36,-37,-52,-53,-60,-61,-63,-64,-67,-68,-71,-72,-80,-81,-90,-91,-101,-102,-103,-104,-105,-106,-107,-108)]

tabela_total <- tabela2[,c(-1,-2)]

avaliar <- tabela2 %>%
  filter(Incidencia > 80)


doencas %>%
  summarise(TOTAL = sum(TOTAL))


#modelos:
#Regressão Linear para achar uma fórmula ou índice de Doenças Negligenciadas.
#Clusterização(K-means) para identificar cidades parecidas.
#Regressão logistica para classificar em 4 níveis igual o IVV (Indice de Vulnerabilidade)

#variáveis mais importantes
#Modelo a partir de 10 mil
#extrapolação dos municipios.
#thamygioia@gmail.com
#estado de goias código 52

#write.table(tabelao, file="tabelao.csv", sep=",")

as.data.frame(tabela_total)
#retira os NA da tabela
tabela_total[is.na(tabela_total)] <- 0
#filtras apenas os Incidencia maior que 0
tabela_total <- tabela_total %>%
  filter(Incidencia > 0)

boxplot(tabela_total$Incidencia)
summary(tabela_total$Incidencia)
describe(tabela_total$Incidencia)
sd(tabela_total$Incidencia)

tabela_total <- tabela_total %>%
  mutate(Inc =
          case_when(
          (Incidencia <= log10(200)) ~ "Baixa",
          (Incidencia <= log10(1000) & Incidencia > log10(200)) ~ "Media",
          (Incidencia > log10(1000)) ~ "Alta")
         )

table(tabela_total$Inc)
tabela_total$Inc <- as.factor(tabela_total$Inc)


#Analisando os tipos de Domicilio
boxplot(tabela_total$Unipessoal)#OK
boxplot(tabela_total$Nuclear)#OK
boxplot(log10(tabela_total$Estendida))#OK (alguns outliers superiores)
boxplot(tabela_total$Composta)#OK Retirar
boxplot(tabela_total$Unipessoal,tabela_total$Nuclear,tabela_total$Estendida,tabela_total$Composta)

#Analisando os tipos de Residência
boxplot(tabela_total$Casa) #OK variável com muitos outliers inferiores
boxplot(tabela_total$`Casa de vila ou em condomínio`)#OK variável com muitos outliers superiores
boxplot(tabela_total$Apartamento)#OK variável com muitos outliers superiores
boxplot(tabela_total$`Habitação em casa de cômodos, cortiço ou cabeça de porco`)#OK variável com muitos outliers superiores
boxplot(tabela_total$`Oca ou maloca`)#OK variável com muitos outliers superiores

boxplot(tabela_total$`Oca ou maloca`+tabela_total$`Habitação em casa de cômodos, cortiço ou cabeça de porco`)

#Analisando os tipos de Esgotamento Sanitário
boxplot(tabela_total$`Rede geral de esgoto ou pluvial`) #OK
boxplot(tabela_total$`Fossa séptica`)#OK retirar
boxplot(tabela_total$`Fossa rudimentar`) #OK
boxplot(tabela_total$Vala) #OK variável com muitos outliers superiores
boxplot(tabela_total$`Rio, lago ou mar`) #OK variável com muitos outliers superiores
boxplot(tabela_total$`Outro tipo`) #OK retirar
boxplot(tabela_total$`Não tinham`) #OK variável com muitos outliers superiores


boxplot(tabela_total$`Não tinham`+tabela_total$`Outro tipo`+tabela_total$`Rio, lago ou mar`+tabela_total$Vala)

#Analisando os tipos de Sanitário
boxplot(tabela_total$`Tinham banheiro de uso exclusivo do domicílio`)
boxplot(tabela_total$`Tinham sanitário`)
boxplot(tabela_total$`Não tinham banheiro nem sanitário`)


boxplot(log10(tabela_total$`Tinham sanitário`+tabela_total$`Não tinham banheiro nem sanitário`))

#Analisando os tipos de Proporção Urbana Rural
boxplot(tabela_total$Urbana) #OK
boxplot(tabela_total$Rural) #Posso excluir já que possuo a urbana (possui zero)

#Analisando os tipos de Proporção de Homens e Mulheres
boxplot(tabela_total$Homens) #OK
boxplot(tabela_total$Mulheres) #Posso excluir já que possuo a prop Homem


#Analisando a distribuição de Idades
boxplot(tabela_total$`0 a 4 anos`)
boxplot(tabela_total$`5 a 9 anos`)
boxplot(tabela_total$`10 a 14 anos`)
boxplot(tabela_total$`15 a 19 anos`)
boxplot(tabela_total$`20 a 24 anos`)
boxplot(tabela_total$`25 a 29 anos`)
boxplot(tabela_total$`30 a 34 anos`)
boxplot(tabela_total$`35 a 39 anos`)
boxplot(tabela_total$`40 a 44 anos`)
boxplot(tabela_total$`45 a 49 anos`)
boxplot(tabela_total$`50 a 54 anos`)
boxplot(tabela_total$`55 a 59 anos`)
boxplot(tabela_total$`60 a 69 anos`)
boxplot(tabela_total$`70 anos ou mais`)


#Analisando a distribuição de Raças
boxplot(tabela_total$Branca) #OK
boxplot(tabela_total$Preta)
boxplot(tabela_total$Amarela)
boxplot(tabela_total$Parda) #OK
boxplot(tabela_total$Indígena)
boxplot(tabela_total$`Sem declaração`)


#Analisando a distribuição de Alfabetizados com mais de 5 anos
boxplot(tabela_total$Alfabetizacao) #OK

#Analisando a distribuição de coleta de lixos
boxplot(tabela_total$Coletado) #OK
boxplot(tabela_total$`Outro destino`) #Posso excluir pq já tenho o Coletado (possue zero)

#Analisando a Energia Elétrica
boxplot(tabela_total$`Tinham Enegia Elétrica`) #OK Possui muitos Outliers Inferiores
boxplot(tabela_total$`Não tinham Enegia Elétrica`) #Possui muitos zeros


#Analisando o acesso a água
boxplot(tabela_total$`Rede geral`) #Ok alguns outliers inferios, mas de maneira geral é bom
boxplot(tabela_total$`Poço ou nascente na propriedade`)
boxplot(tabela_total$`Poço ou nascente fora da propriedade`)
boxplot(tabela_total$`Rio, açude, lago ou igarapé`)
boxplot(tabela_total$`Poço ou nascente na aldeia`)
boxplot(tabela_total$`Poço ou nascente fora da aldeia`)
boxplot(tabela_total$Outra)



boxplot(tabela_total$`Poço ou nascente na propriedade`+tabela_total$`Poço ou nascente fora da propriedade`)

#Analisando a renda per caita por domicilio
boxplot(tabela_total$`Até 1/4 de salário mínimo Per Capita Por Domc`) #OK
boxplot(tabela_total$`Mais de 1/4 a 1/2 salário mínimo  Per Capita Por Domc`)
boxplot(tabela_total$`Mais de 1/2 a 1 salário mínimo Per Capita Por Domc`)
boxplot(tabela_total$`Mais de 1 a 2 salários mínimos Per Capita Por Domc`) #OK
boxplot(tabela_total$`Mais de 2 a 3 salários mínimos Per Capita Por Domc`)
boxplot(tabela_total$`Mais de 3 a 5 salários mínimos Per Capita Por Domc`)
boxplot(tabela_total$`Mais de 5 salários mínimos Per Capita Por Domc`)
boxplot(tabela_total$`Sem rendimento Per Capita Por Domc`)

tabela_total <- tabela_total[,c(-4,-5,-6,-7,-8,-9,-11,-13,-14,-15,-16,-21,-23,-39,-40,-42,-43,-46,-48)]

#Analisando a renda nominal por domicilio
boxplot(tabela_total$`Até 1/2 salário mínimo Nominal por Domc`) #Ok
boxplot(tabela_total$`Mais de 1/2 a 1 salário mínimo Nominal por Domc`)
boxplot(tabela_total$`Mais de 1 a 2 salários mínimos Nominal por Domc`)
boxplot(tabela_total$`Mais de 2 a 5 salários mínimos Nominal por Domc`)
boxplot(tabela_total$`Mais de 5 a 10 salários mínimos Nominal por Domc`)
boxplot(tabela_total$`Mais de 10 a 20 salários mínimos Nominal por Domc`)
boxplot(tabela_total$`Mais de 20 salários mínimos Nominal por Domc`)
boxplot(tabela_total$`Sem rendimento Nominal por Domc`)
boxplot(tabela_total$`Sem declaração Nominal por Domc`)


summary(tabela_total)


boxplot(log10(tabela_total$Incidencia))
summary(log10(tabela_total$Incidencia))



which(tabela_total$`Fossa séptica`>0.3) #descobrir o outliers

#Divide em treino e teste
set.seed(123)
smp_size <- floor(0.75 * nrow(tabela_total))

ind <- sample(seq_len(nrow(tabela_total)), size = smp_size)

train <- tabela_total[ind, ]
test <- tabela_total[-ind, ]

#Regressão Linear 
lm <- lm(Incidencia ~ . -1, data = train)
summary(lm)
lm1 <- step(lm)
summary(lm1)
vif(lm1)

lm2 <- lm(formula = Incidencia ~ -1 + `Rede geral de esgoto ou pluvial` + 
            `Fossa rudimentar` + 
            `Tinham sanitário` + `Não tinham banheiro nem sanitário` + 
            Branca + `Poço ou nascente na propriedade` + 
            `Poço ou nascente fora da propriedade` + 
            `Mais de 5 salários mínimos Per Capita Por Domc` + `Sem rendimento Per Capita Por Domc` + 
            `Até 1/2 salário mínimo Nominal por Domc` ,
            data = train)

summary(lm2)
vif(lm2)

lm2

predito_lm <- tabela2
predito_lm$predito = predict(lm2,tabela2)
predito_lm <- predito_lm %>%
  arrange(Incidencia)

predito_lm[is.na(predito_lm)] <- 0

#Transforma a resposta e a Incidencia em Categoria
#85<, 85 a 144, 144 a 239, >239
predito_lm <- predito_lm %>%
  mutate(Label_Incidencia =
           case_when(
             (Incidencia <= log10(150)) ~ "1 - Baixa",
             #(Incidencia <= log10(250) & Incidencia > log10(150)) ~ "2 - Media",
             (Incidencia > log10(150)) ~ "3 - Alta"),
         
         Label_predito =
           case_when(
             (predito <= log10(150)) ~ "1 - Baixa",
            # (predito <= log10(250) & predito > log10(150)) ~ "2 - Media",
             (predito > log10(150)) ~ "3 - Alta")
  )

write.table(predito_lm, file="predito_lm.csv", sep=",")

boxplot(predito_lm$Incidencia,predito_lm$predito)
summary(10^(predito_lm$Incidencia))
summary(10^(predito_lm$predito))

table(predito_lm$Label_Incidencia)
table(predito_lm$Label_predito)

#Filtrar
base <- predito_lm %>%
filter(Incidencia > log10(250))

tabela_teste = table(predito_lm$Label_Incidencia,predito_lm$Label_predito)
tabela_teste


Acuracia = (tabela_teste[1,1] + tabela_teste[2,2] + tabela_teste[3,3])/(
    (tabela_teste[1,1] + tabela_teste[1,2] + tabela_teste[1,3]) +
    (tabela_teste[2,1] + tabela_teste[2,2] + tabela_teste[2,3]) +
    (tabela_teste[3,1] + tabela_teste[3,2] + tabela_teste[3,3]) )*100
Acuracia

Acuracia = (tabela_teste[1,1] + tabela_teste[2,2])/(
  (tabela_teste[1,1] + tabela_teste[1,2])  +
    (tabela_teste[2,1] + tabela_teste[2,2] ))*100
Acuracia


plot(predito_lm$Incidencia,predito_lm$predito)

#Erro Percentual
predito_lm$residuo=predito_lm$Incidencia-predito_lm$predito
predito_lm$ep = predito_lm$residuo/predito_lm$Incidencia*100
predito_lm <- predito_lm %>%
  arrange(ep)
summary(predito_lm$ep)
plot(predito_lm$ep, pch=20)
grid(col = 4)

#analisando o residuo
summary(predito_lm$residuo)
boxplot(predito_lm$residuo)

#analisando o erro percentual
summary(predito_lm$ep)
boxplot(predito_lm$ep)

#Mesmos após a seleção de variáveis o R-quadrado ficou em 0,09
#vamos para regressão Generalizada
#Regressão Linear Generalizada
glm <- glm(Inc ~ . -Incidencia, data = train,family = poisson(link="log"))
summary(glm)
glm1 <- step(glm)
summary(glm1)
#VIF maior que 10 é ruim
options(scipen=999)
vif(glm1)
predito_glm <- predict(glm,train)

glm2 <- glm(formula = Incidencia ~ Composta + 
      `Fossa rudimentar` + 
      `Habitação em casa de cômodos, cortiço ou cabeça de porco` + 
      `Mais de 3 a 5 salários mínimos Per Capita Por Domc` + 
      `Mais de 5 salários mínimos Per Capita Por Domc` + 
      Parda + 
      `Poço ou nascente fora da propriedade` + 
      `Rio, açude, lago ou igarapé` + 
      Unipessoal, 
    data = train)

vif(glm2)
summary(glm2)

glm2$coefficients

library(AUC)
predito_glm <- train
predito_glm$predito <- predict(glm2,train)

#Erro Percentual
predito_glm$residuo=predito_glm$Incidencia-predito_glm$predito
predito_glm$ep = predito_glm$residuo/predito_glm$Incidencia*100
predito_glm <- predito_glm %>%
  arrange(ep)
summary(predito_glm$ep)
plot(predito_glm$ep, pch=20)
grid(col = 4)

#analisando o residuo
summary(predito_glm$residuo)
boxplot(predito_glm$residuo)

#analisando o erro percentual
summary(predito_glm$ep)
boxplot(predito_glm$ep)



require(leaps)
rs = summary(regsubsets(Incidencia~.,nbest=2,data=train))

#Clusterizacao K-means
wss <- (nrow(tabela_total)-1)*sum(apply(tabela_total,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(tabela_total,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Número of Clusters",
     ylab="Soma dos quadrados dentro dos clusteres") 

#Desenhando os cluster
dendo <- tabela_total %>% dist %>% hclust
plot(dendo)
rect.hclust(dendo, k = 3, border = "blue")
rect.hclust(dendo, k = 4, border = "red")

#Rodar modelo sem hanseniase

#Divide em treino e teste
set.seed(123)
table(tabela_total$Inc)
Alta <- tabela_total %>%
  filter(Inc == "Alta" )
Alta <- Alta[sample(seq_len(nrow(Alta)), size = 2994, replace = TRUE), ]

Media <- tabela_total %>%
  filter(Inc == "Media" )
Media <- Media[sample(seq_len(nrow(Media)), size = 2994, replace = TRUE), ]

Baixa <- tabela_total %>%
  filter(Inc == "Baixa" )

Juntas <- bind_rows(Baixa,Media,Alta)

smp_size <- floor(0.75 * nrow(Juntas))

ind <- sample(seq_len(nrow(Juntas)), size = smp_size)

train <- Juntas[ind, ]
test <- Juntas[-ind, ]

table(train$Inc)
table(test$Inc)

#Rodando a arvore
library(rpart)
library(rpart.plot)
library(gmodels)
library(hmeasure)

#control = rpart.control(minsplit = 100)
ad1= rpart (data=train, Inc~. -Incidencia, method = "class") 
ad1
summary(ad1)
prp (ad1, type =3, extra=104,nn=T, fallen.leaves = T, branch.col = " red ", branch.lty = 5,box.col = c(" white ",' green ')) 
printcp(ad1)
phat_test=predict(ad1, newdata = train,type = "prob") 
yhat_test=predict(ad1, newdata = train,type = "class")

predito_arv <- test
predito_arv$predito = predict(ad1, newdata = test,type = "class")
#HMeasure(predito_arv$predito,predito_arv$Inc)$metrics
CrossTable(predito_arv$predito,predito_arv$Inc)

ad2= rpart (data=train, Incidencia~.) 
ad2
predito_arv <- train
predito_arv$predito = predict(ad2, newdata = train)

#Testando PCA
teste <- tabela_total[,c(-72,-74)]
pca <- prcomp(teste, center = TRUE,scale. = TRUE)
pca
summary(pca)

library(ggfortify)

autoplot(pca)

autoplot(pca, data = tabela_total, colour = 'Incidencia',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

