### The Economic Consequences of Increasing Sleep

### Increasing Sleep


########### RCT's

# Librerias

#install.packages("devtools")
#library(devtools)
#install_github("isidorogu/RCT")
#library(Matching)
library(RCT)
library(stargazer)
library(tidyverse)
library(dplyr)
library(sandwich)
library(fixest)
library(pacman)
library(EnvStats)
library(kableExtra)
library(haven)
library(MatchIt)
library(plm)
library(ggplot2)
library(gplots)
library(ggpubr)





# setwd("d:/Users/guillermo_reza/Desktop/Maestría/Semestre Ene-Jun 2022/Econometría Aplicada II/Tarea 3")
baseline <- read.csv("baseline.csv")
endline <- read.csv("endline.csv")
completa <- read.csv("completa.csv")


#### 1) 2 X 2

## Se integran las medias del grupo de tratamiento T_nap para el antes (baseline)
## y el después (endline).


## baseline

antes_grupo_0 <- baseline %>%
  filter(T_nap == 0, !is.na(productivity)) %>%
  pull(productivity) %>%
  mean()


antes_grupo_1 <- baseline %>%
  filter(T_nap == 1, !is.na(productivity)) %>%
  pull(productivity) %>%
  mean()



## endline

despues_grupo_0 <- endline %>%
  filter(T_nap == 0, !is.na(productivity)) %>%
  pull(productivity) %>%
  mean()


despues_grupo_1 <- endline %>%
  filter(T_nap == 1, !is.na(productivity)) %>%
  pull(productivity) %>%
  mean()


## Tabla 2X2

Tabla_2X2 <- c("Control","Treat")
Tabla_TOT <- c("Control","Treat","TOT")
Antes <- c(antes_grupo_0,antes_grupo_1)
Despues <- c(despues_grupo_0,despues_grupo_1)
Diferencia <- c(despues_grupo_0 - antes_grupo_0, despues_grupo_1 - antes_grupo_1,
                (despues_grupo_1 - antes_grupo_1)-(despues_grupo_0 - antes_grupo_0))

stargazer(cbind(Tabla_2X2,Antes,Despues),type = "text")

stargazer(rbind(Tabla_TOT,Diferencia),type = "text")


## Esta tabla puede obtenerse si se juntan las bases de datos de baseline y de 
## 3 endline, después correr un modelo OLS con interacción entre el tratamiento
## y la variable day_in_study





## 2) Tabla 2X2 Apartado B

# completa_panel <- pdata.frame(completa, index =c("pid","day_in_study")) 

# completa_panel$sleep_night_mean <- plm::Between(completa_panel$sleep_night, na.rm = TRUE)

# completa_panel$media_panel <- plm::Between(completa_panel$productivity, na.rm = TRUE)

# mean(completa_panel$media_panel,na.rm = "TRUE")



# completa_antes_C <- subset(completa_panel, (T_nap==0 & day_in_study < 8))


## Antes

antes_compl_0 <- completa %>%
  filter(T_nap == 0, day_in_study < 8, !is.na(productivity)) %>%
  group_by(pid) %>% 
  nest() %>% 
  mutate(media = map(data, ~mean(.x$productivity,na.rm = TRUE))) %>% 
  unnest(media) %>% 
  pull(media) %>% mean()

antes_compl_1 <- completa %>%
  filter(T_nap == 1, day_in_study < 8, !is.na(productivity)) %>%
  group_by(pid) %>% 
  nest() %>% 
  mutate(media = map(data, ~mean(.x$productivity,na.rm = TRUE))) %>% 
  unnest(media) %>% 
  pull(media) %>% mean()


## Después

despues_compl_0 <- completa %>%
  filter(T_nap == 0, day_in_study >= 8, !is.na(productivity)) %>%
  group_by(pid) %>% 
  nest() %>% 
  mutate(media = map(data, ~mean(.x$productivity,na.rm = TRUE))) %>% 
  unnest(media) %>% 
  pull(media) %>% mean()


despues_compl_1 <- completa %>%
  filter(T_nap == 1, day_in_study >= 8, !is.na(productivity)) %>%
  group_by(pid) %>% 
  nest() %>% 
  mutate(media = map(data, ~mean(.x$productivity,na.rm = TRUE))) %>% 
  unnest(media) %>% 
  pull(media) %>% mean()


## Tabla de Resultados

Antes_panel <- c(antes_compl_0,antes_compl_1)
Despues_panel <- c(despues_compl_0,despues_compl_1)
Diferencia_panel <- c(despues_compl_0 - antes_compl_0, despues_compl_1 - antes_compl_1,
                (despues_compl_1 - antes_compl_1)-(despues_compl_0 - antes_compl_0))

stargazer(cbind(Tabla_2X2,Antes_panel,Despues_panel),type = "text")

stargazer(rbind(Tabla_TOT,Diferencia_panel),type = "text")





## Regresión OLS

completa <- completa %>% 
  mutate(despues = ifelse(day_in_study > 7,1,0),
         age=age_, female = female_, education = education_)


# Regresión sin controles, errores heteroscedásticos

reg_1 <- lm(formula = productivity ~ T_nap + despues + (T_nap*despues),
            data = completa)

cov_reg1 <- vcovHC(reg_1, type = "HC")

robust.se_reg1 <- sqrt(diag(cov_reg1))


stargazer(reg_1,se=list(robust.se_reg1),
          column.labels = c("OLS S/C"),type="text")



# Regresión con Controles, errores heteroscedásticos

reg_2 <- lm(formula = productivity ~ T_nap + despues + (T_nap*despues) +
              age + female + education, data = completa)

cov_reg2 <- vcovHC(reg_2, type = "HC")

robust.se_reg2 <- sqrt(diag(cov_reg2))



## Tabla de Regresión

stargazer(reg_1, reg_2,se=list(robust.se_reg1,robust.se_reg2), 
          column.labels = c("OLS S/C","OLS Controles"), type = "text")





Antes_reg <- c(reg_1$coefficients[1],(reg_1$coefficients[1]+reg_1$coefficients[2]))
Despues_reg <- c((reg_1$coefficients[1]+reg_1$coefficients[3]),
                   (reg_1$coefficients[1]+reg_1$coefficients[2]+
                      reg_1$coefficients[3]+reg_1$coefficients[4]))
Diferencia_reg <- c(reg_1$coefficients[3], (reg_1$coefficients[3]+reg_1$coefficients[4]),
                      (reg_1$coefficients[3]+reg_1$coefficients[4])-(reg_1$coefficients[3]))

stargazer(cbind(Tabla_2X2,Antes_reg,Despues_reg),type = "text")

stargazer(rbind(Tabla_TOT,Diferencia_reg),type = "text")



# Se usaron errores heteroscedásticos porque los individuos tienen diferentes tratamientos
# además del T_nap. Esto hace que aun dentro del grupo control, los individuos no sean homogéneos
# entre ellos. Por lo tanto se tomaron los errores heteroscedásticos.




## 3) Comparación de Resultados con la Pregunta 2 de la Tarea 1

# El primer estimador que se obtuvo considerando las dos bases de datos, baseline y endline
# fue el TOT considerando el grupo de control y el grupo de tratamiento para los primeros 7 días
# y después de los 7 días y hasta terminar con el tratamiento. El estimador del TOT 
# para la Tabla 2X2 considerando las bases de datos por separado da un TOT de 87.55 unidades de
# productividad mayor para el grupo de tratamiento.

# Cuando se consideró la estimación del TOT a partir de la base de datos completa, 
# el resultado fue similiar con 87.55 unidades de productividad más para el grupo de tratamiento
# esta diferencia se debe principalmente a que el número de observaciones en las bases
# por separado es 414 y en la base completa es de 452. Las diferencias se pueden observar 
# en el grupo de control antes de iniciar la intervención que el número es ligeramente 
# menor cuando se considera la base completa.

# La tercera estimación se hizo por medio de un modelo OLS con y sin controles. El resultado de
# esta regresión es un TOT menor a las dos estimaciones anteriores con solo 48.95 puntos 
# de productividad más para el grupo de tratamiento que para el grupo de control. Esta estimación consideró 
# erores heteroscedásticos por las características de los individuos en los tratamientos.
# por la naturaleza del experimento hubo individuos que fueron parte de un segundo tratamiento
# mientras que otros no. Por lo tanto, es conveniente pensar en errores heteroscedásticos.

# Al comparar los resultados del TOT con los resultados del inciso 2 de la tarea 1
# nos podemos dar cuenta que en principio, el TOT no es igual al ATE. El ATE considera el efecto
# del contrafactual del tratamiento y el del control. En el TOT solo se considera la parte del
# tratamiento con respecto a su contrafactual. Por esta razón, las estimaciones difieren. 
# El primer estimador es de Neyman de la tarea 1 es 170 considerando errores heteroscedásticos,
# el resultado es 170 que es casi el doble del TOT calculado en la tabla 2X2. La segunda
# estimación es la del OLS con y sin controles considerando nuevamente errores heteroscedásticos.
# Esta estimación del ATE es parecida a la de Neyman y por lo tanto es casi el doble de
# la estimación obtenida por el OLS considerando la base completa. En ambos casos, la 
# principal diferencia entre los resultados es el problema de Atrición y de la falta de 
# datos en cada uno de los días. Es por esto que los resultados resultan ser diferentes.





## 4) Gráficas A.V & A.VI


## Gráfica V.1 sleep_night

panel_1<- completa %>%
  filter(T_nap == 1, !is.na(sleep_night)) %>%
  group_by(day_in_study) %>% 
  nest() %>% 
  mutate(media_1 = map(data, ~mean(.x$sleep_night,na.rm = TRUE))) %>% 
  unnest(media_1)


graph_sleep_night <- ggplot(data = panel_1, aes(x= day_in_study, y=media_1)) +
  geom_line(linetype = "dashed")+
  geom_vline(aes(xintercept = 8.2))+
  ggtitle("(a) Night Sleep") +
  scale_x_continuous(name="Day in Study", limits=c(2, 30)) +
  scale_y_continuous(name="Night Sleep", limits=c(5.4,6))  +
  theme(plot.title = element_text(hjust = 0.5))


## Gráfica V.2 time_in_bed (act_inbed)

panel_2<- completa %>%
  filter(T_nap == 1, !is.na(act_inbed)) %>%
  group_by(day_in_study) %>% 
  nest() %>% 
  mutate(media_2 = map(data, ~mean(.x$act_inbed,na.rm = TRUE))) %>% 
  unnest(media_2)


graph_time_in_bed <- ggplot(data = panel_2, aes(x= day_in_study, y=media_2)) +
  geom_line(linetype = "dashed")+
  geom_vline(aes(xintercept = 8.2))+
  ggtitle("(b) Time in Bed") +
  scale_x_continuous(name="Day in Study", limits=c(2, 30)) +
  scale_y_continuous(name="Time in Bed (Hours)", limits=c(7.9,9))  +
  theme(plot.title = element_text(hjust = 0.5))



## Gráfica V.3 sleep_efficiency (sleep_eff)

panel_3 <- completa %>%
  filter(T_nap == 1, !is.na(sleep_eff)) %>%
  group_by(day_in_study) %>% 
  nest() %>% 
  mutate(media_3 = map(data, ~mean(.x$sleep_eff,na.rm = TRUE))) %>% 
  unnest(media_3)


graph_sleep_efficiency <- ggplot(data = panel_3, aes(x= day_in_study, y=media_3)) +
  geom_line(linetype = "dashed")+
  geom_vline(aes(xintercept = 8.2))+
  ggtitle("(c) Sleep Efficiency") +
  scale_x_continuous(name="day_in_study", limits=c(2, 30)) +
  scale_y_continuous(name="Sleep Efficiency", limits=c(2.51,2.519))  +
  theme(plot.title = element_text(hjust = 0.5))



## Gráfica V.4 Nap Sleep (nap_time_mins)

panel_4 <- completa %>%
  filter(T_nap == 1, !is.na(nap_time_mins)) %>%
  group_by(day_in_study) %>% 
  nest() %>% 
  mutate(media_4 = map(data, ~mean(.x$nap_time_mins,na.rm = TRUE))) %>% 
  unnest(media_4)


graph_nap_sleep <- ggplot(data = panel_4, aes(x= day_in_study, y=media_4)) +
  geom_line(linetype = "dashed")+
  geom_vline(aes(xintercept = 8.2))+
  ggtitle("(d) Nap Sleep") +
  scale_x_continuous(name="day_in_study", limits=c(2, 30)) +
  scale_y_continuous(name="Nap Sleep", limits=c(12.5,15))  +
  theme(plot.title = element_text(hjust = 0.5))


## Gráfica Conjunta

g1 <- ggarrange(graph_sleep_night, graph_time_in_bed, graph_sleep_efficiency,
          graph_nap_sleep,ncol = 2, nrow = 2)


ggsave (g1 , filename = 'FIGURE A.V.pdf ',device = "pdf")



## Gráfica VI.1 Earnings


completa_means <- completa %>%
  filter(T_nap == 1, day_in_study > 8) %>% 
  mutate(dias = case_when((day_in_study>8 & day_in_study<=12) ~ "9-12",
                          (day_in_study>12 & day_in_study<=15) ~ "13-15",
                          (day_in_study>15 & day_in_study<=18) ~ "16-18",
                          (day_in_study>18 & day_in_study<=21) ~ "19-21",
                          (day_in_study>21 & day_in_study<=24) ~ "22-24",
                          (day_in_study>24 & day_in_study<=30) ~ "25-28",
                          TRUE ~ "otro"))
           
## Media de cada grupo

datos_earnings <- completa_means %>%
  filter(T_nap == 1, !is.na(earnings)) %>%
  group_by(dias) %>% 
  nest() %>% 
  mutate(media_earnings = map(data, ~mean(.x$earnings,na.rm = TRUE)-240)) %>%
  unnest(media_earnings)

## Desviación Estándar de cada grupo

datos_earnings_sd <- completa_means %>%
  filter(T_nap == 1, !is.na(earnings)) %>%
  group_by(dias) %>% 
  nest() %>% 
  mutate(sd_earnings = map(data, ~sd(.x$earnings,na.rm = TRUE))) %>%
  unnest(sd_earnings)


## Se une la columna de errores estándar a la de medias por grupo

datos_earnings$sd <- datos_earnings_sd$sd_earnings


## Total de observaciones

n_earnings <- completa_means %>%
  filter(T_nap == 1, !is.na(earnings)) %>% nrow()


## Se crean los Errores Estándar

datos_earnings <- datos_earnings %>% 
  mutate(se = sd/sqrt(n_earnings))




graph_earnings <- ggplot(data = datos_earnings,
                         aes(x= dias, y=media_earnings)) +
  geom_point(shape=24, fill="blue")+
  geom_errorbar(data=datos_earnings,
                mapping=aes(x=dias, ymin=media_earnings-se,
                            ymax=media_earnings+se), color="blue")+
  geom_hline(aes(yintercept = 0))+
  ggtitle("(a) Earnings")+ 
  scale_x_discrete(name="Days in Study") +
  scale_y_continuous(name="Earnings")  +
  theme(plot.title = element_text(hjust = 0.5))




## Gráfica VI.2 Hours Typing  (typing_time_hr)


## Media de cada grupo

datos_typing_time_hr <- completa_means %>%
  filter(T_nap == 1, !is.na(typing_time_hr)) %>%
  group_by(dias) %>% 
  nest() %>% 
  mutate(media_typing_time_hr = map(data, ~mean(.x$typing_time_hr,na.rm = TRUE)/4)) %>%
  unnest(media_typing_time_hr)

## Desviación Estándar de cada grupo

datos_typing_time_hr_sd <- completa_means %>%
  filter(T_nap == 1, !is.na(typing_time_hr)) %>%
  group_by(dias) %>% 
  nest() %>% 
  mutate(sd_typing_time_hr = map(data, ~sd(.x$typing_time_hr,na.rm = TRUE))) %>%
  unnest(sd_typing_time_hr)


## Se une la columna de errores estándar a la de medias por grupo

datos_typing_time_hr$sd <- datos_typing_time_hr_sd$sd_typing_time_hr


## Total de observaciones

n_typing_time_hr <- completa_means %>%
  filter(T_nap == 1, !is.na(typing_time_hr)) %>% nrow()


## Se crean los Errores Estándar

datos_typing_time_hr <- datos_typing_time_hr %>% 
  mutate(se = sd/sqrt(n_typing_time_hr))




graph_typing_time_hr <- ggplot(data = datos_typing_time_hr,
                         aes(x= dias, y=media_typing_time_hr)) +
  geom_point(shape=24, fill="blue")+
  geom_errorbar(data=datos_typing_time_hr,
                mapping=aes(x=dias, ymin=media_typing_time_hr-se,
                            ymax=media_typing_time_hr+se), color="blue")+
  geom_hline(aes(yintercept = 1))+
  ggtitle("(b) Hours Typing")+ 
  scale_x_discrete(name="Days in Study") +
  scale_y_continuous(name="Time Typing (Minutes)")  +
  theme(plot.title = element_text(hjust = 0.5))


## Gráfica VI.3 Productivity  (productivity)


## Media de cada grupo

datos_productivity <- completa_means %>%
  filter(T_nap == 1, !is.na(productivity)) %>%
  group_by(dias) %>% 
  nest() %>% 
  mutate(media_productivity = map(data, ~mean(.x$productivity,na.rm = TRUE)-3100)) %>%
  unnest(media_productivity)

## Desviación Estándar de cada grupo

datos_productivity_sd <- completa_means %>%
  filter(T_nap == 1, !is.na(productivity)) %>%
  group_by(dias) %>% 
  nest() %>% 
  mutate(sd_productivity = map(data, ~sd(.x$productivity,na.rm = TRUE))) %>%
  unnest(sd_productivity)


## Se une la columna de errores estándar a la de medias por grupo

datos_productivity$sd <- datos_productivity_sd$sd_productivity


## Total de observaciones

n_productivity <- completa_means %>%
  filter(T_nap == 1, !is.na(productivity)) %>% nrow()


## Se crean los Errores Estándar

datos_productivity <- datos_productivity %>% 
  mutate(se = sd/sqrt(n_productivity))




graph_productivity <- ggplot(data = datos_productivity,
                               aes(x= dias, y=media_productivity)) +
  geom_point(shape=24, fill="blue")+
  geom_errorbar(data=datos_productivity,
                mapping=aes(x=dias, ymin=media_productivity-se,
                            ymax=media_productivity+se), color="blue")+
  geom_hline(aes(yintercept = 0))+
  ggtitle("(c) Productivity")+ 
  scale_x_discrete(name="Days in Study") +
  scale_y_continuous(name="Productivity")  +
  theme(plot.title = element_text(hjust = 0.5))






## Gráfica Conjunta


g2 <- ggarrange(graph_earnings, graph_typing_time_hr, graph_productivity,
          ncol = 1, nrow = 3)


ggsave (g2 , filename = 'FIGURE A.VI.pdf ',device = "pdf")





## 5) Tendencia Paralela

## Para probar la tendencia paralela se pueden realizar 3 pruebas:
## La primera que la regresión con OLS el estimador del parámetro para el
## Tratamiento no sea significativo. La segunda que exista balance en las 
## diferentes variables de los dos grupos (Control y Tratamiento). La tercera
## es verificar si en periodos pasados existe una diferencia entre los grupos
## (Control y Tratamiento) con el uso de lags.

## a) Regresión simple con OLS para saber si el coeficiente del tratamiento es
## estadísticamente significativo.

merge_completa <-inner_join(baseline,endline,by = "pid") %>%
  filter(drop_indicator.x == 0)

completa_5a <- full_join(completa,merge_completa, by = "pid")
  

completa_5a <- completa_5a %>% 
  mutate(despues = ifelse(day_in_study > 7,1,0),
         age=age_, female = female_, education = education_,
         children = no_of_children_, T_nap = T_nap)

tp_1 <- lm(productivity ~ T_nap + despues + T_nap*despues, data = completa_5a)


stargazer(tp_1, type = "text")


## El resultado de la primera prueba es que el coeficiente que marca tratamiento
## es estadísticamente significativo y muestra que hay diferencia entre los dos
## grupos por lo que no se cumple el supuesto de tendencias paralelas con esta
## prueba.


## b) La segunda forma de revisar la tendencia paralela es por una tabla de 
## balance de las variables explicativas.

baseline <- baseline %>% 
  mutate(age = age_,
         female = female_,
         education = education_,
         children = no_of_children_,
  )

variables<- data.frame(time_in_office = baseline$time_in_office, 
                       age=baseline$age, 
                       female=baseline$female,
                       education=baseline$education, 
                       sleep_night=baseline$sleep_night,
                       children=baseline$children,
                       health_bsl=baseline$health_bsl,
                       absences=baseline$absences_baseline,
                       T_nap=baseline$T_nap )


# Tabla de Balance Individual

tabla1<-balance_table(variables,treatment = "T_nap")

stargazer(as.data.frame(tabla1),type = "text" ,summary=FALSE)


## El resultado de esta segunda forma es que como los grupos están balanceados 
## se puede pensar en que la tendencia es paralela.

## c) EL último método para comprobar la tendencia paralela es el uso de lags.
## Con lags se probará la hipótesis nula de que la distancia entre la tendencia
## del grupo de control y el grupo de tratamiento para cada uno de los días 
## previos al tratamiento no es estadísticamente significativa. Esto nos dará 
## el cumplimiento del supuesto de tendencias paralelas


# Se construyen los lags


completa_lags <- completa_5a %>% group_by(pid) %>%
  mutate (lag_1 = ifelse (day_in_study == 1 ,1 ,0),
          lag_2 = ifelse (day_in_study == 2 ,1 ,0),
          lag_3 = ifelse (day_in_study == 3 ,1 ,0),
          lag_4 = ifelse (day_in_study == 4 ,1 ,0),
          lag_5 = ifelse (day_in_study == 5 ,1 ,0),
          lag_6 = ifelse (day_in_study == 6 ,1 ,0),
          lag_7 = ifelse (day_in_study == 7 ,1 ,0))

# Se construye el perido cero

completa_lags$inicio <- ifelse (completa_lags$day_in_study == 8, 1 ,0)


reg_lag <- lm(productivity ~ T_nap + lag_1 + lag_2 + lag_3 + lag_4 + lag_5 +
                lag_6 + lag_7 + lag_1*T_nap + lag_2*T_nap + lag_3*T_nap +
                lag_4*T_nap + lag_5*T_nap + lag_6*T_nap + lag_7*T_nap,
              data = completa_lags)

stargazer(reg_lag, type = "text")


## El resultado de la regresión con lags es que no es estadísticamente
## significativa la diferencia entre el grupo de tratamiento contra el grupo
## de control. Es decir, antes del tratamiento los grupos no son diferentes.


## 6) Matching y DID: Tendencia Paralela

## a) Resolver el problema de tendencia paralela con matching y DD

## Para asegurar que el grupo de tratamiento cuente con un contrafactual
## correcto podría usar dos de los métodos de matching. El primero para
## variables categóricas CEM y el segundo si se quiere hacer el emparejamiento
## con variables contínuas PSM. Entonces el camino a seguir sería hacer el
## emparejamiento de los dos grupos con variables categóricas como mujer y con
## una variable discreta como años de educación.
## El segundo paso sería utilizar la base de datos resultante del emparejamiento
## con ello se podría optar por hacer los lags y se esperaría que no tuvieran 
## diferencia los dos grupos en cada uno de los primeros 7 días del experimento.





## b) Matching con DD para Productividad

# Se realiza el Matching con las variables female, education y edad (age)

match_DD <- matchit(T_nap ~ female + education + age, data = completa_5a, 
                     method = 'cem', estimand = 'ATE')

summary(match_DD)

CEM_DD <- match.data(match_DD)








## TOT General de Productividad

productivity_DD <- lm(productivity ~ T_nap + despues + T_nap*despues ,
                      data = CEM_DD)


TOT_DD_productivity <- (sum(productivity_DD$coefficients[c(1,2,3,4)])-
                          sum(productivity_DD$coefficients[c(1,2)]))-
  (sum(productivity_DD$coefficients[c(1,3)])-
     sum(productivity_DD$coefficients[c(1)]))

# Para todos los casos es el coeficiente de la interacción




## TOT para cada periodo siguiente al periodo cero de Productividad


# Se crean los leads en la base emparejada

CEM_DD <- CEM_DD %>% group_by(pid) %>%
  mutate (lead_1 = ifelse (day_in_study == 9 ,1 ,0),
          lead_2 = ifelse (day_in_study == 10 ,1 ,0),
          lead_3 = ifelse (day_in_study == 11 ,1 ,0),
          lead_4 = ifelse (day_in_study == 12 ,1 ,0),
          lead_5 = ifelse (day_in_study == 13 ,1 ,0),
          lead_6 = ifelse (day_in_study == 14 ,1 ,0),
          lead_7 = ifelse (day_in_study == 15 ,1 ,0))



productivity_DD_leads <- lm(productivity ~ T_nap + lead_1 + lead_2 + lead_3
                            + lead_4 + lead_5 + lead_6 + lead_7+ lead_1*T_nap +
                              lead_2*T_nap + lead_3*T_nap + lead_4*T_nap +
                              lead_5*T_nap + lead_6*T_nap + lead_7*T_nap,
                            data = CEM_DD)


stargazer(productivity_DD,productivity_DD_leads,type = "text")



## Los TOT para cada momento en el tiempo es el lead con la interacción, como 
## se realizó en el TOT general.

Periodo <- c("TOT General","Día 9","Día 10","Día 11","Día 11","Día 12","Día 13","Día 14")
coef_DD_productivity <-c(productivity_DD$coefficients[4],productivity_DD_leads$coefficients[c(10:16)])

stargazer(cbind(Periodo,coef_DD_productivity),type = "text")

## Al comparar este resultado con los resultados de la pregunta 2 de la tarea 1
## resulta que con respecto al estimador de Neyman, el resultado del DD después
## del matching es menor que los 170 del estimador de Neyman.

## Cuando se compara con la regresión, se obtiene un resultado parecido con la
## regresión OLS. Sin embargo, cuando se consideran los controles el efecto se
## parece al obtenido con el DD después del matching con 59.419 y 69.81 del DD.
## Con esto podemos concluir que el efecto fue menor cuando se calculó por
## matching, lo que hace pensar en que el efecto estaba sobre estimado.



## c) Reproducir el resultado con nap_time_mins, sleep_report, happy, cognitive,
## y typing_time_hr.



## nap_time_mins

## TOT General de nap_time_mins

nap_time_mins_DD <- lm(nap_time_mins ~ T_nap + despues + T_nap*despues ,
                      data = CEM_DD)


TOT_DD_nap_time_mins <- (sum(nap_time_mins_DD$coefficients[c(1,2,3,4)])-
                          sum(nap_time_mins_DD$coefficients[c(1,2)]))-
  (sum(nap_time_mins_DD$coefficients[c(1,3)])-
     sum(nap_time_mins_DD$coefficients[c(1)]))

# Para todos los casos es el coeficiente de la interacción



## TOT para cada periodo siguiente al periodo cero de nap_time_mins




nap_time_mins_DD_leads <- lm(nap_time_mins ~ T_nap + lead_1 + lead_2 + lead_3
                            + lead_4 + lead_5 + lead_6 + lead_7+ lead_1*T_nap +
                              lead_2*T_nap + lead_3*T_nap + lead_4*T_nap +
                              lead_5*T_nap + lead_6*T_nap + lead_7*T_nap,
                            data = CEM_DD)


stargazer(nap_time_mins_DD,nap_time_mins_DD_leads,type = "text")



## Los TOT para cada momento en el tiempo es el lead con la interacción, como 
## se realizó en el TOT general.

Periodo <- c("TOT General","Día 9","Día 10","Día 11","Día 11","Día 12","Día 13","Día 14")
coef_DD_nap_time_mins <-c(nap_time_mins_DD$coefficients[4],nap_time_mins_DD_leads$coefficients[c(10:16)])

stargazer(cbind(Periodo,coef_DD_nap_time_mins),type = "text")


## En este caso no es posible completar el TOT general porque hay observaciones
## que no se emparejaron porque el nap_time_mins es cero para todos los del 
## grupo control y su valor es NA.







## sleep_report

## TOT General de sleep_report

sleep_report_DD <- lm(sleep_report ~ T_nap + despues + T_nap*despues ,
                       data = CEM_DD)


TOT_DD_sleep_report <- (sum(sleep_report_DD$coefficients[c(1,2,3,4)])-
                           sum(sleep_report_DD$coefficients[c(1,2)]))-
  (sum(sleep_report_DD$coefficients[c(1,3)])-
     sum(sleep_report_DD$coefficients[c(1)]))

# Para todos los casos es el coeficiente de la interacción



## TOT para cada periodo siguiente al periodo cero de sleep_report




sleep_report_DD_leads <- lm(sleep_report ~ T_nap + lead_1 + lead_2 + lead_3
                             + lead_4 + lead_5 + lead_6 + lead_7+ lead_1*T_nap +
                               lead_2*T_nap + lead_3*T_nap + lead_4*T_nap +
                               lead_5*T_nap + lead_6*T_nap + lead_7*T_nap,
                             data = CEM_DD)


stargazer(sleep_report_DD,sleep_report_DD_leads,type = "text")



## Los TOT para cada momento en el tiempo es el lead con la interacción, como 
## se realizó en el TOT general.

Periodo <- c("TOT General","Día 9","Día 10","Día 11","Día 11","Día 12","Día 13","Día 14")
coef_DD_sleep_report <-c(sleep_report_DD$coefficients[4],sleep_report_DD_leads$coefficients[c(10:16)])

stargazer(cbind(Periodo,coef_DD_sleep_report),type = "text")







## happy



## TOT General de happy

happy_DD <- lm(happy ~ T_nap + despues + T_nap*despues ,
                      data = CEM_DD)


TOT_DD_happy <- (sum(happy_DD$coefficients[c(1,2,3,4)])-
                          sum(happy_DD$coefficients[c(1,2)]))-
  (sum(happy_DD$coefficients[c(1,3)])-
     sum(happy_DD$coefficients[c(1)]))

# Para todos los casos es el coeficiente de la interacción



## TOT para cada periodo siguiente al periodo cero de happy




happy_DD_leads <- lm(happy ~ T_nap + lead_1 + lead_2 + lead_3
                            + lead_4 + lead_5 + lead_6 + lead_7+ lead_1*T_nap +
                              lead_2*T_nap + lead_3*T_nap + lead_4*T_nap +
                              lead_5*T_nap + lead_6*T_nap + lead_7*T_nap,
                            data = CEM_DD)


stargazer(happy_DD,happy_DD_leads,type = "text")



## Los TOT para cada momento en el tiempo es el lead con la interacción, como 
## se realizó en el TOT general.

Periodo <- c("TOT General","Día 9","Día 10","Día 11","Día 11","Día 12","Día 13","Día 14")
coef_DD_happy <-c(happy_DD$coefficients[4],happy_DD_leads$coefficients[c(10:16)])

stargazer(cbind(Periodo,coef_DD_happy),type = "text")








## Cognitive

## Para este ejercicio se normaliza [0,1] cada uno de los scores que obtuvo 
## la observación i para corsi, hf y pvt. De tal manera que el score más bajo
## de corsi representa el 0 y la más alta el 1. Con esta normalización los 3
## scores son comparables. El siguiente paso es obtener la media de estos scores
## que están dentro del [0,1]. No es necesario poner una ponderación diferente
## para cada uno de los scores,
## La variable cognitive es el promedio de los 3 scores normalizados.

CEM_DD <- CEM_DD %>%
  filter(!is.na(corsi_measure),!is.na(hf_measure),!is.na(pvt_measure)) %>%
  mutate(c_corsi = abs(corsi_measure / sqrt(sum(corsi_measure^2))),
         c_hf = abs(hf_measure / sqrt(sum(hf_measure^2))),
         c_pvt = abs(pvt_measure / sqrt(sum(pvt_measure^2))))

CEM_DD <- CEM_DD %>% mutate(cognitive = (c_corsi+c_hf+c_pvt)/3 )



## TOT General de Cognitive

cognitive_DD <- lm(cognitive ~ T_nap + despues + T_nap*despues ,
               data = CEM_DD)


TOT_DD_cognitive <- (sum(cognitive_DD$coefficients[c(1,2,3,4)])-
                   sum(cognitive_DD$coefficients[c(1,2)]))-
  (sum(cognitive_DD$coefficients[c(1,3)])-
     sum(cognitive_DD$coefficients[c(1)]))

# Para todos los casos es el coeficiente de la interacción



## TOT para cada periodo siguiente al periodo cero de Cognitive




cognitive_DD_leads <- lm(cognitive ~ T_nap + lead_1 + lead_2 + lead_3
                     + lead_4 + lead_5 + lead_6 + lead_7+ lead_1*T_nap +
                       lead_2*T_nap + lead_3*T_nap + lead_4*T_nap +
                       lead_5*T_nap + lead_6*T_nap + lead_7*T_nap,
                     data = CEM_DD)


stargazer(cognitive_DD,cognitive_DD_leads,type = "text")



## Los TOT para cada momento en el tiempo es el lead con la interacción, como 
## se realizó en el TOT general.

Periodo <- c("TOT General","Día 9","Día 10","Día 11","Día 11","Día 12","Día 13","Día 14")
coef_DD_Cognitive <-c(Cognitive_DD$coefficients[4],Cognitive_DD_leads$coefficients[c(10:16)])

stargazer(cbind(Periodo,coef_DD_Cognitive),type = "text")







## typing_time_hr




## TOT General de typing_time_hr

typing_time_hr_DD <- lm(typing_time_hr ~ T_nap + despues + T_nap*despues ,
                   data = CEM_DD)


TOT_DD_typing_time_hr <- (sum(typing_time_hr_DD$coefficients[c(1,2,3,4)])-
                       sum(typing_time_hr_DD$coefficients[c(1,2)]))-
  (sum(typing_time_hr_DD$coefficients[c(1,3)])-
     sum(typing_time_hr_DD$coefficients[c(1)]))

# Para todos los casos es el coeficiente de la interacción



## TOT para cada periodo siguiente al periodo cero de typing_time_hr




typing_time_hr_DD_leads <- lm(typing_time_hr ~ T_nap + lead_1 + lead_2 + lead_3
                         + lead_4 + lead_5 + lead_6 + lead_7+ lead_1*T_nap +
                           lead_2*T_nap + lead_3*T_nap + lead_4*T_nap +
                           lead_5*T_nap + lead_6*T_nap + lead_7*T_nap,
                         data = CEM_DD)


stargazer(typing_time_hr_DD,typing_time_hr_DD_leads,type = "text")



## Los TOT para cada momento en el tiempo es el lead con la interacción, como 
## se realizó en el TOT general.

Periodo <- c("TOT General","Día 9","Día 10","Día 11","Día 11","Día 12","Día 13","Día 14")
coef_DD_typing_time_hr <-c(typing_time_hr_DD$coefficients[4],typing_time_hr_DD_leads$coefficients[c(10:16)])

stargazer(cbind(Periodo,coef_DD_typing_time_hr),type = "text")


## La estratégia que se utilizó para poder construir un contrafactual 
## fue primero emparejar los datos por medio del método de matching CEM
## una vez que se contaba con la base de datos emparejada se pudo realizar 
## la regresion de Diferencias en Diferencias y con ello se oprobó el supuesto
## de tendencia paralela. Para verificar la tendencia paralela se construyeron
## lags de los primeros 7 días cuando los dos grupos eran del grupo control, 
## lo cual dio como resultado que los coeficientes asociados a los lags 
## interactuados con el grupo de tratamiento no fueran estadísticamente
## significativos, lo cual confirma la tendencia paralela dentro de los primeros
## 7 días de estudio.


## También se utilizó la base de datos emparejada para el cálculo de los TOT
## tanto general como para cada uno de los días siguientes al inicio del
## tratamiento. Para calcular el efecto de cada uno de los días siguientes
## al día 8 se construyeron leads de tal manera que fue posible conocer
## los TOT para cada día entre el grupo de tratamiento y el grupo control.
## Esto dió como resultado que los efectos calculados en la pregunta dos para
## los tres incisos resultaran menores que los calculados para la pregunta 2.
## Se destaca que con el modelo de OLS con controles, el efecto es muy
## parecido al DID con matching. Esto se debe a que cuando controlas por más
## variables explicativas, logras disminuir el sesgo por variables omitidas.
## Con el matching se hace algo semejante pues se comparan personas similares
## y esto resulta en estimaciones que no están sesgadas.

## En general, los TOT que resutaron de utilizar el DID con matching tienen 
## menor magnitud que los realizados con MCO con y sin controles. El sentido 
## de los efectos es el mismo.





















