library("lmtest")
library("GGally")

data = swiss
data
summary(data)

#Оценка среднего значения, дисперсии и СКО переменной Infant.Mortality
mean(data$Infant.Mortality)
var(data$Infant.Mortality)
sd(data$Infant.Mortality)
#Если посмотреть на значения заданного столбца, то можно увидеть, что
#все значения не сильно отличаются от среднего, то есть СКО и дисперсия
#не слишком велики.

#Оценка среднего значения, дисперсии и СКО переменной Agriculture
mean(data$Agriculture)
var(data$Agriculture)
sd(data$Agriculture)
#Дисперсия и СКО велики, т.е. значения столбца сильно отличаются от
#среднего.

#Оценка среднего значения, дисперсии и СКО переменной Examination
mean(data$Examination)
var(data$Examination)
sd(data$Examination)
#Значения варьируются от среднего, но не так сильно, как во 2 случае, но
#при этом сильнее, чем в 1 случае



#Построение зависимостей 

model1 = lm(Infant.Mortality~Agriculture, data)
plot(data$Infant.Mortality~ data$Agriculture) + abline(model1)
summary(model1)
#Построим зависимость исходя из полученных данных:
#Infant.Mortality = 20.34 - 0.008*Agriculture
#Связь отрицательная
#Рассмотрим коэффициент детерминации R^2:
#R^2 = 0.004, значение очень мало, значит доля необъясненных моделью значений велика.
#Рассмотрим p-value:
#p-value = 0.68, т.е значение p-статистики говорит о том, что велика вероятность
#отсутствия взаимосвязи между переменными Infant.Mortality и Agriculture.


model2 = lm(Infant.Mortality~Examination, data)
plot(data$Infant.Mortality~ data$Examination) + abline(model)
summary(model2)
#Построим зависимость исходя из полученных данных:
#Infant.Mortality = 20.63 - 0.042*Examination 
#Связь отрицательная
#Рассмотрим коэффициент детерминации R^2:
#R^2 = 0.013, значение очень мало, значит доля необъясненных моделью значений велика.
#Рассмотрим p-value:
#p-value = 0.45, т.е значение p-статистики говорит о том, что велика вероятность
#отсутствия взаимосвязи между переменными Infant.Mortality и Examination.
