#Практическая работа 2.1.
#Вариант 29: набор данных - attitude; объясняемая переменная - rating; регрессоры - complaints, learning, raises

library("lmtest")
library("car")
data = attitude
help(attitude)

# Пункт №1.

#Исследуем набор данных на линейную зависимость.

#Построим зависимость между переменными complaints и learning
compllearn = lm(complaints ~ learning, data)
summary(compllearn) 
#R^2 = 35,61%, т.е. линейной зависимости нет.

#Построим зависимость между переменными complaints и raises
complrais = lm(complaints ~ raises, data)
summary(complrais) 
#R^2 = 44,78%, т.е линейной зависимости нет.

#Построим зависимость между переменными raises и learning
raislearn = lm(raises ~ learning, data)
summary(raislearn) 
#R^2 = 41%, т.е линейной зависимости нет.

# Пункт №2.

model1 = lm(rating ~ complaints + learning + raises, data)
summary(model1)
#R^2 = 70,83% - модель неплохая
#p-value при compaints: 5.82e-05 (***); регрессор является значимым
#p-value при learning: 0.152; p-value велико, регрессор плохо объясняет динамику rating
#p-value при raises: 0.876; p-value велико, регрессор плохо объясняет динамику rating
vif(model1)
#vif при всех регрессорах < 5, т.е. сильной зависимости между регрессорами нет.

model2 = lm(rating ~ complaints, data)
summary(model2)
#R^2 = 68,13%

model3 = lm(rating ~ learning, data)
summary(model3)
#R^2 = 38,9%

model4 = lm(rating ~ raises, data)
summary(model4)
#R^2 = 34,835

model5 = lm(rating ~ complaints + learning, data)
summary(model5)
#R^2 = 70,8%

model6 = lm(rating ~ complaints + raises, data)
summary(model6)
#R^2 = 68,39%

model7 = lm(rating ~ learning + raises, data)
summary(model7)
#R^2 = 45,07%

#Можем заметить, что разница между model1 (R^2 = 70,83%) И model5 (R^2 = 70,8%) незначительна.
#Также мы выяснили, что в model1 p-value у raises велико (0.876)
#Попробуем проводить исследования без регрессора raises.


# Пункт №3.

#Будем вводить в модель логарифмы регрессоров.

model8 = lm(rating ~ complaints + learning + log(complaints) + log(learning) + log(raises), data)
summary(model8)
#R^2 = 73,38%
vif(model8)

model9 = lm(rating ~ complaints + learning + log(complaints) + log(learning), data)
summary(model9)
#R^2 = 73,38%
vif(model9)

model10 = lm(rating ~ complaints + learning + log(complaints) + log(raises), data)
summary(model10)
#R^2 = 70,96%
vif(model10)

model11 = lm(rating ~ complaints + learning + log(learning) + log(raises), data)
summary(model11)
#R^2 = 72,88%
vif(model11)

model12 = lm(rating ~ complaints + learning + log(complaints), data)
summary(model12)
#R^2 = 70,88%
vif(model12)

model13 = lm(rating ~ complaints + learning + log(learning), data)
summary(model13)
#R^2 = 72,83%
vif(model13)

model14 = lm(rating ~ complaints + learning + log(raises), data)
summary(model14)
#R^2 = 70,83%
vif(model14)

model15 = lm(rating ~ complaints + log(learning) + log(raises), data)
summary(model15)
#R^2 = 70,2%
vif(model15)

model16 = lm(rating ~ log(complaints) + log(learning), data)
summary(model16)
#R^2 = 69,64%
vif(model16)

model17 = lm(rating ~ log(complaints) + learning + log(raises), data)
summary(model17)
#R^2 = 70,69%
vif(model17)

model18 = lm(rating ~ complaints + log(learning) + log(raises), data)
summary(model18)
#R^2 = 70,2%
vif(model18)

model19 = lm(rating ~ log(complaints) + log(learning) + log(raises), data)
summary(model19)
#R^2 = 69,81%
vif(model19)

#Судя по R^2, На данный момент лучшей моделью является model9 (R^2 = 73,38%)


# Пункт №4. 

#Будем вводить в модель произведения пар регрессоров и квадраты регрессоров. 

model20 = lm(rating ~ complaints + learning + I(complaints*learning), data)
summary(model20)
#R^2 = 71,33%
vif(model20)

model21 = lm(rating ~ complaints + learning + I(complaints*raises), data)
summary(model21)
#R^2 = 70,83%
vif(model21)

model22 = lm(rating ~ complaints + learning + I(learning*raises), data)
summary(model22)
#R^2 = 70,81%
vif(model22)

model23 = lm(rating ~ complaints + learning + I(complaints^2), data)
summary(model23)
#R^2 = 70,89%
vif(model23)

model24 = lm(rating ~ complaints + learning + I(learning^2), data)
summary(model24)
#R^2 = 72,6%

model25 = lm(rating ~ complaints + learning + I(raises^2), data)
summary(model25)
#R^2 = 70,84%

model26 = lm(rating ~ complaints + I(learning^2) + I(raises^2), data)
summary(model26)
#R^2 = 71,36%

model27 = lm(rating ~ I(complaints^2) + learning + I(raises^2), data)
summary(model27)
#R^2 = 69,62

model28 = lm(rating ~ I(complaints^2) + I(learning^2) + I(raises^2), data)
summary(model28)
#R^2 = 70,03%


#Попробуем добавить логарифмы. 

model29 = lm(rating ~ complaints + learning + log(complaints) + log(learning) + I(raises^2), data)
summary(model29)
#R^2 = 73,42%
vif(model29)

model30 = lm(rating ~ complaints + learning + log(complaints) + I(learning^2) + log(raises), data)
summary(model30)
#R^2 = 72,98%
vif(model30)

model31 = lm(rating ~ complaints + learning + I(complaints^2) + log(learning) + log(raises), data)
summary(model31)
#R^2 = 73,48%
vif(model31)

model32 = lm(rating ~ I(complaints^2) + log(learning) + log(raises), data)
summary(model32)
#R^2 = 69,2%
vif(model32)

model33 = lm(rating ~ log(complaints) + I(learning^2) + log(raises), data)
summary(model33)
#R^2 = 71,35%
vif(model33)

model34 = lm(rating ~ log(complaints) + log(learning) + I(raises^2), data)
summary(model34)
#R^2 = 69,73%
vif(model34)

#Мы рассмотрели множество моделей с логарифмами, квадратами регрессоров и их произведениями
#Судя по R^2, cреди них всех лучшей моделью оказалась model31 (R^2 = 73,48%)

# Пункт №5.

#Будем работать с model31.
#rating ~ complaints + learning + I(complaints^2) + log(learning) + log(raises)

#Построим парные регрессии.

pr1 = lm(rating ~ complaints, data)
summary(pr1)
#R^2 = 68.13%
#rating = 0.75*complaints + 14.38
#Выявленная взаимосвязь положительная (т.к коэффициент перед complaints положительный)

pr2= lm(rating ~ learning, data)
summary(pr2)
#R^2 = 38,9%
#rating = 0.64*learning + 28.2
#Выявленная взаимосвязь положительная (т.к коэффициент перед learning положительный)

pr3= lm(rating ~ I(complaints^2), data)
summary(pr3)
#R^2 = 67,2%
#rating = 5.706e-03*I(complaints^2) + 3.835e+01
#Выявленная взаимосвязь положительная (т.к коэффициент перед I(complaints^2) положительный)

pr4= lm(rating ~ log(learning), data)
summary(pr4)
#R^2 = 37,3%
#rating = 34.3*log(learning) - 72.8
#Выявленная взаимосвязь положительная (т.к коэффициент перед learning положительный)

pr5 = lm(rating ~ log(raises), data)
summary(pr5)
#R^2 = 36,44%
#rating = 45.21*log(raises) - 123.26
#Выявленная взаимосвязь положительная (т.к коэффициент перед learning положительный)



# Практическая работа 2.2 

#Пункты №1,2. 

#Лучшая модель из практической работы 2.1:
model31 = lm(rating ~ complaints + learning + I(complaints^2) + log(learning) + log(raises), data)
summary(model31)
#R^2 = 73,48%

#Количество степеней свободы в обучающей выборке – 30.
#Рассчитано 6 коэффициентов. 
#Число степеней свободы в модели: 30 – 6 = 24.

#Рассчитаем t-критерий Стьюдента для 24 степеней свободы при p = 95%:
t_critical = qt(0.975, df = 24)
t_critical
#t-критерий Стьюдента равен ~ 2.06.


#Рассчитаем доверительные интервалы для регрессоров:

# 1. Свободный коэффицент
#Значение коэффициента в модели: ~ 260.2
#Стандартная ошибка: ~ 170.2
#Тогда доверительный интервал примет следующий вид: [260.2 - 2.06 * 170.2, 260.2 + 2.06 * 170.2]
#Вычислим значения на концах интервала и получим: [-90.412, 610,812]
#0 лежит в этом интервале, гипотезу о том, что коэффициент равен 0, нельзя отвергнуть.

# 2. complaints
#Значение коэффициента в модели: ~ 1.34
#Стандартная ошибка: ~ 0.95
#Тогда доверительный интервал примет следующий вид: [1.34 - 2.06 * 0.95, 1.34 + 2.06 * 0.95,]
#Вычислим значения на концах интервала и получим: [-0.617, 3.297]
#0 лежит в этом интервале, гипотезу о том, что коэффициент равен 0, нельзя отвергнуть.

# 3. learning
#Значение коэффициента в модели: ~ 1.88
#Стандартная ошибка: ~ 1.1
#Тогда доверительный интервал примет следующий вид: [1.88 - 2.06 * 1.1, 1.88 + 2.06 * 1.1]
#Вычислим значения на концах интервала и получим: [-0.386, 4.146]
#0 лежит в этом интервале, гипотезу о том, что коэффициент равен 0, нельзя отвергнуть.

# 4. complaints ^ 2
#Значение коэффициента в модели: ~ -0.005
#Стандартная ошибка: ~ 0.007
#Тогда доверительный интервал примет следующий вид: [-0.005 - 2.06 * 0.007, -0.005 - 2.06 * 0.007]
#Вычислим значения на концах интервала и получим: [-0.01942, 0.00942]
#0 лежит в этом интервале, гипотезу о том, что коэффициент равен 0, нельзя отвергнуть.

# 5. log(learning)
#Значение коэффициента в модели: ~ -90.3
#Стандартная ошибка: ~ 60.06
#Тогда доверительный интервал примет следующий вид: [-90.3 - 2.06 * 60.06, -90.3 + 2.06 * 60.06]
#Вычислим значения на концах интервала и получим: [-214.0236, 33.4236]
#0 лежит в этом интервале, гипотезу о том, что коэффициент равен 0, нельзя отвергнуть.

# 6. log(raises)
#Значение коэффициента в модели: ~ -1.1
#Стандартная ошибка: ~ 13.2
#Тогда доверительный интервал примет следующий вид: [-1.1 - 2.06 * 13.2, -1.1 + 2.06 * 13.2]
#Вычислим значения на концах интервала и получим: [-28.292, 26.092]
#0 лежит в этом интервале, гипотезу о том, что коэффициент равен 0, нельзя отвергнуть.



# Пункт №3. 

#Доверительный интервал для прогноза
#Будем работать со следующей моделью:
model = lm(rating ~ learning + complaints + raises, data)
new_data = data.frame(learning = 20, complaints = 10, raises = 10)
predict(model, new_data, interval = "confidence")
#        fit      lwr      upr 
#        21.18483 6.668517 35.70114



# Пункт №4.

#Построим парные регрессиии оценим доверительные интервалы, для каждого регрессора участвующего в лучшей модели.
#Лучшая модель: rating ~ complaints + learning + I(complaints^2) + log(learning) + log(raises)

#Рассчитаем t-критерий Стьюдента для оценки доверительного коэффициента.
#Для любой из парных регрессий справедливо следующее:
#Количество степеней свободы в обучающей выборке – 30.
#Рассчитано 2 коэффициентов. 
#Число степеней свободы в модели: 30 – 2 = 28.
#t-критерий Стьюдента для 28 степеней свободы при p = 95%:
t_critical = qt(0.975, df = 28)
t_critical
#t-критерий Стьюдента равен ~ 2.05.

pr1_2 = lm(rating ~ complaints, data)
summary(pr1_2)
#rating = 0.75*сomplaints + 14.38 (выявленная взаимосвязь положительная)
#Доверительный интервал: 
#[0.75 - 2.05*0.098, 0.75 + 2.05*0.098] -> [0.5491, 0.9509]
#Доверительный интервал не содержит 0, наиболее вероятное значение коэффициента положительное

pr2_2 = lm(rating ~ learning, data)
summary(pr2_2)
#rating = 0.64*learning + 28.2
#Доверительный интервал: 
#[0.64 - 2.05*0.15, 0.64 + 2.05*0.15] -> [0.3325, 0.9475]
#Доверительный интервал не содержит 0, наиболее вероятное значение коэффициента положительное

pr3_2 = lm(rating ~ I(complaints^2), data)
summary(pr3_2)
#rating = 5.706e-03*I(complaints^2) + 3.835e+01
#Доверительный интервал: 
#[5.706e-03 - 2.05*7.564e-04, 5.706e-03 + 2.05*7.564e-04] -> [41.5538е-04, 72.5662е-04]
#Доверительный интервал не содержит 0, наиболее вероятное значение коэффициента положительное

pr4_2 = lm(rating ~ log(learning), data)
summary(pr4_2)
#rating = 34.3*log(learning) - 72.798
#Доверительный интервал: 
#[34.3 - 2.05*8.4, 34.3 + 2.05*8.4] -> [17.08, 51.52]
#Доверительный интервал не содержит 0, наиболее вероятное значение коэффициента положительное

pr5_2 = lm(rating ~ log(raises), data)
summary(pr5_2)
#rating = 45.21*log(raises) - 123.26
#Доверительный интервал: 
#[45.21 - 2.05*11.28, 45.21 + 2.05*11.28] -> [22.086, 68.334]
#Доверительный интервал не содержит 0, наиболее вероятное значение коэффициента положительное




