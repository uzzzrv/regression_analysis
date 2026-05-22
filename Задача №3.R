#Практическая работа №3. 
#Вариант 29: волна 21.

library("lmtest")
library("rlms")
library("dplyr")
library("GGally")
library("car")
library("sandwich")


#Прочитаем файл и выберем признаки, необходимые, чтобы описать социально-экономическое положение граждан Российской Федерации. 
data <- read.csv('r27.csv', sep = ',')
glimpse(data) 
data2 = select(data, wj13.2, wh5, w_marst, w_diplom, w_age, status, wj6.2, wj26)


#Теперь избавимся от пропусков и "выбросов" в данных.
data2 = na.omit(data2)
data2 <- data2 %>%
filter_all(all_vars(. < 9999996))
sum(is.na(data2))
sum(data2 > 9999990)
str(data2)

#Для удобства работы с данными переименуем признаки.
names(data2) <- c('salary', 'sex', 'marst', 'education', 'age', 'city_status', 'working_day', 'owner')
names(data2)
glimpse(data2)
#salary - зарплата; sex - пол; marst - семейное положение; education - законченное образование;
#city_status - тип населенного пункта; working_day - продолжительность рабочего дня в часах; 
#owner - является ли владельцем/совладельцем
data2$education


#Создадим дамми-переменные из параметра marst:
#1) wed1 = 1 в случае, если респондент женат, 0 – в противном случае; 
#2) wed2 = 1, если респондент разведён или вдовец;
#3) wed3 = 1, если респондент никогда не состоял в браке.
data2$wed1 = ifelse(data2$marst == 2, 1, 0) 
data2$wed2 = ifelse((data2$marst == 3 | data2$marst == 4) , 1, 0)
data2$wed3 = ifelse(data2$marst == 1, 1, 0)
data2 <- subset(data2, select = -marst)
names(data2)


#Признаку sex присвоим значение sex = 1 - для мужчин, sex = 0 - для женщин.
data2$sex
data2$sex <- ifelse(data2$sex == 1, 1, 0)
data2$sex


#Создадим дамми-переменную из параметра city_status:
#city_status = 1 для города или областного центра, city_status = 0 – в противоположном случае.
data2$city_status = ifelse((data2$city_status == 1 | data2$city_status == 2),1,0)


#Введём параметр higher_educ, характеризующий наличие полного высшего образования.
#(В демографическом исследовании 6 - это законченное высшее образование и выше)
data2$higher_educ = ifelse(data2$education == 6, 1,0)
data2 <- subset(data2, select = -education) 


#Создадим дамми-переменную из параметра owner:
data2$owner <- ifelse(data2$owner == 1, 1, 0)


glimpse(data2)
apply(data2, 2, summary) 


#Преобразуем факторные переменные (salary, age, working_day) в вещественные и нормализуем их.

data2$age <- as.numeric(data2$age)
data2$working_day <- as.numeric(data2$working_day)
str(data2)

#Нормализация
mean_salary <- mean(data2$salary) 
sd_salary <- sd(data2$salary) 
data2$salary <- (data2$salary - mean_salary) / sd_salary 

mean_age <- mean(data2$age)
sd_age <- sd(data2$age)
data2$age <- (data2$age - mean_age) / sd_age

mean_working_day <- mean(data2$working_day)
sd_working_day <- sd(data2$working_day)
data2$working_day <- (data2$working_day - mean_working_day)/ sd_working_day

apply(data2, 2, summary)

#Немного (на минимальное число, большее минимального значения по столбцу) сдвинем 
#значения столбцов, чтобы избавиться от отрицательных чисел, полученных в ходе стандартизации.
data2$salary = data2$salary + 2
data2$age = data2$age + 2
data2$working_day = data2$working_day + 3
apply(data2, 2, summary) 


# Пункт №1.

model = lm(salary ~ sex + higher_educ + age + city_status + working_day + owner + wed1 + wed2 + wed3, data2)
summary(model)
vif(model)
#R^2 = 22.34%
#Исходя из значения vif, параметры age и working_day коррелируют друг с другом.


#Попробуем убрать working_day (параметр working_day не является значимым, p-value большое, звездочек нет)
model1 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + wed3, data2)
summary(model1)
vif(model1) 
#R^2 = 22.69%
#Без параметра working_day зависимость между регрессорами стала меньше, а также сама модель стала лучше.

#Можем заметить, что переменные wed1, wed2, wed3 коррелируют друг с другом и не сильно значимы (p-value велико, звездочек нет).
#Попробуем избавиться от какого\каких-нибудь параметра\ов из них.

model2_1 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed2 + wed3, data2)
summary(model2_1)
vif(model2_1) 
#R^2 = 22.03%
#sex      higher_educ         age city_status       owner        wed2        wed3 
#1.070341    1.089778    2.070450    1.109242    1.943499    1.094344    1.178440 

model2_2 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed3, data2)
summary(model2_2)
vif(model2_2) 
#R^2 = 22.09%
#sex        higher_educ       age city_status       owner        wed1        wed3 
#1.128789    1.098474    2.102875    1.112786    1.943232    1.407931    1.440699 

model2_3 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2, data2)
summary(model2_3)
vif(model2_3) 
#R^2 = 22.43%
#sex       higher_educ       age  city_status       owner        wed1        wed2 
#1.064077    1.064654    2.118570    1.103794    1.934176    1.475980    1.402551 

#Убрав wed3 получили наименьшую зависимость между регрессорами и незначительную разницу относительно model1 (R^2 = 22,69%).
#Остановимся на модели model2_3.


#Пункты №2,3.

#Будем эксперементировать с функциями вещественных параметров.

#Логарифмы
model3 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + log(age), data2)
summary(model3)
vif(model3) 
#R^2 = 22.23%

model4 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + log(working_day), data2)
summary(model4)
vif(model4) 
#R^2 = 22.81%

model5 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + log(age) + log(working_day), data2)
summary(model5)
vif(model5) 
#R^2 = 28.83%
#vif у логарифмов большой, но они улучшают модель


#Степени
model6 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.1) + I(working_day^0.1), data2)
summary(model6)
vif(model6)
#R^2 = 28.83%

model7 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.2) + I(working_day^0.2), data2)
summary(model7)
vif(model7)
#R^2 = 28.82%

model8 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.3) + I(working_day^0.3), data2)
summary(model8)
vif(model8)
#R^2 = 28.82%

model9 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.4) + I(working_day^0.4), data2)
summary(model9)
vif(model9)
#R^2 = 28.82%

model10 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.5) + I(working_day^0.5), data2)
summary(model10)
vif(model10)
#R^2 = 28.81%

model11 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.9) + I(working_day^0.9), data2)
summary(model11)
vif(model11)
#R^2 = 28.78%

model12 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^1.1) + I(working_day^1.1), data2)
summary(model12)
vif(model12)
#R^2 = 28.75%

model13 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^1.3) + I(working_day^1.3), data2)
summary(model13)
vif(model13)
#R^2 = 28.7%

model14 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^1.5) + I(working_day^1.5), data2)
summary(model14)
vif(model14)
#R^2 = 28.64%

model15 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^1.7) + I(working_day^1.7), data2)
summary(model15)
vif(model15)
#R^2 = 28.54%

model16 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^2) + I(working_day^2), data2)
summary(model16)
vif(model16)
#R^2 = 28.31%

model17 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.1) + I(working_day^2), data2)
summary(model17)
vif(model17)
#R^2 = 28.51%

model18 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^2) + I(working_day^0.1), data2)
summary(model18)
vif(model18)
#R^2 = 28.78%

model19 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^1.8) + I(working_day^0.3), data2)
summary(model19)
vif(model19)
#R^2 = 28.78%

model20 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^1.6) + I(working_day^0.5), data2)
summary(model20)
vif(model20)
#R^2 = 28.78%

model21 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^1.4) + I(working_day^0.7), data2)
summary(model21)
vif(model21)
#R^2 = 28.77%

model22 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^1.2) + I(working_day^0.9), data2)
summary(model22)
vif(model22)
#R^2 = 28.76%

model23 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^1) + I(working_day^1.1), data2)
summary(model23)
#vif(model23)
#R^2 = 22.06%

model24 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.8) + I(working_day^1.3), data2)
summary(model24)
vif(model24)
#R^2 = 28.73%

model25 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.6) + I(working_day^1.5), data2)
summary(model25)
vif(model25)
#R^2 = 28.69%

model26 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.4) + I(working_day^1.7), data2)
summary(model26)
vif(model26)
#R^2 = 28.62%

model27 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.2) + I(working_day^1.9), data2)
summary(model27)
vif(model27)
#R^2 = 28.5%

model28 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.1) + I(working_day^2), data2)
summary(model28)
vif(model28)
#R^2 = 28.41%


#Мы перебрали большое количество моделей с различными степенями вещественных параметров
#На данный момент лучшей из них назовем модель model6 (исходя из R^2, R^2 = 28.83%)

#Попробуем добавить в эту модель логарифмы вещественных параметров.

model29 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.8) + I(working_day^1.3) + log(age) + log(working_day), data2)
summary(model29)
vif(model29)
#R^2 = 28.04%

model30 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.8) + I(working_day^1.3) + log(working_day), data2)
summary(model30)
vif(model30)
#R^2 = 28.45%

model31 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.8) + I(working_day^1.3) + log(age), data2)
summary(model31)
vif(model31)
#R^2 = 28.32%

#Можем заметить, что логарифмы не улучшают ситуации. 

#Попробуем добавить в модель произведение вещественных параметров.
model32 = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.8) + I(working_day^1.3) + I(age*working_day), data2)
summary(model32)
vif(model32)
#R^2 = 28.32%

#Таким образом, model6 осталась лучшей (R^2 = 28.83%); 
#неплохая p-статистика, много звездочек, бОльшая часть регрессоров - значимые.
#Будем работать с этой моделью.


# Пункт №4.

#Построим парную регрессию для каждого регрессора, участующего в лучшей модели.

#Лучшая модель:
#salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + I(age^0.1) + I(working_day^0.1)

m1 = lm(salary ~ sex, data2)
summary(m1)
confint(m1, level = 0.95) 
#salary = 0.52 * sex + 1.77; *** - регрессор значим; взаимосвязь положительная.
#Доверительный интервал: (0.2377892, 0.8049572).
#Доверительный интервал не содержит 0, наиболее вероятное значение коэффициента положительное.

m2 = lm(salary ~ higher_educ, data2)
summary(m2)
confint(m2, level = 0.95) 
#salary = 0.07 * higher_educ + 0.97; *** - регрессор значим; взаимосвязь положительная.
#Доверительный интервал: (0.269732, 0.8565448).
#Доверительный интервал не содержит 0, наиболее вероятное значение коэффициента положительное.

m3 = lm(salary ~ age, data2)
summary(m3)
confint(m3, level = 0.95) 
#salary = 0.08 * sex + 0.96; *** - регрессор значим; взаимосвязь положительная.
#Доверительный интервал: (-0.4176475, -0.1374076).
#Доверительный интервал не содержит 0, наиболее вероятное значение коэффициента отрицательное.

m4 = lm(salary ~ city_status, data2)
summary(m4)
confint(m4, level = 0.95) 
#salary = 0.23 * city_status + 1.86; регрессор не значим.
#Доверительный интервал: (-0.07276566, 0.5269539).
#Доверительный интервал содержит 0.

m5 = lm(salary ~ owner, data2)
summary(m5)
confint(m5, level = 0.95) 
#salary = 0.0004 * owner + 2; регрессор не значим.
#Доверительный интервал: (-0.7318398, 0.5544002).
#Доверительный интервал содержит 0.

m6 = lm(salary ~ wed1, data2)
summary(m6)
confint(m6, level = 0.95) 
#salary = 0.19 * sex + 0.998; регрессор не значим.
#Доверительный интервал: (-0.09718947 0.4825998).
#Доверительный интервал содержит 0.

m7 = lm(salary ~ wed2, data2)
summary(m7)
confint(m7, level = 0.95) 
#salary = 0.02 * sex + 1.995; регрессор не значим.
#Доверительный интервал: (-0.3325647, 0.3806155).
#Доверительный интервал содержит 0.

m8 = lm(salary ~ I(age^0.1), data2)
summary(m8)
confint(m8, level = 0.95) 
#salary = -10.237 * sex + 12.9; *** - регрессор значим; взаимосвязь отрицательная.
#Доверительный интервал: (-15.376129 -5.097288).
#Доверительный интервал не содержит 0, наиболее вероятное значение коэффициента отрицательное.

m9 = lm(salary ~ I(working_day^0.1), data2)
summary(m9)
confint(m9, level = 0.95) 
#salary = -11.871 * sex + 15.2; *** - регрессор значим; взаимосвязь отрицательная.
#Доверительный интервал: (-18.236880, -5.505343).
#Доверительный интервал не содержит 0, наиболее вероятное значение коэффициента отрицательное.

#Можем сделать вывод, что зарплата зависит от пола, возраста наличия высшего образования
#и продолжительности рабочего дня у индивида.


#Пункт №5.

#Проверим лучшую модель на заданных подмножествах.

#Подмножества: 
# 1) городские жители, женщины, не состоявшие в браке
# 2) разведенные женщины, с высшим образованием

#Зададим подмножества:
subset1 <- data2[data2$city_status == 1 & data2$sex == 0 & data2$wed3 == 1, ] 
subset2 <- data2[data2$higher_educ == 1 & data2$sex == 0 & data2$wed2 == 1, ] 

#Добавим в подмножества недостающие параметры (функции вещественных параметров)
subset1$working_day_deg = I(subset1$working_day^0.1)
subset1$age_deg = I(subset1$age^0.1)

subset2$working_day_deg= I(subset2$working_day^0.1)
subset2$age_deg = I(subset2$age^0.1)

# Проверим доверительные интервалы для оставшихся в модели коэффициентов.
s1model = lm(salary ~ sex + higher_educ + age + city_status + owner + wed1 + wed2 + age_deg + working_day_deg, subset1)
summary(s1model)
confint(s1model,level = 0.95)
#для higher_educ: (-1.118681, 1.694868) - содержит 0
#для age: (-240.064324, 635.707448) - содержит 0 
#для owner: (-2.429928, 2.268492) - содержит 0 
#для working_day_deg: (-126.640226, 480.021189) - содержит 0

s2model = lm(salary ~ sex + higher_educ  + city_status + owner + wed1 + wed2 + age_deg + working_day_deg, subset2)
summary(s2model)
confint(s2model,level = 0.95)

#для city_status: (-3.763845, 3.556905) - содержит 0 
#для age_deg: (-75993.608061, 30537.331851) - содержит 0
#для working_day_deg: (-2541.442377, 5577.086902) - содержит 0


