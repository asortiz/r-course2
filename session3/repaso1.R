#¿Es distinto el IMC en personas diabéticas y no diabéticas?


data <- read.csv("diabetes.csv", sep = ",", header = TRUE)

diabeticos <- subset(data, Outcome == 1)
no_diabeticos <- subset(data, Outcome == 0)


mean_dia <- mean(diabeticos$BMI)
mean_nodia <- mean(no_diabeticos$BMI)
sd_dia <- sd(diabeticos$BMI)
sd_nodia <- sd(no_diabeticos$BMI)


d <- (mean_dia - mean_nodia)/ sqrt((sd_dia^2 + sd_nodia^2)/2)
d


library(pwr)

n <- pwr.t.test (d=d, sig.level =0.05, power = 0.8, 
            type ="two.sample", alternative="two.sided")
n



shapiro.test(diabeticos$BMI)
shapiro.test(no_diabeticos$BMI)
# p < 0.05 asumimos que se alejan de la normalidad.

hist(diabeticos$BMI)
hist(no_diabeticos$BMI)

n$n * 1.15 #45 por grupo

