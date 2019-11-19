# -------------------------------------- xxxxxxxxx   ---------------------------
# Capítulo Multivariables

#  Question 1

library(datasets)
data(Seatbelts)
seatbelts <- as.data.frame(Seatbelts)
fit <- lm(DriversKilled ~ kms + PetrolPrice, data = seatbelts)
summary(fit)$coef

# De acordo com o vídeo que acompanha o livro, os coeficientes dessa análise estão
# em escalas muito diferentes e a linha de interseção sugere um número absurdo para 
# a situação em que ambos, preço de petróleo e distância percorrida são zero.
# Uma nova regresão foi proposta com centralizando as duas médias

library(dplyr)
seatbelts = mutate(seatbelts,
                   pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
                   mm = kms/1000,
                   mmc = mm - mean(mm))

fit2 <- lm(DriversKilled ~ mmc + pp, data = seatbelts)
summary(fit2)$coef

# Question 2

predict(fit, newdata = data_frame(kms = mean(seatbelts$kms), PetrolPrice = mean(seatbelts$PetrolPrice)))
# O resultado equivale à interseção apresentada na segunda regressão onde justamente os 
# estimadores foram levados à média.


# Question 3

dk <- seatbelts$DriversKilled
pp <- seatbelts$PetrolPrice
km <- seatbelts$kms


fitFull <- lm(dk ~ pp+km)
summary(fitFull)$coef

rdk <- resid(lm(dk ~ km))
rkm <- resid(lm(pp ~ km))
summary(lm(rdk~rkm -1))$coef


# Question 4

rdk <- resid(lm(dk ~ pp))
rpp <- resid(lm(km ~ pp))
summary(lm(rdk~rpp -1))$coef


# -------------------------------------- xxxxxxxxx   ---------------------------
# Capitulo examples and tricks



data(Seatbelts)
seatbelts <- as.data.frame(Seatbelts)
library(dplyr)

seatbelts = mutate(seatbelts,
                   dk_log = log(DriversKilled),
                   pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
                   mm = kms/1000,
                   mmc = mm - mean(mm))
fit <- lm (DriversKilled ~ pp + mmc, data = seatbelts)
fit_log <- lm(dk_log ~ mmc + pp, data = seatbelts)
summary(fit_log)$coef

# Analise do coeficiente para o preço do pretroleo

1 - exp(fit_log$coef[3])

# Este resultado indica que um aumento de um desvio padrão no preço do petroleo
# provoca uma redução de 6% no número médio de motoristas mortos

1 - exp(fit_log$coef[2])

# Este resultado indica que um aumento de 1000 kilometros
# provoca uma redução de 1% no número médio de motoristas mortos

fit2 <- lm (DriversKilled ~ pp + mmc + law, data = seatbelts)
summary(fit2)

# Este resultado indica que a presença da lei acarreta em uma redução média de 12
# Motoristas mortos mantendo constantes os preços de petróleo e o número de kms


seatbelts2 = mutate(seatbelts,
                   lawFactor = factor(law, levels = c(0,1), labels = c("No", "Yes")),
                   pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
                   mm = kms/1000,
                   mmc = mm - mean(mm))
relevel(seatbelts2$lawFactor,"Yes")


fitlaw <- lm (DriversKilled ~ pp + mmc + lawFactor, data = seatbelts2)
summary(fitlaw)


fitlaw2 <- lm (DriversKilled ~ pp + mmc + I(factor(law)), data = seatbelts)
summary(fitlaw2)

# Não houve alteração no resultado como esperado



seatbelts3 = mutate(seatbelts,
                    lawFactor = factor(law, levels = c(0,1), labels = c("No", "Yes")),
                    pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
                    ppf = as.factor((pp<= -1.5)+(pp<=0)+(pp<1.5)+(pp< Inf)),
                    mm = kms/1000,
                    mmc = mm - mean(mm))

fitpp <- lm(DriversKilled ~factor(ppf) +mmc + law , data = seatbelts3)
summary(fitpp)

# -------------------------------------- xxxxxxxxx   ---------------------------
# Capitulo adjustments

data(Seatbelts)
seatbelts <- as.data.frame(Seatbelts)
library(dplyr)

seatbelts = mutate(seatbelts,
                   pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
                   mm = kms/1000,
                   mmc = mm - mean(mm))

fitfull <- lm(DriversKilled ~ mmc + pp, data = seatbelts)
fitkm <- lm(DriversKilled ~ mmc, data = seatbelts)


summary(fitfull)$coef
summary(fitkm)$coef
summary(fitkm)$coef[2]-summary(fitfull)$coef[2]
anova(fitfull,fitkm)

# Não houve variação na interseção e no modelo sem incluir o preço do petróleo,
# o decréscimo da motoristas mortos em função do aumento de 1000 kms ficou mais acentuado.
# Fica evidenciado que o preço de petroleo confunde um pouco a variável de kms percorridos.


fitpp <- lm(DriversKilled ~ pp, data = seatbelts)

summary(fitfull)$coef
summary(fitpp)$coef
summary(fitpp)$coef[2]-summary(fitfull)$coef[3]
anova(fitfull,fitpp)

# A exemplo do ocorrido com a variação do número de mortes em função da quilometragem
# a variação em função do preço do petróleo ficou mais acentuada.
