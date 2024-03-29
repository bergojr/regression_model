# -------------------------------------- xxxxxxxxx   ---------------------------
# Cap�tulo Multivariables

#  Question 1

library(datasets)
data(Seatbelts)
seatbelts <- as.data.frame(Seatbelts)
fit <- lm(DriversKilled ~ kms + PetrolPrice, data = seatbelts)
summary(fit)$coef

# De acordo com o v�deo que acompanha o livro, os coeficientes dessa an�lise est�o
# em escalas muito diferentes e a linha de interse��o sugere um n�mero absurdo para 
# a situa��o em que ambos, pre�o de petr�leo e dist�ncia percorrida s�o zero.
# Uma nova regres�o foi proposta com centralizando as duas m�dias

library(dplyr)
seatbelts = mutate(seatbelts,
                   pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
                   mm = kms/1000,
                   mmc = mm - mean(mm))

fit2 <- lm(DriversKilled ~ mmc + pp, data = seatbelts)
summary(fit2)$coef

# Question 2

predict(fit, newdata = data_frame(kms = mean(seatbelts$kms), PetrolPrice = mean(seatbelts$PetrolPrice)))
# O resultado equivale � interse��o apresentada na segunda regress�o onde justamente os 
# estimadores foram levados � m�dia.


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

# Analise do coeficiente para o pre�o do pretroleo

1 - exp(fit_log$coef[3])

# Este resultado indica que um aumento de um desvio padr�o no pre�o do petroleo
# provoca uma redu��o de 6% no n�mero m�dio de motoristas mortos

1 - exp(fit_log$coef[2])

# Este resultado indica que um aumento de 1000 kilometros
# provoca uma redu��o de 1% no n�mero m�dio de motoristas mortos

fit2 <- lm (DriversKilled ~ pp + mmc + law, data = seatbelts)
summary(fit2)

# Este resultado indica que a presen�a da lei acarreta em uma redu��o m�dia de 12
# Motoristas mortos mantendo constantes os pre�os de petr�leo e o n�mero de kms


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

# N�o houve altera��o no resultado como esperado



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

# N�o houve varia��o na interse��o e no modelo sem incluir o pre�o do petr�leo,
# o decr�scimo da motoristas mortos em fun��o do aumento de 1000 kms ficou mais acentuado.
# Fica evidenciado que o pre�o de petroleo confunde um pouco a vari�vel de kms percorridos.


fitpp <- lm(DriversKilled ~ pp, data = seatbelts)

summary(fitfull)$coef
summary(fitpp)$coef
summary(fitpp)$coef[2]-summary(fitfull)$coef[3]
anova(fitfull,fitpp)

# A exemplo do ocorrido com a varia��o do n�mero de mortes em fun��o da quilometragem
# a varia��o em fun��o do pre�o do petr�leo ficou mais acentuada.
