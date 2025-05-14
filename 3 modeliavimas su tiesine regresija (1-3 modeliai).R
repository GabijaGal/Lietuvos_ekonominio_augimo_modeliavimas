################################################################################
#3 žingsnis - modeliavimas. 3 ŽINGSNYJE MODELIUOJU TIK SU TIESINE REGRESIJA

#Naudoju skirtingus modeliavimo metodus ir testuoju
#modelius įtraukiant skirtingus nepriklausomus kintamuosius

#Rodiklių pasiskirstymo grafikai yra 2 žingsnyje (2 pradinė analizė (sklaida, 
#sezoniškumas, koreliacija))
################################################################################

library(tseries)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(gridExtra)
library(tidyr)
library(grid)
library(car)
library(lmtest)
library(prais)
library(knitr)
library(kableExtra)
library(Metrics)

################################################################################
################################################################################
             #1 modelis - be BVP komponenčių, be rodiklių sąveikų
################################################################################
################################################################################
data1_1 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
data1_1 <- read_excel(data1_1, sheet = "BE SEZONIŠKUMO")

rodikliai1_1 <- data1_1 %>%
  dplyr::select(Metai, BVP_I_palyg, Imigrantai, Emigrantai, Taupymas, Mirštamumas, Paslaugų_pasitikėjimas,
                Nedarbas_bendras, Gyventojai, Vartotojų_pasitikėjimas, Gimstamumas, BVP_vartojimas_palyg)

rodikliai1_1_pries <- rodikliai1_1

################################################################################
#stacionarumo tikrinimas

#sezoniškumas iš duomenų jau pašalintas, bet prieš pradedant modeliuot tikrinu
#stacionarumą dėl galimai reikalingo diferencijavimo
transformavimui <- colnames(rodikliai1_1)

for (k in transformavimui) {
  print(k)
  print(adf.test(na.omit(rodikliai1_1[[k]]), alternative = "stationary"))}
  
#BVP_I_palyg p-value = 0.7032
#Imigrantai  p-value = 0.01
#Emigrantai  p-value = 0.0166
#Taupymas  p-value = 0.02693
#Mirštamumas   p-value = 0.02578
#Paslaugų_pasitikėjimas  p-value = 0.1726
#Nedarbas_bendras  p-value = 0.313
#Gyventojai  p-value = 0.99
#Vartotojų_pasitikėjimas  p-value = 0.536
#Gimstamumas  p-value = 0.9197
#BVP_vartojimas_palyg  p-value = 0.4602

#papildomai vizualiai patikrinu autokoreliaciją prieš diferencijavimą
acf(rodikliai1_1$BVP_I_palyg, na.action = na.pass, main = "ACF: BVP_I_palyg")
acf(rodikliai1_1$Paslaugų_pasitikėjimas, na.action = na.pass, main = "ACF: Paslaugų pasitikėjimas")
acf(rodikliai1_1$Nedarbas_bendras, na.action = na.pass, main = "ACF: Nedarbas bendras")
acf(rodikliai1_1$Gyventojai, na.action = na.pass, main = "ACF: Gyventojai")
acf(rodikliai1_1$Vartotojų_pasitikėjimas, na.action = na.pass, main = "ACF: Vartotojų pasitikėjimas")
acf(rodikliai1_1$Gimstamumas, na.action = na.pass, main = "ACF: Gimstamumas")
acf(rodikliai1_1$BVP_vartojimas_palyg, na.action = na.pass, main = "ACF: BVP vartojimas")

rodikliai1_1 <- rodikliai1_1 %>%
  mutate(BVP_I_palyg_orig = BVP_I_palyg)

#reikalingi rodikliai diferencijuojami
rodikliai1_1_dif <- rodikliai1_1 %>%
  arrange(Metai) %>%  
  mutate(
    BVP_I_palyg = c(NA, diff(BVP_I_palyg_orig)),
    BVP_I_palyg_lag = dplyr::lag(c(NA, diff(BVP_I_palyg_orig)), 1),
    Paslaugų_pasitikėjimas = c(NA, diff(Paslaugų_pasitikėjimas)),
    Nedarbas_bendras = c(NA, diff(Nedarbas_bendras)),
    Gyventojai = c(NA, diff(Gyventojai)),
    Vartotojų_pasitikėjimas = c(NA, diff(Vartotojų_pasitikėjimas)),
    Gimstamumas = c(NA, diff(Gimstamumas)),
    BVP_vartojimas_palyg = c(NA, diff(BVP_vartojimas_palyg))
    )

rodikliai1_1_dif <- rodikliai1_1_dif %>% na.omit()

#tikrinu rodiklių autokolinearumą po diferencijavimo (kitoms regresijoms nekartoju
#šio žingsnio, nes diferencijavimas atliekamas tame pačiame žingsnyje, su tais pačiais
#duomenimis)
par(mfrow = c(2, 2))

acf(rodikliai1_1_dif$BVP_I_palyg, na.action = na.pass, main = "ACF: BVP_I_palyg")
acf(rodikliai1_1_dif$Paslaugų_pasitikėjimas, na.action = na.pass, main = "ACF: Paslaugų pasitikėjimas")
acf(rodikliai1_1_dif$Nedarbas_bendras, na.action = na.pass, main = "ACF: Nedarbas bendras")
acf(rodikliai1_1_dif$Gyventojai, na.action = na.pass, main = "ACF: Gyventojai")
acf(rodikliai1_1_dif$Vartotojų_pasitikėjimas, na.action = na.pass, main = "ACF: Vartotojų pasitikėjimas")
acf(rodikliai1_1_dif$Gimstamumas, na.action = na.pass, main = "ACF: Gimstamumas")
acf(rodikliai1_1_dif$BVP_I_palyg_lag, na.action = na.pass, main = "ACF: BVP_I_palyg_lag")
acf(rodikliai1_1_dif$BVP_vartojimas_palyg, na.action = na.pass, main = "ACF: BVP vartojimas")

par(mfrow = c(1, 1))

################################################################################
#transformacijų dalis

#rodiklius turinčius neigiamas arba 0 reikšmes nelogaritmuoju
neig <- sapply(rodikliai1_1_dif, function(x) sum(x <= 0, na.rm = TRUE))
nereikalingi <- names(neig[neig > 0])
cat("Netinkami rodikliai: ")
print(nereikalingi)

#transformacijos remiantis duomenų pasiskirstymu 
rodikliai1_1_dif_log <- rodikliai1_1_dif %>%
  mutate(
    Mirštamumas = log(Mirštamumas))

gr1 <- function(data, y_var, x_vars, pavadinimas) {
  grafikai <- lapply(x_vars, function(x) {
    ggplot(data, aes_string(x = x, y = y_var)) +
      geom_point(color = "#78003F", alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
      theme_minimal() +
      labs(title = paste(y_var, "~", x), x = x, y = y_var)})
  
  title <- textGrob(pavadinimas, gp = gpar(fontsize = 16))
  grid.arrange(grobs = grafikai, ncol = 3, top = title)}

x_rod <- setdiff(colnames(rodikliai1_1_pries), c("BVP_I_palyg", "Metai"))
gr1(rodikliai1_1_pries, "BVP_I_palyg", x_rod, "Prieš transformacijas")

x_rod <- setdiff(colnames(rodikliai1_1_dif), c("BVP_I_palyg", "Metai"))
gr1(rodikliai1_1_dif, "BVP_I_palyg", x_rod, "Po diferencijavimo")

x_rod <- setdiff(colnames(rodikliai1_1_dif_log), c("BVP_I_palyg", "Metai"))
gr1(rodikliai1_1_dif_log, "BVP_I_palyg", x_rod, "Po diferencijavimo ir logaritmavimo")

#logaritmavimas nepadėjo, todėl jokių papildomų transformacijų netaikau toliau
############################################################################
#kriziniai laikotarpiai 
rodikliai1_1 <- rodikliai1_1_dif

rodikliai1_1 <- rodikliai1_1 %>% mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0))

#prieš modeliavimą dalinu duomenis į mokymosi ir testavimo imtis
train <- rodikliai1_1 %>% filter(Metai <= 2017) %>% na.omit()
test <- rodikliai1_1 %>% filter(Metai > 2017) %>% na.omit()

################################################################################
#pirminė regresija (1 MODELIS) su visais rodikliais (išskyrus metų ir
#vartojimo rodiklius, kuris naudojamas kitame modelyje)
modelis1_1 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig,
                 data = train)
summary(modelis1_1)
vif(modelis1_1)

modelis1_1 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
                 -Taupymas,
                 data = train)
summary(modelis1_1)
vif(modelis1_1)

modelis1_1 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
                 -Taupymas -Gimstamumas,
                 data = train)
summary(modelis1_1)
vif(modelis1_1)

modelis1_1 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
                 -Taupymas -Gimstamumas -Mirštamumas,
                 data = train)
summary(modelis1_1)
vif(modelis1_1)

modelis1_1 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
                 -Taupymas -Gimstamumas -Mirštamumas -Imigrantai,
                 data = train)
summary(modelis1_1)
vif(modelis1_1)

modelis1_1 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
                 -Taupymas -Gimstamumas -Mirštamumas -Imigrantai -Nedarbas_bendras,
                 data = train)
summary(modelis1_1)
vif(modelis1_1)

modelis1_1 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
                 -Taupymas -Gimstamumas -Mirštamumas -Imigrantai -Nedarbas_bendras
                 -Gyventojai, data = train)
summary(modelis1_1)
vif(modelis1_1)

modelis1_1 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
                 -Taupymas -Gimstamumas -Mirštamumas -Imigrantai -Nedarbas_bendras
                 -Gyventojai -Paslaugų_pasitikėjimas, data = train)
summary(modelis1_1)
vif(modelis1_1)

modelis1_1 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
                 -Taupymas -Gimstamumas -Mirštamumas -Imigrantai -Nedarbas_bendras
                 -Gyventojai -Paslaugų_pasitikėjimas -Emigrantai, data = train)
summary(modelis1_1)
vif(modelis1_1)

modelis1_1 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
                 -Taupymas -Gimstamumas -Mirštamumas -Imigrantai -Nedarbas_bendras
                 -Gyventojai -Paslaugų_pasitikėjimas -Emigrantai
                 -Krizė, data = train)
summary(modelis1_1)
vif(modelis1_1)
################################################################################
#1 modelio liekanų pasiskirstymas (atlieku tikrinimą prieš papildomą modelio 
#rezultatų įvertinimą)
train$likuciai <- resid(modelis1_1)
train$pritaikyta <- fitted(modelis1_1)

ggplot(train, aes(x = pritaikyta, y = likuciai)) +
  geom_point(color = "#78003F", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "Liekanos ir prognozuotos reikšmės",
       x = "Prognozuotos reikšmės",
       y = "Liekanos")

ggplot(train, aes(x = likuciai)) +
  geom_histogram(fill = "#78003F", color = "white", bins = 30) +
  theme_minimal() +
  labs(title = "Liekanų pasiskirstymas",
       x = "Liekanos",
       y = "Dažnis")

qqnorm(train$likuciai, col = "#78003F")
qqline(train$likuciai, col = "black", lwd = 2)

################################################################################
#modelio pritaikymas test imčiai 
test$prognozuota <- predict(modelis1_1, test)
test$likuciai <- test$BVP_I_palyg - test$prognozuota

ggplot(test, aes(x = prognozuota, y = likuciai)) +
  geom_point(color = "#78003F", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "Testinės imties liekanos ir prognozuotos reikšmės",
       x = "Prognozuotos reikšmės",
       y = "Liekanos")

ggplot(test, aes(x = likuciai)) +
  geom_histogram(fill = "#78003F", color = "white", bins = 30) +
  theme_minimal() +
  labs(title = "Testinės imties liekanų pasiskirstymas",
       x = "Liekanos",
       y = "Dažnis")

qqnorm(test$likuciai, col = "#78003F")
qqline(test$likuciai, col = "black", lwd = 2)

################################################################################
#liekanų autokoreliacijos tikrinimas (1.378868)
durbinWatsonTest(modelis1_1)  

#liekanos (0.00122)
shapiro.test(residuals(modelis1_1))

#mae (85.30156)/rmse (111.7249)/mse (12482.46)
prognoze1_1 <- predict(modelis1_1, test)

mse(test$BVP_I_palyg, prognoze1_1)
mae(test$BVP_I_palyg, prognoze1_1)
rmse(test$BVP_I_palyg, prognoze1_1)

#aic (2180.905)/bic(2193.765)
AIC(modelis1_1)
BIC(modelis1_1)

#bp (0.004728)
bptest(modelis1_1)

################################################################################
################################################################################
         #2 modelis - be BVP komponenčių, su rodiklių sąveikomis
                     #remiantis transformacijomis iš 1.1 modelio
################################################################################
################################################################################
train <- rodikliai1_1 %>% filter(Metai <= 2017) %>% na.omit()
test <- rodikliai1_1 %>% filter(Metai > 2017) %>% na.omit()

#rodiklių sąveikų pridėjimas prie duomenų 
train2 <- train %>%
  mutate(
    Nedarbas_Emigracija = Nedarbas_bendras * Emigrantai,
    Nedarbas_Gyventojai = Nedarbas_bendras * Gyventojai,
    Krizė_Taupymas = Krizė * Taupymas,
    Krizė_Emigracija = Krizė * Emigrantai
  )

test2 <- test %>%
  mutate(
    Nedarbas_Emigracija = Nedarbas_bendras * Emigrantai,
    Nedarbas_Gyventojai = Nedarbas_bendras * Gyventojai,
    Krizė_Taupymas = Krizė * Taupymas,
    Krizė_Emigracija = Krizė * Emigrantai
  )

#testavimui
print(train)

################################################################################
#pirminė regresija (2 MODELIS) su visais rodikliais (išskyrus metų ir
#vartojimo rodiklius, kuris naudojamas kitame modelyje)
modelis2 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig, data = train2)
summary(modelis2)
vif(modelis2)

modelis2 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
               -Krizė_Emigracija, data = train2)
summary(modelis2)
vif(modelis2)

modelis2 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
               -Krizė_Emigracija -Krizė_Taupymas, data = train2)
summary(modelis2)
vif(modelis2)

modelis2 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
               -Krizė_Emigracija -Krizė_Taupymas -Nedarbas_Emigracija, data = train2)
summary(modelis2)
vif(modelis2)

modelis2 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
               -Krizė_Emigracija -Krizė_Taupymas -Nedarbas_Emigracija
               -Nedarbas_Gyventojai, data = train2)
summary(modelis2)
vif(modelis2)

modelis2 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
               -Krizė_Emigracija -Krizė_Taupymas -Nedarbas_Emigracija
               -Nedarbas_Gyventojai -Taupymas, data = train2)
summary(modelis2)
vif(modelis2)

modelis2 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
               -Krizė_Emigracija -Krizė_Taupymas -Nedarbas_Emigracija
               -Nedarbas_Gyventojai -Taupymas -Gimstamumas, data = train2)
summary(modelis2)
vif(modelis2)

modelis2 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
               -Krizė_Emigracija -Krizė_Taupymas -Nedarbas_Emigracija
               -Nedarbas_Gyventojai -Taupymas -Gimstamumas -Mirštamumas, data = train2)
summary(modelis2)
vif(modelis2)

modelis2 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
               -Krizė_Emigracija -Krizė_Taupymas -Nedarbas_Emigracija
               -Nedarbas_Gyventojai -Taupymas -Gimstamumas -Mirštamumas
               -Imigrantai, data = train2)
summary(modelis2)
vif(modelis2)

modelis2 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
               -Krizė_Emigracija -Krizė_Taupymas -Nedarbas_Emigracija
               -Nedarbas_Gyventojai -Taupymas -Gimstamumas -Mirštamumas
               -Imigrantai -Nedarbas_bendras, data = train2)
summary(modelis2)
vif(modelis2)

modelis2 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
               -Krizė_Emigracija -Krizė_Taupymas -Nedarbas_Emigracija
               -Nedarbas_Gyventojai -Taupymas -Gimstamumas -Mirštamumas
               -Imigrantai -Nedarbas_bendras -Gyventojai, data = train2)
summary(modelis2)
vif(modelis2)

modelis2 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
               -Krizė_Emigracija -Krizė_Taupymas -Nedarbas_Emigracija
               -Nedarbas_Gyventojai -Taupymas -Gimstamumas -Mirštamumas
               -Imigrantai -Nedarbas_bendras -Gyventojai -Paslaugų_pasitikėjimas, data = train2)
summary(modelis2)
vif(modelis2)

modelis2 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
               -Krizė_Emigracija -Krizė_Taupymas -Nedarbas_Emigracija
               -Nedarbas_Gyventojai -Taupymas -Gimstamumas -Mirštamumas
               -Imigrantai -Nedarbas_bendras -Gyventojai -Paslaugų_pasitikėjimas
               -Emigrantai, data = train2)
summary(modelis2)
vif(modelis2)

modelis2 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
               -Krizė_Emigracija -Krizė_Taupymas -Nedarbas_Emigracija
               -Nedarbas_Gyventojai -Taupymas -Gimstamumas -Mirštamumas
               -Imigrantai -Nedarbas_bendras -Gyventojai -Paslaugų_pasitikėjimas
               -Emigrantai -Krizė, data = train2)
summary(modelis2)
vif(modelis2)

################################################################################
#2 modelio liekanų pasiskirstymas (atlieku tikrinimą prieš papildomą modelio 
#rezultatų įvertinimą)
train2$likuciai <- resid(modelis2)
train2$pritaikyta <- fitted(modelis2)

ggplot(train2, aes(x = pritaikyta, y = likuciai)) +
  geom_point(color = "#78003F", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "Liekanos ir prognozuotos reikšmės",
       x = "Prognozuotos reikšmės",
       y = "Liekanos")

ggplot(train2, aes(x = likuciai)) +
  geom_histogram(fill = "#78003F", color = "white", bins = 30) +
  theme_minimal() +
  labs(title = "Liekanų pasiskirstymas",
       x = "Liekanos",
       y = "Dažnis")

qqnorm(train2$likuciai, col = "#78003F")
qqline(train2$likuciai, col = "black", lwd = 2)

################################################################################
#modelio pritaikymas test imčiai
test2$prognozuota <- predict(modelis2, test2)
test2$likuciai <- test2$BVP_I_palyg - test2$prognozuota

ggplot(test2, aes(x = prognozuota, y = likuciai)) +
  geom_point(color = "#78003F", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "Testinės imties liekanos ir prognozuotos reikšmės",
       x = "Prognozuotos reikšmės",
       y = "Liekanos")

ggplot(test2, aes(x = likuciai)) +
  geom_histogram(fill = "#78003F", color = "white", bins = 30) +
  theme_minimal() +
  labs(title = "Testinės imties liekanų pasiskirstymas",
       x = "Liekanos",
       y = "Dažnis")

qqnorm(test2$likuciai, col = "#78003F")
qqline(test2$likuciai, col = "black", lwd = 2)

################################################################################
#liekanų autokoreliacijos tikrinimas (1.378868)
durbinWatsonTest(modelis2)  

#liekanos nėra normalios (0.00122)
shapiro.test(residuals(modelis2))

#mae (85.30156)/rmse (111.7249)/mse (12482.46)
prognoze2 <- predict(modelis2, test2)

mse(test$BVP_I_palyg, prognoze2)
mae(test$BVP_I_palyg, prognoze2)
rmse(test$BVP_I_palyg, prognoze2)

#aic (2180.905)/bic(2193.765)
AIC(modelis2)
BIC(modelis2)

#bp (0.004728)
bptest(modelis2)


################################################################################
################################################################################
          #3 modelis - su BVP komponente, be rodiklių sąveikos
################################################################################
################################################################################
data1_1 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
data1_1 <- read_excel(data1_1, sheet = "BE SEZONIŠKUMO")

rodikliai1_1 <- data1_1 %>%
  dplyr::select(Metai, BVP_I_palyg, Imigrantai, Emigrantai, Taupymas, Mirštamumas, Paslaugų_pasitikėjimas,
                Nedarbas_bendras, Gyventojai, Vartotojų_pasitikėjimas, Gimstamumas, BVP_vartojimas_palyg)

rodikliai1_1 <- rodikliai1_1 %>%
  mutate(BVP_I_palyg_orig = BVP_I_palyg)

rodikliai1_1_dif <- rodikliai1_1 %>%
  arrange(Metai) %>%  
  mutate(
    BVP_I_palyg = c(NA, diff(BVP_I_palyg_orig)),
    BVP_I_palyg_lag = dplyr::lag(c(NA, diff(BVP_I_palyg_orig)), 1),
    Paslaugų_pasitikėjimas = c(NA, diff(Paslaugų_pasitikėjimas)),
    Nedarbas_bendras = c(NA, diff(Nedarbas_bendras)),
    Gyventojai = c(NA, diff(Gyventojai)),
    Vartotojų_pasitikėjimas = c(NA, diff(Vartotojų_pasitikėjimas)),
    Gimstamumas = c(NA, diff(Gimstamumas)),
    BVP_vartojimas_palyg = c(NA, diff(BVP_vartojimas_palyg))
  )

rodikliai1_1_dif <- rodikliai1_1_dif %>% na.omit()

rodikliai1_1 <- rodikliai1_1_dif

rodikliai1_1 <- rodikliai1_1 %>%
  mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0))

train3 <- rodikliai1_1 %>% filter(Metai <= 2017)  
test3 <- rodikliai1_1 %>% filter(Metai > 2017)

modelis3 <- lm(BVP_I_palyg ~ . -Metai -BVP_I_palyg_orig -BVP_I_palyg_lag, data = train3)
summary(modelis3)
vif(modelis3)

modelis3 <- lm(BVP_I_palyg ~ . -Metai -BVP_I_palyg_orig -BVP_I_palyg_lag
               -Taupymas, data = train3)
summary(modelis3)
vif(modelis3)

modelis3 <- lm(BVP_I_palyg ~ . -Metai -BVP_I_palyg_orig -BVP_I_palyg_lag
               -Taupymas -Krizė, data = train3)
summary(modelis3)
vif(modelis3)

modelis3 <- lm(BVP_I_palyg ~ . -Metai -BVP_I_palyg_orig -BVP_I_palyg_lag
               -Taupymas -Krizė -Emigrantai, data = train3)
summary(modelis3)
vif(modelis3)

modelis3 <- lm(BVP_I_palyg ~ . -Metai -BVP_I_palyg_orig -BVP_I_palyg_lag
               -Taupymas -Krizė -Emigrantai -Imigrantai, data = train3)
summary(modelis3)
vif(modelis3)

modelis3 <- lm(BVP_I_palyg ~ . -Metai -BVP_I_palyg_orig -BVP_I_palyg_lag
               -Taupymas -Krizė -Emigrantai -Imigrantai -Mirštamumas, data = train3)
summary(modelis3)
vif(modelis3)

modelis3 <- lm(BVP_I_palyg ~ . -Metai -BVP_I_palyg_orig -BVP_I_palyg_lag
               -Taupymas -Krizė -Emigrantai -Imigrantai -Mirštamumas
               -Gimstamumas, data = train3)
summary(modelis3)
vif(modelis3)

modelis3 <- lm(BVP_I_palyg ~ . -Metai -BVP_I_palyg_orig -BVP_I_palyg_lag
               -Taupymas -Krizė -Emigrantai -Imigrantai -Mirštamumas
               -Gimstamumas -Vartotojų_pasitikėjimas, data = train3)
summary(modelis3)
vif(modelis3)

#DW 0.673. Netenkina rezultatas, todėl taikau korekciją liekanų autokoreliacijos mažinimui
#išbandžiau ir lag(BVP) įtraukimą, nepadėjo
durbinWatsonTest(modelis3)  

########################
#Prais-Winsten korekcija
modelis3_kor2 <- prais_winsten(BVP_I_palyg ~ Paslaugų_pasitikėjimas + 
                                 Nedarbas_bendras + Gyventojai + BVP_vartojimas_palyg, 
                               data = train3, index = "Metai")

summary(modelis3_kor2)

modelis3_kor2 <- prais_winsten(BVP_I_palyg ~  + 
                                 Nedarbas_bendras + Gyventojai + BVP_vartojimas_palyg, 
                               data = train3, index = "Metai")

summary(modelis3_kor2)

modelis3_kor2 <- prais_winsten(BVP_I_palyg ~  + 
                                 Gyventojai + BVP_vartojimas_palyg, 
                               data = train3, index = "Metai")

#DW 1.282
summary(modelis3_kor2)

modelis3 <- modelis3_kor2
summary(modelis3)

likuciai <- residuals(modelis3_kor2)

durbinWatsonTest(likuciai)

################################################################################
#liekanos (0.01829)
shapiro.test(residuals(modelis3))

#bp (0.09265)
bptest(modelis3)

X_test3 <- model.matrix(~ Gyventojai + BVP_vartojimas_palyg, data = test3)
prognozes3 <- X_test3 %*% coef(modelis3)

reali <- test3$BVP_I_palyg
mse(reali, prognozes3) #14629.42
rmse(reali, prognozes3) #120.9521
mae(reali, prognozes3) #94.89238

n <- length(reali)
k <- length(coef(modelis3))
RSS <- sum((reali - prognozes3)^2)
AIC3 <- n * log(RSS / n) + 2 * k
BIC3 <- n * log(RSS / n) + log(n) * k

AIC3 #782.854
BIC3 #790.0373

################################################################################
################################################################################
                          #1-3 modelių prognozių lyginimas
################################################################################
################################################################################
n_prognoziu <- nrow(test)
prog_datos <- seq(as.Date("2018-01-01"), by = "month", length.out = n_prognoziu)

test$Data  <- prog_datos
test2$Data <- prog_datos
test3$Data <- prog_datos

bvp_pradine1_1 <- rodikliai1_1_pries %>%
  filter(Metai == 2017) %>% tail(1) %>% pull(BVP_I_palyg)

bvp_pradine2 <- bvp_pradine1_1
bvp_pradine3 <- bvp_pradine1_1

#atlieku atgalines transformacijas, kad modelių prognozės būtų toje pačioje skalėje
#kaip ir realūs BVP duomenys
atdiferen <- function(diff_reiksmes, pradzia) {
  n <- length(diff_reiksmes)
  lygiu_reiksmes <- numeric(n)
  lygiu_reiksmes[1] <- pradzia + diff_reiksmes[1]
  for (i in 2:n) {
    lygiu_reiksmes[i] <- lygiu_reiksmes[i - 1] + diff_reiksmes[i]
  }
  return(lygiu_reiksmes)
}

X_test3 <- model.matrix(~ Gyventojai + BVP_vartojimas_palyg, data = test3)
prognozes3 <- X_test3 %*% coef(modelis3_kor2)

prognozes_diff <- data.frame(
  Data = prognoziu_datos,
  Modelis1_1 = predict(modelis1_1, newdata = test),
  Modelis2   = predict(modelis2,   newdata = test2),
  Modelis3   = as.numeric(prognozes3)
)

prognozes_lygio <- prognozes_diff %>%
  mutate(
    Modelis1_1 = atdiferen(Modelis1_1, bvp_pradine1_1),
    Modelis2   = atdiferen(Modelis2,   bvp_pradine2),
    Modelis3   = atdiferen(Modelis3,   bvp_pradine3)
  )

prognozes_long <- prognozes_lygio %>%
  pivot_longer(cols = starts_with("Modelis"), names_to = "Tipas", values_to = "BVP_I_palyg")

rodikliai1_1_pries$Data <- seq(as.Date("2002-07-01"), by = "month", length.out = nrow(rodikliai1_1_pries))

originalus <- rodikliai1_1_pries %>%
  select(Data, BVP_I_palyg) %>% mutate(Tipas = "Reali reikšmė")

grafiko_duomenys <- bind_rows(originalus, prognozes_long)

grafiko_duomenys$Tipas <- factor(grafiko_duomenys$Tipas,
                                 levels = c("Reali reikšmė", "Modelis1_1", "Modelis2", "Modelis3"),
                                 labels = c("Reali reikšmė", "Pradinis modelis", "Su sąveikomis", "Su BVP komponente"))

prog <- ggplot(grafiko_duomenys, aes(x = Data, y = BVP_I_palyg / 1000, color = Tipas, linetype = Tipas, group = Tipas)) +  
  geom_line(size = 1.3) +
  geom_vline(xintercept = as.Date("2017-12-01"), linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("2018-01-01"),
           y = max(grafiko_duomenys$BVP_I_palyg, na.rm = TRUE) / 1000,
           label = "Prasideda prognozės", hjust = 0, vjust = 1.2, color = "black", size = 6) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1, big.mark = ","),
                     name = "BVP (mlrd. eur.)",
                     breaks = seq(5, 20, by = 2)) +
  labs(title = "Tiesinės regresijos modelių prognozių palyginimas",
       x = "Metai", y = NULL, color = NULL, linetype = NULL) +
  scale_color_manual(values = c(
    "Reali reikšmė" = "black", "Pradinis modelis" = "#78003F",
    "Su sąveikomis"      = "#E64164", "Su BVP komponente" = "#DCDCDC"
  )) +
  scale_linetype_manual(values = c(
    "Reali reikšmė" = "solid",
    "Pradinis modelis" = "solid",
    "Su sąveikomis" = "dashed",
    "Su BVP komponente" = "solid"
  )) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),          
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16), 
    axis.text.y = element_text(size = 16),
    legend.title = element_blank(),        
    legend.position = "bottom",
    legend.text = element_text(size = 17),
    plot.title = element_text(size = 24),
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = "gray80", linewidth = 0.8)  
  )

prog

#3 modelis (su vartojimu) prastai prognozuoja, todėl tikrinu tendenciją
ggplot(rodikliai1_1_pries, aes(x = Metai, y = BVP_vartojimas_palyg)) +
  geom_line(color = "#E64164", size = 1) +
  theme_minimal() +
  labs(title = "Vartojimo tendencija", x = "Metai", y = "Vartojimas")

ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/tiespro.png", prog,
       width = 10, height = 6.5, dpi = 300, units = "in")


################################################################################
#GALUTINIS 1-3 modelių testų rezultatų palyginimas
################################################################################

data1_1 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
data1_1 <- read_excel(data1_1, sheet = "BE SEZONIŠKUMO")

rodikliai1_1 <- data1_1 %>%
  dplyr::select(Metai, BVP_I_palyg, Imigrantai, Emigrantai, Taupymas, Mirštamumas, Paslaugų_pasitikėjimas,
                Nedarbas_bendras, Gyventojai, Vartotojų_pasitikėjimas, Gimstamumas, BVP_vartojimas_palyg)

rodikliai1_1 <- rodikliai1_1 %>% mutate(BVP_I_palyg_orig = BVP_I_palyg)

rodikliai1_1_dif <- rodikliai1_1 %>%
  arrange(Metai) %>%  
  mutate(
    BVP_I_palyg = c(NA, diff(BVP_I_palyg_orig)),
    BVP_I_palyg_lag = dplyr::lag(c(NA, diff(BVP_I_palyg_orig)), 1),
    Paslaugų_pasitikėjimas = c(NA, diff(Paslaugų_pasitikėjimas)),
    Nedarbas_bendras = c(NA, diff(Nedarbas_bendras)),
    Gyventojai = c(NA, diff(Gyventojai)),
    Vartotojų_pasitikėjimas = c(NA, diff(Vartotojų_pasitikėjimas)),
    Gimstamumas = c(NA, diff(Gimstamumas)),
    BVP_vartojimas_palyg = c(NA, diff(BVP_vartojimas_palyg)))

rodikliai1_1_dif <- rodikliai1_1_dif %>% na.omit()
rodikliai1_1 <- rodikliai1_1_dif
rodikliai1_1 <- rodikliai1_1 %>%
  mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0))

train <- rodikliai1_1 %>% filter(Metai <= 2017) %>% na.omit()
test <- rodikliai1_1 %>% filter(Metai > 2017) %>% na.omit()

########PRADINIŲ (SU SAVO TRANSFORMACIJOMIS) MODELIŲ PALYGINIMAS

mae1_dif <- mae(test$BVP_I_palyg, predict(modelis1_1, newdata = test))
mae2_dif <- mae(test2$BVP_I_palyg, predict(modelis2, newdata = test2))
mae3_dif <- mae(test3$BVP_I_palyg, as.numeric(X_test3 %*% coef(modelis3)))

rmse1_dif <- rmse(test$BVP_I_palyg, predict(modelis1_1, newdata = test))
rmse2_dif <- rmse(test2$BVP_I_palyg, predict(modelis2, newdata = test2))
rmse3_dif <- rmse(test3$BVP_I_palyg, as.numeric(X_test3 %*% coef(modelis3)))

mse1_dif <- mse(test$BVP_I_palyg, predict(modelis1_1, newdata = test))
mse2_dif <- mse(test2$BVP_I_palyg, predict(modelis2, newdata = test2))
mse3_dif <- mse(test3$BVP_I_palyg, as.numeric(X_test3 %*% coef(modelis3)))

n_dif <- nrow(test3)
k1 <- length(coef(modelis1_1))
k2 <- length(coef(modelis2))
k3 <- length(coef(modelis3))
RSS3_dif <- sum((test3$BVP_I_palyg - as.numeric(X_test3 %*% coef(modelis3)))^2)

aic1_dif <- AIC(modelis1_1)
aic2_dif <- AIC(modelis2)
aic3_dif <- n_dif * log(RSS3_dif / n_dif) + 2 * k3

bic1_dif <- BIC(modelis1_1)
bic2_dif <- BIC(modelis2)
bic3_dif <- n_dif * log(RSS3_dif / n_dif) + log(n_dif) * k3

rez_dif <- data.frame(
  Modelis = c("Pradinis modelis", "Su sąveikomis", "Su BVP komponente"),
  MAE  = round(c(mae1_dif,  mae2_dif,  mae3_dif), 2),
  RMSE = round(c(rmse1_dif, rmse2_dif, rmse3_dif), 2),
  MSE = round(c(mse1_dif, mse2_dif, mse3_dif), 2),
  AIC = round(c(aic1_dif, aic2_dif, aic3_dif), 2),
  BIC = round(c(bic1_dif, bic2_dif, bic3_dif), 2)
)

rez_dif

###########DETRANSFORMUOTŲ MODELIŲ PALYGINIMAS

reali1 <- atdiferen(test$BVP_I_palyg, bvp_pradine1_1)
reali2 <- atdiferen(test2$BVP_I_palyg, bvp_pradine2)
reali3 <- atdiferen(test3$BVP_I_palyg, bvp_pradine3)

prog1 <- prognozes_lygio$Modelis1_1
prog2 <- prognozes_lygio$Modelis2
prog3 <- prognozes_lygio$Modelis3

resid1 <- reali1 - prog1
resid2 <- reali2 - prog2
resid3 <- reali3 - prog3

mae_detr <- c(mae(reali1, prog1), mae(reali2, prog2), mae(reali3, prog3))
rmse_detr <- c(rmse(reali1, prog1), rmse(reali2, prog2), rmse(reali3, prog3))
mse_detr <- c(mse(reali1, prog1), mse(reali2, prog2), mse(reali3, prog3))

n <- length(reali1)
k1 <- length(coef(modelis1_1))
k2 <- length(coef(modelis2))
k3 <- length(coef(modelis3))

RSS1 <- sum(resid1^2)
RSS2 <- sum(resid2^2)
RSS3 <- sum(resid3^2)

aic_detr <- c(n * log(RSS1 / n) + 2 * k1,
              n * log(RSS2 / n) + 2 * k2,
              n * log(RSS3 / n) + 2 * k3)

bic_detr <- c(n * log(RSS1 / n) + log(n) * k1,
              n * log(RSS2 / n) + log(n) * k2,
              n * log(RSS3 / n) + log(n) * k3)

rez_detr <- data.frame(
  Modelis = c("Pradinis modelis", "Su sąveikomis", "Su BVP komponente"),
  MAE = round(mae_detr, 2),
  RMSE = round(rmse_detr, 2),
  MSE = round(mse_detr, 3),
  AIC = round(aic_detr, 2),
  BIC = round(bic_detr, 2))

rez_detr