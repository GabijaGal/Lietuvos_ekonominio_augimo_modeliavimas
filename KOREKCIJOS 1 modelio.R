################################################################################
################################################################################
         #Liekanų autokoreliacijos korekcijų testavimas 1 modeliui
################################################################################
################################################################################

################################################################################
################################################################################
   #ORIGINALUS MODELIS 1 modelis - be BVP komponenčių, be rodiklių sąveikų

#šiam modeliui testuotos liekanų autokoreliacijos korekcijos, nes pradinė
#DW reikšmė nėra ideali (2)
################################################################################
################################################################################

data1_1 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
data1_1 <- read_excel(data1_1, sheet = "BE SEZONIŠKUMO")

rodikliai1_1 <- data1_1 %>%
  dplyr::select(Metai, BVP_I_palyg, Imigrantai, Emigrantai, Taupymas, Mirštamumas, Paslaugų_pasitikėjimas,
                Nedarbas_bendras, Gyventojai, Vartotojų_pasitikėjimas, Gimstamumas, BVP_vartojimas_palyg)

rodikliai1_1_pries <- rodikliai1_1

rodikliai1_1_dif <- rodikliai1_1 %>%
  arrange(Metai) %>%  
  mutate(
    BVP_I_palyg = c(NA, diff(BVP_I_palyg)),
    Paslaugų_pasitikėjimas = c(NA, diff(Paslaugų_pasitikėjimas)),
    Nedarbas_bendras = c(NA, diff(Nedarbas_bendras)),
    Gyventojai = c(NA, diff(Gyventojai)),
    Vartotojų_pasitikėjimas = c(NA, diff(Vartotojų_pasitikėjimas)),
    Gimstamumas = c(NA, diff(Gimstamumas)),
    BVP_vartojimas_palyg = c(NA, diff(BVP_vartojimas_palyg))
  )

rodikliai1_1 <- rodikliai1_1_dif

rodikliai1_1 <- rodikliai1_1 %>%
  mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0))

train <- rodikliai1_1 %>% filter(Metai <= 2017)  
test <- rodikliai1_1 %>% filter(Metai > 2017)

################################################################################
train <- na.omit(train)
test <- na.omit(test)

modelis1_1 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -Vartotojų_pasitikėjimas
                 -Emigrantai -Mirštamumas -Imigrantai -Gimstamumas
                 -Taupymas, data = train)
summary(modelis1_1)
vif(modelis1_1)

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
#liekanų autokoreliacijos tikrinimas (0.8285378)
durbinWatsonTest(modelis1_1)  

#liekanos (0.000493)
shapiro.test(residuals(modelis1_1))

#mae (117.5627)/rmse (159.1615)/mse (25332.39)
prognoze1_1 <- predict(modelis1_1, test)

mse(test$BVP_I_palyg, prognoze1_1)
mae(test$BVP_I_palyg, prognoze1_1)
rmse(test$BVP_I_palyg, prognoze1_1)

#aic (2288.33)/bic(2307.652)
AIC(modelis1_1)
BIC(modelis1_1)

#bp (0.0005713)
bptest(modelis1_1)


################################################################################
#Cochrane-Orcutt
################################################################################

modelis1_1 <- lm(BVP_I_palyg ~ Paslaugų_pasitikėjimas + Nedarbas_bendras +  
                 Gyventojai + Krizė, data = train)

dw_test <- durbinWatsonTest(modelis1_1)

rho <- 1 - dw_test$dw / 2  
print(paste("Pirmos eilės autokoreliacija rho:", rho))

train_kor1 <- train %>%
  arrange(Metai) %>%  
  mutate(
    BVP_I_palyg_lag = lag(BVP_I_palyg),
    Paslaugų_pasitikėjimas_lag = lag(Paslaugų_pasitikėjimas),
    Nedarbas_bendras_lag = lag(Nedarbas_bendras),
    Gyventojai_lag = lag(Gyventojai),
    Krizė_lag = lag(Krizė)
  ) %>%
  mutate(
    BVP_I_palyg = BVP_I_palyg - rho * BVP_I_palyg_lag,
    Paslaugų_pasitikėjimas = Paslaugų_pasitikėjimas - rho * Paslaugų_pasitikėjimas_lag,
    Nedarbas_bendras = Nedarbas_bendras - rho * Nedarbas_bendras_lag,
    Gyventojai = Gyventojai - rho * Gyventojai_lag,
    Krizė = Krizė - rho * Krizė_lag
  ) %>%
  na.omit()

modelis1_kor1 <- lm(BVP_I_palyg ~ Paslaugų_pasitikėjimas + Nedarbas_bendras +  
                      Gyventojai + Krizė, data = train_kor1)

summary(modelis1_kor1)

modelis1_kor1 <- lm(BVP_I_palyg ~ 
                      Gyventojai + Krizė, data = train_kor1)

summary(modelis1_kor1)

#korekcija nepavyko, autokoreliacija pilnai nepanaikinta su Cochrane-Orcutt 
#metodu, taip pat R reikšmė labai suprastėjo
durbinWatsonTest(modelis1_kor1)

################################################################################
#Prais-Winsten
################################################################################

modelis1_kor2 <- prais_winsten(BVP_I_palyg ~ Paslaugų_pasitikėjimas + 
                                 Nedarbas_bendras + Gyventojai + Krizė, 
                               data = train, index = "Metai")

#korekcija nepavyko, autokoreliacija pilnai nepanaikinta su Prais-Winsten 
#metodu, taip pat R reikšmė labai suprastėjo
summary(modelis1_kor2)

#šalinu nereikšmingus rodiklius
modelis1_kor2 <- prais_winsten(BVP_I_palyg ~ 
                                 Nedarbas_bendras + Gyventojai + Krizė, 
                               data = train, index = "Metai")
summary(modelis1_kor2)

modelis1_kor2 <- prais_winsten(BVP_I_palyg ~ 
                               Gyventojai + Krizė, 
                               data = train, index = "Metai")
summary(modelis1_kor2)


################################################################################
#liekanų pasiskirstymo tikrinimas po autokoreliacijos korekcijos
train$likuciai_kor <- residuals(modelis1_kor2)
train$pritaikyta_kor <- fitted(modelis1_kor2)

ggplot(train, aes(x = pritaikyta_kor, y = likuciai_kor)) +
  geom_point(color = "#78003F", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "Liekanos ir prognozuotos reikšmės po korekcijos",
       x = "Prognozuotos reikšmės",
       y = "Liekanos")

ggplot(train, aes(x = likuciai_kor)) +
  geom_histogram(fill = "#78003F", color = "white", bins = 30) +
  theme_minimal() +
  labs(title = "Liekanų pasiskirstymas po korekcijos",
       x = "Liekanos",
       y = "Dažnis")

qqnorm(train$likuciai_kor, col = "#78003F")
qqline(train$likuciai_kor, col = "black", lwd = 2)

shapiro.test(residuals(modelis1_kor2))

################################################################################
#Atskiras modelis liekanoms
################################################################################
data <- read_excel("C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx", sheet = "BE SEZONIŠKUMO")

rodikliai <- data %>%
  select(Metai, BVP_I_palyg, Imigrantai, Emigrantai, Taupymas, Mirštamumas,
         Paslaugų_pasitikėjimas, Nedarbas_bendras, Gyventojai,
         Vartotojų_pasitikėjimas, Gimstamumas, BVP_vartojimas_palyg)

rodikliai_pries <- rodikliai

rodikliai_dif <- rodikliai %>%
  arrange(Metai) %>%
  mutate(across(c(BVP_I_palyg, Paslaugų_pasitikėjimas, Nedarbas_bendras,
                  Gyventojai, Vartotojų_pasitikėjimas, Gimstamumas, BVP_vartojimas_palyg),
                ~ c(NA, diff(.)))) %>%
  mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0)) %>%
  na.omit()

train <- filter(rodikliai_dif, Metai <= 2017)
test <- filter(rodikliai_dif, Metai > 2017)

modelis1 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -Emigrantai -Mirštamumas -
                 Imigrantai -Vartotojų_pasitikėjimas -Gimstamumas -Taupymas,
               data = train)

train$likuciai <- resid(modelis1)

#AR(1) modelis
train$lag1 <- lag(train$likuciai, 1)
modelis_AR1 <- lm(likuciai ~ lag1, data = train)
rho1 <- coef(modelis_AR1)["lag1"]

test$prognozuota <- predict(modelis1, newdata = test)

#AR(1) prognozė
e_hat <- numeric(nrow(test))
e_hat[1] <- rho1 * tail(train$likuciai, 1)
for (i in 2:nrow(test)) {
  e_hat[i] <- rho1 * e_hat[i - 1]
}
test$kor_AR1 <- test$prognozuota + e_hat

#AR(2) modelis liekanoms (palyginimui, nes AR(1) neužtenka)
train <- train %>%
  mutate(lag2 = lag(likuciai, 2)) %>%
  na.omit()

modelis_AR2 <- lm(likuciai ~ lag1 + lag2, data = train)

test$lag1 <- NA
test$lag2 <- NA
test$lag1[1] <- tail(train$likuciai, 1)
test$lag2[1] <- tail(train$likuciai, 2)[1]

#AR(2) prognozė
e_hat2 <- numeric(nrow(test))
for (i in 1:nrow(test)) {
  if (i == 1) {
    e_hat2[i] <- coef(modelis_AR2)[1] +
      coef(modelis_AR2)[2] * test$lag1[i] +
      coef(modelis_AR2)[3] * test$lag2[i]
  } else if (i == 2) {
    e_hat2[i] <- coef(modelis_AR2)[1] +
      coef(modelis_AR2)[2] * e_hat2[i - 1] +
      coef(modelis_AR2)[3] * test$lag1[i]
  } else {
    e_hat2[i] <- coef(modelis_AR2)[1] +
      coef(modelis_AR2)[2] * e_hat2[i - 1] +
      coef(modelis_AR2)[3] * e_hat2[i - 2]
  }
}
test$kor_AR2 <- test$prognozuota + e_hat2

#atdiferencijavimas
pradine <- rodikliai_pries %>%
  filter(Metai == 2017) %>%
  tail(1) %>%
  pull(BVP_I_palyg)

atdif <- function(diff_reiksmes, pradzia) {
  lygis <- numeric(length(diff_reiksmes))
  lygis[1] <- pradzia + diff_reiksmes[1]
  for (i in 2:length(diff_reiksmes)) {
    lygis[i] <- lygis[i - 1] + diff_reiksmes[i]
  }
  return(lygis)
}

test$Data <- seq(as.Date("2018-01-01"), by = "month", length.out = nrow(test))

prognozes_df <- data.frame(
  Data = test$Data,
  Pradinis = test$prognozuota,
  AR1 = test$kor_AR1,
  AR2 = test$kor_AR2
) %>%
  mutate(across(-Data, ~ atdif(., pradine)))

istoriniai <- rodikliai_pries %>%
  mutate(Data = seq(as.Date("2002-07-01"), by = "month", length.out = n())) %>%
  select(Data, BVP_I_palyg) %>%
  mutate(Tipas = "Reali reikšmė")

prog_ilgas <- prognozes_df %>%
  pivot_longer(cols = -Data, names_to = "Tipas", values_to = "BVP_I_palyg") %>%
  mutate(Tipas = as.character(Tipas)) %>%
  mutate(Tipas = dplyr::recode(Tipas,"Pradinis" = "Pradinis modelis",
                               "AR1" = "AR(1) modelis", "AR2" = "AR(2) modelis"))

grafikas <- bind_rows(istoriniai, prog_ilgas)

ggplot(grafikas, aes(x = Data, y = BVP_I_palyg / 1000, color = Tipas)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.Date("2017-12-01"), linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("2018-01-01"),
           y = max(grafikas$BVP_I_palyg, na.rm = TRUE) / 1000,
           label = "Prasideda prognozės", hjust = 0, vjust = 1.2, color = "black", size = 5) +
  scale_color_manual(values = c(
    "Reali reikšmė" = "black", "Pradinis modelis" = "#78003F",
    "AR(1) modelis" = "#E64164", "AR(2) modelis" = "#DCDCDC"
  )) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1, big.mark = ","), name = "BVP (mlrd.)") +
  labs(title = "BVP prognozių palyginimas: pradinis, AR(1) ir AR(2)",
       x = "Data", y = NULL, color = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    plot.title = element_text(size = 18),
    panel.grid = element_blank()
  )

train$likuciai_lag <- dplyr::lag(train$likuciai, 1)
modelis_liek <- lm(likuciai ~ likuciai_lag, data = train)
rho <- coef(modelis_liek)["likuciai_lag"]

e_last <- tail(train$likuciai, 1)
n_test <- nrow(test)
e_hat <- numeric(n_test)

for (i in 1:n_test) {
  if (i == 1) {
    e_hat[i] <- rho * e_last
  } else {
    e_hat[i] <- rho * e_hat[i - 1]}}

test$prognozuota_liekanoms <- e_hat
test$prog_kor <- test$prognozuota + test$prognozuota_liekanoms
test$likuciai_kor <- test$BVP_I_palyg - test$prog_kor

test$prog_kor_AR2 <- y_hat_AR2
test$likuciai_AR2 <- test$BVP_I_palyg - test$prog_kor_AR2

RMSE_1 <- rmse(test$BVP_I_palyg, test$prognozuota)
RMSE_1
MAE_1  <- mae(test$BVP_I_palyg, test$prognozuota)
MAE_1
MSE_1  <- mse(test$BVP_I_palyg, test$prognozuota)
MSE_1
DW_1   <- durbinWatsonTest(lm(likuciai ~ 1, data = test))$r
DW_1

RMSE_ar1 <- rmse(test$BVP_I_palyg, test$prog_kor)
RMSE_ar1
MAE_ar1  <- mae(test$BVP_I_palyg, test$prog_kor)
MAE_ar1
MSE_ar1  <- mse(test$BVP_I_palyg, test$prog_kor)
MSE_ar1
DW_ar1   <- durbinWatsonTest(lm(likuciai_kor ~ 1, data = test))$r
DW_ar1

RMSE_ar2 <- rmse(test$BVP_I_palyg, test$prog_kor_AR2)
RMSE_ar2
MAE_ar2  <- mae(test$BVP_I_palyg, test$prog_kor_AR2)
MAE_ar2
MSE_ar2  <- mse(test$BVP_I_palyg, test$prog_kor_AR2)
MSE_ar2
DW_ar2   <- durbinWatsonTest(lm(likuciai_AR2 ~ 1, data = test))$r
DW_ar2

#korekcija nepasiteisino

################################################################################
#2022m. įtraukimas į krizės rodiklį
################################################################################

#1 VERSIJA - SU 2022M.

data1_1 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
data1_1 <- read_excel(data1_1, sheet = "BE SEZONIŠKUMO")

rodikliai1_1 <- data1_1 %>%
  dplyr::select(Metai, BVP_I_palyg, Imigrantai, Emigrantai, Taupymas, Mirštamumas,
                Paslaugų_pasitikėjimas, Nedarbas_bendras, Gyventojai,
                Vartotojų_pasitikėjimas, Gimstamumas, BVP_vartojimas_palyg) %>%
  mutate(Data = as.Date(paste0(Metai, "-01-01")))

rodikliai1_1_pries <- rodikliai1_1

#diferencijuoju (kaip ir ankstesniame modeliavime) reikalingus rodiklius
rodikliai1_1_dif <- rodikliai1_1 %>%
  arrange(Metai) %>%
  mutate(
    BVP_I_palyg = c(NA, diff(BVP_I_palyg)),
    Paslaugų_pasitikėjimas = c(NA, diff(Paslaugų_pasitikėjimas)),
    Nedarbas_bendras = c(NA, diff(Nedarbas_bendras)),
    Gyventojai = c(NA, diff(Gyventojai)),
    Vartotojų_pasitikėjimas = c(NA, diff(Vartotojų_pasitikėjimas)),
    Gimstamumas = c(NA, diff(Gimstamumas)),
    BVP_vartojimas_palyg = c(NA, diff(BVP_vartojimas_palyg))
  ) %>%
  na.omit()

rodikliai1_1 <- rodikliai1_1_dif %>%
  mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021, 2022), 1, 0))

train <- rodikliai1_1 %>% filter(Metai <= 2017)
test <- rodikliai1_1 %>% filter(Metai > 2017)

#modelis su 2022m krizes metais
modelis1_1 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -Vartotojų_pasitikėjimas -
                   Emigrantai -Mirštamumas -Imigrantai -Gimstamumas -Taupymas -Data,
                 data = train)

summary(modelis1_1)

test$prognoze <- predict(modelis1_1, newdata = test)

################################################################################
#liekanų autokoreliacijos tikrinimas (0.8285378)
durbinWatsonTest(modelis1_1)  

#liekanos (0.000493)
shapiro.test(residuals(modelis1_1))

#mae (117.5627)/rmse (159.1615)/mse (25332.39)
prognoze1_1 <- predict(modelis1_1, test)

mse(test$BVP_I_palyg, prognoze1_1)
mae(test$BVP_I_palyg, prognoze1_1)
rmse(test$BVP_I_palyg, prognoze1_1)

#aic (2288.33)/bic(2307.652)
AIC(modelis1_1)
BIC(modelis1_1)

#bp (0.0005713)
bptest(modelis1_1)

#2 VERSIJA - BE 2022M.

#pakeistas krizes rodiklis (be 2022m)
rodikliai_be_2022 <- rodikliai1_1_dif %>%
  mutate(Krize_be_2022 = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0))

train_be_2022 <- rodikliai_be_2022 %>% filter(Metai <= 2017)
test_be_2022  <- rodikliai_be_2022 %>% filter(Metai > 2017)

modelis_be_2022 <- lm(BVP_I_palyg ~ Paslaugų_pasitikėjimas + Nedarbas_bendras +
                        Gyventojai + Krize_be_2022, data = train_be_2022)

summary(modelis_be_2022)

test_be_2022$prognoze <- predict(modelis_be_2022, newdata = test_be_2022)

mse(test_be_2022$BVP_I_palyg, prognoze)
mae(test_be_2022$BVP_I_palyg, prognoze)
rmse(test_be_2022$BVP_I_palyg, prognoze)

AIC(modelis_be_2022)
BIC(modelis_be_2022)

bptest(modelis_be_2022)

################################################################################
#BVP lag įtraukimas
################################################################################
data1_1 <- read_excel("C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx", sheet = "BE SEZONIŠKUMO")

rodikliai1_1 <- data1_1 %>%
  dplyr::select(Metai, BVP_I_palyg, Imigrantai, Emigrantai, Taupymas, Mirštamumas,
                Paslaugų_pasitikėjimas, Nedarbas_bendras, Gyventojai,
                Vartotojų_pasitikėjimas, Gimstamumas, BVP_vartojimas_palyg)

rodikliai1_1_pries <- rodikliai1_1

rodikliai1_1_dif <- rodikliai1_1 %>%
  arrange(Metai) %>%
  mutate(
    BVP_I_palyg = c(NA, diff(BVP_I_palyg)),
    Paslaugų_pasitikėjimas = c(NA, diff(Paslaugų_pasitikėjimas)),
    Nedarbas_bendras = c(NA, diff(Nedarbas_bendras)),
    Gyventojai = c(NA, diff(Gyventojai)),
    Vartotojų_pasitikėjimas = c(NA, diff(Vartotojų_pasitikėjimas)),
    Gimstamumas = c(NA, diff(Gimstamumas)),
    BVP_vartojimas_palyg = c(NA, diff(BVP_vartojimas_palyg)),
    BVP_I_palyg_lag1 = lag(c(NA, diff(BVP_I_palyg)), 1)

  ) %>%
  mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0)) %>%
  na.omit()

train <- rodikliai1_1_dif %>% filter(Metai <= 2017)
test  <- rodikliai1_1_dif %>% filter(Metai > 2017)

modelis_be_lag <- lm(BVP_I_palyg ~ Paslaugų_pasitikėjimas + Nedarbas_bendras +
                       Gyventojai + Krizė, data = train)
summary(modelis_be_lag)
vif(modelis_be_lag)

modelis_su_lag <- lm(BVP_I_palyg ~ BVP_I_palyg_lag1 + Paslaugų_pasitikėjimas +
                       Nedarbas_bendras + Gyventojai + Krizė, data = train)
summary(modelis_su_lag)
vif(modelis_su_lag)

modelis_su_lag <- lm(BVP_I_palyg ~ BVP_I_palyg_lag1 + Paslaugų_pasitikėjimas +
                       Gyventojai + Krizė, data = train)
summary(modelis_su_lag)
vif(modelis_su_lag)

prog_be_lag <- predict(modelis_be_lag, newdata = test)
prog_su_lag <- predict(modelis_su_lag, newdata = test)

bvp_pradine <- rodikliai1_1_pries %>% filter(Metai == 2017) %>% tail(1) %>% pull(BVP_I_palyg)

atdiferen <- function(diff_reiksmes, pradzia) {
  n <- length(diff_reiksmes)
  lygiu_reiksmes <- numeric(n)
  lygiu_reiksmes[1] <- pradzia + diff_reiksmes[1]
  for (i in 2:n) {
    lygiu_reiksmes[i] <- lygiu_reiksmes[i - 1] + diff_reiksmes[i]
  }
  return(lygiu_reiksmes)
}

prog_datos <- seq(as.Date("2018-01-01"), by = "month", length.out = nrow(test))

prog_df <- data.frame(
  Data = prog_datos,
  Be_lag = atdiferen(prog_be_lag, bvp_pradine),
  Su_lag = atdiferen(prog_su_lag, bvp_pradine)
)

rodikliai1_1_pries$Data <- seq(as.Date("2002-07-01"), by = "month", length.out = nrow(rodikliai1_1_pries))

reali <- rodikliai1_1_pries %>%
  select(Data, BVP_I_palyg) %>%
  mutate(Modelis = "Reali reikšmė")

prog_long <- prog_df %>%
  pivot_longer(cols = -Data, names_to = "Modelis", values_to = "BVP_I_palyg") %>%
  mutate(Modelis = dplyr::recode(Modelis,
                                 "Be_lag" = "Modelis be BVP lag", "Su_lag" = "Modelis su BVP lag"))

graf_data <- bind_rows(reali, prog_long) %>%
  mutate(Modelis = factor(Modelis, levels = c("Reali reikšmė", "Modelis be BVP lag", "Modelis su BVP lag")))

ggplot(graf_data, aes(x = Data, y = BVP_I_palyg / 1000, color = Modelis)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.Date("2017-12-01"), linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("2018-01-01"),
           y = max(graf_data$BVP_I_palyg, na.rm = TRUE) / 1000,
           label = "Prasideda prognozės", hjust = 0, vjust = 1.2, color = "black", size = 5) +
  scale_y_continuous(name = "BVP (mlrd.)") +
  labs(title = "lag įtraukimo poveikis", x = "Data", color = NULL) +
  scale_color_manual(values = c(
    "Reali reikšmė" = "black", "Modelis be BVP lag" = "#E64164", "Modelis su BVP lag" = "#78003F")) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 13),
    plot.title = element_text(size = 18), panel.grid = element_blank()
  )

lik_be_lag <- residuals(modelis_be_lag)
lik_su_lag <- residuals(modelis_su_lag)

mae(test$BVP_I_palyg, prog_be_lag)
rmse(test$BVP_I_palyg, prog_be_lag)
mse(test$BVP_I_palyg, prog_be_lag)
AIC(modelis_be_lag)
BIC(modelis_be_lag)
durbinWatsonTest(modelis_be_lag)
shapiro.test(lik_be_lag)
bptest(modelis_be_lag)

mae(test$BVP_I_palyg, prog_su_lag)
rmse(test$BVP_I_palyg, prog_su_lag)
mse(test$BVP_I_palyg, prog_su_lag)
AIC(modelis_su_lag)
BIC(modelis_su_lag)
durbinWatsonTest(modelis_su_lag)
shapiro.test(lik_su_lag)
bptest(modelis_su_lag)

################################################################################
#lag ir 2022m. įtraukimas
################################################################################
data1_1 <- read_excel("C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx", sheet = "BE SEZONIŠKUMO")

rodikliai <- data1_1 %>%
  dplyr::select(Metai, BVP_I_palyg, Paslaugų_pasitikėjimas, Nedarbas_bendras, Gyventojai)

rodikliai_dif <- rodikliai %>%
  arrange(Metai) %>%
  mutate(
    BVP_I_palyg = c(NA, diff(BVP_I_palyg)),
    Paslaugų_pasitikėjimas = c(NA, diff(Paslaugų_pasitikėjimas)),
    Nedarbas_bendras = c(NA, diff(Nedarbas_bendras)),
    Gyventojai = c(NA, diff(Gyventojai)),
    Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0),
    Krizė_2022 = ifelse(Metai %in% c(2008, 2009, 2020, 2021, 2022), 1, 0)
  ) %>%
  mutate(BVP_I_palyg_lag1 = lag(c(NA, diff(BVP_I_palyg)), 1)) %>%
  na.omit()

train <- rodikliai_dif %>% filter(Metai <= 2017)
test  <- rodikliai_dif %>% filter(Metai > 2017)

modelis_ori <- lm(BVP_I_palyg ~ Paslaugų_pasitikėjimas + Nedarbas_bendras + Gyventojai + Krizė, data = train)
summary(modelis_ori)
vif(modelis_ori)

modelis_2022 <- lm(BVP_I_palyg ~ Paslaugų_pasitikėjimas + Nedarbas_bendras + Gyventojai + Krizė_2022, data = train)
summary(modelis_2022)
vif(modelis_2022)

modelis_lag <- lm(BVP_I_palyg ~ BVP_I_palyg_lag1 + Paslaugų_pasitikėjimas +
                    Gyventojai + Krizė, data = train)
summary(modelis_lag)
vif(modelis_lag)

modelis_abu <- lm(BVP_I_palyg ~ Paslaugų_pasitikėjimas + Nedarbas_bendras + Gyventojai + Krizė_2022 + BVP_I_palyg_lag1, data = train)
summary(modelis_abu)
vif(modelis_abu)

modelis_abu <- lm(BVP_I_palyg ~  Krizė_2022 + BVP_I_palyg_lag1, data = train)
summary(modelis_abu)
vif(modelis_abu)

modeliai <- list(modelis_ori, modelis_2022, modelis_lag, modelis_abu)
names(modeliai) <- c("Originalus", "Su_2022", "Su_lag", "Su_abu")

rez <- lapply(modeliai, function(mod) {
  prog <- predict(mod, newdata = test)
  data.frame(
    DW = durbinWatsonTest(mod)$r,
    Shapiro = shapiro.test(residuals(mod))$p.value,
    BP = bptest(mod)$p.value,
    MAE = mae(test$BVP_I_palyg, prog),
    RMSE = rmse(test$BVP_I_palyg, prog),
    MSE = mse(test$BVP_I_palyg, prog),
    AIC = AIC(mod),
    BIC = BIC(mod)
  )
})

rez_df <- do.call(rbind, rez)
rez_df <- cbind(Modelis = rownames(rez_df), rez_df)
print(rez_df)

atdiferen <- function(diff_reiksmes, pradzia) {
  n <- length(diff_reiksmes)
  lygiu_reiksmes <- numeric(n)
  lygiu_reiksmes[1] <- pradzia + diff_reiksmes[1]
  for (i in 2:n) {
    lygiu_reiksmes[i] <- lygiu_reiksmes[i - 1] + diff_reiksmes[i]
  }
  return(lygiu_reiksmes)
}

bvp_pradine <- tail(rodikliai %>% filter(Metai == 2017), 1)$BVP_I_palyg
datos <- seq(as.Date("2018-01-01"), by = "month", length.out = nrow(test))

prognozes <- data.frame(Data = datos)
prognozes$Originalus <- atdiferen(predict(modelis_ori, test), bvp_pradine)
prognozes$Su_2022 <- atdiferen(predict(modelis_2022, test), bvp_pradine)
prognozes$Su_lag <- atdiferen(predict(modelis_lag, test), bvp_pradine)
prognozes$Su_abu <- atdiferen(predict(modelis_abu, test), bvp_pradine)

prog_long <- prognozes %>%
  pivot_longer(-Data, names_to = "Modelis", values_to = "BVP_I_palyg") %>%
  mutate(Modelis = factor(Modelis, levels = c("Originalus", "Su_2022", "Su_lag", "Su_abu"),
                          labels = c("Originalus modelis",
                                     "Modelis su 2022 m. krize",
                                     "Modelis su BVP lag",
                                     "Modelis su krize ir lag")))

reali <- rodikliai %>% filter(Metai > 2017) %>%
  mutate(Data = datos,
         Modelis = "Reali reikšmė") %>% select(Data, BVP_I_palyg, Modelis)

graf_duom <- bind_rows(reali, prog_long) %>%
  mutate(Modelis = factor(Modelis,
                          levels = c("Reali reikšmė", "Originalus modelis", "Modelis su 2022 m. krize",
                                     "Modelis su BVP lag", "Modelis su krize ir lag")))

ggplot(graf_duom, aes(x = Data, y = BVP_I_palyg / 1000, color = Modelis)) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = as.Date("2017-12-01"), linetype = "dashed") +
  labs(title = "Prognozių palyginimas",
       x = "Data", y = "BVP (mlrd.)", color = NULL) +
  theme_minimal() + theme(legend.position = "bottom")

