################################################################################
#4 žingsnis - modeliavimas. 4 ŽINGSNYJE MODELIUOJU SU SUDĖTINGESNĖMIS REGRESIJOMIS
#(LASSO, RIDGE, ELASTIC NET)

#Naudoju skirtingus modeliavimo metodus ir testuoju
#modelius įtraukiant skirtingus nepriklausomus kintamuosius

#Rodiklių pasiskirstymo grafikai yra 2 žingsnyje (2 pradinė analizė (sklaida, 
#sezoniškumas, koreliacija))
################################################################################

library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggcorrplot)
library(grid)
library(GGally)
library(glmnet) 
library(caret)   

################################################################################
################################################################################
     #4 modelis - Ridge regresija be BVP komponenčių, be rodiklių sąveikų
################################################################################
################################################################################
data4 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
data4 <- read_excel(data4, sheet = "BE SEZONIŠKUMO")

#iškart įtraukiu krizės rodiklį
rodikliai4 <- data4 %>%
  dplyr::select(Metai, BVP_I_palyg, Imigrantai, Emigrantai, Taupymas, Mirštamumas, 
                Paslaugų_pasitikėjimas, Nedarbas_bendras, Gyventojai, 
                Vartotojų_pasitikėjimas, Gimstamumas) %>%
  mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0))

rodikliai4 <- rodikliai4 %>% arrange(Metai) %>% mutate(BVP_lag = dplyr::lag(BVP_I_palyg, 1))
rodikliai4 <- rodikliai4 %>% na.omit()

#dalinu duomenis į test ir train imtis prieš normalizavimą
train <- rodikliai4 %>% filter(Metai <= 2017) %>% dplyr::select(-Metai)
test <- rodikliai4 %>% filter(Metai > 2017) %>% dplyr::select(-Metai)

#duomenų formato keitimas ir automatinis normalizavimas 
X_train <- scale(as.matrix(train %>% dplyr::select(-BVP_I_palyg)))
X_test <- scale(as.matrix(test %>% dplyr::select(-BVP_I_palyg)),
                center = attr(X_train, "scaled:center"),
                scale = attr(X_train, "scaled:scale"))

y_train <- scale(train$BVP_I_palyg)
y_test <- scale(test$BVP_I_palyg,
                center = attr(y_train, "scaled:center"),
                scale = attr(y_train, "scaled:scale"))

#lambda parametro parinkimas
lambda_seq <- 10^seq(3, -3, length.out = 100)

#kryžminė validacija (dėl lambda parametro geriausio parinkimo)
#naudoju set seed, kad rezultatai nesikeistų
set.seed(123) 
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, lambda = lambda_seq, nfolds = 10,
                      standardize = FALSE)

lambda <- cv_ridge$lambda.min
cat("Geriausias lambda parametras", lambda)

df <- data.frame(lambda = log(cv_ridge$lambda), mse = cv_ridge$cvm)

ggplot(df, aes(x = lambda, y = mse)) +
  geom_line(color = "#414141") +
  geom_point(color = "#78003F") +
  geom_vline(xintercept = log(cv_ridge$lambda.min), linetype = "dashed", color = "black") +
  labs(title = "Lambda pasirinkimas Ridge regresijai", x = "log(Lambda)",
       y = "Vidutinė kryžminės validacijos klaida (MSE)") + theme_minimal()

#pagr. modelis
ridge_model4 <- glmnet(X_train, y_train, alpha = 0, lambda = lambda, standardize = FALSE, standardize.response = FALSE)

coef_ridge4 <- coef(ridge_model4)
print(coef_ridge4)

coef_df4 <- data.frame(variable = rownames(coef_ridge4), coefficient = as.vector(coef_ridge4))
coef_df4 <- coef_df4[coef_df4$variable != "(Intercept)", ]  

#koeficientai, rodiklių įtaka y
ggplot(coef_df4, aes(x = reorder(variable, coefficient), y = coefficient)) +
  geom_bar(stat = "identity", fill = "#78003F") + coord_flip() +
  labs(title = "Ridge regresijos koeficientai", x = "Kintamieji", y = "Koeficiento reikšmė") +
  theme_minimal()

#prognozavimas (siekiant įvertinti modelį)
y_prog <- predict(ridge_model4, s = lambda, newx = X_test) %>% as.vector()

prognoze_4 <- data.frame(Realios_reiksmes = y_test, Prognozuotos_reiksmes = y_prog)

ggplot(prognoze_4, aes(x = Realios_reiksmes, y = Prognozuotos_reiksmes)) +
  geom_point(color = "#78003F", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Tikros ir prognozuotos Ridge modelio reikšmės",
       x = "Tikros reikšmės", y = "Prognozuotos reikšmės") + theme_minimal()

#tikrų ir modelio prognozuotų reikšmių koreliacijos tikrinimas
cor(y_test, y_prog, method = "spearman")

################################################################################
#modelio testavimas
y_test_original <- test$BVP_I_palyg
likuciai <- y_test - y_prog

qqnorm(likuciai, col = "#78003F", main = "Modelio liekanos")
qqline(likuciai, col = "black")

rmse <- sqrt(mean(likuciai^2))
rmse

mse <- mean(likuciai^2)
mse

mae <- mean(abs(likuciai))
mae

n <- length(y_test_original)
RSS <- sum(likuciai^2)
k <- sum(coef(ridge_model4)[-1] != 0) 

aic <- n * log(RSS / n) + 2 * k
bic <- n * log(RSS / n) + log(n) * k
aic
bic

################################################################################
################################################################################
   #5 modelis - Ridge regresija be BVP komponenčių, su rodiklių sąveikomis
################################################################################
################################################################################
data5 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
data5 <- read_excel(data5, sheet = "BE SEZONIŠKUMO")

rodikliai5 <- data5 %>%
  dplyr::select(Metai, BVP_I_palyg, Imigrantai, Emigrantai, Taupymas, Mirštamumas, 
                Paslaugų_pasitikėjimas, Nedarbas_bendras, Gyventojai, 
                Vartotojų_pasitikėjimas, Gimstamumas) %>%
  mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0))

rodikliai5 <- rodikliai5 %>% arrange(Metai) %>% mutate(BVP_lag = dplyr::lag(BVP_I_palyg, 1))
rodikliai5 <- rodikliai5 %>% na.omit()

train5 <- rodikliai5 %>% filter(Metai <= 2017) %>% dplyr::select(-Metai)
test5 <- rodikliai5 %>% filter(Metai > 2017) %>% dplyr::select(-Metai)

train5 <- train5 %>%
  mutate(
    Nedarbas_Emigracija = Nedarbas_bendras * Emigrantai,
    Nedarbas_Gyventojai = Nedarbas_bendras * Gyventojai,
    Krizė_Taupymas = Krizė * Taupymas,
    Krizė_Emigracija = Krizė * Emigrantai)

test5 <- test5 %>%
  mutate(
    Nedarbas_Emigracija = Nedarbas_bendras * Emigrantai,
    Nedarbas_Gyventojai = Nedarbas_bendras * Gyventojai,
    Krizė_Taupymas = Krizė * Taupymas,
    Krizė_Emigracija = Krizė * Emigrantai)

X_train5 <- scale(as.matrix(train5 %>% dplyr::select(-BVP_I_palyg)))
X_test5 <- scale(as.matrix(test5 %>% dplyr::select(-BVP_I_palyg)),
                center = attr(X_train5, "scaled:center"),
                scale = attr(X_train5, "scaled:scale"))

y_train5 <- scale(train5$BVP_I_palyg)
y_test5 <- scale(test5$BVP_I_palyg,
                center = attr(y_train5, "scaled:center"),
                scale = attr(y_train5, "scaled:scale"))

lambda_seq <- 10^seq(3, -3, length.out = 100)

set.seed(123) 
cv_ridge <- cv.glmnet(X_train5, y_train5, alpha = 0, lambda = lambda_seq, nfolds = 10,
                      standardize = FALSE)

lambda <- cv_ridge$lambda.min
cat("Geriausias lambda parametras", lambda)

df <- data.frame(lambda = log(cv_ridge$lambda), mse = cv_ridge$cvm)

ggplot(df, aes(x = lambda, y = mse)) +
  geom_line(color = "#414141") +
  geom_point(color = "#78003F") +
  geom_vline(xintercept = log(cv_ridge$lambda.min), linetype = "dashed", color = "black") +
  labs(title = "Lambda pasirinkimas Ridge regresijai", x = "log(Lambda)",
       y = "Vidutinė kryžminės validacijos klaida (MSE)") + theme_minimal()

ridge_model5 <- glmnet(X_train5, y_train5, alpha = 0, lambda = lambda, standardize = FALSE)

coef_ridge5 <- coef(ridge_model5)
print(coef_ridge5)

coef_df5 <- data.frame(variable = rownames(coef_ridge5), coefficient = as.vector(coef_ridge5))
coef_df5 <- coef_df5[coef_df5$variable != "(Intercept)", ]

ggplot(coef_df5, aes(x = reorder(variable, coefficient), y = coefficient)) +
  geom_bar(stat = "identity", fill = "#78003F") + coord_flip() +
  labs(title = "Ridge regresijos koeficientai", x = "Kintamieji", y = "Koeficiento reikšmė") +
  theme_minimal()

y_pred5 <- predict(ridge_model5, s = lambda, newx = X_test5) %>% as.vector()

prognoze_5 <- data.frame(Realios_reiksmes = y_test5, Prognozuotos_reiksmes = y_pred5)

ggplot(prognoze_5, aes(x = Realios_reiksmes, y = Prognozuotos_reiksmes)) +
  geom_point(color = "#78003F", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Tikros ir prognozuotos Ridge modelio reikšmės",
       x = "Tikros reikšmės", y = "Prognozuotos reikšmės") + theme_minimal()

cor(y_test5, y_pred5, method = "spearman")

################################################################################
#modelio testavimas

y_test_original <- test5$BVP_I_palyg
likuciai <- y_test5 - y_pred5

qqnorm(likuciai, col = "#78003F", main = "Modelio liekanos")
qqline(likuciai, col = "black")

rmse <- sqrt(mean(likuciai^2))
rmse

mse <- mean(likuciai^2)
mse

mae <- mean(abs(likuciai))
mae

n <- length(y_test_original)
RSS <- sum(likuciai^2)
k <- sum(coef(ridge_model5)[-1] != 0) 

aic <- n * log(RSS / n) + 2 * k
bic <- n * log(RSS / n) + log(n) * k
aic
bic

################################################################################
################################################################################
     #6 modelis - Ridge regresija su BVP komponente, be rodiklių sąveikų
################################################################################
################################################################################
data6 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
data6 <- read_excel(data6, sheet = "BE SEZONIŠKUMO")

rodikliai6 <- data6 %>%
  dplyr::select(Metai, BVP_I_palyg, Imigrantai, Emigrantai, Taupymas, Mirštamumas, 
                Paslaugų_pasitikėjimas, Nedarbas_bendras, Gyventojai, 
                Vartotojų_pasitikėjimas, Gimstamumas, BVP_vartojimas_palyg) %>%
  mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0))

rodikliai6 <- rodikliai6 %>% arrange(Metai) %>% mutate(BVP_lag = dplyr::lag(BVP_I_palyg, 1))
rodikliai6 <- rodikliai6 %>% na.omit()

train6 <- rodikliai6 %>% filter(Metai <= 2017) %>% dplyr::select(-Metai)
test6 <- rodikliai6 %>% filter(Metai > 2017) %>% dplyr::select(-Metai)

X_train6 <- scale(as.matrix(train6 %>% dplyr::select(-BVP_I_palyg)))
X_test6 <- scale(as.matrix(test6 %>% dplyr::select(-BVP_I_palyg)),
                center = attr(X_train6, "scaled:center"),
                scale = attr(X_train6, "scaled:scale"))

y_train6 <- scale(train6$BVP_I_palyg)
y_test6 <- scale(test6$BVP_I_palyg,
                center = attr(y_train6, "scaled:center"),
                scale = attr(y_train6, "scaled:scale"))

lambda_seq <- 10^seq(3, -3, length.out = 100)

set.seed(123) 
cv_ridge <- cv.glmnet(X_train6, y_train6, alpha = 0, lambda = lambda_seq, nfolds = 10,
                      standardize = FALSE)

lambda <- cv_ridge$lambda.min
cat("Geriausias lambda parametras", lambda)

df <- data.frame(lambda = log(cv_ridge$lambda), mse = cv_ridge$cvm)

ggplot(df, aes(x = lambda, y = mse)) +
  geom_line(color = "#414141") +
  geom_point(color = "#78003F") +
  geom_vline(xintercept = log(cv_ridge$lambda.min), linetype = "dashed", color = "black") +
  labs(title = "Lambda pasirinkimas Ridge regresijai", x = "log(Lambda)",
       y = "Vidutinė kryžminės validacijos klaida (MSE)") + theme_minimal()

ridge_model6 <- glmnet(X_train6, y_train6, alpha = 0, lambda = lambda, standardize = FALSE)

coef_ridge6 <- coef(ridge_model6)
print(coef_ridge6)

coef_df6 <- data.frame(variable = rownames(coef_ridge6), coefficient = as.vector(coef_ridge6))
coef_df6 <- coef_df6[coef_df6$variable != "(Intercept)", ]  

ggplot(coef_df6, aes(x = reorder(variable, coefficient), y = coefficient)) +
  geom_bar(stat = "identity", fill = "#78003F") + coord_flip() +
  labs(title = "Ridge regresijos koeficientai", x = "Kintamieji", y = "Koeficiento reikšmė") +
  theme_minimal()

y_prog6 <- predict(ridge_model6, s = lambda, newx = X_test6) %>% as.vector()

prognoze_6 <- data.frame(Realios_reiksmes = y_test6, Prognozuotos_reiksmes = y_prog6)

ggplot(prognoze_6, aes(x = Realios_reiksmes, y = Prognozuotos_reiksmes)) +
  geom_point(color = "#78003F", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Tikros ir prognozuotos Ridge modelio reikšmės",
       x = "Tikros reikšmės", y = "Prognozuotos reikšmės") + theme_minimal()

cor(y_test6, y_prog6, method = "spearman")

################################################################################
#modelio testavimas
y_test_original <- test6$BVP_I_palyg
likuciai <- y_test6 - y_prog6

qqnorm(likuciai, col = "#78003F", main = "Modelio liekanos")
qqline(likuciai, col = "black")

rmse <- sqrt(mean(likuciai^2))
rmse

mse <- mean(likuciai^2)
mse

mae <- mean(abs(likuciai))
mae

n <- length(y_test_original)
RSS <- sum(likuciai^2)
k <- sum(coef(ridge_model6)[-1] != 0) 

aic <- n * log(RSS / n) + 2 * k
bic <- n * log(RSS / n) + log(n) * k
aic
bic



################################################################################
################################################################################
                    #4-6 modelių prognozių lyginimas
################################################################################
################################################################################
#'atnormalizavimas'
reali_4 <- y_test * attr(y_train, "scaled:scale") + attr(y_train, "scaled:center")
prog_4 <- y_prog * attr(y_train, "scaled:scale") + attr(y_train, "scaled:center")
prog_5 <- y_pred5 * attr(y_train5, "scaled:scale") + attr(y_train5, "scaled:center")
prog_6 <- y_prog6 * attr(y_train6, "scaled:scale") + attr(y_train6, "scaled:center")

prog_datos <- seq(as.Date("2018-01-01"), by = "month", length.out = length(reali_4))

ridge_df <- data.frame(
  Data = prog_datos,
  'Reali reikšmė' = reali_4,
  'Pradinis modelis' = prog_4,
  'Su sąveikomis' = prog_5,
  'Su BVP komponente' = prog_6,
  check.names = FALSE
)

ridge_long <- ridge_df %>%
  pivot_longer(cols = -Data, names_to = "Tipas", values_to = "BVP_I_palyg")

originalus_df <- rodikliai4 %>%
  filter(Metai <= 2017) %>%
  mutate(Data = seq(as.Date("2002-07-01"), by = "month", length.out = n()),
         Tipas = "Reali reikšmė") %>%
  dplyr::select(Data, BVP_I_palyg, Tipas)

grafiko_duomenys <- bind_rows(originalus_df, ridge_long)

grafiko_duomenys$Tipas <- factor(grafiko_duomenys$Tipas,
                                 levels = c("Reali reikšmė", "Pradinis modelis", "Su sąveikomis", "Su BVP komponente"))

prog <- ggplot(grafiko_duomenys, aes(x = Data, y = BVP_I_palyg / 1000, color = Tipas, linetype = Tipas, group = Tipas)) +
  geom_line(size = 1.3) +
  geom_vline(xintercept = as.Date("2017-12-01"), linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("2018-01-01"),
           y = max(grafiko_duomenys$BVP_I_palyg, na.rm = TRUE) / 1000,
           label = "Prasideda prognozės", hjust = 0, vjust = 1.2, color = "black", size = 6) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1, big.mark = ","),
                     name = "BVP (mlrd. eur.)",
                     breaks = seq(5, 20, by = 2)) +
  labs(title = "Ridge regresijos modelių prognozių palyginimas",
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

ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/ridgepro.png", prog,
       width = 10, height = 6.5, dpi = 300, units = "in")

################################################################################
#GALUTINIS 4-6 modelių testų rezultatų palyginimas
################################################################################

###########TRANSFORMUOTŲ (STANDARTIZUOTŲ) RIDGE MODELIŲ PALYGINIMAS
likuciai4 <- y_test - y_prog
likuciai5 <- y_test5 - y_pred5
likuciai6 <- y_test6 - y_prog6

mae_ridge <- c(mae(y_test, y_prog),
               mae(y_test5, y_pred5),
               mae(y_test6, y_prog6))

rmse_ridge <- c(rmse(y_test, y_prog),
                rmse(y_test5, y_pred5),
                rmse(y_test6, y_prog6))

mse_ridge <- c(mse(y_test, y_prog),
               mse(y_test5, y_pred5),
               mse(y_test6, y_prog6))

n4 <- length(y_test)
n5 <- length(y_test5)
n6 <- length(y_test6)

RSS4 <- sum(likuciai4^2)
RSS5 <- sum(likuciai5^2)
RSS6 <- sum(likuciai6^2)

k4 <- sum(coef(ridge_model4)[-1] != 0)
k5 <- sum(coef(ridge_model5)[-1] != 0)
k6 <- sum(coef(ridge_model6)[-1] != 0)

aic_ridge <- c(n4 * log(RSS4 / n4) + 2 * k4,
               n5 * log(RSS5 / n5) + 2 * k5,
               n6 * log(RSS6 / n6) + 2 * k6)

bic_ridge <- c(n4 * log(RSS4 / n4) + log(n4) * k4,
               n5 * log(RSS5 / n5) + log(n5) * k5,
               n6 * log(RSS6 / n6) + log(n6) * k6)

rez_ridge_transf <- data.frame(
  Modelis = c("Pradinis modelis", "Su sąveikomis", "Su BVP komponente"),
  MAE = round(mae_ridge, 3),
  RMSE = round(rmse_ridge, 3),
  MSE = round(mse_ridge, 3),
  AIC = round(aic_ridge, 2),
  BIC = round(bic_ridge, 2)
)

rez_ridge_transf


###########ATTRANSFORMUOTŲ RIDGE MODELIŲ PALYGINIMAS
reali_4 <- y_test * attr(y_train, "scaled:scale") + attr(y_train, "scaled:center")
prog_4  <- y_prog * attr(y_train, "scaled:scale") + attr(y_train, "scaled:center")
prog_5  <- y_pred5 * attr(y_train5, "scaled:scale") + attr(y_train5, "scaled:center")
prog_6  <- y_prog6 * attr(y_train6, "scaled:scale") + attr(y_train6, "scaled:center")

resid4 <- reali_4 - prog_4
resid5 <- reali_4 - prog_5 
resid6 <- reali_4 - prog_6

mae_ridge_detr <- c(mean(abs(resid4)), mean(abs(resid5)), mean(abs(resid6)))
rmse_ridge_detr <- c(sqrt(mean(resid4^2)), sqrt(mean(resid5^2)), sqrt(mean(resid6^2)))
mse_ridge_detr <- c(mean(resid4^2), mean(resid5^2), mean(resid6^2))

n <- length(reali_4)
k4 <- sum(coef(ridge_model4)[-1] != 0)
k5 <- sum(coef(ridge_model5)[-1] != 0)
k6 <- sum(coef(ridge_model6)[-1] != 0)

RSS4 <- sum(resid4^2)
RSS5 <- sum(resid5^2)
RSS6 <- sum(resid6^2)

aic_ridge_detr <- c(n * log(RSS4 / n) + 2 * k4,
                    n * log(RSS5 / n) + 2 * k5,
                    n * log(RSS6 / n) + 2 * k6)

bic_ridge_detr <- c(n * log(RSS4 / n) + log(n) * k4,
                    n * log(RSS5 / n) + log(n) * k5,
                    n * log(RSS6 / n) + log(n) * k6)

rez_ridge_detr <- data.frame(
  Modelis = c("Pradinis modelis", "Su sąveikomis", "Su BVP komponente"),
  MAE = round(mae_ridge_detr, 2),
  RMSE = round(rmse_ridge_detr, 2),
  MSE = round(mse_ridge_detr, 2),
  AIC = round(aic_ridge_detr, 2),
  BIC = round(bic_ridge_detr, 2)
)

rez_ridge_detr



################################################################################
################################################################################
    #7 modelis - Lasso regresija be BVP komponenčių, be rodiklių sąveikų
################################################################################
################################################################################
data7 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
data7 <- read_excel(data7, sheet = "BE SEZONIŠKUMO")

rodikliai7 <- data7 %>%
  dplyr::select(Metai, BVP_I_palyg, Imigrantai, Emigrantai, Taupymas, Mirštamumas, 
                Paslaugų_pasitikėjimas, Nedarbas_bendras, Gyventojai, 
                Vartotojų_pasitikėjimas, Gimstamumas) %>%
  mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0))

rodikliai7 <- rodikliai7 %>% arrange(Metai) %>% mutate(BVP_lag = dplyr::lag(BVP_I_palyg, 1))
rodikliai7 <- rodikliai7 %>% na.omit()

train7 <- rodikliai7 %>% filter(Metai <= 2017) %>% dplyr::select(-Metai)
test7 <- rodikliai7 %>% filter(Metai > 2017) %>% dplyr::select(-Metai)

X_train7 <- scale(as.matrix(train7 %>% dplyr::select(-BVP_I_palyg)))
X_test7 <- scale(as.matrix(test7 %>% dplyr::select(-BVP_I_palyg)),
                center = attr(X_train7, "scaled:center"),
                scale = attr(X_train7, "scaled:scale"))

y_train7 <- scale(train7$BVP_I_palyg)
y_test7 <- scale(test7$BVP_I_palyg,
                center = attr(y_train7, "scaled:center"),
                scale = attr(y_train7, "scaled:scale"))

lambda_seq <- 10^seq(3, -3, length.out = 100)

set.seed(123)
cv_lasso <- cv.glmnet(X_train7, y_train7, alpha = 1, lambda = lambda_seq, nfolds = 10,
                      standardize = FALSE)

lambda <- cv_lasso$lambda.min
cat("Geriausias lambda parametras", lambda)

df <- data.frame(lambda = log(cv_lasso$lambda), mse = cv_lasso$cvm)

ggplot(df, aes(x = lambda, y = mse)) +
  geom_line(color = "#414141") +
  geom_point(color = "#78003F") +
  geom_vline(xintercept = log(cv_lasso$lambda.min), linetype = "dashed", color = "black") +
  labs(title = "Lambda pasirinkimas Lasso regresijai",
       x = "log(Lambda)", y = "Vidutinė kryžminės validacijos klaida (MSE)") + theme_minimal()

#modelis
lasso_model7 <- glmnet(X_train7, y_train7, alpha = 1, lambda = lambda, standardize = FALSE)

coef_lasso7 <- coef(lasso_model7)
print(coef_lasso7)

coef_df7 <- data.frame(variable = rownames(coef_lasso7), coefficient = as.vector(coef_lasso7))
coef_df7 <- coef_df7[coef_df7$variable != "(Intercept)", ]

ggplot(coef_df7, aes(x = reorder(variable, coefficient), y = coefficient)) +
  geom_bar(stat = "identity", fill = "#78003F") + coord_flip() +
  labs(title = "Lasso regresijos koeficientai", x = "Kintamieji", y = "Koeficiento reikšmė") +
  theme_minimal()

y_pred7 <- predict(lasso_model7, s = lambda, newx = X_test7) %>% as.vector()

prognoze_7 <- data.frame(Realios_reiksmes = y_test7, Prognozuotos_reiksmes = y_pred7)

ggplot(prognoze_7, aes(x = Realios_reiksmes, y = Prognozuotos_reiksmes)) +
  geom_point(color = "#78003F", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Tikros ir prognozuotos Lasso modelio reikšmės",
       x = "Tikros reikšmės", y = "Prognozuotos reikšmės") + theme_minimal()

cor(y_test7, y_pred7, method = "spearman")

################################################################################
#modelio testavimas
y_test_original <- test7$BVP_I_palyg
likuciai <- y_test7 - y_pred7

qqnorm(likuciai, col = "#78003F", main = "Modelio liekanos")
qqline(likuciai, col = "black")

rmse <- sqrt(mean(likuciai^2))
rmse

mse <- mean(likuciai^2)
mse

mae <- mean(abs(likuciai))
mae

n <- length(y_test_original)
RSS <- sum(likuciai^2)
k <- sum(coef(lasso_model7)[-1] != 0) 

aic <- n * log(RSS / n) + 2 * k
bic <- n * log(RSS / n) + log(n) * k
aic
bic


################################################################################
################################################################################
 #8 modelis - Lasso regresija be BVP komponenčių, įtraukiant rodiklių sąveikas
################################################################################
################################################################################
data8 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
data8 <- read_excel(data8, sheet = "BE SEZONIŠKUMO")

rodikliai8 <- data8 %>%
  dplyr::select(Metai, BVP_I_palyg, Imigrantai, Emigrantai, Taupymas, Mirštamumas, 
                Paslaugų_pasitikėjimas, Nedarbas_bendras, Gyventojai, 
                Vartotojų_pasitikėjimas, Gimstamumas) %>%
  mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0))

rodikliai8 <- rodikliai8 %>% arrange(Metai) %>% mutate(BVP_lag = dplyr::lag(BVP_I_palyg, 1))
rodikliai8 <- rodikliai8 %>% na.omit()

train8 <- rodikliai8 %>% filter(Metai <= 2017) %>% dplyr::select(-Metai)
test8 <- rodikliai8 %>% filter(Metai > 2017) %>% dplyr::select(-Metai)

train8 <- train8 %>%
  mutate(
    Nedarbas_Emigracija = Nedarbas_bendras * Emigrantai,
    Nedarbas_Gyventojai = Nedarbas_bendras * Gyventojai,
    Krizė_Taupymas = Krizė * Taupymas,
    Krizė_Emigracija = Krizė * Emigrantai)

test8 <- test8 %>%
  mutate(
    Nedarbas_Emigracija = Nedarbas_bendras * Emigrantai,
    Nedarbas_Gyventojai = Nedarbas_bendras * Gyventojai,
    Krizė_Taupymas = Krizė * Taupymas,
    Krizė_Emigracija = Krizė * Emigrantai)

X_train8 <- scale(as.matrix(train8 %>% dplyr::select(-BVP_I_palyg)))
X_test8 <- scale(as.matrix(test8 %>% dplyr::select(-BVP_I_palyg)),
                 center = attr(X_train8, "scaled:center"),
                 scale = attr(X_train8, "scaled:scale"))

y_train8 <- scale(train8$BVP_I_palyg)
y_test8 <- scale(test8$BVP_I_palyg,
                 center = attr(y_train8, "scaled:center"),
                 scale = attr(y_train8, "scaled:scale"))

lambda_seq <- 10^seq(3, -3, length.out = 100)

set.seed(123)
cv_lasso <- cv.glmnet(X_train8, y_train8, alpha = 1, lambda = lambda_seq, nfolds = 10,
                      standardize = FALSE)

lambda <- cv_lasso$lambda.min
cat("Geriausias lambda parametras", lambda)

df <- data.frame(lambda = log(cv_lasso$lambda), mse = cv_lasso$cvm)

ggplot(df, aes(x = lambda, y = mse)) +
  geom_line(color = "#414141") +
  geom_point(color = "#78003F") +
  geom_vline(xintercept = log(cv_lasso$lambda.min), linetype = "dashed", color = "black") +
  labs(title = "Lambda pasirinkimas Lasso regresijai",
       x = "log(Lambda)", y = "Vidutinė kryžminės validacijos klaida (MSE)") + theme_minimal()

#modelis
lasso_model8 <- glmnet(X_train8, y_train8, alpha = 1, lambda = lambda, standardize = FALSE)

coef_lasso8 <- coef(lasso_model8)
print(coef_lasso8)

coef_df8 <- data.frame(variable = rownames(coef_lasso8), coefficient = as.vector(coef_lasso8))
coef_df8 <- coef_df8[coef_df8$variable != "(Intercept)", ]

#koeficientai, rodiklių įtaka y
ggplot(coef_df8, aes(x = reorder(variable, coefficient), y = coefficient)) +
  geom_bar(stat = "identity", fill = "#78003F") + coord_flip() +
  labs(title = "Lasso regresijos koeficientai", x = "Kintamieji", y = "Koeficiento reikšmė") +
  theme_minimal()

#prognozavimas (siekiant įvertinti modelį)
y_pred8 <- predict(lasso_model8, s = lambda, newx = X_test8) %>% as.vector()

prognoze_8 <- data.frame(Realios_reiksmes = y_test8, Prognozuotos_reiksmes = y_pred8)

ggplot(prognoze_8, aes(x = Realios_reiksmes, y = Prognozuotos_reiksmes)) +
  geom_point(color = "#78003F", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Tikros ir prognozuotos Lasso modelio reikšmės",
       x = "Tikros reikšmės", y = "Prognozuotos reikšmės") + theme_minimal()

cor(y_test8, y_pred8, method = "spearman")

################################################################################
#modelio testavimas
y_test_original <- test8$BVP_I_palyg
likuciai <- y_test8 - y_pred8

qqnorm(likuciai, col = "#78003F", main = "Modelio liekanos")
qqline(likuciai, col = "black")

rmse <- sqrt(mean(likuciai^2))
rmse

mse <- mean(likuciai^2)
mse

mae <- mean(abs(likuciai))
mae

n <- length(y_test_original)
RSS <- sum(likuciai^2)
k <- sum(coef(lasso_model8)[-1] != 0) 

aic <- n * log(RSS / n) + 2 * k
bic <- n * log(RSS / n) + log(n) * k
aic
bic

################################################################################
################################################################################
#9 modelis - Lasso regresija su BVP komponente (vartojimu), be rodiklių sąveikų
################################################################################
################################################################################
data9 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
data9 <- read_excel(data9, sheet = "BE SEZONIŠKUMO")

rodikliai9 <- data9 %>%
  dplyr::select(Metai, BVP_I_palyg, Imigrantai, Emigrantai, Taupymas, Mirštamumas, 
                Paslaugų_pasitikėjimas, Nedarbas_bendras, Gyventojai, 
                Vartotojų_pasitikėjimas, Gimstamumas, BVP_vartojimas_palyg) %>%
  mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0))

rodikliai9 <- rodikliai9 %>% arrange(Metai) %>% mutate(BVP_lag = dplyr::lag(BVP_I_palyg, 1))
rodikliai9 <- rodikliai9 %>% na.omit()

train9 <- rodikliai9 %>% filter(Metai <= 2017) %>% dplyr::select(-Metai)
test9 <- rodikliai9 %>% filter(Metai > 2017) %>% dplyr::select(-Metai)

X_train9 <- scale(as.matrix(train9 %>% dplyr::select(-BVP_I_palyg)))
X_test9 <- scale(as.matrix(test9 %>% dplyr::select(-BVP_I_palyg)),
                 center = attr(X_train9, "scaled:center"),
                 scale = attr(X_train9, "scaled:scale"))

y_train9 <- scale(train9$BVP_I_palyg)
y_test9 <- scale(test9$BVP_I_palyg,
                 center = attr(y_train9, "scaled:center"),
                 scale = attr(y_train9, "scaled:scale"))

lambda_seq <- 10^seq(3, -3, length.out = 100)

set.seed(123)
cv_lasso <- cv.glmnet(X_train9, y_train9, alpha = 1, lambda = lambda_seq, nfolds = 10,
                      standardize = FALSE)

lambda <- cv_lasso$lambda.min
cat("Geriausias lambda parametras", lambda)

df <- data.frame(lambda = log(cv_lasso$lambda), mse = cv_lasso$cvm)

ggplot(df, aes(x = lambda, y = mse)) +
  geom_line(color = "#414141") +
  geom_point(color = "#78003F") +
  geom_vline(xintercept = log(cv_lasso$lambda.min), linetype = "dashed", color = "black") +
  labs(title = "Lambda pasirinkimas Lasso regresijai",
       x = "log(Lambda)", y = "Vidutinė kryžminės validacijos klaida (MSE)") + theme_minimal()

#modelis
lasso_model9 <- glmnet(X_train9, y_train9, alpha = 1, lambda = lambda, standardize = FALSE)

coef_lasso9 <- coef(lasso_model9)
print(coef_lasso9)

coef_df9 <- data.frame(variable = rownames(coef_lasso9), coefficient = as.vector(coef_lasso9))
coef_df9 <- coef_df9[coef_df9$variable != "(Intercept)", ]

ggplot(coef_df9, aes(x = reorder(variable, coefficient), y = coefficient)) +
  geom_bar(stat = "identity", fill = "#78003F") + coord_flip() +
  labs(title = "Lasso regresijos koeficientai", x = "Kintamieji", y = "Koeficiento reikšmė") +
  theme_minimal()

y_pred9 <- predict(lasso_model9, s = lambda, newx = X_test9) %>% as.vector()

prognoze_9 <- data.frame(Realios_reiksmes = y_test9, Prognozuotos_reiksmes = y_pred9)

ggplot(prognoze_9, aes(x = Realios_reiksmes, y = Prognozuotos_reiksmes)) +
  geom_point(color = "#78003F", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Tikros ir prognozuotos Lasso modelio reikšmės",
       x = "Tikros reikšmės", y = "Prognozuotos reikšmės") + theme_minimal()

cor(y_test9, y_pred9, method = "spearman")

################################################################################
#modelio testavimas
y_test_original <- test9$BVP_I_palyg
likuciai <- y_test9 - y_pred9

qqnorm(likuciai, col = "#78003F", main = "Modelio liekanos")
qqline(likuciai, col = "black")

rmse <- sqrt(mean(likuciai^2))
rmse

mse <- mean(likuciai^2)
mse

mae <- mean(abs(likuciai))
mae

n <- length(y_test_original)
RSS <- sum(likuciai^2)
k <- sum(coef(lasso_model9)[-1] != 0)

aic <- n * log(RSS / n) + 2 * k
bic <- n * log(RSS / n) + log(n) * k
aic
bic



################################################################################
################################################################################
                    #7-9 modelių prognozių lyginimas
################################################################################
################################################################################
#'atnormalizavimas'
reali_7 <- y_test7 * attr(y_train7, "scaled:scale") + attr(y_train7, "scaled:center")
prog_7 <- y_pred7 * attr(y_train7, "scaled:scale") + attr(y_train7, "scaled:center")
prog_8 <- y_pred8 * attr(y_train8, "scaled:scale") + attr(y_train8, "scaled:center")
prog_9 <- y_pred9 * attr(y_train9, "scaled:scale") + attr(y_train9, "scaled:center")

prog_datos <- seq(as.Date("2018-01-01"), by = "month", length.out = length(reali_7))

lasso_df <- data.frame(
  Data = prog_datos,
  'Reali reikšmė' = reali_7,
  'Pradinis modelis' = prog_7,
  'Su sąveikomis' = prog_8,
  'Su BVP komponente' = prog_9,
  check.names = FALSE
)

lasso_long <- lasso_df %>%
  pivot_longer(cols = -Data, names_to = "Tipas", values_to = "BVP_I_palyg")

originalus_df <- rodikliai7 %>%
  filter(Metai <= 2017) %>%
  mutate(Data = seq(as.Date("2002-07-01"), by = "month", length.out = n()),
         Tipas = "Reali reikšmė") %>%
  dplyr::select(Data, BVP_I_palyg, Tipas)

grafiko_duomenys <- bind_rows(originalus_df, lasso_long)

grafiko_duomenys$Tipas <- factor(grafiko_duomenys$Tipas,
                                 levels = c("Reali reikšmė", "Pradinis modelis", "Su sąveikomis", "Su BVP komponente"))

prog <- ggplot(grafiko_duomenys, aes(x = Data, y = BVP_I_palyg / 1000, color = Tipas, linetype = Tipas, group = Tipas)) +
  geom_line(size = 1.3) +
  geom_vline(xintercept = as.Date("2017-12-01"), linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("2018-01-01"),
           y = max(grafiko_duomenys$BVP_I_palyg, na.rm = TRUE) / 1000,
           label = "Prasideda prognozės", hjust = 0, vjust = 1.2, color = "black", size = 6) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1, big.mark = ","),
                     name = "BVP (mlrd. eur.)",
                     breaks = seq(5, 20, by = 2)) +
  labs(title = "Lasso regresijos modelių prognozių palyginimas",
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

ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/lassopro.png", prog,
       width = 10, height = 6.5, dpi = 300, units = "in")


################################################################################
#GALUTINIS 7-9 modelių testų rezultatų palyginimas
################################################################################

###########TRANSFORMUOTŲ (STANDARTIZUOTŲ) LASSO MODELIŲ PALYGINIMAS
likuciai7 <- y_test7 - y_pred7
likuciai8 <- y_test8 - y_pred8
likuciai9 <- y_test9 - y_pred9

mae_lasso <- c(mae(y_test7, y_pred7),
               mae(y_test8, y_pred8),
               mae(y_test9, y_pred9))

rmse_lasso <- c(rmse(y_test7, y_pred7),
                rmse(y_test8, y_pred8),
                rmse(y_test9, y_pred9))

mse_lasso <- c(mse(y_test7, y_pred7),
               mse(y_test8, y_pred8),
               mse(y_test9, y_pred9))

n7 <- length(y_test7)
n8 <- length(y_test8)
n9 <- length(y_test9)

RSS7 <- sum(likuciai7^2)
RSS8 <- sum(likuciai8^2)
RSS9 <- sum(likuciai9^2)

k7 <- sum(coef(lasso_model7)[-1] != 0)
k8 <- sum(coef(lasso_model8)[-1] != 0)
k9 <- sum(coef(lasso_model9)[-1] != 0)

aic_lasso <- c(n7 * log(RSS7 / n7) + 2 * k7,
               n8 * log(RSS8 / n8) + 2 * k8,
               n9 * log(RSS9 / n9) + 2 * k9)

bic_lasso <- c(n7 * log(RSS7 / n7) + log(n7) * k7,
               n8 * log(RSS8 / n8) + log(n8) * k8,
               n9 * log(RSS9 / n9) + log(n9) * k9)

rez_lasso_transf <- data.frame(
  Modelis = c("Pradinis modelis", "Su sąveikomis", "Su BVP komponente"),
  MAE = round(mae_lasso, 3),
  RMSE = round(rmse_lasso, 3),
  MSE = round(mse_lasso, 3),
  AIC = round(aic_lasso, 2),
  BIC = round(bic_lasso, 2)
)

rez_lasso_transf

###########ATTRANSFORMUOTŲ LASSO MODELIŲ PALYGINIMAS
reali_7 <- y_test7 * attr(y_train7, "scaled:scale") + attr(y_train7, "scaled:center")
prog_7  <- y_pred7 * attr(y_train7, "scaled:scale") + attr(y_train7, "scaled:center")
prog_8  <- y_pred8 * attr(y_train8, "scaled:scale") + attr(y_train8, "scaled:center")
prog_9  <- y_pred9 * attr(y_train9, "scaled:scale") + attr(y_train9, "scaled:center")

resid7 <- reali_7 - prog_7
resid8 <- reali_7 - prog_8 
resid9 <- reali_7 - prog_9

mae_lasso_detr <- c(mean(abs(resid7)), mean(abs(resid8)), mean(abs(resid9)))
rmse_lasso_detr <- c(sqrt(mean(resid7^2)), sqrt(mean(resid8^2)), sqrt(mean(resid9^2)))
mse_lasso_detr <- c(mean(resid7^2), mean(resid8^2), mean(resid9^2))

n <- length(reali_7)
k7 <- sum(coef(lasso_model7)[-1] != 0)
k8 <- sum(coef(lasso_model8)[-1] != 0)
k9 <- sum(coef(lasso_model9)[-1] != 0)

RSS7 <- sum(resid7^2)
RSS8 <- sum(resid8^2)
RSS9 <- sum(resid9^2)

aic_lasso_detr <- c(n * log(RSS7 / n) + 2 * k7,
                    n * log(RSS8 / n) + 2 * k8,
                    n * log(RSS9 / n) + 2 * k9)

bic_lasso_detr <- c(n * log(RSS7 / n) + log(n) * k7,
                    n * log(RSS8 / n) + log(n) * k8,
                    n * log(RSS9 / n) + log(n) * k9)

rez_lasso_detr <- data.frame(
  Modelis = c("Pradinis modelis", "Su sąveikomis", "Su BVP komponente"),
  MAE = round(mae_lasso_detr, 2),
  RMSE = round(rmse_lasso_detr, 2),
  MSE = round(mse_lasso_detr, 2),
  AIC = round(aic_lasso_detr, 2),
  BIC = round(bic_lasso_detr, 2)
)

rez_lasso_detr



################################################################################
################################################################################
  #10 modelis - Elastic Net regresija be BVP komponenčių, be rodiklių sąveikų
################################################################################
################################################################################
data10 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
data10 <- read_excel(data10, sheet = "BE SEZONIŠKUMO")

rodikliai10 <- data10 %>%
  dplyr::select(Metai, BVP_I_palyg, Imigrantai, Emigrantai, Taupymas, Mirštamumas, 
                Paslaugų_pasitikėjimas, Nedarbas_bendras, Gyventojai, 
                Vartotojų_pasitikėjimas, Gimstamumas) %>%
  mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0))

rodikliai10 <- rodikliai10 %>% arrange(Metai) %>% mutate(BVP_lag = dplyr::lag(BVP_I_palyg, 1))
rodikliai10 <- rodikliai10 %>% na.omit()

train10 <- rodikliai10 %>% filter(Metai <= 2017) %>% dplyr::select(-Metai)
test10 <- rodikliai10 %>% filter(Metai > 2017) %>% dplyr::select(-Metai)

X_train10 <- scale(as.matrix(train10 %>% dplyr::select(-BVP_I_palyg)))
X_test10 <- scale(as.matrix(test10 %>% dplyr::select(-BVP_I_palyg)),
                 center = attr(X_train10, "scaled:center"),
                 scale = attr(X_train10, "scaled:scale"))

y_train10 <- scale(train10$BVP_I_palyg)
y_test10 <- scale(test10$BVP_I_palyg,
                 center = attr(y_train10, "scaled:center"),
                 scale = attr(y_train10, "scaled:scale"))

lambda_seq <- 10^seq(3, -3, length.out = 100)

set.seed(123)
cv_enet <- cv.glmnet(X_train10, y_train10, alpha = 0.5, lambda = lambda_seq, nfolds = 10,
                     standardize = FALSE)

lambda <- cv_enet$lambda.min
cat("Geriausias lambda parametras", lambda)

df <- data.frame(lambda = log(cv_enet$lambda), mse = cv_enet$cvm)

ggplot(df, aes(x = lambda, y = mse)) +
  geom_line(color = "#414141") +
  geom_point(color = "#78003F") +
  geom_vline(xintercept = log(cv_enet$lambda.min), linetype = "dashed", color = "black") +
  labs(title = "Lambda pasirinkimas Elastic Net regresijai",
       x = "log(Lambda)", y = "Vidutinė kryžminės validacijos klaida (MSE)") + theme_minimal()

enet_model10 <- glmnet(X_train10, y_train10, alpha = 0.5, lambda = lambda, standardize = FALSE)

coef_enet10 <- coef(enet_model10)
print(coef_enet10)

coef_df10 <- data.frame(variable = rownames(coef_enet10), coefficient = as.vector(coef_enet10))
coef_df10 <- coef_df10[coef_df10$variable != "(Intercept)", ]

ggplot(coef_df10, aes(x = reorder(variable, coefficient), y = coefficient)) +
  geom_bar(stat = "identity", fill = "#78003F") + coord_flip() +
  labs(title = "Elastic Net regresijos koeficientai", x = "Kintamieji", y = "Koeficiento reikšmė") +
  theme_minimal()

y_pred10 <- predict(enet_model10, s = lambda, newx = X_test10) %>% as.vector()

prediction_10 <- data.frame(Realios_reiksmes10 = y_test10, Prognozuotos_reiksmes10 = y_pred10)

ggplot(prediction_10, aes(x = Realios_reiksmes10, y = Prognozuotos_reiksmes10)) +
  geom_point(color = "#78003F", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Tikros ir prognozuotos Elastic Net modelio reikšmės",
       x = "Tikros reikšmės", y = "Prognozuotos reikšmės") + theme_minimal()

cor(y_test10, y_pred10, method = "spearman")

################################################################################
#modelio testavimas
y_test_original <- test10$BVP_I_palyg
likuciai <- y_test10 - y_pred10

qqnorm(likuciai, col = "#78003F", main = "Modelio liekanos")
qqline(likuciai, col = "black")

rmse <- sqrt(mean(likuciai^2))
rmse

mse <- mean(likuciai^2)
mse

mae <- mean(abs(likuciai))
mae

n <- length(y_test_original)
RSS <- sum(likuciai^2)
k <- sum(coef(enet_model10)[-1] != 0)

aic <- n * log(RSS / n) + 2 * k
bic <- n * log(RSS / n) + log(n) * k
aic
bic


################################################################################
################################################################################
 #11 modelis - Elastic Net regresija be BVP komponenčių, su rodiklių sąveikomis
################################################################################
################################################################################
data11 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
data11 <- read_excel(data11, sheet = "BE SEZONIŠKUMO")

rodikliai11 <- data11 %>%
  dplyr::select(Metai, BVP_I_palyg, Imigrantai, Emigrantai, Taupymas, Mirštamumas, 
                Paslaugų_pasitikėjimas, Nedarbas_bendras, Gyventojai, 
                Vartotojų_pasitikėjimas, Gimstamumas) %>%
  mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0))

rodikliai11 <- rodikliai11 %>% arrange(Metai) %>% mutate(BVP_lag = dplyr::lag(BVP_I_palyg, 1))
rodikliai11 <- rodikliai11 %>% na.omit()

train11 <- rodikliai11 %>% filter(Metai <= 2017) %>% dplyr::select(-Metai)
test11 <- rodikliai11 %>% filter(Metai > 2017) %>% dplyr::select(-Metai)

train11 <- train11 %>%
  mutate(
    Nedarbas_Emigracija = Nedarbas_bendras * Emigrantai,
    Nedarbas_Gyventojai = Nedarbas_bendras * Gyventojai,
    Krizė_Taupymas = Krizė * Taupymas,
    Krizė_Emigracija = Krizė * Emigrantai)

test11 <- test11 %>%
  mutate(
    Nedarbas_Emigracija = Nedarbas_bendras * Emigrantai,
    Nedarbas_Gyventojai = Nedarbas_bendras * Gyventojai,
    Krizė_Taupymas = Krizė * Taupymas,
    Krizė_Emigracija = Krizė * Emigrantai)

X_train11 <- scale(as.matrix(train11 %>% dplyr::select(-BVP_I_palyg)))
X_test11 <- scale(as.matrix(test11 %>% dplyr::select(-BVP_I_palyg)),
                center = attr(X_train11, "scaled:center"),
                scale = attr(X_train11, "scaled:scale"))

y_train11 <- scale(train11$BVP_I_palyg)
y_test11 <- scale(test11$BVP_I_palyg,
                center = attr(y_train11, "scaled:center"),
                scale = attr(y_train11, "scaled:scale"))

lambda_seq <- 10^seq(3, -3, length.out = 100)

set.seed(123)
cv_enet <- cv.glmnet(X_train11, y_train11, alpha = 0.5, lambda = lambda_seq, nfolds = 10,
                     standardize = FALSE)

lambda <- cv_enet$lambda.min
cat("Geriausias lambda parametras", lambda)

df <- data.frame(lambda = log(cv_enet$lambda), mse = cv_enet$cvm)

ggplot(df, aes(x = lambda, y = mse)) +
  geom_line(color = "#414141") +
  geom_point(color = "#78003F") +
  geom_vline(xintercept = log(cv_enet$lambda.min), linetype = "dashed", color = "black") +
  labs(title = "Lambda pasirinkimas Elastic Net regresijai",
       x = "log(Lambda)", y = "Vidutinė kryžminės validacijos klaida (MSE)") + theme_minimal()

enet_model11 <- glmnet(X_train11, y_train11, alpha = 0.5, lambda = lambda, standardize = FALSE)

coef_enet11 <- coef(enet_model11)
print(coef_enet11)

coef_df11 <- data.frame(variable = rownames(coef_enet11), coefficient = as.vector(coef_enet11))
coef_df11 <- coef_df11[coef_df11$variable != "(Intercept)", ]

ggplot(coef_df11, aes(x = reorder(variable, coefficient), y = coefficient)) +
  geom_bar(stat = "identity", fill = "#78003F") + coord_flip() +
  labs(title = "Elastic Net regresijos koeficientai", x = "Kintamieji", y = "Koeficiento reikšmė") +
  theme_minimal()

y_pred11 <- predict(enet_model11, s = lambda, newx = X_test11) %>% as.vector()

prediction_11 <- data.frame(Realios_reiksmes11 = y_test11, Prognozuotos_reiksmes11 = y_pred11)

ggplot(prediction_11, aes(x = Realios_reiksmes11, y = Prognozuotos_reiksmes11)) +
  geom_point(color = "#78003F", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Tikros ir prognozuotos Elastic Net modelio reikšmės",
       x = "Tikros reikšmės", y = "Prognozuotos reikšmės") + theme_minimal()

cor(y_test11, y_pred11, method = "spearman")

################################################################################
#modelio testavimas
y_test_original <- test11$BVP_I_palyg
likuciai <- y_test11 - y_pred11

qqnorm(likuciai, col = "#78003F", main = "Modelio liekanos")
qqline(likuciai, col = "black")

rmse <- sqrt(mean(likuciai^2))
rmse

mse <- mean(likuciai^2)
mse

mae <- mean(abs(likuciai))
mae

n <- length(y_test_original)
RSS <- sum(likuciai^2)
k <- sum(coef(enet_model11)[-1] != 0)

aic <- n * log(RSS / n) + 2 * k
bic <- n * log(RSS / n) + log(n) * k
aic
bic


################################################################################
################################################################################
  #12 modelis - Elastic Net regresija su BVP komponente, be rodiklių sąveikų
################################################################################
################################################################################
data12 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
data12 <- read_excel(data12, sheet = "BE SEZONIŠKUMO")

rodikliai12 <- data12 %>%
  dplyr::select(Metai, BVP_I_palyg, Imigrantai, Emigrantai, Taupymas, Mirštamumas, 
                Paslaugų_pasitikėjimas, Nedarbas_bendras, Gyventojai, 
                Vartotojų_pasitikėjimas, Gimstamumas, BVP_vartojimas_palyg) %>%
  mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0))

rodikliai12 <- rodikliai12 %>% arrange(Metai) %>% mutate(BVP_lag = dplyr::lag(BVP_I_palyg, 1))
rodikliai12 <- rodikliai12 %>% na.omit()

train12 <- rodikliai12 %>% filter(Metai <= 2017) %>% dplyr::select(-Metai)
test12 <- rodikliai12 %>% filter(Metai > 2017) %>% dplyr::select(-Metai)

X_train12 <- scale(as.matrix(train12 %>% dplyr::select(-BVP_I_palyg)))
X_test12 <- scale(as.matrix(test12 %>% dplyr::select(-BVP_I_palyg)),
                center = attr(X_train12, "scaled:center"),
                scale = attr(X_train12, "scaled:scale"))

y_train12 <- scale(train12$BVP_I_palyg)
y_test12 <- scale(test12$BVP_I_palyg,
                center = attr(y_train12, "scaled:center"),
                scale = attr(y_train12, "scaled:scale"))

lambda_seq <- 10^seq(3, -3, length.out = 100)

set.seed(123)
cv_enet <- cv.glmnet(X_train12, y_train12, alpha = 0.5, lambda = lambda_seq, nfolds = 10,
                     standardize = FALSE)

lambda <- cv_enet$lambda.min
cat("Geriausias lambda parametras", lambda)

df <- data.frame(lambda = log(cv_enet$lambda), mse = cv_enet$cvm)

ggplot(df, aes(x = lambda, y = mse)) +
  geom_line(color = "#414141") +
  geom_point(color = "#78003F") +
  geom_vline(xintercept = log(cv_enet$lambda.min), linetype = "dashed", color = "black") +
  labs(title = "Lambda pasirinkimas Elastic Net regresijai",
       x = "log(Lambda)", y = "Vidutinė kryžminės validacijos klaida (MSE)") + theme_minimal()

enet_model12 <- glmnet(X_train12, y_train12, alpha = 0.5, lambda = lambda, standardize = FALSE)

coef_enet12 <- coef(enet_model12)
print(coef_enet12)

coef_df12 <- data.frame(variable = rownames(coef_enet12), coefficient = as.vector(coef_enet12))
coef_df12 <- coef_df12[coef_df12$variable != "(Intercept)", ]

ggplot(coef_df12, aes(x = reorder(variable, coefficient), y = coefficient)) +
  geom_bar(stat = "identity", fill = "#78003F") + coord_flip() +
  labs(title = "Elastic Net regresijos koeficientai", x = "Kintamieji", y = "Koeficiento reikšmė") +
  theme_minimal()

y_pred12 <- predict(enet_model12, s = lambda, newx = X_test12) %>% as.vector()

prediction_12 <- data.frame(Realios_reiksmes12 = y_test12, Prognozuotos_reiksmes12 = y_pred12)

ggplot(prediction_12, aes(x = Realios_reiksmes12, y = Prognozuotos_reiksmes12)) +
  geom_point(color = "#78003F", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Tikros ir prognozuotos Elastic Net modelio reikšmės",
       x = "Tikros reikšmės", y = "Prognozuotos reikšmės") + theme_minimal()

cor(y_test12, y_pred12, method = "spearman")

################################################################################
#modelio testavimas
y_test_original <- test12$BVP_I_palyg
likuciai <- y_test12 - y_pred12

qqnorm(likuciai, col = "#78003F", main = "Modelio liekanos")
qqline(likuciai, col = "black")

rmse <- sqrt(mean(likuciai^2))
rmse

mse <- mean(likuciai^2)
mse

mae <- mean(abs(likuciai))
mae

n <- length(y_test_original)
RSS <- sum(likuciai^2)
k <- sum(coef(enet_model12)[-1] != 0)

aic <- n * log(RSS / n) + 2 * k
bic <- n * log(RSS / n) + log(n) * k
aic
bic



################################################################################
################################################################################
                   #10-12 modelių prognozių lyginimas
################################################################################
################################################################################
#'atnormalizavimas'
reali_10 <- y_test10 * attr(y_train10, "scaled:scale") + attr(y_train10, "scaled:center")
prog_10 <- y_pred10 * attr(y_train10, "scaled:scale") + attr(y_train10, "scaled:center")
prog_11 <- y_pred11 * attr(y_train11, "scaled:scale") + attr(y_train11, "scaled:center")
prog_12 <- y_pred12 * attr(y_train12, "scaled:scale") + attr(y_train12, "scaled:center")

prog_datos <- seq(as.Date("2018-01-01"), by = "month", length.out = length(reali_10))

elnet_df <- data.frame(
  Data = prog_datos,
  'Reali reikšmė' = reali_10,
  'Pradinis modelis' = prog_10,
  'Su sąveikomis' = prog_11,
  'Su BVP komponente' = prog_12,
  check.names = FALSE
)

elnet_long <- elnet_df %>%
  pivot_longer(cols = -Data, names_to = "Tipas", values_to = "BVP_I_palyg")

originalus_df <- rodikliai4 %>%
  filter(Metai <= 2017) %>%
  mutate(Data = seq(as.Date("2002-07-01"), by = "month", length.out = n()),
         Tipas = "Reali reikšmė") %>%
  dplyr::select(Data, BVP_I_palyg, Tipas)

grafiko_duomenys <- bind_rows(originalus_df, elnet_long)

grafiko_duomenys$Tipas <- factor(grafiko_duomenys$Tipas,
                                 levels = c("Reali reikšmė", "Pradinis modelis", "Su sąveikomis", "Su BVP komponente"))

prog <- ggplot(grafiko_duomenys, aes(x = Data, y = BVP_I_palyg / 1000, color = Tipas, linetype = Tipas, group = Tipas)) +
  geom_line(size = 1.3) +
  geom_vline(xintercept = as.Date("2017-12-01"), linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("2018-01-01"),
           y = max(grafiko_duomenys$BVP_I_palyg, na.rm = TRUE) / 1000,
           label = "Prasideda prognozės", hjust = 0, vjust = 1.2, color = "black", size = 5) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1, big.mark = ","),
                     name = "BVP (mlrd. eur.)",
                     breaks = seq(5, 20, by = 2)) +
  labs(title = "Elastic Net regresijos modelių prognozių palyginimas",
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

ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/elnetpro.png", prog,
       width = 10, height = 6.5, dpi = 300, units = "in")


################################################################################
#GALUTINIS 10-12 modelių testų rezultatų palyginimas
################################################################################

###########TRANSFORMUOTŲ (STANDARTIZUOTŲ) ELASTIC NET MODELIŲ PALYGINIMAS
likuciai10 <- y_test10 - y_pred10
likuciai11 <- y_test11 - y_pred11
likuciai12 <- y_test12 - y_pred12

mae_enet <- c(mae(y_test10, y_pred10),
               mae(y_test11, y_pred11),
               mae(y_test12, y_pred12))

rmse_enet <- c(rmse(y_test10, y_pred10),
                rmse(y_test11, y_pred11),
                rmse(y_test12, y_pred12))

mse_enet <- c(mse(y_test10, y_pred10),
               mse(y_test11, y_pred11),
               mse(y_test12, y_pred12))

n10 <- length(y_test10)
n11 <- length(y_test11)
n12 <- length(y_test12)

RSS10 <- sum(likuciai10^2)
RSS11 <- sum(likuciai11^2)
RSS12 <- sum(likuciai12^2)

k10 <- sum(coef(enet_model10)[-1] != 0)
k11 <- sum(coef(enet_model11)[-1] != 0)
k12 <- sum(coef(enet_model12)[-1] != 0)

aic_enet <- c(n10 * log(RSS10 / n10) + 2 * k10,
              n11 * log(RSS11 / n11) + 2 * k11,
              n12 * log(RSS12 / n12) + 2 * k12)

bic_enet <- c(n10 * log(RSS10 / n10) + log(n10) * k10,
              n11 * log(RSS11 / n11) + log(n11) * k11,
              n12 * log(RSS12 / n12) + log(n12) * k12)

rez_enet_transf <- data.frame(
  Modelis = c("Pradinis modelis", "Su sąveikomis", "Su BVP komponente"),
  MAE = round(mae_enet, 3),
  RMSE = round(rmse_enet, 3),
  MSE = round(mse_enet, 3),
  AIC = round(aic_enet, 2),
  BIC = round(bic_enet, 2)
)

rez_enet_transf

###########ATTRANSFORMUOTŲ ELASTIC NET MODELIŲ PALYGINIMAS
reali_10 <- y_test10 * attr(y_train10, "scaled:scale") + attr(y_train10, "scaled:center")
prog_10  <- y_pred10 * attr(y_train10, "scaled:scale") + attr(y_train10, "scaled:center")
prog_11  <- y_pred11 * attr(y_train11, "scaled:scale") + attr(y_train11, "scaled:center")
prog_12  <- y_pred12 * attr(y_train12, "scaled:scale") + attr(y_train12, "scaled:center")

resid10 <- reali_10 - prog_10
resid11 <- reali_10 - prog_11
resid12 <- reali_10 - prog_12

mae_enet_detr <- c(mean(abs(resid10)), mean(abs(resid11)), mean(abs(resid12)))
rmse_enet_detr <- c(sqrt(mean(resid10^2)), sqrt(mean(resid11^2)), sqrt(mean(resid12^2)))
mse_enet_detr <- c(mean(resid10^2), mean(resid11^2), mean(resid12^2))

n <- length(reali_10)
k10 <- sum(coef(enet_model10)[-1] != 0)
k11 <- sum(coef(enet_model11)[-1] != 0)
k12 <- sum(coef(enet_model12)[-1] != 0)

RSS10 <- sum(resid10^2)
RSS11 <- sum(resid11^2)
RSS12 <- sum(resid12^2)

aic_enet_detr <- c(n * log(RSS10 / n) + 2 * k10,
                    n * log(RSS11 / n) + 2 * k11,
                    n * log(RSS12 / n) + 2 * k12)

bic_enet_detr <- c(n * log(RSS10 / n) + log(n) * k10,
                    n * log(RSS11 / n) + log(n) * k11,
                    n * log(RSS12 / n) + log(n) * k12)

rez_enet_detr <- data.frame(
  Modelis = c("Pradinis modelis", "Su sąveikomis", "Su BVP komponente"),
  MAE = round(mae_enet_detr, 2),
  RMSE = round(rmse_enet_detr, 2),
  MSE = round(mse_enet_detr, 2),
  AIC = round(aic_enet_detr, 2),
  BIC = round(bic_enet_detr, 2)
)

rez_enet_detr