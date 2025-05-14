################################################################################
################################################################################
         #Liekanų autokoreliacijos korekcijų testavimas 3 modeliui
################################################################################
################################################################################

################################################################################
################################################################################
    #ORIGINALUS MODELIS 3 modelis - su BVP komponente, be rodiklių sąveikos
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



################################################################################
#Cochrane-Orcutt
################################################################################

#KOREKCIJOS su rankiniu būdu pritaikytu Cochrane-Orcutt metodu
modelis3 <- lm(BVP_I_palyg ~ Paslaugų_pasitikėjimas + Nedarbas_bendras +  
                   Gyventojai + BVP_vartojimas_palyg, data = train3)

dw_test <- durbinWatsonTest(modelis3)

rho <- 1 - dw_test$dw / 2  
print(paste("Pirmos eilės autokoreliacija rho:", rho))

train_kor1 <- train3 %>%
  arrange(Metai) %>%  
  mutate(
    BVP_I_palyg_lag = lag(BVP_I_palyg),
    Paslaugų_pasitikėjimas_lag = lag(Paslaugų_pasitikėjimas),
    Nedarbas_bendras_lag = lag(Nedarbas_bendras),
    Gyventojai_lag = lag(Gyventojai),
    BVP_vartojimas_palyg_lag = lag(BVP_vartojimas_palyg)
  ) %>%
  mutate(
    BVP_I_palyg = BVP_I_palyg - rho * BVP_I_palyg_lag,
    Paslaugų_pasitikėjimas = Paslaugų_pasitikėjimas - rho * Paslaugų_pasitikėjimas_lag,
    Nedarbas_bendras = Nedarbas_bendras - rho * Nedarbas_bendras_lag,
    Gyventojai = Gyventojai - rho * Gyventojai_lag,
    BVP_vartojimas_palyg = BVP_vartojimas_palyg - rho * BVP_vartojimas_palyg_lag
  ) %>%
  na.omit()

modelis3_kor1 <- lm(BVP_I_palyg ~ Paslaugų_pasitikėjimas + Nedarbas_bendras +  
                      Gyventojai + BVP_vartojimas_palyg, data = train_kor1)

summary(modelis3_kor1)

modelis3_kor1 <- lm(BVP_I_palyg ~   
                      Gyventojai + BVP_vartojimas_palyg, data = train_kor1)

summary(modelis3_kor1)

#1.1043
durbinWatsonTest(modelis3_kor1)


################################################################################
#Prais-Winsten
################################################################################
modelis3_kor2 <- prais_winsten(BVP_I_palyg ~ Paslaugų_pasitikėjimas + 
                                 Nedarbas_bendras + Gyventojai + BVP_vartojimas_palyg, 
                               data = train3, index = "Metai")

summary(modelis3_kor2)

#šalinu nereikšmingus rodiklius
modelis3_kor2 <- prais_winsten(BVP_I_palyg ~  + 
                                 Nedarbas_bendras + Gyventojai + BVP_vartojimas_palyg, 
                               data = train3, index = "Metai")

summary(modelis3_kor2)

modelis3_kor2 <- prais_winsten(BVP_I_palyg ~  + 
                               Gyventojai + BVP_vartojimas_palyg, 
                               data = train3, index = "Metai")

#DW 1.282
summary(modelis3_kor2)

