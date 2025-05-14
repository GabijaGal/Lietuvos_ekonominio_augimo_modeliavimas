################################################################################
#1 žingsnis - prieš pradedant modeliuoti paverčiu ketvirčio dažnio duomenis
#į mėnesinius (kad dažnis sutaptų ir rodiklius galėčiau naudoti vienu metu)

#testuojami metodai: nuo lengviausių (linijinės interpoliacijos) iki
#sudėtingesnių (kur naudojamas papildomas rodiklis siekiant tiksliau užpilyti
#mėnesio duomenis)

################################################################################

library(readxl)
library(tidyverse)
library(zoo)
library(ggplot2)
library(tempdisagg)
library(urca)
library(writexl)
library(patchwork)

################################################################################
#Linijinė interpoliacija
################################################################################

#duomenys
data <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
sheet <- "ORIG_KETV"

df_q <- read_excel(data, sheet = sheet) %>%
mutate(Date = as.yearqtr(Laikotarpis, format = "%YK%q")) %>% arrange(Date)

#duomenų formato koregavimas (skirtas interpoliacijai)
m_data <- function(df_q) {
  df_m <- df_q %>%
  slice(rep(1:n(), each = 3)) %>%  
  mutate(
    Mėnesis = rep(c(1, 2, 3), times = nrow(df_q)) + (as.numeric(format(Date, "%q")) - 1) * 3,
    Metai = as.numeric(format(Date, "%Y")),
    Date = as.Date(paste(Metai, Mėnesis, "01", sep = "-"))  
    ) %>%
    dplyr::select(Date, Metai, Mėnesis)
  return(df_m)
}

df_m <- m_data(df_q)
exclude <- c("Metai", "Ketvirtis", "Laikotarpis", "Date")
rodikliai <- setdiff(colnames(df_q), exclude)

interp <- function(q_reiksmes) {
  num_q <- length(q_reiksmes)
  num_m <- num_q * 3 
  
#indeksavimas
q_ind <- seq(1, num_m, by = 3)
m_ind <- seq(1, num_m, by = 1)
  
#interpoliacija
interp_reiksmes <- approx(q_ind, q_reiksmes, xout = m_ind, method = "linear", na.rm = TRUE)$y
return(interp_reiksmes)
}

for (rodiklis in rodikliai) {
  df_m[[rodiklis]] <- interp(df_q[[rodiklis]])
}

#pritaikau laikotarpio formatavimą (vizualizacijai)
df_q_viz <- df_q %>% mutate(Date = as.Date(paste(format(Date, "%Y"), (as.numeric(format(Date, "%q")) - 1) * 3 + 2, "01", sep = "-")))

#vizualizacija
plot <- function(var) {
  ggplot(na.omit(df_m), aes(x = Date, y = .data[[var]], color = "Linijinė interpoliacija")) +
  geom_line(size = 1) +
  geom_point(data = df_q_viz, aes(x = Date, y = .data[[var]]), color = "black", size = 2, na.rm = TRUE) +
  scale_color_manual(values = c("Linijinė interpoliacija" = "#78003F")) +
  labs(title = paste("Ketvirčio dažnio duomenų konvertavimas į mėnesinius"), x = "Metai", y = var, color = "Metodas") + theme_minimal()
}

print(plot("BVP_I_palyg"))
print(plot("BVP_vartojimas_palyg"))

print(head(df_m))

################################################################################
#Splaino interpoliacija (su fmm versija)
################################################################################

#duomenys
data1 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
sheet1 <- "ORIG_KETV"

df_q1 <- read_excel(data1, sheet = sheet1) %>%
mutate(Date = as.yearqtr(Laikotarpis, format = "%YK%q")) %>% arrange(Date)

m_data1 <- function(df_q1) {
  df_m1 <- df_q1 %>%
  slice(rep(1:n(), each = 3)) %>%  
  mutate(
    Mėnesis = rep(c(1, 2, 3), times = nrow(df_q1)) + (as.numeric(format(Date, "%q")) - 1) * 3,
    Metai = as.numeric(format(Date, "%Y")),
    Date = as.Date(paste(Metai, Mėnesis, "01", sep = "-"))  
    ) %>%
    dplyr::select(Date, Metai, Mėnesis)
  return(df_m1)
}

df_m1 <- m_data1(df_q1)

exclude1 <- c("Metai", "Ketvirtis", "Laikotarpis", "Date")
rodikliai1 <- setdiff(colnames(df_q1), exclude1)

#sukuriamos mėnesio reikšmės (pagal ketvirčio duomenų ilgį)
interp1 <- function(q_reiksmes1) {
  num_q1 <- length(q_reiksmes1)
  num_m1 <- num_q1 * 3  
  
q_ind1 <- seq(1, num_m1, by = 3)
m_ind1 <- seq(1, num_m1, by = 1)
  
interp_reiksmes1 <- spline(q_ind1, q_reiksmes1, xout = m_ind1, method = "fmm")$y
  return(interp_reiksmes1)
}

for (rodiklis in rodikliai) {
  df_m1[[rodiklis]] <- interp1(df_q1[[rodiklis]])
}

df_q_viz1 <- df_q1 %>% mutate(Date = as.Date(paste(format(Date, "%Y"), (as.numeric(format(Date, "%q")) - 1) * 3 + 2, "01", sep = "-")))

plot1 <- function(var) {
  ggplot(na.omit(df_m1), aes(x = Date, y = .data[[var]], color = "Splaino interpoliacija (fmm)")) +
  geom_line(size = 1) +
  geom_point(data = df_q_viz1, aes(x = Date, y = .data[[var]]), color = "black", size = 2, na.rm = TRUE) +
  scale_color_manual(values = c("Splaino interpoliacija (fmm)" = "#78003F")) +
  labs(title = paste("Ketvirčio dažnio duomenų konvertavimas į mėnesinius"), x = "Metai", y = var, color = "Metodas") + theme_minimal()
}

print(plot1("BVP_I_palyg"))
print(plot1("BVP_vartojimas_palyg"))

print(head(df_m1))

################################################################################
#Splaino interpoliacija (su natural versija)
################################################################################

data2 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
sheet2 <- "ORIG_KETV"

df_q2 <- read_excel(data2, sheet = sheet2) %>%
mutate(Date = as.yearqtr(Laikotarpis, format = "%YK%q")) %>% arrange(Date)

m_data2 <- function(df_q2) {
  df_m2 <- df_q2 %>%
  slice(rep(1:n(), each = 3)) %>%  
  mutate(
    Mėnesis = rep(c(1, 2, 3), times = nrow(df_q2)) + (as.numeric(format(Date, "%q")) - 1) * 3,
    Metai = as.numeric(format(Date, "%Y")),
    Date = as.Date(paste(Metai, Mėnesis, "01", sep = "-"))  
    ) %>%
    dplyr::select(Date, Metai, Mėnesis)
  return(df_m2)
}

df_m2 <- m_data2(df_q2)

exclude2 <- c("Metai", "Ketvirtis", "Laikotarpis", "Date")
rodikliai2 <- setdiff(colnames(df_q2), exclude2)

interp2 <- function(q_reiksmes2) {
  num_q2 <- length(q_reiksmes2)
  num_m2 <- num_q2 * 3  
  
q_ind2 <- seq(1, num_m2, by = 3)
m_ind2 <- seq(1, num_m2, by = 1)
  
interp_reiksmes2 <- spline(q_ind2, q_reiksmes2, xout = m_ind2, method = "natural")$y
  return(interp_reiksmes2)
}

#pagal interpoliacijos funkciją ją atlieku visiems rodikliams
for (rodiklis in rodikliai) {
  df_m2[[rodiklis]] <- interp2(df_q2[[rodiklis]])
}

df_q_viz2 <- df_q2 %>% mutate(Date = as.Date(paste(format(Date, "%Y"), (as.numeric(format(Date, "%q")) - 1) * 3 + 2, "01", sep = "-")))

plot2 <- function(var) {
  ggplot(na.omit(df_m2), aes(x = Date, y = .data[[var]], color = "Splaino interpoliacija (natural)")) +
  geom_line(size = 1) +
  geom_point(data = df_q_viz2, aes(x = Date, y = .data[[var]]), color = "black", size = 2, na.rm = TRUE) +
  scale_color_manual(values = c("Splaino interpoliacija (natural)" = "#78003F")) +
  labs(title = paste("Ketvirčio dažnio duomenų konvertavimas į mėnesinius"), x = "Metai", y = var, color = "Metodas") + theme_minimal()
}

print(plot2("BVP_I_palyg"))
print(plot2("BVP_vartojimas_palyg"))

print(head(df_m2))

################################################################################
#Denton-Cholette (be papildomo rodiklio)
################################################################################

data3 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
sheet3 <- "ORIG_KETV"

df_q3 <- read_excel(data3, sheet = sheet3) %>%
mutate(Date = as.yearqtr(Laikotarpis, format = "%YK%q")) %>% arrange(Date)

m_data3 <- function(df_q3) {
  df_m3 <- df_q3 %>%
  slice(rep(1:n(), each = 3)) %>%  
  mutate(
      Mėnesis = rep(c(1, 2, 3), times = nrow(df_q3)) + (as.numeric(format(Date, "%q")) - 1) * 3,
      Metai = as.numeric(format(Date, "%Y")),
      Date = as.Date(paste(Metai, Mėnesis, "01", sep = "-"))  
    ) %>%
    dplyr::select(Date, Metai, Mėnesis)
  return(df_m3)
}

df_m3 <- m_data3(df_q3)

exclude3 <- c("Metai", "Ketvirtis", "Laikotarpis", "Date")
rodikliai3 <- setdiff(colnames(df_q3), exclude3)

interp3 <- function(qtr_values) {
  ts_qtr <- ts(qtr_values, frequency = 4)  
  
td_model <- td(ts_qtr ~ 1, to = 12, method = "denton-cholette")  
interp_reiksmes3 <- as.numeric(predict(td_model))  
  return(interp_reiksmes3)
}

for (rodiklis in rodikliai) {
  df_m3[[rodiklis]] <- as.numeric(interp3(df_q3[[rodiklis]]))
}

df_q_viz3 <- df_q3 %>% mutate(Date = as.Date(paste(format(Date, "%Y"), (as.numeric(format(Date, "%q")) - 1) * 3 + 2, "01", sep = "-")))

plot3 <- function(var) {
  ggplot(df_m3, aes(x = Date, y = .data[[var]], color = "Denton-Chollete (be rodiklio)")) +
  geom_line(size = 1) +
  geom_point(data = df_q_viz3, aes(x = Date, y = .data[[var]]), color = "black", size = 2, na.rm = TRUE) +
  scale_color_manual(values = c("Denton-Chollete (be rodiklio)" = "#78003F")) +
  labs(title = paste("Ketvirčio dažnio duomenų konvertavimas į mėnesinius"), x = "Metai", y = var, color = "Metodas") + theme_minimal()
}

print(plot3("BVP_I_palyg"))
print(plot3("BVP_vartojimas_palyg"))

print(head(df_m3))

################################################################################
#Denton-Cholette (su papildomu rodikliu - VKI)
################################################################################

data4 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"

df_q4 <- read_excel(data4, sheet = "ORIG_KETV") %>%
mutate(Date = as.yearmon(as.yearqtr(Laikotarpis, format = "%YK%q"))) %>% arrange(Date)  

df_m4 <- read_excel(data4, sheet = "ORIG_MĖN") %>%
mutate(Date = as.yearmon(paste(Metai, Mėnesis, sep = "-"))) %>% arrange(Date)

exclude4 <- c("Metai", "Ketvirtis", "Laikotarpis", "Date")
rodikliai4 <- setdiff(colnames(df_q4), exclude4)

df_m_filtruota_4 <- df_m4 %>% filter(Date >= min(df_q4$Date) & Date <= max(df_q4$Date) + 2/12)

vki4 <- ts(as.numeric(df_m_filtruota_4$VKI), 
        start = c(year(min(df_m_filtruota_4$Date)), month(min(df_m_filtruota_4$Date))), frequency = 12)

#tinkama pabaiga (iki kurios naudojami duomenys)
pab_m4 <- year(max(df_q4$Date))
pab_q4 <- quarter(max(df_q4$Date))*3

if (pab_m4 > year(max(df_m_filtruota_4$Date))||pab_q4>month(max(df_m_filtruota_4$Date))) {
  pab_m4 <- year(max(df_m_filtruota_4$Date))
  pab_q4 <- month(max(df_m_filtruota_4$Date))}

vki4 <- window(vki4, start = c(year(min(df_q4$Date)), quarter(min(df_q4$Date)) * 3 - 2), 
        end = c(pab_m4, pab_q4))

df_m4 <- data.frame(Date = df_m_filtruota_4$Date)

for (rodiklis in rodikliai4) {
  data_q4 <- ts(as.numeric(df_q4[[rodiklis]]), start = c(year(min(df_q4$Date)), quarter(min(df_q4$Date))), 
            frequency = 4)
  
  data_q4 <- window(data_q4, start = c(year(min(df_q4$Date)), quarter(min(df_q4$Date))), 
            end = c(pab_m4, pab_q4))
  
interp4 <- td(data_q4 ~ 0 + vki4, method = "denton-cholette", conversion = "average") 
df_m4[[rodiklis]] <- as.numeric(predict(interp4))
}
print(head(df_m4))

plot4 <- function(var) {
  ggplot(df_m4, aes(x = Date, y = .data[[var]], color = "Denton-Cholette su rodikliu")) +
  geom_line(size = 1) +
  geom_point(data = df_q4, aes(x = as.yearmon(Date), y = .data[[var]]), color = "black", size = 2, na.rm = TRUE) +
  scale_color_manual(values = c("Denton-Cholette su rodikliu" = "#78003F")) +
  labs(title = paste("Ketvirčio dažnio duomenų konvertavimas į mėnesinius"), x = "Metai", y = var, color = "Metodas") + theme_minimal()
}

print(plot4("BVP_I_palyg"))
print(plot4("BVP_vartojimas_palyg"))

print(head(df_m4))

cor(df_m4$BVP_I_palyg, vki4, use = "complete.obs")
cor(df_m4$BVP_vartojimas_palyg, vki4, use = "complete.obs")

#šis metodas buvo pasirinktas kaip geriausias, todėl gauti duomenys išsaugomi 
#(kad juos galima būtų naudoti tolimesniam darbui, modeliavimui)
output_dir <- "C:/Users/Gabija/Desktop/Interpoliuoti_Duomenys1"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

for (rodiklis in rodikliai4) {
  file_name <- paste0(output_dir, "/", rodiklis, "_Interpoliuoti.xlsx")
df_save <- df_m4 %>% dplyr::select(Date, all_of(rodiklis))
write_xlsx(df_save, path = file_name)
}


################################################################################
#Chow-Lin (su papildomu rodikliu - VKI)
################################################################################

data5 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"

df_q5 <- read_excel(data5, sheet = "ORIG_KETV") %>%
mutate(Date = as.yearqtr(Laikotarpis, format = "%YK%q")) %>% arrange(Date)

df_m5 <- read_excel(data5, sheet = "ORIG_MĖN") %>%
mutate(Date = as.yearmon(paste(Metai, Mėnesis, sep = "-"))) %>% arrange(Date)

exclude5 <- c("Metai", "Ketvirtis", "Laikotarpis", "Date")
rodikliai5 <- setdiff(colnames(df_q5), exclude5)

df_m_filtruota_5 <- df_m5 %>% filter(Date >= min(as.yearmon(df_q5$Date)) & Date <= max(as.yearmon(df_q5$Date)) + 2/12) 

vki5 <- ts(as.numeric(df_m_filtruota_5$VKI), start = c(year(min(df_m_filtruota_5$Date)), 
        month(min(df_m_filtruota_5$Date))), frequency = 12)

vki5 <- window(vki5, start = c(year(min(df_q5$Date)), quarter(min(df_q5$Date)) * 3-2), 
        end = c(year(max(df_q5$Date)), quarter(max(df_q5$Date)) *3))

df_m5 <- data.frame(Date = df_m_filtruota_5$Date)

for (rodiklis in rodikliai5) {
  data_q5 <- ts(as.numeric(df_q5[[rodiklis]]), start = c(year(min(df_q5$Date)), quarter(min(df_q5$Date))), 
          frequency = 4)
  
  data_q5 <- window(data_q5, start = c(year(min(df_q5$Date)), quarter(min(df_q5$Date))), 
          end = c(year(max(df_q5$Date)), quarter(max(df_q5$Date))))
  
interp5 <- td(data_q5 ~ 1 + vki5, method = "chow-lin-maxlog", conversion = "average")
df_m5[[rodiklis]] <- as.numeric(predict(interp5))
}

plot5 <- function(var) {
  ggplot(df_m5, aes(x = Date, y = .data[[var]], color = "Chow-Lin su rodikliu")) +
  geom_line(size = 1) +
  geom_point(data = df_q5, aes(x = as.yearmon(Date), y = .data[[var]]), color = "black", size = 2, na.rm = TRUE) +
  scale_color_manual(values = c("Chow-Lin su rodikliu" = "#78003F")) +
  labs(title = paste("Ketvirčio dažnio duomenų konvertavimas į mėnesinius"), x = "Metai", y = var, color = "Metodas") + theme_minimal()
}

print(plot5("BVP_I_palyg"))
print(plot5("BVP_vartojimas_palyg"))

print(head(df_m5))

#VKI rodiklio (čia naudojamo kaip papildomo indikatoriaus) laiko eilutės stacionarumo 
#tikrinimas (sprendžiant dėl pasirinkimo tarp Chow-Lin su rodikliu ir 
#Denton-Cholette su rodikliu metodų)

adf <- ur.df(vki5, type="drift", lags=3)
summary(adf)

#gautas nestacionarumas, todėl renkuosi Denton-Cholette metodą

################################################################################
#Grafikai konvertavimo metodų rezultatų pavaizdavimui
################################################################################

#laiko eilutė ilga ir sunku pastebėti skirtumus tarp metodų vienu metu,
#todėl ją išskiriu į 4 dalis

#papildoma transformacija dėl laikotarpio formato suderinimo
df_m$Date <- as.Date(df_m$Date)
df_m1$Date <- as.Date(df_m1$Date)
df_m2$Date <- as.Date(df_m2$Date)
df_m4$Date <- as.Date(df_m4$Date)
df_m5$Date <- as.Date(df_m5$Date)
df_q_viz$Date <- as.Date(df_q_viz$Date)

#grafikų funkcijos skirtingiems laikotarpiams
plot_2005 <- function(var) {
  ggplot() +
  geom_line(data = df_m %>% filter(Date < as.Date("2005-01-01")), aes(x = Date, y = .data[[var]], color = "Linijinė interpolacija"), linetype = "solid", size = 1.2) +
  geom_line(data = df_m1 %>% filter(Date < as.Date("2005-01-01")), aes(x = Date, y = .data[[var]], color = "Spline (fmm) interpolacija"), linetype = "dotdash", size = 1.2) +
  geom_line(data = df_m2 %>% filter(Date < as.Date("2005-01-01")), aes(x = Date, y = .data[[var]], color = "Spline (natural) interpolacija"), linetype = "dotdash", size = 1.2) +
  #geom_line(data = df_m3 %>% filter(Date < as.Date("2005-01-01")), aes(x = Date, y = .data[[var]], color = "Denton-Cholette be rodiklio"), linetype = "dotdash", size = 1.2) +
  geom_line(data = df_m4 %>% filter(Date < as.Date("2005-01-01")), aes(x = Date, y = .data[[var]], color = "Denton-Cholette su rodikliu"), linetype = "solid", size = 1.2) +
  geom_line(data = df_m5 %>% filter(Date < as.Date("2005-01-01")), aes(x = Date, y = .data[[var]], color = "Chow-Lin su rodikliu"), linetype = "solid", size = 1.2) +
  geom_point(data = df_q_viz %>% filter(Date < as.Date("2005-01-01")), aes(x = Date, y = .data[[var]]), color = "black", size = 2) +
  geom_line(data = df_q_viz %>% filter(Date < as.Date("2005-01-01")), aes(x = Date, y = .data[[var]]), color = "black", linetype = "solid", size = 1.2) +
  scale_color_manual(values = c("Linijinė interpolacija" = "#78003F", "Spline (fmm) interpolacija" = "#414141", "Spline (natural) interpolacija" = "#E64164", "Denton-Cholette be rodiklio" = "#78003F", "Denton-Cholette su rodikliu" = "#414141", "Chow-Lin su rodikliu" = "#E64164")) +
  labs(title = paste("Ketvirčio rodiklio duomenų konvertavimas į mėnesinius (iki 2005m.)"), x = "Data", y = var) + theme_minimal()
}

plot_2005_2010 <- function(var) {
  ggplot() +
  geom_line(data = df_m %>% filter(Date >= as.Date("2005-01-01") & Date < as.Date("2010-01-01")), aes(x = Date, y = .data[[var]], color = "Linijinė interpolacija"), linetype = "solid", size = 1.2) +
  geom_line(data = df_m1 %>% filter(Date >= as.Date("2005-01-01") & Date < as.Date("2010-01-01")), aes(x = Date, y = .data[[var]], color = "Spline (fmm) interpolacija"), linetype = "dotdash", size = 1.2) +
  geom_line(data = df_m2 %>% filter(Date >= as.Date("2005-01-01") & Date < as.Date("2010-01-01")), aes(x = Date, y = .data[[var]], color = "Spline (natural) interpolacija"), linetype = "dotdash", size = 1.2) +
  #geom_line(data = df_m3 %>% filter(Date >= as.Date("2005-01-01") & Date < as.Date("2010-01-01")), aes(x = Date, y = .data[[var]], color = "Denton-Cholette be rodiklio"), linetype = "dotdash", size = 1.2) +
  geom_line(data = df_m4 %>% filter(Date >= as.Date("2005-01-01") & Date < as.Date("2010-01-01")), aes(x = Date, y = .data[[var]], color = "Denton-Cholette su rodikliu"), linetype = "solid", size = 1.2) +
  geom_line(data = df_m5 %>% filter(Date >= as.Date("2005-01-01") & Date < as.Date("2010-01-01")), aes(x = Date, y = .data[[var]], color = "Chow-Lin su rodikliu"), linetype = "solid", size = 1.2) +
  geom_point(data = df_q_viz %>% filter(Date >= as.Date("2005-01-01") & Date < as.Date("2010-01-01")), aes(x = Date, y = .data[[var]]), color = "black", size = 2) +
  geom_line(data = df_q_viz %>% filter(Date >= as.Date("2005-01-01") & Date < as.Date("2010-01-01")), aes(x = Date, y = .data[[var]]), color = "black", linetype = "solid", size = 1.2) +
  scale_color_manual(values = c("Linijinė interpolacija" = "#78003F", "Spline (fmm) interpolacija" = "#414141", "Spline (natural) interpolacija" = "#E64164", "Denton-Cholette be rodiklio" = "#78003F", "Denton-Cholette su rodikliu" = "#414141", "Chow-Lin su rodikliu" = "#E64164")) +
  labs(title = paste("Ketvirčio rodiklio duomenų konvertavimas į mėnesinius (2005-2010m.)"), x = "Data", y = var) + theme_minimal()
}

plot_2010_2015 <- function(var) {
  ggplot() +
  geom_line(data = df_m %>% filter(Date >= as.Date("2010-01-01") & Date < as.Date("2015-01-01")), aes(x = Date, y = .data[[var]], color = "Linijinė interpolacija"), linetype = "solid", size = 1.2) +
  geom_line(data = df_m1 %>% filter(Date >= as.Date("2010-01-01") & Date < as.Date("2015-01-01")), aes(x = Date, y = .data[[var]], color = "Spline (fmm) interpolacija"), linetype = "dotdash", size = 1.2) +
  geom_line(data = df_m2 %>% filter(Date >= as.Date("2010-01-01") & Date < as.Date("2015-01-01")), aes(x = Date, y = .data[[var]], color = "Spline (natural) interpolacija"), linetype = "dotdash", size = 1.2) +
  #geom_line(data = df_m3 %>% filter(Date >= as.Date("2010-01-01") & Date < as.Date("2015-01-01")), aes(x = Date, y = .data[[var]], color = "Denton-Cholette be rodiklio"), linetype = "dotdash", size = 1.2) +
  geom_line(data = df_m4 %>% filter(Date >= as.Date("2010-01-01") & Date < as.Date("2015-01-01")), aes(x = Date, y = .data[[var]], color = "Denton-Cholette su rodikliu"), linetype = "solid", size = 1.2) +
  geom_line(data = df_m5 %>% filter(Date >= as.Date("2010-01-01") & Date < as.Date("2015-01-01")), aes(x = Date, y = .data[[var]], color = "Chow-Lin su rodikliu"), linetype = "solid", size = 1.2) +
  geom_point(data = df_q_viz %>% filter(Date >= as.Date("2010-01-01") & Date < as.Date("2015-01-01")), aes(x = Date, y = .data[[var]]), color = "black", size = 2) +
  geom_line(data = df_q_viz %>% filter(Date >= as.Date("2010-01-01") & Date < as.Date("2015-01-01")), aes(x = Date, y = .data[[var]]), color = "black", linetype = "solid", size = 1.2) +
  scale_color_manual(values = c("Linijinė interpolacija" = "#78003F", "Spline (fmm) interpolacija" = "#414141", "Spline (natural) interpolacija" = "#E64164", "Denton-Cholette be rodiklio" = "#78003F", "Denton-Cholette su rodikliu" = "#414141", "Chow-Lin su rodikliu" = "#E64164")) +
  labs(title = paste("Ketvirčio rodiklio duomenų konvertavimas į mėnesinius (2010-2015m.)"), x = "Data", y = var) + theme_minimal()
}

plot_2015 <- function(var) {
  ggplot() +
  geom_line(data = df_m %>% filter(Date > as.Date("2015-01-01")), aes(x = Date, y = .data[[var]], color = "Linijinė interpolacija"), linetype = "solid", size = 1.2) +
  geom_line(data = df_m1 %>% filter(Date > as.Date("2015-01-01")), aes(x = Date, y = .data[[var]], color = "Spline (fmm) interpolacija"), linetype = "dotdash", size = 1.2) +
  geom_line(data = df_m2 %>% filter(Date > as.Date("2015-01-01")), aes(x = Date, y = .data[[var]], color = "Spline (natural) interpolacija"), linetype = "dotdash", size = 1.2) +
  #geom_line(data = df_m3 %>% filter(Date > as.Date("2015-01-01")), aes(x = Date, y = .data[[var]], color = "Denton-Cholette be rodiklio"), linetype = "dotdash", size = 1.2) +
  geom_line(data = df_m4 %>% filter(Date > as.Date("2015-01-01")), aes(x = Date, y = .data[[var]], color = "Denton-Cholette su rodikliu"), linetype = "solid", size = 1.2) +
  geom_line(data = df_m5 %>% filter(Date > as.Date("2015-01-01")), aes(x = Date, y = .data[[var]], color = "Chow-Lin su rodikliu"), linetype = "solid", size = 1.2) +
  geom_point(data = df_q_viz %>% filter(Date > as.Date("2015-01-01")), aes(x = Date, y = .data[[var]]), color = "black", size = 2) +
  geom_line(data = df_q_viz %>% filter(Date > as.Date("2015-01-01")), aes(x = Date, y = .data[[var]]), color = "black", linetype = "solid", size = 1.2) +
  scale_color_manual(values = c("Linijinė interpolacija" = "#78003F", "Spline (fmm) interpolacija" = "#414141", "Spline (natural) interpolacija" = "#E64164", "Denton-Cholette be rodiklio" = "#78003F", "Denton-Cholette su rodikliu" = "#414141", "Chow-Lin su rodikliu" = "#E64164")) +
  labs(title = paste("Ketvirčio rodiklio duomenų konvertavimas į mėnesinius (nuo 2015m.)"), x = "Data", y = var) + theme_minimal()
}

#rodikliai
#BVP_I_palyg	BVP_vartojimas_palyg
var <- "BVP_I_palyg"

print(plot_2005(rodiklis))
print(plot_2005_2010(rodiklis))
print(plot_2010_2015(rodiklis))
print(plot_2015(rodiklis))

#bendras grafikas visiems laikotarpiams
plot6 <- function(var) {
  ggplot() +
  geom_line(data = df_m, aes(x = Date, y = .data[[var]], color = "Linijinė interpolacija"), linetype = "solid", size = 1.2) +
  geom_line(data = df_m1, aes(x = Date, y = .data[[var]], color = "Spline FMM interpolacija"), linetype = "dashed", size = 1.2) +
  geom_line(data = df_m2, aes(x = Date, y = .data[[var]], color = "Spline Natural interpolacija"), linetype = "dotted", size = 1.2) +
  #geom_line(data = df_m3, aes(x = Date, y = .data[[var]], color = "Denton be rodiklio"), linetype = "dotdash", size = 1.2) +
  geom_line(data = df_m4, aes(x = Date, y = .data[[var]], color = "Denton su rodikliu"), linetype = "longdash", size = 1.2) +
  geom_line(data = df_m5, aes(x = Date, y = .data[[var]], color = "Chow-Lin su rodikliu"), linetype = "twodash", size = 1.2) +
  geom_point(data = df_q_viz, aes(x = Date, y = .data[[var]]), color = "black", size = 2) +
  geom_line(data = df_q_viz, aes(x = Date, y = .data[[var]]), color = "black", linetype = "solid", size = 1.2) +
  scale_color_manual(values = c("Linijinė interpolacija" = "#78003F", "Spline FMM interpolacija" = "#414141", "Denton be rodiklio" = "#E64164", "Spline Natural interpolacija" = "#E64164", "Denton su rodikliu" = "#414141", "Chow-Lin su rodikliu" = "#E64164")) +
  labs(title = paste("ketvirčio rodiklio konvertavimas į mėnesinius (visi laikotarpiai)"), x = "Data", y = var) + theme_minimal()
}

var <- "BVP_I_palyg"
print(plot6(var))

################################################################################
#Galutiniai grafikai geriausio metodo (Denton-Cholette) rezultatams
################################################################################

data7 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"

df_q7 <- read_excel(data7, sheet = "ORIG_KETV") %>%
  mutate(Date = as.yearqtr(Laikotarpis, format = "%YK%q")) %>% arrange(Date)

df_m7 <- read_excel(data7, sheet = "KOREG_KETV") %>%
  mutate(Date = as.yearmon(Laikotarpis, format = "%YM%m")) %>% arrange(Date)

plot7 <- function(var) {
  ggplot() +
  geom_point(data = df_q7, aes(x = Date, y = .data[[var]]), color = "black", size = 1) +
  geom_line(data = df_q7, aes(x = Date, y = .data[[var]]), color = "black", linetype = "dashed", size = 0.7) +
  geom_line(data = df_m7, aes(x = Date, y = .data[[var]]), color = "#78003F", size = 0.7) +
  labs(title = paste(var, "ketvirtiniai ir mėnesiniai duomenys"), x = "Data", y = var) + theme_minimal()
}

print(plot7("BVP_I_palyg"))
print(plot7("BVP_vartojimas_palyg"))



#galutinis grafikas (panelė)

data7 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"

df_q7 <- read_excel(data7, sheet = "ORIG_KETV") %>%
  mutate(Date = as.yearqtr(Laikotarpis, format = "%YK%q")) %>% arrange(Date)

df_m7 <- read_excel(data7, sheet = "KOREG_KETV") %>%
  mutate(Date = as.yearmon(Laikotarpis, format = "%YM%m")) %>% arrange(Date)

#konvertuoju y ašies reikšmes į milijardus (dėl vizualizacijos grožio)
df_q7 <- df_q7 %>%
  mutate(across(c("BVP_I_palyg", "BVP_vartojimas_palyg"), ~.x/1000))

df_m7 <- df_m7 %>%
  mutate(across(c("BVP_I_palyg", "BVP_vartojimas_palyg"), ~.x/1000))

grafikas_virs <- function(var, y_label) {
  max_y <- max(df_m7[[var]], na.rm = TRUE)
  min_y <- min(df_m7[[var]], na.rm = TRUE)
  ylim_top <- ceiling(max_y) + 0.5
  ylim_bottom <- floor(min_y) - 0.5
  
  ggplot() +
    geom_point(data = df_q7, aes(x = as.Date(Date), y = .data[[var]], color = "Ketvirčio duomenys"), size = 1.5) +
    geom_line(data = df_m7, aes(x = as.Date(Date), y = .data[[var]], color = "Mėnesio duomenys"), size = 1.2) +
    scale_color_manual(values = c("Ketvirčio duomenys" = "black", "Mėnesio duomenys" = "#78003F")) +
    scale_x_date(
    breaks = seq(as.Date("2004-01-01"), as.Date("2025-01-01"), by = "4 years"),
    labels = function(x) format(x, "%Y")) +
    scale_y_continuous(
      limits = c(ylim_bottom, ylim_top),
      expand = c(0, 0),
      labels = scales::label_number(decimal_mark = ",", accuracy = 0.1)
    ) +
    labs(x = NULL, y = y_label, color = NULL) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray80"),
      axis.text.x = element_blank(),      
      axis.title.x = element_blank(),     
      legend.position = "bottom",
      plot.title = element_blank())}

grafikas_ap <- function(var, y_label) {
  max_y <- max(df_m7[[var]], na.rm = TRUE)
  min_y <- min(df_m7[[var]], na.rm = TRUE)
  ylim_top <- ceiling(max_y) + 0.5
  ylim_bottom <- floor(min_y) - 0.5
  
    ggplot() +
    geom_point(data = df_q7, aes(x = as.Date(Date), y = .data[[var]], color = "Ketvirčio duomenys"), size = 1.5) +
    geom_line(data = df_m7, aes(x = as.Date(Date), y = .data[[var]], color = "Mėnesio duomenys"), size = 1.2) +
    scale_color_manual(values = c("Ketvirčio duomenys" = "black", "Mėnesio duomenys" = "#78003F")) +
    scale_x_date(
    breaks = seq(as.Date("2004-01-01"), as.Date("2025-01-01"), by = "4 years"),
    labels = function(x) format(x, "%Y")) +
    labs(x = "Metai", y = y_label, color = NULL) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray80"),
      legend.position = "bottom",
      axis.text.x = element_text(hjust = 0.5),
      plot.title = element_blank())}

gr1 <- grafikas_virs("BVP_I_palyg", "BVP (mlrd. eur.)")
gr2 <- grafikas_ap("BVP_vartojimas_palyg", "Vartojimas (mlrd. eur.)")

panele <- gr1/gr2 +
  plot_layout(guides = "collect", heights = c(1, 1)) &
  theme(legend.position = "bottom", panel.background = element_rect(fill = "white", color = NA),  
        plot.background = element_rect(fill = "white", color = NA),)

panele1 <- panele + plot_annotation(
  title = "Rodiklių ketvirčio ir mėnesio duomenys",
  theme = theme(
    plot.title = element_text(hjust = 0.5, size = 22),
    plot.background = element_rect(color = "gray80", fill = NA, linewidth = 0.8) 
  )
)

print(panele1)

#svarbu tam, kad neišsikraipytų grafiko elementų dydžiai, pozicijos
ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/Q_i_M.png", panele1,
       width = 10, height = 6.5, dpi = 300, units = "in")
