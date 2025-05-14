################################################################################
################################################################################
#1 modelis - be BVP komponenčių, be rodiklių sąveikų

#šiame faile iš naujo aprašau geriausią modelį, išbrėžiu vizualizacijas
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

rodikliai1_1 <- rodikliai1_1 %>% mutate(Krizė = ifelse(Metai %in% c(2008, 2009, 2020, 2021), 1, 0))

train <- rodikliai1_1 %>% filter(Metai <= 2017) %>% na.omit()
test <- rodikliai1_1 %>% filter(Metai > 2017) %>% na.omit()

################################################################################
modelis1_1 <- lm(BVP_I_palyg ~ . -Metai -BVP_vartojimas_palyg -BVP_I_palyg_orig
                 -Taupymas -Gimstamumas -Mirštamumas -Imigrantai -Nedarbas_bendras
                 -Gyventojai -Paslaugų_pasitikėjimas -Emigrantai
                 -Krizė, data = train)
summary(modelis1_1)
################################################################################
train$likuciai <- resid(modelis1_1)
train$pritaikyta <- fitted(modelis1_1)

a111 <- ggplot(train, aes(x = pritaikyta, y = likuciai)) +
  geom_point(color = "#78003F", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
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
  ) +
  labs(title = "Treniravimo imties liekanos ir prognozuotos reikšmės",
       x = "Prognozuotos reikšmės",
       y = "Liekanos")

a111

ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/geriausio_modelio_liekanos_ir_progn_reiksmesTRAIN.png", a111,
       width = 10, height = 6.5, dpi = 300, units = "in")


a112 <- ggplot(train, aes(x = likuciai)) +
  geom_histogram(fill = "#78003F", color = "white", bins = 30) +
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
  ) +
  labs(title = "Treniravimo imties liekanų pasiskirstymo histograma",
       x = "Liekanos",
       y = "Dažnis")

a112

ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/geriausio_modelio_liekanu_pasiskirstymasTRAIN.png", a112,
       width = 10, height = 6.5, dpi = 300, units = "in")


qqnorm(train$likuciai, col = "#78003F")
qqline(train$likuciai, col = "black", lwd = 2)

################################################################################
#modelio pritaikymas test imčiai 
test$prognozuota <- predict(modelis1_1, test)
test$likuciai <- test$BVP_I_palyg - test$prognozuota

a113 <- ggplot(test, aes(x = prognozuota, y = likuciai)) +
  geom_point(color = "#78003F", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
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
  ) +
  labs(title = "Testavimo imties liekanos ir prognozuotos reikšmės",
       x = "Prognozuotos reikšmės",
       y = "Liekanos")


a113

ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/geriausio_modelio_liekanos_ir_progn_reiksmesTEST.png", a113,
       width = 10, height = 6.5, dpi = 300, units = "in")


a114 <- ggplot(test, aes(x = likuciai)) +
  geom_histogram(fill = "#78003F", color = "white", bins = 30) +
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
  ) +
  labs(title = "Testavimo imties liekanų pasiskirstymo histograma",
       x = "Liekanos",
       y = "Dažnis")

a114

ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/geriausio_modelio_liekanu_pasiskirstymasTEST.png", a114,
       width = 10, height = 6.5, dpi = 300, units = "in")

qqnorm(test$likuciai, col = "#78003F")
qqline(test$likuciai, col = "black", lwd = 2)

################################################################################
#geriausio modelio prognozių vizualizacija
n_prognoziu <- nrow(test)
prog_datos <- seq(as.Date("2018-01-01"), by = "month", length.out = n_prognoziu)

test$Data  <- prog_datos

bvp_pradine1_1 <- rodikliai1_1_pries %>%
  filter(Metai == 2017) %>% tail(1) %>% pull(BVP_I_palyg)

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
prognozes3 <- X_test3 %*% coef(modelis3)

prognozes_diff <- data.frame(
  Data = prog_datos,
  Modelis1_1 = predict(modelis1_1, newdata = test))

prognozes_lygio <- prognozes_diff %>%
  mutate(
    Modelis1_1 = atdiferen(Modelis1_1, bvp_pradine1_1))

prognozes_long <- prognozes_lygio %>%
  pivot_longer(cols = starts_with("Modelis"), names_to = "Tipas", values_to = "BVP_I_palyg")

rodikliai1_1_pries$Data <- seq(as.Date("2002-07-01"), by = "month", length.out = nrow(rodikliai1_1_pries))

originalus <- rodikliai1_1_pries %>%
  select(Data, BVP_I_palyg) %>% mutate(Tipas = "Reali reikšmė")

grafiko_duomenys <- bind_rows(originalus, prognozes_long)

grafiko_duomenys$Tipas <- factor(grafiko_duomenys$Tipas,
                                 levels = c("Reali reikšmė", "Modelis1_1"),
                                 labels = c("Reali reikšmė", "Modelio prognozės"))

prog <- ggplot(grafiko_duomenys, aes(x = Data, y = BVP_I_palyg / 1000, color = Tipas, group = Tipas)) +  
  geom_line(size = 1.3) +
  geom_vline(xintercept = as.Date("2017-12-01"), linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("2018-01-01"),
           y = max(grafiko_duomenys$BVP_I_palyg, na.rm = TRUE) / 1000,
           label = "Prasideda prognozės", hjust = 0, vjust = 1.2, color = "black", size = 5) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1, big.mark = ","),
                     name = "BVP (mlrd. eur.)",
                     breaks = seq(5, 20, by = 2)) +
  labs(title = "Pradinio tiesinės regresijos modelio prognozės",
       x = "Metai", y = NULL, color = NULL, linetype = NULL) +
  scale_color_manual(values = c(
    "Reali reikšmė" = "black", "Modelio prognozės" = "#E64164")) +
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


ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/geriausias_modelis_pro.png", prog,
       width = 10, height = 6.5, dpi = 300, units = "in")
