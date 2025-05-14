#R versijos patikrinimas (reikalinga darbo įžangoje)
R.version.string

################################################################################
#2 žingsnis - duomenų tendencijų grafikai 
#sezoniškumo šalinimas sujungtiems duomenims
#koreliacija tarp rodiklių
################################################################################

library(ggplot2)
library(readxl)
library(dplyr)
library(zoo)  
library(ggpubr)
library(tidyr)
library(gridExtra)
library(lubridate)
library(seasonal)
library(ggpubr)
library(ggcorrplot)

##########################################################################
#Pirminių (prieš modeliavimą) rodiklių grafikai: mėnesiniai
#rodikliai + konvertuoti ketvirčio rodikliai
##########################################################################
#duomenys
data <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"

df_q <- read_excel(data, sheet = "ORIG_KETV") %>%
  mutate(Date = as.Date(as.yearqtr(Laikotarpis, format = "%YK%q"))) %>% arrange(Date)

df_koreg <- read_excel(data, sheet = "KOREG_KETV") %>%
  mutate(Date = as.Date(as.yearmon(Laikotarpis, format = "%YM%m"))) %>% arrange(Date)

df_m <- read_excel(data, sheet = "ORIG_MĖN") %>%
  mutate(Date = as.Date(as.yearmon(paste(Metai, Mėnesis, sep = "-")))) %>% arrange(Date)

#pilni rodiklių pavadinimai (grafikų pavadinimų tikslumui)
pavadinimai <- list(
  "BVP_I_palyg" = "BVP", "BVP_vartojimas_palyg" = "Vartojimas",
  "Imigrantai" = "Imigrantai", "Emigrantai" = "Emigrantai", 
  "Taupymas" = "Taupymas", "Nedarbas_bendras" = "Nedarbas",
  "Paslaugų_pasitikėjimas" = "Paslaugų pasitikėjimas", "Vartotojų_pasitikėjimas" = "Vartotojų pasitikėjimas",
  "Mirštamumas" = "Mirtingumas", "Gimstamumas" = "Gimstamumas", "Gyventojai" = "Gyventojai", "VKI" = "VKI"
)

#matavimo vienetai (y ašiai)
vnt <- list(
  "BVP_I_palyg" = "mln. eur.", "BVP_vartojimas_palyg" = "mln. Eur",
  "Imigrantai" = "Asmenys", "Emigrantai" = "Asmenys", 
  "Nedarbas_bendras" = "Asmenys\n(tūkst.)", "Paslaugų_pasitikėjimas" = "Balai", "Taupymas" = "Procentai\n(%)",
  "Vartotojų_pasitikėjimas" = "Balai", "Mirštamumas" = "Asmenys", 
  "Gimstamumas" = "Asmenys", "Gyventojai" = "Asmenys", "VKI" = "Indeksas"
)

#mėnesinių rodiklių grafikų funkcija
men <- function(rodiklis) {
  ggplot(df_m, aes(x = Date, y = .data[[rodiklis]])) +
    geom_line(color = "#78003F", size = 1.15) +
    labs(title = pavadinimai[[rodiklis]], y = vnt[[rodiklis]]) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(size = 18), 
      axis.title.y = element_text(size = 15), 
      panel.grid.major.y = element_line(color = "gray80"),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.y = element_text(size = 11),
      panel.background = element_rect(fill = "white", color = NA),  
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "none"
    )
}

#ketvirčio rodiklių grafikikų funkcija
ketv <- function(rodiklis) {
  ggplot() +
    geom_line(data = df_q, aes(x = Date, y = .data[[rodiklis]], color = "Ketvirtiniai"), size = 1.1) +
    geom_line(data = df_koreg, aes(x = Date, y = .data[[rodiklis]], color = "Konvertuoti mėnesiniai"), size = 1.1) +
    scale_color_manual(values = c("Ketvirtiniai" = "#414141", "Konvertuoti mėnesiniai" = "#78003F")) +
    labs(title = pavadinimai[[rodiklis]], y = vnt[[rodiklis]], color = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15), 
      axis.title.y = element_text(size = 10),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray80"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.y = element_text(size = 10),
      panel.background = element_rect(fill = "white", color = NA),  
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "none"
    )
}

#funkcija panelėms 
pan <- function(grafikai, pavadinimas) {
  grafikai_mod <- grafikai
  for (i in 1:(length(grafikai_mod) - 1)) {
    grafikai_mod[[i]] <- grafikai_mod[[i]] +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA))
  }
  
  p <- ggarrange(plotlist = grafikai_mod, ncol = 1, nrow = length(grafikai_mod),
                 common.legend = TRUE, legend = "bottom")
  
  annotate_figure(
    p,
    top = text_grob(pavadinimas, size = 22),
    bottom = text_grob("Metai", size = 16),
    fig.lab = NULL,
    fig.lab.face = NULL
  ) +
    theme(
      plot.background = element_rect(fill = "white", color = "gray80", linewidth = 0.8),
      plot.margin = margin(15, 15, 15, 15, unit = "pt")  
    )
}

pan(list(ketv("BVP_I_palyg"), ketv("BVP_vartojimas_palyg")), "Konvertuoti ketvirčio dažnio duomenys")

a1 <- pan(list(men("Imigrantai"), men("Emigrantai"), men("Taupymas"), 
    men("Nedarbas_bendras")),"Mėnesinių rodiklių laiko eilutės")
a1

a2 <- pan(list(men("Paslaugų_pasitikėjimas"), men("Vartotojų_pasitikėjimas"),
  men("Mirštamumas")), "Mėnesinių rodiklių laiko eilutės")
a2

a3 <- pan(list(men("Gimstamumas"), men("Gyventojai"), men("VKI")), 
    "Mėnesinių rodiklių laiko eilutės")
a3

ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/pasiskirst1.png", a1,
       width = 10, height = 6.5, dpi = 300, units = "in")

ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/pasiskirst2.png", a2,
       width = 10, height = 6.5, dpi = 300, units = "in")

ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/pasiskirst3.png", a3,
       width = 10, height = 6.5, dpi = 300, units = "in")

##########################################################################
#Sezoniškumo šalinimas (prieš modeliavimą) iš galutinių (sujungtų) duomenų

#Išspausdinu grafikų paneles, kuriose matosi, kurie rodikliai ir kokio
#dydžio turi sezoniškumą
##########################################################################

data2 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"

df2 <- read_excel(data2, sheet = "SUJUNGTA") %>%
  mutate(Date = as.Date(paste(Metai, Mėnesis, "01", sep = "-"), format="%Y-%m-%d")) %>% arrange(Date)

#konvertavimas į mlrd
df2 <- df2 %>%
  mutate(
    BVP_I_palyg = BVP_I_palyg/1000,
    BVP_vartojimas_palyg = BVP_vartojimas_palyg/1000)

#testavimas
print(colnames(df2))

#perteklinių rodiklių šalinimas (tų, kurie nereikalingi sezoniškumo šalinimui)
exclude2 <- c("Metai", "Mėnesis", "Laikotarpis", "Date")
rodikliai2 <- setdiff(colnames(df2), exclude2)

#sezoniškumo komponentės šalinimo (naudoju dekompoziciją) funkcija
sez <- function(data2, rod){
  ts_data <- ts(data2[[rod]], start = c(year(min(data2$Date, na.rm=TRUE)),
                                        month(min(data2$Date, na.rm=TRUE))), frequency = 12)
  decomposed <- stl(ts_data, s.window = "periodic")
  
  data2[[paste0(rod, "_sez")]] <- as.numeric(decomposed$time.series[, "seasonal"])
  data2[[paste0(rod, "_galutiniai")]] <- as.numeric(decomposed$time.series[, "trend"] + 
                                                      decomposed$time.series[, "remainder"])
  return(data2)}

#šalinimas visiems rodikliams
for (rod in rodikliai2) {
  df2 <- sez(df2, rod) 
}

print(df2)

#testavimas
print(colnames(df2))

plot_sez <- function(data2, rod){
  pavadinimai <- list(
    "BVP_I_palyg" = "BVP", "BVP_vartojimas_palyg" = "Vartojimas",
    "Imigrantai" = "Imigrantai", "Emigrantai" = "Emigrantai", 
    "Taupymas" = "Taupymas", "Nedarbas_bendras" = "Nedarbas",
    "Paslaugų_pasitikėjimas" = "Paslaugų pasitikėjimas", "Vartotojų_pasitikėjimas" = "Vartotojų pasitikėjimas",
    "Mirštamumas" = "Mirtingumas", "Gimstamumas" = "Gimstamumas", "Gyventojai" = "Gyventojai", "VKI" = "VKI"
  )
  
  vnt <- list(
    "BVP_I_palyg" = "mlrd. eur.", "BVP_vartojimas_palyg" = "mlrd. eur",
    "Imigrantai" = "Asmenys", "Emigrantai" = "Asmenys", 
    "Nedarbas_bendras" = "Asmenys (tūkst.)", "Paslaugų_pasitikėjimas" = "Balai", "Taupymas" = "Procentai (%)",
    "Vartotojų_pasitikėjimas" = "Balai", "Mirštamumas" = "Asmenys", 
    "Gimstamumas" = "Asmenys", "Gyventojai" = "Asmenys", "VKI" = "Indeksas"
  )
  
  rod_pav <- pavadinimai[[rod]]
  y_vnt <- vnt[[rod]]
  
  theme <- theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(color = "#00000022"),
      panel.background = element_rect(fill = "white", color = NA),  
      plot.background = element_rect(fill = "white", color = NA),    
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      text = element_text(size = 16)
    )
  
  plot1 <- ggplot(data2, aes(x = Date)) +
    geom_line(aes(y = .data[[rod]]), color = "#414141", size = 1) +
    labs(title = paste0(rod_pav, " – originalūs duomenys"), y = y_vnt) + theme
  
  plot2 <- ggplot(data2, aes(x = Date)) +
    geom_line(aes(y = .data[[paste0(rod, "_sez")]]), color = "#E64164", size = 1) +
    labs(title = paste0(rod_pav, " – sezoniniškumo komponentė"), y = y_vnt) + theme
  
  plot3 <- ggplot(data2, aes(x = Date)) +
    geom_line(aes(y = .data[[paste0(rod, "_galutiniai")]]), color = "#78003F", size = 1) +
    labs(title = paste0(rod_pav, " – po sezoniniškumo šalinimo"), x = "Metai", y = y_vnt) +
    theme() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks = element_blank(),  
      axis.line = element_blank(),   
      panel.background = element_rect(fill = "white", color = NA),  
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major.y = element_line(color = "gray80"),
      axis.title.x = element_text(),
      axis.text.x = element_text(),
      text = element_text(size = 16)
    ) +
    scale_x_date(date_breaks = "4 years", date_labels = "%Y")
  
  pan2 <- ggarrange(plot1, plot2, plot3, ncol = 1, nrow = 3,
                    common.legend = TRUE, legend = "bottom", align = "v") +
    theme(
      plot.background = element_rect(fill = "white", color = "gray80", linewidth = 0.8),
      plot.margin = margin(10, 10, 10, 10, unit = "pt")
    )
  return(pan2)
}

print(plot_sez(df2, "BVP_I_palyg"))
print(plot_sez(df2, "BVP_vartojimas_palyg"))
print(plot_sez(df2, "Imigrantai"))
print(plot_sez(df2, "Emigrantai"))
print(plot_sez(df2, "Taupymas"))
print(plot_sez(df2, "Nedarbas_bendras"))
print(plot_sez(df2, "Paslaugų_pasitikėjimas"))
print(plot_sez(df2, "Vartotojų_pasitikėjimas"))
print(plot_sez(df2, "Mirštamumas"))
print(plot_sez(df2, "Gimstamumas"))
print(plot_sez(df2, "Gyventojai"))

ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/bvp_sez.png", plot_sez(df2, "BVP_I_palyg"),
       width = 10, height = 6.5, dpi = 300, units = "in")
ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/vart_sez.png", plot_sez(df2, "BVP_vartojimas_palyg"),
       width = 10, height = 6.5, dpi = 300, units = "in")
ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/imig_sez.png", plot_sez(df2, "Imigrantai"),
       width = 10, height = 6.5, dpi = 300, units = "in")
ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/emig_sez.png", plot_sez(df2, "Emigrantai"),
       width = 10, height = 6.5, dpi = 300, units = "in")
ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/ned_sez.png", plot_sez(df2, "Nedarbas_bendras"),
       width = 10, height = 6.5, dpi = 300, units = "in")
ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/paslp_sez.png", plot_sez(df2, "Paslaugų_pasitikėjimas"),
       width = 10, height = 6.5, dpi = 300, units = "in")
ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/vartp_sez.png", plot_sez(df2, "Vartotojų_pasitikėjimas"),
       width = 10, height = 6.5, dpi = 300, units = "in")
ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/mirst_sez.png", plot_sez(df2, "Mirštamumas"),
       width = 10, height = 6.5, dpi = 300, units = "in")
ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/gimst_sez.png", plot_sez(df2, "Gimstamumas"),
       width = 10, height = 6.5, dpi = 300, units = "in")
ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/gyven_sez.png", plot_sez(df2, "Gyventojai"),
       width = 10, height = 6.5, dpi = 300, units = "in")
ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/taup_sez.png", plot_sez(df2, "Taupymas"),
       width = 10, height = 6.5, dpi = 300, units = "in")


#panelių kūrimas (tikriausiai nebus naudojama toliau, nes sunku įžiūrėti rezultatus)
rodikliai <- c(
  "BVP_I_palyg", "BVP_vartojimas_palyg", "Imigrantai", "Emigrantai",
  "Taupymas", "Nedarbas_bendras", "Paslaugų_pasitikėjimas", "Vartotojų_pasitikėjimas",
  "Mirštamumas", "Gimstamumas", "Gyventojai"
)

grafiku_sarasas <- lapply(rodikliai, function(r) plot_sez(df2, r))

grupe1 <- grafiku_sarasas[1:4]
grupe2 <- grafiku_sarasas[5:8]
grupe3 <- grafiku_sarasas[9:11]

panele1 <- ggarrange(plotlist = grupe1, ncol = 2, nrow = 2)
panele2 <- ggarrange(plotlist = grupe2, ncol = 2, nrow = 2)
panele3 <- ggarrange(plotlist = grupe3, ncol = 1, nrow = 3)

print(panele1)
print(panele2)
print(panele3)


#prieš duomenų pakeitimą iš mln į mlrd išsaugojau duomenis (kad skalė sutaptų su kitais rodikliais)
df_galutiniai <- df2 %>% dplyr::select(Date, ends_with("_galutiniai"))
write.csv(df_galutiniai, "C:/Users/Gabija/Desktop/duomenys_be_sezoniskumo1.csv", row.names = FALSE)

##########################################################################
#Koreliacijos tikrinimas tarp y (BVP_I_palyg) ir kitų rodiklių

#Testavimo tikslais tikrinu Pearson ir Spearman koreliaciją, bet remiuosi
#Spearman koreliacijos rezultatais, nes nėra normalaus pasiskirstymo, yra išskirčių
##########################################################################
data33 <- "C:/Users/Gabija/Desktop/pradiniai_duomenys.xlsx"
df33 <- read_excel(data33, sheet = "BE SEZONIŠKUMO")

df33 <- df33 %>%
  mutate(Date = as.Date(paste(Metai, Mėnesis, "01", sep = "-"), format="%Y-%m-%d")) %>% arrange(Date)

exclude33 <- c("Metai", "Mėnesis", "Laikotarpis", "Date")
rod33 <- setdiff(colnames(df33), exclude33)
rod33 <- rod33[sapply(df3[rod33], is.numeric)]

#Pearsono koreliacija
pearson <- sapply(rod33, function(var) {
  cor(df3[[var]], df33$BVP_I_palyg, use = "complete.obs", method = "pearson")
})

#Spearmano koreliacija
spearman <- sapply(rod33, function(var) {
  cor(df3[[var]], df33$BVP_I_palyg, use = "complete.obs", method = "spearman")
})

#rezultatai
p_rez <- data.frame(Pearson_koreliacija = pearson)
s_rez <- data.frame(Spearman_koreliacija = spearman)

print(p_rez)
print(s_rez)

#koreliacijos grafikas (heatmap)
koreliacija1 <- cor(dplyr::select(df33, all_of(rod33)), use = "complete.obs", method = "spearman")

pavadinimai <- list(
  "BVP_I_palyg" = "BVP", "BVP_vartojimas_palyg" = "Vartojimas",
  "Imigrantai" = "Imigrantai", "Emigrantai" = "Emigrantai", 
  "Taupymas" = "Taupymas", "Nedarbas_bendras" = "Nedarbas",
  "Paslaugų_pasitikėjimas" = "Paslaugų pasitikėjimas", "Vartotojų_pasitikėjimas" = "Vartotojų pasitikėjimas",
  "Mirštamumas" = "Mirtingumas", "Gimstamumas" = "Gimstamumas", "Gyventojai" = "Gyventojai", "VKI" = "VKI"
)

n_pavadinimai <- unlist(pavadinimai[rownames(koreliacija1)])
rownames(koreliacija1) <- n_pavadinimai
colnames(koreliacija1) <- n_pavadinimai

korel <- ggcorrplot(koreliacija1,
           method = "square", type = "lower", lab = TRUE, lab_size = 4,
           colors = c("#DCDCDC", "#FFFFFF", "#78003F"), title = "Koreliacija tarp rodiklių",
           legend.title = "Koreliacijos koeficientas") + 
  theme_minimal(base_size = 14) +         
  theme(
    panel.grid = element_blank(),        
    plot.title = element_text(size = 18, hjust = 0.5), 
    axis.text.x = element_text(color = "black", size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(color = "black", size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    axis.title.x = element_blank(),  
    axis.title.y = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = "gray80", linewidth = 0.8)) 


korel

ggsave("C:/Users/Gabija/Desktop//GRAFIKAI/skorel.png", korel,
       width = 10, height = 6.5, dpi = 300, units = "in")
