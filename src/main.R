# ==================================================
# Statystyka Inżynierska
# Damian Rynczak & Bartłomiej Piekarz
# Projekt - 12.07.2020r.
# ==================================================
rm(list = ls())


# uzywane funkcje
# ==================================================
# krotkie podsumowanie wynikow (okrojone summary)
podsumowanie <- function(dane) {
  srednia <- round(mean(dane$wynik, na.rm = TRUE), digits = 2)
  odchylenie <- round(sd(dane$wynik, na.rm = TRUE), digits = 2)
  kwantyle <- quantile(dane$wynik, na.rm = TRUE, probs = c(0.25, 0.5, 0.75),
                       names = FALSE)
  q1 <- kwantyle[1]
  q2 <- kwantyle[2]
  q3 <- kwantyle[3]
  return(data.frame(srednia, odchylenie, q1, q2, q3))
}


# glowny kod
# ==================================================
dane_surowe <- read.csv(file.choose(), header = TRUE)

# odsiew niepoprawnych danych
dane_surowe$wynik[dane_surowe$wynik < 0] = NA

# 1 srednia, odchylenie standardowe, mediana, kwartyle dla starej wtyczki
dane_stara <- filter(dane_surowe, dane_surowe$wersja == "S")
pods_stara <- podsumowanie(dane_stara)

# 2 srednia, odchylenie standardowe, mediana, kwartyle dla nowej wtyczki
dane_nowa <- filter(dane_surowe, dane_surowe$wersja == "N")
pods_nowa <- podsumowanie(dane_nowa)

# 3 podstawowe wartosci z podzialem na doswiadczenie
dane_stara_brak <- filter(dane_stara, dane_stara$doswiadczenie == "brak")
pods_stara_brak <- podsumowanie(dane_stara_brak)
dane_stara_muzyk <- filter(dane_stara, dane_stara$doswiadczenie == "muzyk")
pods_stara_muzyk <- podsumowanie(dane_stara_muzyk)

dane_nowa_brak <- filter(dane_nowa, dane_nowa$doswiadczenie == "brak")
pods_nowa_brak <- podsumowanie(dane_nowa_brak)
dane_nowa_muzyk <- filter(dane_nowa, dane_nowa$doswiadczenie == "muzyk")
pods_nowa_muzyk <- podsumowanie(dane_nowa_muzyk)

# 4 podstawowe wartosci z podzialem na gatunek
dane_stara_jazz <- filter(dane_stara, dane_stara$muzyka == "jazz")
pods_stara_jazz <- podsumowanie(dane_stara_jazz)
dane_nowa_jazz <- filter(dane_nowa, dane_nowa$muzyka == "jazz")
pods_nowa_jazz <- podsumowanie(dane_nowa_jazz)

dane_stara_pop <- filter(dane_stara, dane_stara$muzyka == "pop")
pods_stara_pop <- podsumowanie(dane_stara_pop)
dane_nowa_pop <- filter(dane_nowa, dane_nowa$muzyka == "pop")
pods_nowa_pop <- podsumowanie(dane_nowa_pop)

dane_stara_symf <- filter(dane_stara, dane_stara$muzyka == "symf")
pods_stara_symf <- podsumowanie(dane_stara_symf)
dane_nowa_symf <- filter(dane_nowa, dane_nowa$muzyka == "symf")
pods_nowa_symf <- podsumowanie(dane_nowa_symf)

# 5 podstawowe wartosci z podzialem na kryterium
dane_stara_naturalnosc <- filter(dane_stara, dane_stara$kryterium == "naturalnosc")
pods_stara_naturalnosc <- podsumowanie(dane_stara_naturalnosc)
dane_nowa_naturalnosc <- filter(dane_nowa, dane_nowa$kryterium == "naturalnosc")
pods_nowa_naturalnosc <- podsumowanie(dane_nowa_naturalnosc)

dane_stara_przestrzennosc <- filter(dane_stara, dane_stara$kryterium == "przestrzennosc")
pods_stara_przestrzennosc <- podsumowanie(dane_stara_przestrzennosc)
dane_nowa_przestrzennosc <- filter(dane_nowa, dane_nowa$kryterium == "przestrzennosc")
pods_nowa_przestrzennosc <- podsumowanie(dane_nowa_przestrzennosc)

dane_stara_wrazenie <- filter(dane_stara, dane_stara$kryterium == "wrazenie")
pods_stara_wrazenie <- podsumowanie(dane_stara_wrazenie)
dane_nowa_wrazenie <- filter(dane_nowa, dane_nowa$kryterium == "wrazenie")
pods_nowa_wrazenie <- podsumowanie(dane_nowa_wrazenie)