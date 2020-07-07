# ==================================================
# Statystyka Inżynierska
# Damian Rynczak & Bartłomiej Piekarz
# Projekt - 12.07.2020r.
# ==================================================

dane_surowe <- read.csv(file.choose(), header = TRUE)

# odsiew niepoprawnych danych i podzial
dane_surowe$wynik[dane_surowe$wynik < 0] = NA
dane_stara <- filter(dane_surowe, dane_surowe$wersja == "S")
dane_nowa <- filter(dane_surowe, dane_surowe$wersja == "N")
rm(dane_surowe)

# 1 srednia, odchylenie standardowe, mediana, kwartyle dla starej wtyczki
srednia_stara <- round(mean(dane_stara$wynik, na.rm = TRUE), digits = 2)
odchylenie_stara <- round(sd(dane_stara$wynik, na.rm = TRUE), digits = 2)
kwantyle_stara <- quantile(dane_stara$wynik, probs = c(0.25, 0.5, 0.75),
                           names = FALSE, na.rm = TRUE)

# 2 srednia, odchylenie standardowe, mediana, kwartyle dla nowej wtyczki
srednia_nowa <- round(mean(dane_nowa$wynik, na.rm = TRUE), digits = 2)
odchylenie_nowa <- round(sd(dane_nowa$wynik, na.rm = TRUE), digits = 2)
kwantyle_nowa <- quantile(dane_nowa$wynik, probs = c(0.25, 0.5, 0.75),
                           names = FALSE, na.rm = TRUE)