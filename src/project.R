# ==================================================
# Statystyka Inzynierska
# Damian Rynczak & Bartlomiej Piekarz
# Projekt - 12.07.2020r.
# ==================================================

# ===== I. PRZYGOTOWANIE DANYCH DO ANALIZY =====

# 1.Import pakiet?w:
library(RCurl)  # Pakiet umozliwiajacy wczytanie danych na podstawie adresu strony.
library(dplyr)  # Pakiet umozliwiajacy prace z danymi.
library(mice)  # Pamiet umozliwiajacy narysowanie wykresu brakujacych wartosci.

# 2. Wczytanie danych do srodowiska programistycznego:
URLaddress <- getURL("https://raw.githubusercontent.com/drynczak/SI_project/master/wyniki%20.csv")
raw_data <- read.csv(text = URLaddress, header=TRUE)

# W przypadku gdyby dane nie mogly zostac odnalezione na serwerze niezbedne jest skorzystanie z ponizszej metody wczytania pliku.
# Funcja ta umozliwia wybranie pliku z rozszerzeniem .csv z folderu znajduj?cego si? na dysku komputera: 
# raw_data <- read.csv(file.choose(), header = TRUE)

# 3. Zapoznanie si? ze wczytanymi danymi:
head(raw_data, n=10)  # Wczytanie pierwszych dziesi?ciu element?w.
tail(raw_data, n=10)  # Wczytanie ostatnich dziesieciu element?w.
summary(raw_data)
# Po skorzystaniu z funkcji summary(), ktora jako argument przyjmuje wczytane dane zauwazono, ze minimalna wartosc w wynikach wynosi -1.
# Wartosc ta jest niezgodna z przyjetymi kryteriami oceny wtyczek, dlatego poczatkowo podjeto decyzje o usunieciu tych wartosci ze zbioru danych.

# 4. Pogrupowanie danych:
# 4a. Podzial wczytanych danych na stara i nowa wersje wtyczki:
old_plugin <- filter(raw_data, wersja == 'S')
new_plugin <- filter(raw_data, wersja == 'N')
summary(old_plugin)
summary(new_plugin)
# Po przeprowadzeniu rozdzielenia danych na stara wersje wtyczki oraz nowa zauwazono, ze w obu tych przypadkach wystepuje wartosc ujemna w wynikach.
# Podjeto zatem decyzje o zamianie wartosci niezgodnych z kryterium oceny na wartosc brakujaca - NA.
# Jednak, aby nie modyfikowac danych wczytanych z pliku przypisano je do zmiennej tak, aby mozna bylo modyfikowac wartosci bez straty danych.

# 4b. Zamiana niepoprawnych danych na wartosci brakujace NA:
imp_data <- raw_data  # Zmienna imp_data zawiera elementy, ktore w dalym kodzie beda modyfikowane.
imp_data$wynik[imp_data$wynik < 0 | imp_data$wynik > 5] = NA  # Zamiana niepoprawnych danych na wartosci brakujace NA.
imp_old_plugin <- filter(imp_data, wersja == 'S')
imp_new_plugin <- filter(imp_data, wersja == 'N')

# 4c.Nastepnie w celu sprawdzenia, czy bledne wartosci zostaly zastapione wartosciami brakujacymi oraz aby sprawdzic, czy 
# wartosci brakujace wystepuja w innych kolumnach sporzadzono wykresy ilustrujace brakujace wartosci:
md.pattern(imp_data, plot = TRUE, rotate.names = FALSE)
md.pattern(imp_old_plugin, plot = TRUE, rotate.names = FALSE)
md.pattern(imp_new_plugin, plot = TRUE, rotate.names = FALSE)

# 4d. Kolejnym etapem analizy blednych wartosci bylo okreslenie liczby osob, ktore udzielaja blednych odpowiedzi.
# Na tym etapie pozwoli to nam podcjac decyzje, czy faktycznie mamy usunac te osoby, czy je zostawic z zamienionymi wartosciami.
# Zarowno dla nowej jak i starej wtyczki wyselekcjonowano osoby, ktore udzielily bledne odpowiedzi.
miss_values_old <- imp_old_plugin %>% filter(is.na(wynik))
miss_values_new <- imp_new_plugin %>% filter(is.na(wynik))

# Nastepnie podzielono brakujace dane ze wzgledu na doswiadczenie osob, ktore udzielily blednych odpowiedzi:
old_miss_nmus <- filter(miss_values_old, doswiadczenie == 'brak')
old_miss_m <- filter(miss_values_old, doswiadczenie == 'muzyk')

new_miss_nmus <- filter(miss_values_new, doswiadczenie == 'brak')
new_miss_m <- filter(miss_values_new, doswiadczenie == 'muzyk')

# I obliczono liczbe osob, ktore udzielily blednych odpowiedzi ze wzgledu na nowa wtyczke i stara:
miss_num_old = 5  # To na razie na stale dalem. Jest to ogolna liczba osob, ktore udzielily blednych odpowiedzi dla starej wtyczki.
miss_num_new = 3  # To tez na razie na stale. Jest to ogolna liczba osob, ktore udzelily blednych odpowiedzi dla nowej wtyczki.

miss_num_old_mus = 3  # Mamy 3 muzykow, ktorzy odpowiedzieli bledne dla starej wtyczki.
miss_num_old_nmus = 2 # I dwie osoby nie bedace muzykami, ktore odpowiedzialy blednie dla starej wtyczki.

miss_num_new_mus = 1  # Jeden muzyk odpowiedzial zle dla nowej wtyczki.
miss_num_new_nmus = 2 # Dwoje osob bez doswiadczenia muzycznego odpowiedzialo blednie dla nowej wtyczki.

# 4e. Na tym etapie pracy z danymi zastanowiono sie, czy warto usunac wyniki. Jest to najbardziej radykalne rozwiazanie, jednak
# prowadzi do sytuacji, ze zawezy nam grupe badanych osob i utrudni nam to dalsza analize.
# Podstanowiono rozwazyc jedna z metod imputacji danych - zastapienie brakujacych danych srednia lub mediana.
# Aby podjac decyzje o wyborze bardziej odpowiedniej metody imputacji danych obliczono zarowno srednia i mediane dla nowej wtyczki
# i starej - bez podzialu na kryteria i doswiadczenie:
# W tym celu stworzono funkcje, ktora umozliwia okreslenie podstawowych wartosci statystyki opisowej:

# Uzywane funkcje
# ==================================================
# Krotkie podsumowanie wynikow:
podsumowanie <- function(dane) {
  srednia <- round(mean(dane$wynik, na.rm = TRUE), digits = 2)
  mediana <- median(dane$wynik, na.rm = TRUE)
  odchylenie <- round(sd(dane$wynik, na.rm = TRUE), digits = 2)
  kwantyle <- quantile(dane$wynik, na.rm = TRUE, probs = c(0.25, 0.5, 0.75),
                       names = FALSE)
  q1 <- kwantyle[1]
  q2 <- kwantyle[2]
  q3 <- kwantyle[3]
  return(data.frame(srednia,mediana,odchylenie, q1, q2, q3))
}

szukane <- function(values) {
  srednia_poprawna <- round(mean(values$wynik, na.rm=TRUE), digits=2)
  mediana_poprawna <- median(values$wynik, na.rm = TRUE)
  uniqv <- unique(values$wynik)
  moda_poprawna <- uniqv[which.max(tabulate(match(values$wynik, uniqv)))]
  odchylenie_poprawne <- round(sd(values$wynik, na.rm = TRUE), digits = 2)
  wspolczynnik_zmiennosci_poprawny <- odchylenie_poprawne/srednia_poprawna
  
  kwantyle_poprawne <- quantile(values$wynik, na.rm = TRUE, probs = c(0.25, 0.5, 0.75),
                                names = FALSE)
  q1_p <- kwantyle_poprawne[1]
  q2_p <- kwantyle_poprawne[2]
  q3_p <- kwantyle_poprawne[3]
  
  rozrzut <- values$wynik
  rozstep_poprawny <- max(rozrzut) - min(rozrzut)
  
  return(data.frame(srednia_poprawna, mediana_poprawna, moda_poprawna, odchylenie_poprawne, wspolczynnik_zmiennosci_poprawny, q1_p, q2_p, q3_p, rozstep_poprawny))
}

# ================================================== 

summ_old_imp <- podsumowanie(imp_old_plugin)
summ_new_imp <- podsumowanie(imp_new_plugin)

# Policzono miedzy innymi srednia i mediane dla nowej wtyczki i starej i widac, ze po zaokragleniu srednia jest rowna medianie.
# Jako ocena musi byc podana wartosc calkowita, dlatego wezmiemy mediane z wynikow jako zastapienie wartosci NA.

# 4f. Zastapienie wartosci brakujacych NA mediana obliczona wczesniej.
imp_old_plugin[is.na(imp_old_plugin)]= summ_old_imp$mediana
imp_new_plugin[is.na(imp_new_plugin)]= summ_new_imp$mediana
imputed_data <- rbind(imp_old_plugin, imp_new_plugin)

# 4g. Sprawdzenie jak zamiana wartosci wplynela na srednie i mediane:
summ_old_median <-podsumowanie(imp_old_plugin)
summ_new_median <- podsumowanie(imp_new_plugin)

# Wniosek: Podmienienie wartosci na mediane nie ma wplywu za bardzo na srednia - damy dane do tabeli i to porownany.
# Na tym etapie mamy juz (albo dopiero) przygotowane dane i bedzie robiona dalsza analiza.

# 4h. Sprawdzenie, czy w wynikach dla starej wtyczki i nowej wystepuja wartosci odstajace:
# W tym celu zastosowano boxplot, aby to sprawdzic - do okreslenia wartosci odstajacych przyjelismy metode IQR:
# Zastosowano rozstep cwiartkowy, ktory docelowo zaprogramowany jest w boxplocie RStudio.
# Polega to na tym, ze bierzemy roznice pomiedzy trzecim i pierwszym kwartylem pomnozona razy 1.5.
# To znaczy: Q1- 1.5(Q3-Q1) i Q1 - nie wezmie mniejszych wartosci
# Q3+1.5(Q3-Q1) i Q3 - nie wezmie wiekszych wartosci.
# Wszystkie obserwacje znajdujace sie poza wasami sa wartosciami odstajacymi, ktorych po rozdzieleniu ocen na stara wtyczke i nowa nie zauwazamy.

boxplot(imp_old_plugin$wynik, imp_new_plugin$wynik, main="Oceny dwoch wersji wtyczki", ylab="wynik", names=c("Stara wtyczka", "Nowa wtyczka"))

# Podzial danych
# Ze wzgledu na doswiadczenie
old_nonmusician <- filter(imp_old_plugin, doswiadczenie == "brak")
old_musician <- filter(imp_old_plugin, doswiadczenie == "muzyk")

new_nonmusician <- filter(imp_new_plugin, doswiadczenie == "brak")
new_musician <- filter(imp_new_plugin, doswiadczenie == "muzyk")

# Ze wzgledu na rodzaj muzyki
old_jazz <- filter(imp_old_plugin, muzyka == "jazz")
old_pop <- filter(imp_old_plugin, muzyka == "pop")
old_symf <- filter(imp_old_plugin, muzyka == "symf")

new_jazz <- filter(imp_new_plugin, muzyka == "jazz")
new_pop <- filter(imp_new_plugin, muzyka == "pop")
new_symf <- filter(imp_new_plugin, muzyka == "symf")

# Ze wzgledu na kryterium
old_natural <- filter(imp_old_plugin, kryterium == "naturalnosc")
old_space <- filter(imp_old_plugin, kryterium == "przestrzennosc")
old_feel <- filter(imp_old_plugin, kryterium == "wrazenie")

new_natural <- filter(imp_new_plugin, kryterium == "naturalnosc")
new_space <- filter(imp_new_plugin, kryterium == "przestrzennosc")
new_feel <- filter(imp_new_plugin, kryterium == "wrazenie")

# Gatunki zaleznie od doswiadczenia
old_jazz_musician <- filter(imp_old_plugin, muzyka == "jazz", doswiadczenie == "muzyk")
old_pop_musician <- filter(imp_old_plugin, muzyka == "pop", doswiadczenie == "muzyk")
old_symf_musician <- filter(imp_old_plugin, muzyka == "symf", doswiadczenie == "muzyk")

old_jazz_nonmusician <- filter(imp_old_plugin, muzyka == "jazz", doswiadczenie == "brak")
old_pop_nonmusician <- filter(imp_old_plugin, muzyka == "pop", doswiadczenie == "brak")
old_symf_nonmusician <- filter(imp_old_plugin, muzyka == "symf", doswiadczenie == "brak")

new_jazz_musician <- filter(imp_new_plugin, muzyka == "jazz", doswiadczenie == "muzyk")
new_pop_musician <- filter(imp_new_plugin, muzyka == "pop", doswiadczenie == "muzyk")
new_symf_musician <- filter(imp_new_plugin, muzyka == "symf", doswiadczenie == "muzyk")

new_jazz_nonmusician <- filter(imp_new_plugin, muzyka == "jazz", doswiadczenie == "brak")
new_pop_nonmusician <- filter(imp_new_plugin, muzyka == "pop", doswiadczenie == "brak")
new_symf_nonmusician <- filter(imp_new_plugin, muzyka == "symf", doswiadczenie == "brak")

# Kryterium zalezne od doswiadczenia
old_natural_musician <- filter(imp_old_plugin, kryterium == "naturalnosc", doswiadczenie == "muzyk")
old_space_musician <- filter(imp_old_plugin, kryterium == "przestrzennosc", doswiadczenie == "muzyk")
old_feel_musician <- filter(imp_old_plugin, kryterium == "wrazenie", doswiadczenie == "muzyk")

old_natural_nonmusician <- filter(imp_old_plugin, kryterium == "naturalnosc", doswiadczenie == "brak")
old_space_nonmusician <- filter(imp_old_plugin, kryterium == "przestrzennosc", doswiadczenie == "brak")
old_feel_nonmusician <- filter(imp_old_plugin, kryterium == "wrazenie", doswiadczenie == "brak")

new_natural_musician <- filter(imp_new_plugin, kryterium == "naturalnosc", doswiadczenie == "muzyk")
new_space_musician <- filter(imp_new_plugin, kryterium == "przestrzennosc", doswiadczenie == "muzyk")
new_feel_musician <- filter(imp_new_plugin, kryterium == "wrazenie", doswiadczenie == "muzyk")

new_natural_nonmusician <- filter(imp_new_plugin, kryterium == "naturalnosc", doswiadczenie == "brak")
new_space_nonmusician <- filter(imp_new_plugin, kryterium == "przestrzennosc", doswiadczenie == "brak")
new_feel_nonmusician <- filter(imp_new_plugin, kryterium == "wrazenie", doswiadczenie == "brak")

# Gatunki zaleznie od kryterium
old_jazz_natural <- filter(imp_old_plugin, muzyka == "jazz", kryterium == "naturalnosc")
old_pop_natural <- filter(imp_old_plugin, muzyka == "pop", kryterium == "naturalnosc")
old_symf_natural <- filter(imp_old_plugin, muzyka == "symf", kryterium == "naturalnosc")

old_jazz_space <- filter(imp_old_plugin, muzyka == "jazz", kryterium == "przestrzennosc")
old_pop_space <- filter(imp_old_plugin, muzyka == "pop", kryterium == "przestrzennosc")
old_symf_space <- filter(imp_old_plugin, muzyka == "symf", kryterium == "przestrzennosc")

old_jazz_feel <- filter(imp_old_plugin, muzyka == "jazz", kryterium == "wrazenie")
old_pop_feel <- filter(imp_old_plugin, muzyka == "pop", kryterium == "wrazenie")
old_symf_feel <- filter(imp_old_plugin, muzyka == "symf", kryterium == "wrazenie")

new_jazz_natural <- filter(imp_new_plugin, muzyka == "jazz", kryterium == "naturalnosc")
new_pop_natural <- filter(imp_new_plugin, muzyka == "pop", kryterium == "naturalnosc")
new_symf_natural <- filter(imp_new_plugin, muzyka == "symf", kryterium == "naturalnosc")

new_jazz_space <- filter(imp_new_plugin, muzyka == "jazz", kryterium == "przestrzennosc")
new_pop_space <- filter(imp_new_plugin, muzyka == "pop", kryterium == "przestrzennosc")
new_symf_space <- filter(imp_new_plugin, muzyka == "symf", kryterium == "przestrzennosc")

new_jazz_feel <- filter(imp_new_plugin, muzyka == "jazz", kryterium == "wrazenie")
new_pop_feel <- filter(imp_new_plugin, muzyka == "pop", kryterium == "wrazenie")
new_symf_feel <- filter(imp_new_plugin, muzyka == "symf", kryterium == "wrazenie")

# Poszczegolne
old_jazz_natural_musician <- filter(imp_old_plugin, muzyka == "jazz", kryterium == "naturalnosc", doswiadczenie == "muzyk")
old_pop_natural_musician <- filter(imp_old_plugin, muzyka == "pop", kryterium == "naturalnosc", doswiadczenie == "muzyk")
old_symf_natural_musician <- filter(imp_old_plugin, muzyka == "symf", kryterium == "naturalnosc", doswiadczenie == "muzyk")

old_jazz_space_musician <- filter(imp_old_plugin, muzyka == "jazz", kryterium == "przestrzennosc", doswiadczenie == "muzyk")
old_pop_space_musician <- filter(imp_old_plugin, muzyka == "pop", kryterium == "przestrzennosc", doswiadczenie == "muzyk")
old_symf_space_musician <- filter(imp_old_plugin, muzyka == "symf", kryterium == "przestrzennosc", doswiadczenie == "muzyk")

old_jazz_feel_musician <- filter(imp_old_plugin, muzyka == "jazz", kryterium == "wrazenie", doswiadczenie == "muzyk")
old_pop_feel_musician <- filter(imp_old_plugin, muzyka == "pop", kryterium == "wrazenie", doswiadczenie == "muzyk")
old_symf_feel_musician <- filter(imp_old_plugin, muzyka == "symf", kryterium == "wrazenie", doswiadczenie == "muzyk")

new_jazz_natural_musician <- filter(imp_new_plugin, muzyka == "jazz", kryterium == "naturalnosc", doswiadczenie == "muzyk")
new_pop_natural_musician <- filter(imp_new_plugin, muzyka == "pop", kryterium == "naturalnosc", doswiadczenie == "muzyk")
new_symf_natural_musician <- filter(imp_new_plugin, muzyka == "symf", kryterium == "naturalnosc", doswiadczenie == "muzyk")

new_jazz_space_musician <- filter(imp_new_plugin, muzyka == "jazz", kryterium == "przestrzennosc", doswiadczenie == "muzyk")
new_pop_space_musician <- filter(imp_new_plugin, muzyka == "pop", kryterium == "przestrzennosc", doswiadczenie == "muzyk")
new_symf_space_musician <- filter(imp_new_plugin, muzyka == "symf", kryterium == "przestrzennosc", doswiadczenie == "muzyk")

new_jazz_feel_musician <- filter(imp_new_plugin, muzyka == "jazz", kryterium == "wrazenie", doswiadczenie == "muzyk")
new_pop_feel_musician <- filter(imp_new_plugin, muzyka == "pop", kryterium == "wrazenie", doswiadczenie == "muzyk")
new_symf_feel_musician <- filter(imp_new_plugin, muzyka == "symf", kryterium == "wrazenie", doswiadczenie == "muzyk")

old_jazz_natural_nonmusician <- filter(imp_old_plugin, muzyka == "jazz", kryterium == "naturalnosc", doswiadczenie == "brak")
old_pop_natural_nonmusician <- filter(imp_old_plugin, muzyka == "pop", kryterium == "naturalnosc", doswiadczenie == "brak")
old_symf_natural_nonmusician <- filter(imp_old_plugin, muzyka == "symf", kryterium == "naturalnosc", doswiadczenie == "brak")

old_jazz_space_nonmusician <- filter(imp_old_plugin, muzyka == "jazz", kryterium == "przestrzennosc", doswiadczenie == "brak")
old_pop_space_nonmusician <- filter(imp_old_plugin, muzyka == "pop", kryterium == "przestrzennosc", doswiadczenie == "brak")
old_symf_space_nonmusician <- filter(imp_old_plugin, muzyka == "symf", kryterium == "przestrzennosc", doswiadczenie == "brak")

old_jazz_feel_nonmusician <- filter(imp_old_plugin, muzyka == "jazz", kryterium == "wrazenie", doswiadczenie == "brak")
old_pop_feel_nonmusician <- filter(imp_old_plugin, muzyka == "pop", kryterium == "wrazenie", doswiadczenie == "brak")
old_symf_feel_nonmusician <- filter(imp_old_plugin, muzyka == "symf", kryterium == "wrazenie", doswiadczenie == "brak")

new_jazz_natural_nonmusician <- filter(imp_new_plugin, muzyka == "jazz", kryterium == "naturalnosc", doswiadczenie == "brak")
new_pop_natural_nonmusician <- filter(imp_new_plugin, muzyka == "pop", kryterium == "naturalnosc", doswiadczenie == "brak")
new_symf_natural_nonmusician <- filter(imp_new_plugin, muzyka == "symf", kryterium == "naturalnosc", doswiadczenie == "brak")

new_jazz_space_nonmusician <- filter(imp_new_plugin, muzyka == "jazz", kryterium == "przestrzennosc", doswiadczenie == "brak")
new_pop_space_nonmusician <- filter(imp_new_plugin, muzyka == "pop", kryterium == "przestrzennosc", doswiadczenie == "brak")
new_symf_space_nonmusician <- filter(imp_new_plugin, muzyka == "symf", kryterium == "przestrzennosc", doswiadczenie == "brak")

new_jazz_feel_nonmusician <- filter(imp_new_plugin, muzyka == "jazz", kryterium == "wrazenie", doswiadczenie == "brak")
new_pop_feel_nonmusician <- filter(imp_new_plugin, muzyka == "pop", kryterium == "wrazenie", doswiadczenie == "brak")
new_symf_feel_nonmusician <- filter(imp_new_plugin, muzyka == "symf", kryterium == "wrazenie", doswiadczenie == "brak")

# ===== II. WSTEPNA EKSPLORACJA DANYCH NA PODSTAWIE STATYSTYKI OPISOWEJ =====

# 2.1. Porowanie ocen starej wtyczki i nowej bez podzialu na doswiadczenie i kryteria
# 2.1.1. Obliczenie parametrow dla starej i nowej wtyczki
old_plug_stat <- szukane(imp_old_plugin)
new_plug_stat <- szukane(imp_new_plugin)

#2.1.2. Histogram
hist(imp_old_plugin$wynik, main="Oceny dla starej wtyczki",xlab="Oceny", ylab="Gestosc wystapien", breaks=0.5:5.5, freq= FALSE, prob=TRUE)
hist(imp_new_plugin$wynik, main="Oceny dla nowej wtyczki",xlab="Oceny", ylab="Gestosc wystapien", breaks=0.5:5.5, freq= FALSE, prob=TRUE)

#2.1.3. Boxplot
boxplot(imp_old_plugin$wynik, imp_new_plugin$wynik, main="Oceny dwoch wersji wtyczki", ylab="Wynik", names=c("Stara wtyczka", "Nowa wtyczka"))

#2.1.4 Wykres gestosci
plot(density(imp_old_plugin$wynik), main="Wykres gestosci - stara wtyczka", ylab="Czestotliwosc wystapien")
plot(density(imp_new_plugin$wynik), main="Wykres gestosci - nowa wtyczka", ylab="Czestotliwosc wystapien")


#2.1.5. Korelacja
old_new_cor = cor(imp_old_plugin$wynik, imp_new_plugin$wynik, method = "pearson")

#2.1.6. Regresja liniowa
model = lm(imp_old_plugin$wynik~imp_new_plugin$wynik)

#2.2. Porowanie starej wtyczki i nowej w zaleznosci od doswiadczenia
# Obliczenie szukanych wartosci dla starej wtyczki - muzycy i niemuzycy
old_nonmusician_stats <- szukane(old_nonmusician)
old_musician_stats <- szukane(old_musician)

new_nonmusician_stats <- szukane(new_nonmusician)
new_musician_stats <- szukane(new_musician)

#2.2.1. Histogramy:
hist(old_musician$wynik, main="Oceny dla starej wtyczki - muzycy",xlab="Oceny", ylab="Gestosc wystapien", breaks=0.5:5.5, freq= FALSE, prob=TRUE)
hist(old_nonmusician$wynik, main = "Oceny dla starej wtyczki - brak doswiadczenia",xlab="Oceny", ylab="Gestosc wystapien", breaks=0.5:5.5, freq= FALSE, prob=TRUE)

hist(new_musician$wynik,main="Oceny dla nowej wtyczki - muzycy",xlab="Oceny", ylab="Gestosc wystapien", breaks=0.5:5.5, freq= FALSE, prob=TRUE)
hist(new_nonmusician$wynik,main="Oceny dla nowej wtyczki - brak doswiadczenia",xlab="Oceny", ylab="Gestosc wystapien", breaks=0.5:5.5, freq= FALSE, prob=TRUE)

#2.2.2. Boxploty:
boxplot(old_musician$wynik, old_nonmusician$wynik, main="Oceny starej wtyczki - muzycy i brak doswiadczenia", ylab="Wynik", names=c("Muzyk", "Brak doswiadczenia"))
boxplot(new_musician$wynik, new_nonmusician$wynik, main="Oceny nowej wtyczki - muzycy i brak doswiadczenia", ylab="Wynik", names=c("Muzyk", "Brak doswiadczenia"))

#2.2.3. Korelacja
# Sprawdzenie, czy wczesniejsze odpowiedzi wplywaja na nowa odpowiedz:
old_new_musicians_cor <- cor(old_musician$wynik, new_musician$wynik)
old_new_nonmusician_cor <- cor(old_nonmusician$wynik, new_nonmusician$wynik)

# Sprawdzenie,czy odpowiedzi muzykow i niemuzykow sa skorelowane - dla starej i nowej wtyczki osobno:
old_music_nonmusic_cor <- cor(old_musician$wynik, old_nonmusician$wynik)
new_music_nonmusic_cor <- cor(new_musician$wynik, new_nonmusician$wynik)

#2.2.4 Regresja liniowa
# Tutaj trzeba zrobic.

# 3. Porownanie gatunkow - ze wzgledu na doswiadczenie i bez wzgledu na doswiadczenie i razem

# 3.1 Wartosci
# 3.2 Histogramy
# 3.3. Boxplot
# 3.4 Korelacja
old_jazz_pop_cor <- cor(old_jazz$wynik, old_pop$wynik)
old_jazz_symf_cor <- cor(old_jazz$wynik, old_symf$wynik)
old_symf_pop_cor <- cor(old_symf$wynik, old_pop$wynik)

new_jazz_pop_cor <- cor(new_jazz$wynik, new_pop$wynik)
new_jazz_symf_cor <- cor(new_jazz$wynik, new_symf$wynik)
new_symf_pop_cor <- cor(new_symf$wynik, new_pop$wynik)

old_jazz_pop_musician_cor <- cor(old_jazz_musician$wynik, old_pop_musician$wynik)
old_jazz_symf_musician_cor <- cor(old_jazz_musician$wynik, old_symf_musician$wynik)
old_symf_pop_musician_cor <- cor(old_symf_musician$wynik, old_pop_musician$wynik)

new_jazz_pop_musician_cor <- cor(new_jazz_musician$wynik, new_pop_musician$wynik)
new_jazz_symf_musician_cor <- cor(new_jazz_musician$wynik, new_symf_musician$wynik)
new_symf_pop_musician_cor <- cor(new_symf_musician$wynik, new_pop_musician$wynik)

old_jazz_pop_nonmusician_cor <- cor(old_jazz_nonmusician$wynik, old_pop_nonmusician$wynik)
old_jazz_symf_nonmusician_cor <- cor(old_jazz_nonmusician$wynik, old_symf_nonmusician$wynik)
old_symf_pop_nonmusician_cor <- cor(old_symf_nonmusician$wynik, old_pop_nonmusician$wynik)

new_jazz_pop_nonmusician_cor <- cor(new_jazz_nonmusician$wynik, new_pop_nonmusician$wynik)
new_jazz_symf_nonmusician_cor <- cor(new_jazz_nonmusician$wynik, new_symf_nonmusician$wynik)
new_symf_pop_nonmusician_cor <- cor(new_symf_nonmusician$wynik, new_pop_nonmusician$wynik)

# a. Korelacja pomiedzy jazzem dla starej wtyczki i nowej dla muzykow
old_new_jazz_musician_cor <- cor(old_jazz_musician$wynik, new_jazz_musician$wynik)
old_new_pop_musician_cor <- cor(old_pop_musician$wynik, new_pop_musician$wynik)
old_new_symf_musician_cor <- cor(old_symf_musician$wynik, new_symf_musician$wynik)

# b. Podobne korelacje

# 4. Porownanie ze wzgledu na wrazenie - bez podzialu na gatunki
old_new_natural_cor <- cor(old_natural$wynik, new_natural$wynik)
old_new_feel_cor <- cor(old_space$wynik, new_space$wynik)
old_new_feel_cor <- cor(old_feel$wynik, new_feel$wynik)

old_natural_space_cor <- cor(old_natural$wynik, old_space$wynik)
old_natural_feel_cor <- cor(old_natural$wynik, old_feel$wynik)
old_feel_space_cor <- cor(old_feel$wynik, old_space$wynik)

new_natural_space_cor <- cor(new_natural$wynik, new_space$wynik)
new_natural_feel_cor <- cor(new_natural$wynik, new_feel$wynik)
new_feel_space_cor <- cor(new_feel$wynik, new_space$wynik)

# 5. Podzial - jazz dla wybranego kryterium
old_pop_natural_space_cor <- cor(old_pop_natural$wynik, old_pop_space$wynik)
old_pop_natural_feel_cor <- cor(old_pop_natural$wynik, old_pop_feel$wynik)
old_pop_feel_space_cor <- cor(old_pop_feel$wynik, old_pop_space$wynik)

new_pop_natural_space_cor <- cor(new_pop_natural$wynik, new_pop_space$wynik)
new_pop_natural_feel_cor <- cor(new_pop_natural$wynik, new_pop_feel$wynik)
new_pop_feel_space_cor <- cor(new_pop_feel$wynik, new_pop_space$wynik)

old_jazz_natural_space_musician_cor <- cor(old_jazz_natural_musician$wynik, old_jazz_space_musician$wynik)
old_jazz_natural_feel_musician_cor <- cor(old_jazz_natural_musician$wynik, old_jazz_feel_musician$wynik)
old_jazz_feel_space_musician_cor <- cor(old_jazz_feel_musician$wynik, old_jazz_space_musician$wynik)

new_jazz_natural_space_musician_cor <- cor(new_jazz_natural_musician$wynik, new_jazz_space_musician$wynik)
new_jazz_natural_feel_musician_cor <- cor(new_jazz_natural_musician$wynik, new_jazz_feel_musician$wynik)
new_jazz_feel_space_musician_cor <- cor(new_jazz_feel_musician$wynik, new_jazz_space_musician$wynik)

old_jazz_natural_space_nonmusician_cor <- cor(old_jazz_natural_nonmusician$wynik, old_jazz_space_nonmusician$wynik)
old_jazz_natural_feel_nonmusician_cor <- cor(old_jazz_natural_nonmusician$wynik, old_jazz_feel_nonmusician$wynik)
old_jazz_feel_space_nonmusician_cor <- cor(old_jazz_feel_nonmusician$wynik, old_jazz_space_nonmusician$wynik)

new_jazz_natural_space_nonmusician_cor <- cor(new_jazz_natural_nonmusician$wynik, new_jazz_space_nonmusician$wynik)
new_jazz_natural_feel_nonmusician_cor <- cor(new_jazz_natural_nonmusician$wynik, new_jazz_feel_nonmusician$wynik)
new_jazz_feel_space_nonmusician_cor <- cor(new_jazz_feel_nonmusician$wynik, new_jazz_space_nonmusician$wynik)

# 6. Podzial - symfoniczna dla wybranego kryterium
old_symf_natural_space_cor <- cor(old_symf_natural$wynik, old_symf_space$wynik)
old_symf_natural_feel_cor <- cor(old_symf_natural$wynik, old_symf_feel$wynik)
old_symf_feel_space_cor <- cor(old_symf_feel$wynik, old_symf_space$wynik)

new_symf_natural_space_cor <- cor(new_symf_natural$wynik, new_symf_space$wynik)
new_symf_natural_feel_cor <- cor(new_symf_natural$wynik, new_symf_feel$wynik)
new_symf_feel_space_cor <- cor(new_symf_feel$wynik, new_symf_space$wynik)

old_symf_natural_space_musician_cor <- cor(old_symf_natural_musician$wynik, old_symf_space_musician$wynik)
old_symf_natural_feel_musician_cor <- cor(old_symf_natural_musician$wynik, old_symf_feel_musician$wynik)
old_symf_feel_space_musician_cor <- cor(old_symf_feel_musician$wynik, old_symf_space_musician$wynik)

new_symf_natural_space_musician_cor <- cor(new_symf_natural_musician$wynik, new_symf_space_musician$wynik)
new_symf_natural_feel_musician_cor <- cor(new_symf_natural_musician$wynik, new_symf_feel_musician$wynik)
new_symf_feel_space_musician_cor <- cor(new_symf_feel_musician$wynik, new_symf_space_musician$wynik)

old_symf_natural_space_nonmusician_cor <- cor(old_symf_natural_nonmusician$wynik, old_symf_space_nonmusician$wynik)
old_symf_natural_feel_nonmusician_cor <- cor(old_symf_natural_nonmusician$wynik, old_symf_feel_nonmusician$wynik)
old_symf_feel_space_nonmusician_cor <- cor(old_symf_feel_nonmusician$wynik, old_symf_space_nonmusician$wynik)

new_symf_natural_space_nonmusician_cor <- cor(new_symf_natural_nonmusician$wynik, new_symf_space_nonmusician$wynik)
new_symf_natural_feel_nonmusician_cor <- cor(new_symf_natural_nonmusician$wynik, new_symf_feel_nonmusician$wynik)
new_symf_feel_space_nonmusician_cor <- cor(new_symf_feel_nonmusician$wynik, new_symf_space_nonmusician$wynik)

# 7. Podzial - pop dla wybranego kryterium
old_jazz_natural_space_cor <- cor(old_jazz_natural$wynik, old_jazz_space$wynik)
old_jazz_natural_feel_cor <- cor(old_jazz_natural$wynik, old_jazz_feel$wynik)
old_jazz_feel_space_cor <- cor(old_jazz_feel$wynik, old_jazz_space$wynik)

new_jazz_natural_space_cor <- cor(new_jazz_natural$wynik, new_jazz_space$wynik)
new_jazz_natural_feel_cor <- cor(new_jazz_natural$wynik, new_jazz_feel$wynik)
new_jazz_feel_space_cor <- cor(new_jazz_feel$wynik, new_jazz_space$wynik)

old_pop_natural_space_musician_cor <- cor(old_pop_natural_musician$wynik, old_pop_space_musician$wynik)
old_pop_natural_feel_musician_cor <- cor(old_pop_natural_musician$wynik, old_pop_feel_musician$wynik)
old_pop_feel_space_musician_cor <- cor(old_pop_feel_musician$wynik, old_pop_space_musician$wynik)

new_pop_natural_space_musician_cor <- cor(new_pop_natural_musician$wynik, new_pop_space_musician$wynik)
new_pop_natural_feel_musician_cor <- cor(new_pop_natural_musician$wynik, new_pop_feel_musician$wynik)
new_pop_feel_space_musician_cor <- cor(new_pop_feel_musician$wynik, new_pop_space_musician$wynik)

old_pop_natural_space_nonmusician_cor <- cor(old_pop_natural_nonmusician$wynik, old_pop_space_nonmusician$wynik)
old_pop_natural_feel_nonmusician_cor <- cor(old_pop_natural_nonmusician$wynik, old_pop_feel_nonmusician$wynik)
old_pop_feel_space_nonmusician_cor <- cor(old_pop_feel_nonmusician$wynik, old_pop_space_nonmusician$wynik)

new_pop_natural_space_nonmusician_cor <- cor(new_pop_natural_nonmusician$wynik, new_pop_space_nonmusician$wynik)
new_pop_natural_feel_nonmusician_cor <- cor(new_pop_natural_nonmusician$wynik, new_pop_feel_nonmusician$wynik)
new_pop_feel_space_nonmusician_cor <- cor(new_pop_feel_nonmusician$wynik, new_pop_space_nonmusician$wynik)