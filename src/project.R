# ==================================================
# Statystyka Inzynierska
# Damian Rynczak & Bartlomiej Piekarz
# Projekt - 12.07.2020r.
# ==================================================

# ===== I. PRZYGOTOWANIE DANYCH DO ANALIZY =====
rm(list = ls())

# 1.Import pakiet?w:
library(RCurl)  # Pakiet umozliwiajacy wczytanie danych na podstawie adresu strony.
library(dplyr)  # Pakiet umozliwiajacy prace z danymi.
library(mice)   # Pakiet umozliwiajacy narysowanie wykresu brakujacych wartosci.
library(e1071)  # Pakiet umozliwiajacy obliczenie wspolczynnika skosnosci

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
miss <- filter(imp_data, is.na(wynik))
miss_old <- filter(miss, wersja == "S")
miss_new <- filter(miss, wersja == "N")

# Nastepnie podzielono brakujace dane ze wzgledu na doswiadczenie osob, ktore udzielily blednych odpowiedzi:
miss_old_nmus <- filter(miss_old, doswiadczenie == 'brak')
miss_old_mus <- filter(miss_old, doswiadczenie == 'muzyk')

miss_new_nmus <- filter(miss_new, doswiadczenie == 'brak')
miss_new_mus <- filter(miss_new, doswiadczenie == 'muzyk')

# Wyniki podliczono:
miss_num_old <- nrow(miss_old)
miss_num_old_mus <- nrow(miss_old_mus)
miss_num_old_nmus <- nrow(miss_old_nmus)

miss_num_old <- nrow(miss_old)
miss_num_old_mus <- nrow(miss_old_mus)
miss_num_old_nmus <- nrow(miss_old_nmus)

# sprawdzono takze ile osob odpowiadalo blednie
miss_people <- distinct(miss, sluchacz, .keep_all = TRUE) %>% select(sluchacz, doswiadczenie)
miss_people_old <- distinct(miss_old, sluchacz, .keep_all = TRUE) %>% select(sluchacz, doswiadczenie)
miss_people_new <- distinct(miss_new, sluchacz, .keep_all = TRUE) %>% select(sluchacz, doswiadczenie)


# 4e. Na tym etapie pracy z danymi zastanowiono sie, czy warto usunac wyniki. Jest to najbardziej radykalne rozwiazanie, jednak
# prowadzi do sytuacji, ze zawezy nam grupe badanych osob i utrudni nam to dalsza analize.
# Podstanowiono rozwazyc jedna z metod imputacji danych - zastapienie brakujacych danych srednia lub mediana.
# Aby podjac decyzje o wyborze bardziej odpowiedniej metody imputacji danych obliczono zarowno srednia i mediane dla nowej wtyczki
# i starej - bez podzialu na kryteria i doswiadczenie:
# W tym celu stworzono funkcje, ktora umozliwia okreslenie podstawowych wartosci statystyki opisowej:

# ===== Uzywane funkcje ========================================================
# Krotkie podsumowanie wynikow:
podsumowanie <- function(dane) {
  srednia <- round(mean(dane$wynik, na.rm = TRUE), digits = 2)
  uniqv <- unique(dane$wynik)
  moda <- uniqv[which.max(tabulate(match(dane$wynik, uniqv)))]
  
  kwantyle <- quantile(dane$wynik, na.rm = TRUE, probs = c(0.25, 0.5, 0.75), names = FALSE)
  q1 <- kwantyle[1]
  mediana <- kwantyle[2]
  q3 <- kwantyle[3]
  
  odchylenie <- round(sd(dane$wynik, na.rm = TRUE), digits = 2)
  skosnosc <- round(skewness(dane$wynik, na.rm = TRUE), digits = 2)
  wspolczynnik_zmiennosci <- odchylenie/srednia
  
  rozrzut <- dane$wynik
  rozstep_poprawny <- max(rozrzut) - min(rozrzut)
  
  return(data.frame(srednia, moda, mediana, q1, q3, rozstep_poprawny, odchylenie, skosnosc, wspolczynnik_zmiennosci))
}

# ================================================== 

summ_old_imp <- podsumowanie(imp_old_plugin)
summ_new_imp <- podsumowanie(imp_new_plugin)

# Policzono miedzy innymi srednia i mediane dla nowej wtyczki i starej i widac, ze po zaokragleniu srednia jest rowna medianie.
# Jako ocena musi byc podana wartosc calkowita, dlatego wezmiemy mediane z wynikow jako zastapienie wartosci NA.

# 4f. Zastapienie wartosci brakujacych NA mediana obliczona wczesniej.
imp_old_plugin[is.na(imp_old_plugin)] <- summ_old_imp$mediana
imp_new_plugin[is.na(imp_new_plugin)] <- summ_new_imp$mediana
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

# ===== Podzial danych =========================================================
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

# ===== Podsumowanie danych ====================================================
# Ze wzgledu na doswiadczenie
summ_old_nonmusician <- podsumowanie(old_nonmusician)
summ_old_musician <- podsumowanie(old_musician)
summ_new_nonmusician <- podsumowanie(new_nonmusician)
summ_new_musician <- podsumowanie(new_musician)

# Ze wzgledu na doswiadczenie
old_jazz_stats <- podsumowanie(old_jazz)
old_pop_stats <- podsumowanie(old_pop)
old_symf_stats <-podsumowanie(old_symf)

new_jazz_stats <- podsumowanie(new_jazz)
new_pop_stats <- podsumowanie(new_pop)
new_symf_stats <- podsumowanie(new_symf)

# Ze wzgledu na kryterium
ONS <- podsumowanie(old_natural)
OSS <- podsumowanie(old_space)
OFS <- podsumowanie(old_feel)

NNS <-podsumowanie(new_natural)
NSS <-podsumowanie(new_space)
NFS <-podsumowanie(new_feel)

# Ze wzgledu na gatunki zaleznie od doswiadczenia
OJMS <-podsumowanie(old_jazz_musician)

old_jazz_nonmusician_stats <- podsumowanie(old_jazz_nonmusician)
Old_pop_nonmusician_stats <- podsumowanie(old_pop_nonmusician)
old_symf_nonmusician_stats <- podsumowanie(old_symf_nonmusician)

new_jms <- podsumowanie(new_jazz_musician)
new_pms <- podsumowanie(new_pop_musician)
new_sms <- podsumowanie(new_symf_musician)

new_JNMS <- podsumowanie(new_jazz_nonmusician)
new_PNMS <-podsumowanie(new_pop_nonmusician)
new_SNMS <-podsumowanie(new_symf_nonmusician)

# Ze wzgledu na kryterium zaleznie od doswiadczenia
ONMS <-podsumowanie(old_natural_musician)
OSM <-podsumowanie(old_space_musician)
OFM <-podsumowanie(old_feel_musician)

ONNM <- podsumowanie(old_natural_nonmusician)
OSNM <-podsumowanie(old_space_nonmusician)
OFNM <-podsumowanie(old_feel_nonmusician)

NNM <-podsumowanie(new_natural_musician)
NSM <-podsumowanie(new_space_musician)
NFM <-podsumowanie(new_feel_musician)

NNNM <-podsumowanie(new_natural_nonmusician)
NSNM <-podsumowanie(new_space_nonmusician)
NFNM <-podsumowanie(new_feel_nonmusician)


# ===== II. WSTEPNA EKSPLORACJA DANYCH NA PODSTAWIE STATYSTYKI OPISOWEJ ========

# 1. Boxploty
boxplot(old_natural_musician$wynik, old_space_musician$wynik, old_feel_musician$wynik, main="Wyniki starej wersji wtyczki - wrazenie - muzycy", ylab="Wynik", names=c("Naturalnosc", "Przestrzennosc", "Wrazenie"))
boxplot(new_natural_musician$wynik, new_space_musician$wynik, new_feel_musician$wynik, main="Wyniki starej nowej wtyczki - wrazenie - muzycy", ylab="Wynik", names=c("Naturalnosc", "Przestrzennosc", "Wrazenie"))

# 2.1. Porowanie ocen starej wtyczki i nowej bez podzialu na doswiadczenie i kryteria
# 2.1.1. Obliczenie parametrow dla starej i nowej wtyczki
old_plug_stat <- podsumowanie(imp_old_plugin)
new_plug_stat <- podsumowanie(imp_new_plugin)

#2.1.2. Histogram
hist(imp_old_plugin$wynik, main="Wyniki dla starej wersji wtyczki",xlab="Oceny", ylab="Gestosc wystapien", breaks=0.5:5.5, freq= FALSE, prob=TRUE)
hist(imp_new_plugin$wynik, main="Wyniki dla nowej wersji wtyczki",xlab="Oceny", ylab="Gestosc wystapien", breaks=0.5:5.5, freq= FALSE, prob=TRUE)

hist(old_nonmusician$wynik, main="Wyniki dla starej wersji wtyczki - brak doswiadczenia",xlab="Oceny", ylab="Gestosc wystapien", breaks=0.5:5.5, freq= FALSE, prob=TRUE)
hist(old_musician$wynik, main="Wyniki dla starej wersji wtyczki - muzycy",xlab="Oceny", ylab="Gestosc wystapien", breaks=0.5:5.5, freq= FALSE, prob=TRUE)

hist(new_nonmusician$wynik, main="Wyniki dla nowej wersji wtyczki - brak doswiadczenia",xlab="Oceny", ylab="Gestosc wystapien", breaks=0.5:5.5, freq= FALSE, prob=TRUE)
hist(new_musician$wynik, main="Wyniki dla nowej wersji wtyczki - muzycy",xlab="Oceny", ylab="Gestosc wystapien", breaks=0.5:5.5, freq= FALSE, prob=TRUE)

#2.1.3. Boxplot
boxplot(imp_old_plugin$wynik, imp_new_plugin$wynik, main="Wyniki starej i nowej wersji wtyczki", ylab="Wynik", names=c("Stara wtyczka", "Nowa wtyczka"))

boxplot(old_nonmusician$wynik, old_musician$wynik, main="Oceny starej wersji wtyczki - podzial na doswiadczenie", ylab="Wynik", names=c("Brak doswiadczenia", "Muzyk"))
boxplot(new_nonmusician$wynik, new_musician$wynik, main="Oceny nowej wersji wtyczki - podzial na doswiadczenie", ylab="Wynik", names=c("Brak doswiadczenia", "Muzyk"))
#2.1.4 Wykres gestosci
plot(density(imp_old_plugin$wynik), main="Wykres gestosci - stara wtyczka", ylab="Czestotliwosc wystapien")
plot(density(imp_new_plugin$wynik), main="Wykres gestosci - nowa wtyczka", ylab="Czestotliwosc wystapien")

#2.2. Porowanie starej wtyczki i nowej w zaleznosci od doswiadczenia
# Obliczenie szukanych wartosci dla starej wtyczki - muzycy i niemuzycy
old_nonmusician_stats <- podsumowanie(old_nonmusician)
old_musician_stats <- podsumowanie(old_musician)

new_nonmusician_stats <- podsumowanie(new_nonmusician)
new_musician_stats <- podsumowanie(new_musician)

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

# 3. Porownanie gatunkow - ze wzgledu na doswiadczenie i bez wzgledu na doswiadczenie i razem
# 3.1. Boxplot
par(mar = c(7.1, 4.1, 4.1, 2.1))
boxplot(old_natural_musician$wynik, old_space_musician$wynik,
        old_feel_musician$wynik, new_natural_musician$wynik, 
        new_space_musician$wynik, new_feel_musician$wynik,
        main="Wyniki ocen wtyczki - kryterium - muzycy", ylab="Wynik", 
        names=c("naturalnosc\nstara wersja", "przestrzennosc\nstara wersja", "wrazenie\nstara wersja",
                "naturalnosc\nnowa wersja", "przestrzennosc\nnowa wersja", "wrazenie\nnowa wersja"),
        las=2)

par(mar = c(7.1, 4.1, 4.1, 2.1))
boxplot(old_natural_nonmusician$wynik, old_space_nonmusician$wynik,
        old_feel_nonmusician$wynik, new_natural_nonmusician$wynik, 
        new_space_nonmusician$wynik, new_feel_nonmusician$wynik,
        main="Wyniki ocen wtyczki - kryterium - brak doswiadczenia", ylab="Wynik", 
        names=c("naturalnosc\nstara wersja", "przestrzennosc\nstara wersja", "wrazenie\nstara wersja",
                "naturalnosc\nnowa wersja", "przestrzennosc\nnowa wersja", "wrazenie\nnowa wersja"),
        las=2)

# 3.2 Korelacja
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

old_new_jazz_musician_cor <- cor(old_jazz_musician$wynik, new_jazz_musician$wynik)
old_new_pop_musician_cor <- cor(old_pop_musician$wynik, new_pop_musician$wynik)
old_new_symf_musician_cor <- cor(old_symf_musician$wynik, new_symf_musician$wynik)

# 4. Porownanie ze wzgledu na wrazenie - bez podzialu na gatunki
par(mar = c(7.1, 4.1, 4.1, 2.1))
boxplot(old_jazz_musician$wynik, old_pop_musician$wynik,
        old_symf_musician$wynik, new_jazz_musician$wynik, 
        new_pop_musician$wynik, new_symf_musician$wynik,
        main="Wyniki ocen wtyczki - gatunek - muzycy", ylab="Wynik", 
        names=c("jazz\nstara wersja", "pop\nstara wersja", "symfoniczna\nstara wersja",
                "jazz\nnowa wersja", "pop\nnowa wersja", "symfoniczna\nnowa wersja"),
        las=2)

par(mar = c(7.1, 4.1, 4.1, 2.1))
boxplot(old_jazz_nonmusician$wynik, old_pop_nonmusician$wynik,
        old_symf_nonmusician$wynik, new_jazz_nonmusician$wynik, 
        new_pop_nonmusician$wynik, new_symf_nonmusician$wynik,
        main="Wyniki ocen wtyczki - gatunek - brak doswiadczenia", ylab="Wynik", 
        names=c("jazz\nstara wersja", "pop\nstara wersja", "symfoniczna\nstara wersja",
                "jazz\nnowa wersja", "pop\nnowa wersja", "symfoniczna\nnowa wersja"),
        las=2)

old_new_natural_cor <- cor(old_natural$wynik, new_natural$wynik)
old_new_feel_cor <- cor(old_space$wynik, new_space$wynik)
old_new_feel_cor <- cor(old_feel$wynik, new_feel$wynik)

old_natural_space_cor <- cor(old_natural$wynik, old_space$wynik)
old_natural_feel_cor <- cor(old_natural$wynik, old_feel$wynik)
old_feel_space_cor <- cor(old_feel$wynik, old_space$wynik)

new_natural_space_cor <- cor(new_natural$wynik, new_space$wynik)
new_natural_feel_cor <- cor(new_natural$wynik, new_feel$wynik)
new_feel_space_cor <- cor(new_feel$wynik, new_space$wynik)

old_natural_space_musician_cor <- cor(old_natural_musician$wynik, old_space_musician$wynik)
old_natural_feel_musician_cor <- cor(old_natural_musician$wynik, old_feel_musician$wynik)
old_feel_space_musician_cor <- cor(old_feel_musician$wynik, old_space_musician$wynik)

new_natural_space_musician_cor <- cor(new_natural_musician$wynik, new_space_musician$wynik)
new_natural_feel_musician_cor <- cor(new_natural_musician$wynik, new_feel_musician$wynik)
new_feel_space_musician_cor <- cor(new_feel_musician$wynik, new_space_musician$wynik)

old_natural_space_nonmusician_cor <- cor(old_natural_nonmusician$wynik, old_space_nonmusician$wynik)
old_natural_feel_nonmusician_cor <- cor(old_natural_nonmusician$wynik, old_feel_nonmusician$wynik)
old_feel_space_nonmusician_cor <- cor(old_feel_nonmusician$wynik, old_space_nonmusician$wynik)

new_natural_space_nonmusician_cor <- cor(new_natural_nonmusician$wynik, new_space_nonmusician$wynik)
new_natural_feel_nonmusician_cor <- cor(new_natural_nonmusician$wynik, new_feel_nonmusician$wynik)
new_feel_space_nonmusician_cor <- cor(new_feel_nonmusician$wynik, new_space_nonmusician$wynik)

# 5. Podzial - jazz dla wybranego kryterium
old_jazz_natural_space_cor <- cor(old_jazz_natural$wynik, old_jazz_space$wynik)
old_jazz_natural_feel_cor <- cor(old_jazz_natural$wynik, old_jazz_feel$wynik)
old_jazz_feel_space_cor <- cor(old_jazz_feel$wynik, old_jazz_space$wynik)

new_jazz_natural_space_cor <- cor(new_jazz_natural$wynik, new_jazz_space$wynik)
new_jazz_natural_feel_cor <- cor(new_jazz_natural$wynik, new_jazz_feel$wynik)
new_jazz_feel_space_cor <- cor(new_jazz_feel$wynik, new_jazz_space$wynik)

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
old_pop_natural_space_cor <- cor(old_pop_natural$wynik, old_pop_space$wynik)
old_pop_natural_feel_cor <- cor(old_pop_natural$wynik, old_pop_feel$wynik)
old_pop_feel_space_cor <- cor(old_pop_feel$wynik, old_pop_space$wynik)

new_pop_natural_space_cor <- cor(new_pop_natural$wynik, new_pop_space$wynik)
new_pop_natural_feel_cor <- cor(new_pop_natural$wynik, new_pop_feel$wynik)
new_pop_feel_space_cor <- cor(new_pop_feel$wynik, new_pop_space$wynik)

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

# 8 regresja liniowa
# 8.1 Regresja liniowa popu i muzyki symfonicznej
plot(old_pop_musician$wynik, old_symf_musician$wynik,
     xlab = "pop", ylab = "muzyka symfoniczna",
     main = "Oceny popu w zaleznosci od ocen muzyki symfonicznej - stara wersja",
     xlim = c(1, 5), ylim = c(1, 5),
     pch = 3, col = "red")
points(old_pop_nonmusician$wynik, old_symf_nonmusician$wynik, pch = 0, col = "green")
abline(lm(old_pop$wynik~old_symf$wynik))
abline(lm(old_pop_musician$wynik~old_symf_musician$wynik), col = "red")
abline(lm(old_pop_nonmusician$wynik~old_symf_nonmusician$wynik), col = "green")
legend("bottomright", legend = c("ogolem", "muzycy", "brak dosw."), col = c("black", "red", "green"), lty = c(1, 1, 1))

plot(new_pop_musician$wynik, new_symf_musician$wynik,
     xlab = "pop", ylab = "muzyka symfoniczna",
     main = "Oceny popu w zaleznosci od ocen muzyki symfonicznej - nowa wersja",
     xlim = c(1, 5), ylim = c(1, 5),
     pch = 3, col = "red")
points(new_pop_nonmusician$wynik, new_symf_nonmusician$wynik, pch = 0, col = "green")
abline(lm(new_pop$wynik~new_symf$wynik))
abline(lm(new_pop_musician$wynik~new_symf_musician$wynik), col = "red")
abline(lm(new_pop_nonmusician$wynik~new_symf_nonmusician$wynik), col = "green")
legend("bottomright", legend = c("ogolem", "muzycy", "brak dosw."), col = c("black", "red", "green"), lty = c(1, 1, 1))

# 8.2 Regresja liniowa przestrzenności i wrażenia z podziałem na gatunki dla nowej wersji
plot(new_space$wynik, new_feel$wynik, 
     xlab = "przestrzennosc", ylab = "wrazenie",
     main = "Oceny przestrzennosci w zaleznosci od wrazenia - ogolem")
abline(lm(new_space$wynik~new_feel$wynik))

plot(new_jazz_space$wynik, new_jazz_feel$wynik, 
     xlab = "przestrzennosc", ylab = "wrazenie", 
     main = "Oceny przestrzennosci w zaleznosci od wrazenia - gatunkami",
     xlim = c(1, 5), ylim = c(1, 5),
     pch = 3, col = "red")
points(new_pop_space$wynik, new_pop_feel$wynik, pch = 0, col = "green")
points(new_symf_space$wynik, new_symf_feel$wynik, pch = 2, col = "blue")
abline(lm(new_jazz_space$wynik~new_jazz_feel$wynik), col = "red")
abline(lm(new_pop_space$wynik~new_pop_feel$wynik), col = "green")
abline(lm(new_symf_space$wynik~new_symf_feel$wynik), col = "blue")
legend("bottomleft", legend = c("jazz", "pop", "symfoniczna"), col = c("red", "green", "blue"), lty = c(1, 1, 1))

# 8.3 Regresja liniowa przestrzenności i wrażenia dla popu i muzyki symfonicznej dla starej wersji
plot(old_pop_space_musician$wynik, old_pop_feel_musician$wynik, 
     xlab = "przestrzennosc", ylab = "wrazenie",
     main = "Oceny przestrzennosci w zaleznosci od wrazenia - pop",
     xlim = c(1, 5), ylim = c(1, 5),
     pch = 3, col = "red")
points(old_pop_space_nonmusician$wynik, old_pop_feel_nonmusician$wynik, pch = 0, col = "green")
abline(lm(old_pop_space$wynik~old_pop_feel$wynik))
abline(lm(old_pop_space_musician$wynik~old_pop_feel_musician$wynik), col = "red")
abline(lm(old_pop_space_nonmusician$wynik~old_pop_feel_nonmusician$wynik), col = "green")
legend("bottomright", legend = c("ogolem", "muzycy", "brak dosw."), col = c("black", "red", "green"), lty = c(1, 1, 1))

plot(old_symf_space_musician$wynik, old_symf_feel_musician$wynik, 
     xlab = "przestrzennosc", ylab = "wrazenie",
     main = "Oceny przestrzennosci w zaleznosci od wrazenia - symfoniczna",
     xlim = c(1, 5), ylim = c(1, 5),
     pch = 3, col = "red")
points(old_symf_space_nonmusician$wynik, old_symf_feel_nonmusician$wynik, pch = 0, col = "green")
abline(lm(old_symf_space$wynik~old_symf_feel$wynik))
abline(lm(old_symf_space_musician$wynik~old_symf_feel_musician$wynik), col = "red")
abline(lm(old_symf_space_nonmusician$wynik~old_symf_feel_nonmusician$wynik), col = "green")
legend("bottomright", legend = c("ogolem", "muzycy", "brak dosw."), col = c("black", "red", "green"), lty = c(1, 1, 1))

# 8.4 Regresja liniowa naturalności i przestrzenności dla jazzu
plot(old_jazz_natural_musician$wynik, old_jazz_space_musician$wynik,
     xlab = "naturalnosc", ylab = "przestrzennosc",
     main = "Oceny naturalnosci w zaleznosci od przestrzennosci - jazz - stara wersja",
     xlim = c(1, 5), ylim = c(1, 5),
     pch = 3, col = "red")
points(old_jazz_natural_nonmusician$wynik, old_jazz_space_nonmusician$wynik, pch = 0, col = "green")
abline(lm(old_jazz_natural$wynik~old_jazz_feel$wynik))
abline(lm(old_jazz_natural_musician$wynik~old_jazz_feel_musician$wynik), col = "red")
abline(lm(old_jazz_natural_nonmusician$wynik~old_jazz_feel_nonmusician$wynik), col = "green")
legend("bottomright", legend = c("ogolem", "muzycy", "brak dosw."), col = c("black", "red", "green"), lty = c(1, 1, 1))

plot(new_jazz_natural_musician$wynik, new_jazz_space_musician$wynik,
     xlab = "naturalnosc", ylab = "przestrzennosc",
     main = "Oceny naturalnosci w zaleznosci od przestrzennosci - jazz - nowa wersja",
     xlim = c(1, 5), ylim = c(1, 5),
     pch = 3, col = "red")
points(new_jazz_natural_nonmusician$wynik, new_jazz_space_nonmusician$wynik, pch = 0, col = "green")
abline(lm(new_jazz_natural$wynik~new_jazz_feel$wynik))
abline(lm(new_jazz_natural_musician$wynik~new_jazz_feel_musician$wynik), col = "red")
abline(lm(new_jazz_natural_nonmusician$wynik~new_jazz_feel_nonmusician$wynik), col = "green")
legend("bottomright", legend = c("ogolem", "muzycy", "brak dosw."), col = c("black", "red", "green"), lty = c(1, 1, 1))

# ===== Wnioskowanie statystyczne ==============================================
# Analiza wariancji dla roznych gatunkow muzyki w zaleznosci od doswiadczenia
# Test Shapiro-Wilka w celu sprawdzenia normalnosci rozkladu
shapiro_old_jazz_musician <- shapiro.test(old_jazz_musician$wynik)
shapiro_new_jazz_musician <- shapiro.test(new_jazz_musician$wynik)
shapiro_old_pop_musician <- shapiro.test(old_pop_musician$wynik)
shapiro_new_pop_musician <- shapiro.test(new_pop_musician$wynik)
shapiro_old_symf_musician <- shapiro.test(old_symf_musician$wynik)
shapiro_new_symf_musician <- shapiro.test(new_symf_musician$wynik)
shapiro_old_jazz_nonmusician <- shapiro.test(old_jazz_nonmusician$wynik)
shapiro_new_jazz_nonmusician <- shapiro.test(new_jazz_nonmusician$wynik)
shapiro_old_pop_nonmusician <- shapiro.test(old_pop_nonmusician$wynik)
shapiro_new_pop_nonmusician <- shapiro.test(new_pop_nonmusician$wynik)
shapiro_old_symf_nonmusician <- shapiro.test(old_symf_nonmusician$wynik)
shapiro_new_symf_nonmusician <- shapiro.test(new_symf_nonmusician$wynik)
