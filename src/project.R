# ==================================================
# Statystyka In≈ºynierska
# Damian Rynczak & Bart≈Çomiej Piekarz
# Projekt - 12.07.2020r.
# ==================================================

# 1.Import pakietÛw:
library(dplyr)

# 2. Wczytanie danych do ≈õrodowiska programistycznego
dane <- read.csv(file.choose(), header=TRUE)  
# Funkcja file.choose() umo≈ºliwia interaktywny wyb√≥r danych z folderu, w kt√≥rym znajduje siƒô projekt lub innego miejsca na dysku.

klasy_ilosciowe = sapply(dane, class) == "numeric" | sapply(dane,class) == "integer"
klasy_jakosciowe = sapply(dane,class) == "factor"
cechy_ilosciowe = names(which(klasy_ilosciowe)) # Zwraca wektor z nazwami cech
cechy_jakosciowe = names(which(klasy_jakosciowe))

summary(dane)
# Po wyodrÍbnieniu cech jakoúciowych i iloúciowych zauwaøono, øe:
# a. Cechy iloúciowe to: liczba porzπdkowa, s≥uchacz (numer s≥uchacza) i wynik - analiza cech jakoúciowych bÍdzie dotyczy≥a tylko
# wyniku, z uwagi na fakt, iø numer s≥uchacza i liczba porzπdkowa nie odwzorowujπ rzeczywistych rezulatÛw, a w R zosta≥y przyporzπdkowane
# do cech iloúciowych z uwagi na to, øe sπ to wartoúci numeric lub integer.
# Cechy jakoúciowe opisujπ przynaleønoúÊ do grup np. wykszta≥cenie (grupy: podstawowe, úrednie, wyøsze), p≥eÊ (grupy: kobieta, mÍøczyzna), kraj
# ...zamieszkania (grupy: Polska) - u nas cechy jakoúciowe to najpewniej muzyk/brak i gatunek.

# ===== STATYSTYKA OPISOWA =====
# Statystyka opisowa zajmuje siÍ wstÍpnym opracowaniem
# wynikÛw pomiarÛw (prÛbki) bez pos≥ugiwania siÍ rachunkiem
# prawdopodobieÒstwa. Nie wyciπgamy wnioskÛw dotyczπcych
# populacji generalnej.
# NIE WYCI•GAMY WNIOSK”W DOTYCZ•CYCH POPULACJI GENERALNEJ - wiÍc pewnie tutaj odpowiednie bÍdπ komentarze typu:
# årednia arytmetyczna badanej grupy osÛb bÍdπcych muzykami...
# årednia badanej grupy osÛb bez doúwiadczenia muzycznego... itd.

# WYMAGANIA DO PROJEKTU:
# - odpowiednie histogramy
# - odpowiednie wykresy pude≥kowe
# - analiza wystÍpowania obserwacji odstajπcych (uwaga! wystÍpowanie obserwacji odstajπcych determinuje dalsze dzia≥ania)
# - wartoúci úrednie, kwartyle, mody, odchylenia standardowe, wspÛ≥czynnik zmiennoúci, rozstÍp
# -  korelacja liniowa
# - regresja liniowa


# Statystyka opisowa dotyczy cech ILOåCIOWYCH, dla niej stosujemy:
# - mean()
# - median()
# - min() - jaka by≥a najniøsza ocena dla nowej i starej wtyczki?
# - czy niøsza ocena by≥a przyznana przez muzykÛw?
# - max()
# - range()
# - summary()
# - quantile()
# - sd()
# ? Czy muzycy majπ bardziej zbliøone odpowiedzi do siebie?
# ? Czy moøna powiedzieÊ, øe nie muzycy odpowiadajπ bardziej przypadkowo?
# ? Jakπ ocenÍ przyzna≥o najwiÍcej osÛb?
# ? Jaka ocena jest najczÍúciej wystÍpujπca WsrÛd muzykÛw?
# ? Jaki procent oceny stanowiπ oceny muzykÛw?
# ? W jakich przypadkach wystπpi≥y wartoúci odstajπce?
# - var()

# OdpowiedzieÊ na pytanie skπd siÍ biorπ ewentualne rÛønice
# pomiÍdzy úredniπ i medianπ danej cechy.
# Po sprawdzeniu wynikÛw dla ogÛlnie starej i nowej wtyczki moøemy
# powiedzieÊ, øe:
# - Moøna wyszczegÛlniÊ podgrupy, w ktÛrych statystyka opisowa
# spe≥ni lepiej swojπ rolÍ. I tutaj zrobiÊ te úrednie
# dla poszczegÛlnych grup.

# SPRAWDZENIE, CZY S• OBSERWACJE BRAKUJ•CE.

# Cecha iloúciowa:
# a. Wykres zaleønoúci úredniej ocen w zaleønoúci od gatunku
# (bez podzia≥u na muzykÛw i nie muzykÛw)
# b. Wykres ocen w zaleønoúci od muzykÛw i nie muzykÛw bez podzia≥u
# na kategorie 
# c. reszta, o ktorej wczoraj mowiliúmy

# Wykres PUDE£KOWY
# PRZED OBLICZENIAMI - usuwamy wartoúci odstajπce
# rysujemy wykresy pude≥kowe
# Wπsy odpowiadajπ kwantylom rzÍdu odpowiednio 0.125 oraz 0.875.

# rysujemy histogramy - ale dla cech iloúciowych!

# Najliczniejsza prÛba:
# # Najliczniejsza proba:
# t = table(dane2$habitat)
# names(which.max(t)) # CZY WYNIK JEST POPRAWNY? NIE! Tutaj szuka do momentu znalezienia pierwszego maksimum.
# To nam zwroci tylko pierwszy indeks, dla ktorego znajdzie maksimum
# Aby wynik byl poprawny robimy tak:
# x = which(t==max(t))
# names(x)

# Wtedy zwroci 3 wartosci i bedzie super

# ? Jakπ ocenÍ przyzna≥o najwiecej osÛb?
# W R nie ma zaimplementowanej gotowej dominanty
# Przyklad:
# auta_mode <- table(auta$Cena)
# mode_a = names(auta_mode)[which(auta_mode==max(auta_mode))]
# print(mode_a)


# Tutaj moøemy teø dodaÊ wnioski:
# - SprawdziÊ ile osÛb dla starej wtyczki da≥o ocenÍ niøszπ niø úrednia
# - Ile z tych osÛb by≥o muzykami?
# - Czy ocena dla starej wtyczki by≥a kiedyú wyøsza niø dla nowej?
# - Czy zawsze muzycy dajπ wyøsze wartoúci nowej wtyczce, a gorsze starej?
# TYLKO to jest statystyka opisowa, wiÍc nie moøemy mÛwiÊ ogÛlnie o muzykach.
# Dobrze bedzie mÛwiÊ, øe w badanej grupie osÛb...

# 1. PoliczyÊ úredniπ dla starej wtyczki - normalnie úredniπ z ca≥oúci bez wzglÍdu na muzyka czy niemuzyka, bez wzglÍdu na wraøenie
# 1a. PoliczyÊ medianÍ, kwantyle i inne te g≥upoty 
# 1b. PoliczyÊ odchylenie standardowe 
# 2. PoliczyÊ úredniπ dla nowej wtyczki tak samo
# 2a. To, co dla starej 
# 2b. To, co dla starej 
# Tutaj juø bÍdziemy mieli jakieú wnioski typu, øe úrednia jest wyøsza dla nowej wtyczki, odchylenie jest takie i takie, co sugeruje, øe sπ duøe rozbieønoúci wynikajπce z rÛønego doúwiadczenia itd.
# Do kwantyli damy coú takiego, øe tyle i tyle osÛb da≥o ocenÍ rÛwnπ bπdü niøszπ od jakiejú i zamkniemy wstÍpnπ analizÍ.
# PÛüniej zrobi≥bym tak:
# 3. Podzieli≥ s≥uchaczy na muzykÛw i nie muzykÛw, policzy≥ znÛw úredniπ dla nowej wtyczki i starej i zobaczy≥ co i jak.
# Do tego odchylenie, mediany itd. i bÍdziemy wiedzieli, ktÛra grupa ma jaki rozrzut wartoúci i teø juø jakieú wnioski.
# 4. PÛüniej zrobi≥bym to samo dla gatunku osobno 
# 5. PÛüniej to samo dla wraøenia, trzy rÛøne kategorie

# ================ DRUGA CZ å∆ ===================
# WYMAGANIA:
# - dwuczynnikowa analiza wariancji, analiza post hoc, analiza interakcji
# - wnioskowanie o korelacji
# - wnioskowanie o wspÛ≥czynnikach regresji liniowej


# 1. Mozna sprobowac znalezc srednia ocene dla calej grupy muzykÛw i nie muzykÛw:
# -estymacja przedzia≥owa (instrukcja 3) - stworzyÊ przedzia≥ w jakim znajduje siÍ úrednia ocena
# 2. SprawdziÊ rozk≥ad danych
# 3. Weryfikacja hipotezy, czy wykszta≥cenie wp≥Ywa na ocenÍ (test na dwie populacje)
# 6. PÛüniej zbadamy wp≥yw tego, czy przeszkolenie wp≥ywa na oceny 
# 7. NastÍpnie to, czy rodzaj muzyki wp≥ywa na oceny dla muzykÛw i niemuzykÛw
# 8. 8. PÛüniej zobaczymy, czy jest jakiú zwiπzek pomiÍdzy muzykami i niemuzykami jeúli chodzi o wraøenia
# 9. Czy rodzaj muzyki jest powiπzany z ocenami
# 10. Badanie czy gatunek ma wp≥yw na ocenÍ Albo wykszta≥cenie - To teoretycznie byúmy mieli oceny zwyk≥ych i muzykÛw
# I zobaczyÊ czy jest faktycznie zwiπzek czy nie
# 11. OkreúliÊ charakter rozk≥adu
