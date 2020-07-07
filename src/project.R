# ==================================================
# Statystyka Inżynierska
# Damian Rynczak & Bartłomiej Piekarz
# Projekt - 12.07.2020r.
# ==================================================

# 1.Import pakiet�w:
library(dplyr)

# 2. Wczytanie danych do środowiska programistycznego
dane <- read.csv(file.choose(), header=TRUE)  
# Funkcja file.choose() umożliwia interaktywny wybór danych z folderu, w którym znajduje się projekt lub innego miejsca na dysku.

klasy_ilosciowe = sapply(dane, class) == "numeric" | sapply(dane,class) == "integer"
klasy_jakosciowe = sapply(dane,class) == "factor"
cechy_ilosciowe = names(which(klasy_ilosciowe)) # Zwraca wektor z nazwami cech
cechy_jakosciowe = names(which(klasy_jakosciowe))

summary(dane)
# Po wyodr�bnieniu cech jako�ciowych i ilo�ciowych zauwa�ono, �e:
# a. Cechy ilo�ciowe to: liczba porz�dkowa, s�uchacz (numer s�uchacza) i wynik - analiza cech jako�ciowych b�dzie dotyczy�a tylko
# wyniku, z uwagi na fakt, i� numer s�uchacza i liczba porz�dkowa nie odwzorowuj� rzeczywistych rezulat�w, a w R zosta�y przyporz�dkowane
# do cech ilo�ciowych z uwagi na to, �e s� to warto�ci numeric lub integer.
# Cechy jako�ciowe opisuj� przynale�no�� do grup np. wykszta�cenie (grupy: podstawowe, �rednie, wy�sze), p�e� (grupy: kobieta, m�czyzna), kraj
# ...zamieszkania (grupy: Polska) - u nas cechy jako�ciowe to najpewniej muzyk/brak i gatunek.

# ===== STATYSTYKA OPISOWA =====
# Statystyka opisowa zajmuje si� wst�pnym opracowaniem
# wynik�w pomiar�w (pr�bki) bez pos�ugiwania si� rachunkiem
# prawdopodobie�stwa. Nie wyci�gamy wniosk�w dotycz�cych
# populacji generalnej.
# NIE WYCI�GAMY WNIOSK�W DOTYCZ�CYCH POPULACJI GENERALNEJ - wi�c pewnie tutaj odpowiednie b�d� komentarze typu:
# �rednia arytmetyczna badanej grupy os�b b�d�cych muzykami...
# �rednia badanej grupy os�b bez do�wiadczenia muzycznego... itd.

# WYMAGANIA DO PROJEKTU:
# - odpowiednie histogramy
# - odpowiednie wykresy pude�kowe
# - analiza wyst�powania obserwacji odstaj�cych (uwaga! wyst�powanie obserwacji odstaj�cych determinuje dalsze dzia�ania)
# - warto�ci �rednie, kwartyle, mody, odchylenia standardowe, wsp�czynnik zmienno�ci, rozst�p
# -  korelacja liniowa
# - regresja liniowa


# Statystyka opisowa dotyczy cech ILO�CIOWYCH, dla niej stosujemy:
# - mean()
# - median()
# - min() - jaka by�a najni�sza ocena dla nowej i starej wtyczki?
# - czy ni�sza ocena by�a przyznana przez muzyk�w?
# - max()
# - range()
# - summary()
# - quantile()
# - sd()
# ? Czy muzycy maj� bardziej zbli�one odpowiedzi do siebie?
# ? Czy mo�na powiedzie�, �e nie muzycy odpowiadaj� bardziej przypadkowo?
# ? Jak� ocen� przyzna�o najwi�cej os�b?
# ? Jaka ocena jest najcz�ciej wyst�puj�ca Wsr�d muzyk�w?
# ? Jaki procent oceny stanowi� oceny muzyk�w?
# ? W jakich przypadkach wyst�pi�y warto�ci odstaj�ce?
# - var()

# Odpowiedzie� na pytanie sk�d si� bior� ewentualne r�nice
# pomi�dzy �redni� i median� danej cechy.
# Po sprawdzeniu wynik�w dla og�lnie starej i nowej wtyczki mo�emy
# powiedzie�, �e:
# - Mo�na wyszczeg�lni� podgrupy, w kt�rych statystyka opisowa
# spe�ni lepiej swoj� rol�. I tutaj zrobi� te �rednie
# dla poszczeg�lnych grup.

# SPRAWDZENIE, CZY S� OBSERWACJE BRAKUJ�CE.

# Cecha ilo�ciowa:
# a. Wykres zale�no�ci �redniej ocen w zale�no�ci od gatunku
# (bez podzia�u na muzyk�w i nie muzyk�w)
# b. Wykres ocen w zale�no�ci od muzyk�w i nie muzyk�w bez podzia�u
# na kategorie 
# c. reszta, o ktorej wczoraj mowili�my

# Wykres PUDE�KOWY
# PRZED OBLICZENIAMI - usuwamy warto�ci odstaj�ce
# rysujemy wykresy pude�kowe
# W�sy odpowiadaj� kwantylom rz�du odpowiednio 0.125 oraz 0.875.

# rysujemy histogramy - ale dla cech ilo�ciowych!

# Najliczniejsza pr�ba:
# # Najliczniejsza proba:
# t = table(dane2$habitat)
# names(which.max(t)) # CZY WYNIK JEST POPRAWNY? NIE! Tutaj szuka do momentu znalezienia pierwszego maksimum.
# To nam zwroci tylko pierwszy indeks, dla ktorego znajdzie maksimum
# Aby wynik byl poprawny robimy tak:
# x = which(t==max(t))
# names(x)

# Wtedy zwroci 3 wartosci i bedzie super

# ? Jak� ocen� przyzna�o najwiecej os�b?
# W R nie ma zaimplementowanej gotowej dominanty
# Przyklad:
# auta_mode <- table(auta$Cena)
# mode_a = names(auta_mode)[which(auta_mode==max(auta_mode))]
# print(mode_a)


# Tutaj mo�emy te� doda� wnioski:
# - Sprawdzi� ile os�b dla starej wtyczki da�o ocen� ni�sz� ni� �rednia
# - Ile z tych os�b by�o muzykami?
# - Czy ocena dla starej wtyczki by�a kiedy� wy�sza ni� dla nowej?
# - Czy zawsze muzycy daj� wy�sze warto�ci nowej wtyczce, a gorsze starej?
# TYLKO to jest statystyka opisowa, wi�c nie mo�emy m�wi� og�lnie o muzykach.
# Dobrze bedzie m�wi�, �e w badanej grupie os�b...

# 1. Policzy� �redni� dla starej wtyczki - normalnie �redni� z ca�o�ci bez wzgl�du na muzyka czy niemuzyka, bez wzgl�du na wra�enie
# 1a. Policzy� median�, kwantyle i inne te g�upoty 
# 1b. Policzy� odchylenie standardowe 
# 2. Policzy� �redni� dla nowej wtyczki tak samo
# 2a. To, co dla starej 
# 2b. To, co dla starej 
# Tutaj ju� b�dziemy mieli jakie� wnioski typu, �e �rednia jest wy�sza dla nowej wtyczki, odchylenie jest takie i takie, co sugeruje, �e s� du�e rozbie�no�ci wynikaj�ce z r�nego do�wiadczenia itd.
# Do kwantyli damy co� takiego, �e tyle i tyle os�b da�o ocen� r�wn� b�d� ni�sz� od jakiej� i zamkniemy wst�pn� analiz�.
# P�niej zrobi�bym tak:
# 3. Podzieli� s�uchaczy na muzyk�w i nie muzyk�w, policzy� zn�w �redni� dla nowej wtyczki i starej i zobaczy� co i jak.
# Do tego odchylenie, mediany itd. i b�dziemy wiedzieli, kt�ra grupa ma jaki rozrzut warto�ci i te� ju� jakie� wnioski.
# 4. P�niej zrobi�bym to samo dla gatunku osobno 
# 5. P�niej to samo dla wra�enia, trzy r�ne kategorie

# ================ DRUGA CZʌ� ===================
# WYMAGANIA:
# - dwuczynnikowa analiza wariancji, analiza post hoc, analiza interakcji
# - wnioskowanie o korelacji
# - wnioskowanie o wsp�czynnikach regresji liniowej


# 1. Mozna sprobowac znalezc srednia ocene dla calej grupy muzyk�w i nie muzyk�w:
# -estymacja przedzia�owa (instrukcja 3) - stworzy� przedzia� w jakim znajduje si� �rednia ocena
# 2. Sprawdzi� rozk�ad danych
# 3. Weryfikacja hipotezy, czy wykszta�cenie wp�Ywa na ocen� (test na dwie populacje)
# 6. P�niej zbadamy wp�yw tego, czy przeszkolenie wp�ywa na oceny 
# 7. Nast�pnie to, czy rodzaj muzyki wp�ywa na oceny dla muzyk�w i niemuzyk�w
# 8. 8. P�niej zobaczymy, czy jest jaki� zwi�zek pomi�dzy muzykami i niemuzykami je�li chodzi o wra�enia
# 9. Czy rodzaj muzyki jest powi�zany z ocenami
# 10. Badanie czy gatunek ma wp�yw na ocen� Albo wykszta�cenie - To teoretycznie by�my mieli oceny zwyk�ych i muzyk�w
# I zobaczy� czy jest faktycznie zwi�zek czy nie
# 11. Okre�li� charakter rozk�adu