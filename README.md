# jpp-zad2

Zadanie: 
1. Deklaracja języka do implementacji – do czwartku 6 kwietnia 2023.
Należy oddać (przez Moodle'a) dokument zawierający opis języka wybranego do implementacji.
Format pliku: PDF, płaski tekst lub ZIP zawierający pliki wspomnianego typu. Prosimy o nazwy
plików postaci imie_nazwisko.pdf. Zawarte mają być:

    a)  gramatyka języka – można ją podać w notacji EBNF (szczególnie zalecane osobom chcącym
    skorzystać z BNFC) lub w dowolnej rozsądnej postaci „na papierze” (można zacząć od
    pewnego poziomu abstrakcji – nie definiować literałów, identyfikatorów itp.),

    B.  kilka przykładowych programów ilustrujących planowane konstrukcje składniowe,

    c) tekstowy opis języka z podkreśleniem nietypowych konstrukcji; uwaga! nie opisujemy rzeczy
    oczywistych, np. w przypadku zapożyczeń ze znanych języków jak Pascal, C, Haskell
    wystarczy je wymienić, bez dokładnego opisu

    d) wypełnioną odpowiednią tabelkę cech (dołączone są do treści zadania) oraz łączną liczbę
    punktów, jaką autor spodziewa się dostać za interpreter, o ile poprawnie zaimplementuje całą
    podaną funkcjonalność – patrz (dużo) niżej.

Za niewykonanie w terminie tego etapu zadania od oceny końcowej zostanie odjętych 8
punktów, a za wykonanie niepełne (np. brak gramatyki) do 8 punktów.
W odpowiedzi na deklarację języka sprawdzający potwierdzi (lub nie :) maksymalną ocenę, jaką
można uzyskać za poprawne zrealizowanie takiego języka. Ostateczny zakres projektu może
jednak jeszcze zostać zmieniony w porozumieniu ze sprawdzającym.


## Opis języka 




## Tabelka cech
  Na 15 punktów: 
  01. (trzy typy) +
  02. (literały, arytmetyka, porównania) + 
  03. (zmienne, przypisanie) + 
  04. (print) + 
  05. (while, if) + 
  06. (funkcje lub procedury, rekurencja) + 
  07. (przez zmienną / przez wartość / in/out) + bez in/out 
  08. (zmienne read-only i pętla for) - 

  Na 20 punktów:

  09. (przesłanianie i statyczne wiązanie) + 
  10. (obsługa błędów wykonania) + 
  11. (funkcje zwracające wartość) +

  Na 30 punktów:
  
  12. (4) (statyczne typowanie) +
  13. (2) (funkcje zagnieżdżone ze statycznym wiązaniem) -
  14. (1/2) (rekordy/listy/tablice/tablice wielowymiarowe) +
  15. (2) (krotki z przypisaniem) -
  16. (1) (break, continue) +
  17. (4) (funkcje wyższego rzędu, anonimowe, domknięcia) -
  18. (3) (generatory) -

  Moje dodatkowe:

  19. (8?) Klasy i obiekty
  20. (8?) Garbage collection

Razem: 27 + 16? 

Nie mam pojęcia jak wycenić 19 oraz 20 więc na wszelki wypadek szacuje od góry, a przynajmniej mam taką nadzieje ;)
