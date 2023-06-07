# JPP Zadanie 2

Link do repo: 
https://github.com/aderylo/jpp-zad2

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

---

## Opis języka 

Bez specjalnych udziwnień oraz ze szczególną uwagą na ułatwienie sobie życia na MRJP,  język składnią będzie łudząco przypomniał Latte. (https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2012/Latte/) Z tego też powodu, część przykładów jest luźno zapożyczona z testów kompilatora Latte udostępnionych pod wyżej podanym linkiem.

Z mniej oczywistych spraw, uważam, że jakąś wartość ma zaimplementowanie obiektowości, głównie ze względu na jej powszechność w świecie języków programowania. Tutaj, gdy mowie o obiektowości mam na myśli istnienie klas, a nie prototypów, chociaż to może się zmienić w trakcie implementacji i przy zbliżającej się perspektywie spędzenia majówki w namiocie z laptopem. 

Mój obecny zamysł, jeżeli chodzi o klasy, jest taki, aby miały atrybuty, metody oraz wstępnie pozwalały na dziedziczenie, ale bez nadpisywania. 
Niemniej, jednak jeśli czas na to pozwoli, mam zamiar implementacji bardziej normalnego i użytecznego dziedziczenia z możliwością nadpisywania metod.

Co więcej, język nie będzie czysto object-oriented, ponieważ zamierzam najpierw zaimplementować typy prymitywne (int, string, bool, void) a dopiero po zrealizowaniu podstaw dopisywać obiektowość. Dlatego też mimo pójścia w stronę OOP nie wszystko będzie obiektem. 


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
  
  12. (4) (statyczne typowanie) +/-
  13. (2) (funkcje zagnieżdżone ze statycznym wiązaniem) -
  14. (1/2) (rekordy/listy/tablice/tablice wielowymiarowe) +
  15. (2) (krotki z przypisaniem) -
  16. (1) (break, continue) +
  17. (4) (funkcje wyższego rzędu, anonimowe, domknięcia) -
  18. (3) (generatory) -

  Moje dodatkowe:

  19. (8?) Klasy i obiekty
  20. (8?) Garbage collection done!

Razem: 27 + 16? 

Nie mam pojęcia jak wycenić 19 oraz 20 więc na wszelki wypadek szacuje od góry, a przynajmniej mam taką nadzieje ;)

---

# Kompilacja

```
cd app
ghc Main.hs -outputdir build
```

# Użycie
```
 ./Main ../examples/printing.lat
```

# Zrobione feature'y:
Na 15 punktów: 
  01. (trzy typy) -> examples/printing.lat
  02. (literały, arytmetyka, porównania) -> examples/while.lat
  03. (zmienne, przypisanie) -> examples/core.lat
  04. (print) + -> examples/printing.lat
  05. (while, if) -> examples/core.lat
  06. (funkcje lub procedury, rekurencja) -> examples/core.lat
  07. (przez zmienną / przez wartość / in/out) -> examples/core.lat
  08. (zmienne read-only i pętla for) - 

  Na 20 punktów:

  09. (przesłanianie i statyczne wiązanie) -> examples/scope.lat
  10. (obsługa błędów wykonania) -> examples/errors.lat
  11. (funkcje zwracające wartość) -> examples/fun.lat

  Na 30 punktów: 

  14. (1/2) (rekordy/listy/tablice/tablice wielowymiarowe) -> examples/arr.lat

  20. Garbage collection -> examples/gc.lat

