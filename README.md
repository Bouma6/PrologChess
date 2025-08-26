# Užívateľská dokumentácia
## Čo program robí
Nájde vynútený mat bielym v najviac N ťahoch bieleho.(Aj keby čierny hral najlepšiu obranu, program overí, že biely dokáže v rámci limitu doraziť do šach-matu.)
Ak riešenie existuje, vypíše postupnosť ťahov.

## Vstupné dáta (pozície)
- Políčka šachovnice sú súradnice (Col, Row) s rozsahom 1..8 (1 = „a“, 8 = „h“).
- Figúra: piece(Type, Color, (Col,Row))
  - Type: king | queen | rook | bishop | knight | pawn
  - Color: white | black
- Stav sa predáva ako zoznam týchto ```piece/3.```

## Ako program používať

### Nájsť a vypísať celú matovú postupnosť

```
?- solve(Path, MaxWhiteMoves, InitialPieces).
```
- MaxWhiteMoves – maximálny počet ťahov bieleho . „Mat v 3“ znamená 3 biele ťahy.
- Path – unifikuje sa so zoznamom stavov od začiatku po finálny mat (pre zobrazenie sa použije vstavaný výpis).
Pri úspechu vytlačí sekvenciu ťahov vo formáte:
```
white moves rook from (3,4) to (3,8)
black moves king from (5,8) to (4,8)
white moves rook from (3,8) to (4,8)
```

### Príklady dotazov
```
?- solve(Path, 2, [
     piece(king, white, (5,6)),
     piece(rook, white, (3,4)),
     piece(king, black, (5,8))
   ]).
```

Ak riešenie neexistuje program vypíše false lebo nenájde žiadne riešenie.
Ak jednoznačné že hra skončí remízou - biely na začiatku neni v šachu a nemože sa pohnúť/na šachovnici sú iba figúrky ktoré nevedia vynútiť šach mat bude vypísane že hra skončí remízou.
Pokial ale je pat vynútení až po viacerých tahoch tak program jednoducho vypíše false(vynútený mat nenájedný).
Program podporuje promócie pešiakov a rošády


# Programátorská dokumentácia
## Architektúra a reprezentácia
- Stav: ```state(PlayerToMove, Pieces)```
- Figúry: ```piece(Type, Color, (Col,Row))```
- Pohyb:
  - ```figure_move/5``` – legálne ťahy (rešpektujú pravidlá pohybu, obsadenie cieľa vlastnou farbou sa filtruje neskôr).
  - ```move/2``` – generuje legálne ťahy: vykoná presun + prípadné braní, overí, že vlastný kráľ nie je v šachu po ťahu.
- Šach/Mat:
  - ```in_check/2``` – kráľ danej farby je napadnutý.
  - ```is_mate/1``` – čierny je v šachu a nemá žiadny legálny ťah.
## Vynútený mat v N (hĺbkovo limitovaný)
- Vstup: solve/3 s MaxWhiteMoves = počet zostávajúcich bielych ťahov.
- Kľúčové predikáty:
  - ```forced_mate_white(N, StateWhite, Visited, Path)```:
    - Skúša biely ťah, konzumuje 1 z``` N```.
    - Ak po bielom ťahu je mat, úspech vyžaduje ```N > 0```.
    - Inak vygeneruje všetky čierne odpovede a overí, že pre každú existuje pokračovanie do matu v ```N-1```.
    - Pre konštrukciu konkrétnej línie vyberie ľubovoľnú (prvú) čiernu odpoveď (keďže všetky sú prehrávajúce).
  - ```all_responses_win/3``` univerzálny kvantifikátor nad čiernymi odpoveďami.





 <img src="Screenshot 2025-08-24 at 14.41.30.png" alt="App Screenshot" width="1600"/>
<img src="Screenshot 2025-08-24 at 14.41.48.png" alt="App Screenshot" width="1600"/>
<img src="Screenshot 2025-08-24 at 14.41.42.png" alt="App Screenshot" width="1600"/>
<img src="Screenshot 2025-08-24 at 14.41.53.png" alt="App Screenshot" width="1600"/>
