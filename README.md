# Spielanleitung

Tierisch VERZWICKT

Aus den 9 Karten ist ein Quadrat zu bilden. Dabei ist darauf zu achten, dass Ober- und Unterteil der Tiere zusammengehören. Also keine Schweine-guine oder ähnliche Fabelwesen bilden. Viel Vergnügen.

## Kartenbeschreibung

Jede quadratische Karte hat vier Seiten mit je einer Tierhälfte (oben/unten) von einem der vier Tiere: Schwein, Katze, Maus oder Pinguin.

(In der Mitte jeder Karte befindet sich ein Mäuschen, das entweder gerade (auf den ersten 4 Karten) oder schräg (auf den restlichen 5 Karten) ausgerichtet ist. Die Beschreibung jeder Karte erfolgt bei "gerader" Ausrichtung der Maus, im Uhrzeigersinn, beginnend mit "oben".)

| ID | Maus | Oben | Rechts | Unten | Links |
|----|------|------|--------|--------|-------|
| 0  | gerade | Schwein(o) | Katze(u)   | Maus(o)    | Pinguin(u) |
| 1  | gerade | Katze(u)   | Schwein(o) | Maus(o)    | Pinguin(u) |
| 2  | gerade | Katze(u)   | Pinguin(u) | Schwein(o) | Maus(o)    |
| 3  | gerade | Maus(u)    | Katze(u)   | Pinguin(o) | Schwein(o) |
| 4  | schräg | Katze(o)   | Maus(o)    | Schwein(o) | Schwein(u) |
| 5  | schräg | Katze(o)   | Schwein(u) | Pinguin(o) | Maus(u)    |
| 6  | schräg | Pinguin(u) | Schwein(u) | Katze(o)   | Maus(u)    |
| 7  | schräg | Maus(u)    | Schwein(u) | Pinguin(o) | Katze(o)   |
| 8  | schräg | Katze(u)   | Maus(o)    | Schwein(u) | Pinguin(o) |

## Lösungsansatz

Die Karten werden in folgender Reihenfolge gelegt:

```
8 1 5
4 0 2
7 3 6
```

Die erste Karte (0) wird in der Mitte ohne Rotation platziert. Alle anderen Karten werden in der angegebenen Reihenfolge platziert und dabei so rotiert, dass sie zu ihren Nachbarkarten passen. Durch geschicktes Betrachten aller Karten sieht man, dass es höchstens nur eine passende Rotation geben kann.
