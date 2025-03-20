# Spielanleitung

Tierisch VERZWICKT

Aus den 9 Karten ist ein Quadrat zu bilden. Dabei ist darauf zu achten, dass Ober- und Unterteil der Tiere zusammengehören. Also keine Schweine-guine oder ähnliche Fabelwesen bilden. Viel Vergnügen.



## Kartenbeschreibung

Jede quadratische Karte hat vier Seiten mit je einer Tierhälfte (oben/unten) von einem der vier Tiere: Schwein, Katze, Maus oder Pinguin. In der Mitte jeder Karte befindet sich eine kleine Maus, die entweder gerade (4 Karten) oder schräg (5 Karten) ausgerichtet ist.

Die Beschreibung jeder Karte erfolgt bei "gerader" Ausrichtung der Maus, im Uhrzeigersinn, beginnend mit "oben".

| ID | Maus | Oben | Rechts | Unten | Links |
|----|------|------|--------|--------|-------|
| 1  | gerade | Schwein(o) | Katze(u) | Maus(o) | Pinguin(u) |
| 2  | gerade | Katze(u) | Schwein(o) | Maus(o) | Pinguin(u) |
| 3  | gerade | Katze(u) | Pinguin(u) | Schwein(o) | Maus(o) |
| 4  | gerade | Maus(u)  | Katze(u)   | Pinguin(o) | Schwein(o) |
| 5  | schräg | Katze(o) | Maus(o)    | Schwein(o) | Schwein(u) |
| 6  | schräg | Katze(o) | Schwein(u) | Pinguin(o) | Maus(u) |
| 7  | schräg | Pinguin(u) | Schwein(u) | Katze(o) | Maus(u) |
| 8  | schräg | Maus(u) | Schwein(u) | Pinguin(o) | Katze(o) |
| 9  | schräg | Katze(u) | Maus(o) | Schwein(u) | Pinguin(o) |

## Implementierung

Wir werden zwei Implementierungen entwickeln:
1. Eine Python-Version
2. Eine Haskell-Version

Dies ermöglicht uns einen interessanten Vergleich der Herangehensweisen in beiden Sprachen.

### Haskell-Version: Aktueller Stand

#### Datenmodell
- `Animal`: Enum für die vier Tierarten (Schwein, Katze, Maus, Pinguin)
- `Side`: Enum für die Position der Tierhälfte (Upper/Lower)
- `Half`: Kombination aus Animal und Side
- `Card`: Eine Karte mit vier Hälften (oben, rechts, unten, links) und Maus-Orientierung
- `Rotation`: Enum für die möglichen Rotationen (R0, R90, R180, R270)
- `Board`: 3x3-Matrix von Karten mit ihrer jeweiligen Rotation

#### Implementierte Funktionen
1. `halvesMatch`: Prüft, ob zwei Tierhälften zusammenpassen (gleiches Tier, verschiedene Seiten)
2. `rotateCard`: Rotiert eine Karte um den angegebenen Winkel
3. `isValidBoard`: Validiert ein komplettes Board:
   - Korrekte Größe (3x3)
   - Keine doppelten Karten
   - Passende Tierhälften zwischen benachbarten Karten

#### Hilfsfunktionen für Tests
- `findMatchingCards`: Findet alle Karten, die mit einer gegebenen Karte in einer bestimmten Rotation zusammenpassen können
- `showCard`: Formatierte Ausgabe einer Karte für bessere Lesbarkeit

#### Nächste Schritte
1. Systematische Suche nach einer gültigen Board-Konfiguration
2. Implementierung eines Algorithmus zum automatischen Lösen des Puzzles
3. Visualisierung der Lösung(en)
4. Performance-Optimierungen für die Suche

#### Offene Fragen
- Wie viele verschiedene Lösungen gibt es?
- Welche Rolle spielt die Maus-Orientierung für die Lösung?
- Gibt es Muster oder Symmetrien in den Lösungen?

## Legeordnung

Die Karten werden in folgender Reihenfolge gelegt:

```
9 2 6
5 1 3
8 4 7
```

Die erste Karte (1) wird in der Mitte ohne Rotation platziert. Alle anderen Karten werden in der angegebenen Reihenfolge platziert und dabei so rotiert, dass sie zu ihren Nachbarkarten passen.
