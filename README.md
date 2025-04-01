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

## Lösungen

```hs
mapM_ print solutions         
```
```
 k   P   k 
p1S s5m M2p
 M   K   S
 m   k   s
K6p P8M m7P
 s   s   K
 S   S   k
P3m M4s S0M
 k   K   p

 m   S   S 
K6p P3m M4s
 s   k   K
 S   K   k
k1M m5s S0M
 p   P   p
 P   p   P
s7K k2S s8k
 m   M   M

 p   P   p 
k2S s5m M1k
 M   K   S
 m   k   s
K6p P8M m7P
 s   s   K
 S   S   k
P3m M4s S0M
 k   K   p

 P   P   k 
k3S s5m M2p
 m   K   S
 M   k   s
S1p P8M m7P
 k   s   K
 K   S   k
s6m M4s S0M
 p   K   p

 S   S   M 
P3m M4s S1p
 k   K   k
 K   k   K
s6m M2p P7m
 p   S   s
 P   s   S
s5m M8P p0k
 K   k   M

 S   S   M 
P3m M4s S1p
 k   K   k
 K   k   K
s6m M2p P7m
 p   S   s
 P   s   S
s8k K5P p0k
 M   m   M

 K   p   k 
s4M m6s S0M
 S   K   p
 s   k   P
K5P p1S s8k
 m   M   M
 M   m   m
S2k K7s S3k
 p   P   P
```
