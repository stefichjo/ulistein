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

```
ghci> mapM_ print solutions         
```
```
 k   P   k 
p.S s.m M.p
 M   K   S
 m   k   s
K.p P.M m.P
 s   s   K
 S   S   k
P.m M.s S.M
 k   K   p

 m   S   S
K.p P.m M.s
 s   k   K
 S   K   k
k.M m.s S.M
 p   P   p
 P   p   P
s.K k.S s.k
 m   M   M

 p   P   p
k.S s.m M.k
 M   K   S
 m   k   s
K.p P.M m.P
 s   s   K
 S   S   k
P.m M.s S.M
 k   K   p

 P   P   k 
k.S s.m M.p
 m   K   S 
 M   k   s 
S.p P.M m.P
 k   s   K 
 K   S   k 
s.m M.s S.M
 p   K   p 

 S   S   M 
P.m M.s S.p
 k   K   k 
 K   k   K 
s.m M.p P.m
 p   S   s 
 P   s   S 
s.m M.P p.k
 K   k   M 

 S   S   M 
P.m M.s S.p
 k   K   k 
 K   k   K 
s.m M.p P.m
 p   S   s 
 P   s   S 
s.k K.P p.k
 M   m   M 

 K   p   k 
s.M m.s S.M
 S   K   p
 s   k   P
K.P p.S s.k
 m   M   M
 M   m   m
S.k K.s S.k
 p   P   P
```