# BayesXShinyApp

Das BayesXShinyApp R-Paket ist eine Webapp (Shiny-App) für das BayesX. Der Benutzer 
kann damit über eine Benutzeroberfläche Statistiken der Resultate von [BayesX](http://www.statistik.lmu.de/~bayesx/bayesx.html) anschauen.
Die App ist in der Lage die Dichte, den Mittelwert, die Varianz und andere
Momente zu visualisieren. Jede erfolgreiche Interaktion mit der App wird im 
aufgezeichnet und der Benutzer kann darauf zurückgreifen, indem er sich am ein R-Skript
von der App generieren lassen kann. Dieses Skript kann dann in der R-Konsole
verwendet werden.

## App
Die App kann mittels


```r
library(BayesXShinyApp)
run_app()
```

gestartet werden. Die Bedienung ist selbsterklärend.

## Erweiterung
Das R-Paket kann mit neuen Verteilungen modular erweitert werden, 
hierzu rufen wir das `R/distribution.R` auf. In der ersten Zeile wird ein 
`.distribution <- list(...)` erstellt, welches alle
unterstützten Verteilung beinhaltet (`dagum`, `bivnormal`). Hinzufügen eines 
neuen Elements in `.distribution(..., newdistribution = structure(list(...), class = c('distribution', 'list'))` macht das Paket 
mit der neuen Verteilung kompatibel. Wir nennen `newdistribution` ein 
Verteilungsobjekt.

### Interface
Jedes Verteilungsobjekt besitzt mindestens folgende Felder

* `density` enthält die Dichtefunktion `function(`**`params`**`) {...}`. 
**`params`** enthält die 
* `class` ist ein `character` vector welcher der App sagt um welchen Typ von 
Verteilung es sich hierbei handelt
* `link` ist entweder eine Funktion `function(`**`params`**`) {...}` oder eine
Liste von Funktionen `list(param1 = function(`**`params`**`) {...}, param2 = function(`**`params`**`) {...})`. Im Ersteren
wir dieselbe Funktion auf jedem Parameter der Verteilung angewandt. Letzere Variante
erlaubt dem Entwickler für jeden Parameter seine Linkfunktion zu definieren.
* `moment` ist eine Liste von Funktionen die Momente definieren. Jede Funktion
ist in der Liste mit dem Momentnamen beschriftet und das R-Paket unterstützt
folgende Namen `mean`,`median`,`mode`,`var`,`cor`. Möchte man die unterstützen Namen
erweitern, so muss im `R/distribution.R` Dokument eine Funktion definiert werden
mit folgender Struktur

```r
# ersetze MOMENTNAME mit dem neuen Momentnamen
MOMENTNAME.distribution <- function(distr, ...){
  fun <- distr$moment$MOMENTNAME
  
  if( is.null(fun) )
    stop("no MOMENTNAME function defined for distribution")
  else
    return(fun)
}
```
Jede Momentfunktion hat eine Liste als Rückgabewert `return(list(...))`.


