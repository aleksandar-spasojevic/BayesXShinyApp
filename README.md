# BayesXShinyApp

Das BayesXShinyApp R-Paket ist eine Webapp (Shiny-App) für das [BayesX](http://www.statistik.lmu.de/~bayesx/bayesx.html). Mit der Benutzeroberfläche können Statistiken von [BayesX](http://www.statistik.lmu.de/~bayesx/bayesx.html) geschätzgen Modellen analysiert werden.
Die App ist in der Lage die Dichte, den Mittelwert, die Varianz und andere
Momente zu visualisieren. Jede erfolgreiche Interaktion mit der App wird
aufgezeichnet und der Benutzer kann darauf zurückgreifen, indem er sich ein R-Skript
von der App generieren lässt. Dieses Skript kann dann in der R-Konsole
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

* `density` enthält die Dichtefunktion `function(`**`param1`**`, ...) {...}`. 
**`param1, ...`** entspricht den Parametern der Verteilung. Dem Namen des Parameters
wird die Dimension in Form einer Zahl hinzugefügt, z.B. `mu_1` entspricht dem
$\mu$ Parameter irgendeiner Verteilung. Handelt es sich um eine multivariate
Verteilung und die besitzt Parameter in Form von Vektoren, so sieht die Namenskonvention wie folgt aus `mu_1, mu_2`. Dies entspricht dem 2 dimensionalen *$\mu$* Vektor
* `class` ist ein `character vector` und beschreibt den Typ der 
Verteilung
* `link` ist entweder eine Funktion `function(eta) {...}` oder eine
Liste von Funktionen `list(`**`param1`**` = function(eta) {...}, `**`param2`**` = function(eta) {...})`. Im Ersteren
wir dieselbe Funktion auf jedem Parameter der Verteilung angewandt. Letzere Variante
erlaubt dem Entwickler für jeden Parameter seine Linkfunktion zu definieren.
* `moment` ist eine Liste von Funktionen die Momente definieren. Jede Funktion 
in der Liste ist mit dem Momentnamen beschriftet und hat als Rückgabewert eine 
Liste `return(list(`**`param1`**` = ...))`. Das R-Paket unterstützt
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

### Beispiel
Wir möchten nun ein Verteilungsobjekt `cauchy` definieren,
da wir mit BayesX ein Modell mit `cauchy` Parametern (equationtypes `t` und `s`) 
geschätzt haben. Der 
Verteilungsname `cauchy` muss BayesX Verteilungsnamen entsprechen. Die Linkfunktion
ist `ìdentity` und besagt dass die aus dem BayesX geschätzten (sampled) Parameter
genau so in die Dichtefunktion und Momentfunktionen verwendet werden sollen. Da
die `cauchy` Verteilung keine Momente besitzt, werden wir den Teil leer lassen.
Das Verteilungsobjekt würde nun so aussehen


```r
.distribution <- 
  list(...,
       cauchy = structure(
         list(
           # wobei 't' und 's' den equationtypes in BayesX entspricht
           density = function(t_1, s_1, ...) {
             return( dcauchy(location = t_1, scale = s_1, ...) )
           },
           class = "univariate",
           # identity
           link = function(eta) eta,
           # https://de.wikipedia.org/wiki/Cauchy-Verteilung
           moment = list()
         ), class = c("distribution", "list"))
  )
```




