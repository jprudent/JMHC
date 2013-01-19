imaginez que vous construisiez un programme qui calcule les points d'une main valide de mahjong.

Pour ceux qui ne connaissent pas, le mahjong est comme un jeu de carte mais qui se joue avec des tuiles.
Certains le comparent au jeu Bridge.

Si vous ne connaissez pas, voici un condensé nécessaire et suffisant à la compréhension de l'article
en 4 phrases:

Le but est de former une combinaison à partir des 14 tuiles en main.
Une combinaison valide est formée de 4 figures de 3 tuiles et 1 figure de 2 tuiles.
Les tuiles sont numérotés de 1 à 9  et existent en 4 exemplaires.
Les figures valides sont : la paire, le brelan, et la petite suite pure

Dans votre calculateur, vous avez un service qui prend en entrée une liste de tuiles non arrangées,
qui les arrange et retourne une liste de figures.
Par exemple : List(b1,b5,b2,b3,b5,b1,b3,b3,b9,b2,b2,b1,b9,b9) => [
    Pair(b5), Suite(b1,b2,b3), Suite(b1,b2,b3), Suite(b1,b2,b3), Brelan(b9)
    Pair(b5), Brelan(b1), Brelan(b2), Brelan(b3), Brelan(b9)
]

Vous avez un hiérarchie d'objets représentant les figures.

Il existe plusieurs algorithmes pour trouver ce genre d'arrangement. Vous avez alors plusieurs
implémentations dudit service dans votre code, du plus niais au plus excentrique.

L'une de ces implémentation a besoin de pouvoir ajouter une tuile à une figure déjà existante pour en
créer une nouvelle.
Par exemple: Pair(b1) + b1 == Brelan(b1)

Là on a 2 possibilités :
- Soit créer une factory qui prend une figure et une tuile en paramètre et renvoie une figure (ou pas).
Concr�tement cela se traduit par un switch dans une factory.
- Soit incorporer une méthode addTile dans chaque figure pour
- Soit incorporer une méthode mayComeFrom dans chaque figure qui retourne la liste des figures dont
this peut �tre sous jacente

solution 1 => elle centralise la connaissance de toutes les figures en un seul endroit. Cela n'a pas de lien
avec les figures. On a l'impression que c'est une entité qui a sa propre sémantique à propos des figures.
C'est isolé.
solution 2 => cela revient à créer une factory dans chaque figure. L'avantage est que c'est contextualisé.
Chaque figure a la responsabilité de générer une autre figure. Par contre elle a besoin de connaitre toutes
les autres figures. Si on crée une nouvelle figure, il faut potentiellement modifier toutes les méthodes.
solution 3 => semblable à solution 2 à l'envers. ...
