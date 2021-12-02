# TP2 PDS, version OCaml

Ceci est notre (Hugo Boulier et Florent Dufay) TP2 de PDS, avec quelques remarques sur nos choix d'implémentations et sur l'état actuel du projet.

- [x] Expressions simples
    - Elles ne respectent pas encore les priorités opératoires usuelles, elles sont pour le moment juste évaluées de gauche à droite.
- [x] Affectation de variables
    - Nous avons décidé de précéder les identifiants de variables (VSL) par des `v` dans la représentation intermédiaire LLVM afin qu'ils ne rentrent pas en conflit avec les identifiants générés automatiquement (de la forme `tmpX`)
- [x] Gestion des blocs
    - Pour l'instant, les blocs doivent être entourés d'accolades dans le code VSL+ pour que le parsing s'effectue correctement.
- [x] Instructions de contrôle `if` et `while`
- [x] Déclaration de variables
    - Les déclarations de variables sont faites par des allocations de pointeurs dans la représentation intermédiaire LLVM.
- [ ] Expressions avec variables
    - Lors de l'utilisation de variables qui ont été déclarées, nous n'avons pas encore utilisé les `load` de LLVM donc beaucoup d'endroits ne produisent pas du code LLVM valide : nous pouvons affecter des variables VSL mais pas encore les utiliser.
- [ ] Fonctions de bibliothèque (`PRINT` & `READ`)
    - Probablement la prochaine étape : pour le moment le parser les reconnaît bien mais la génération de code est encore incomplète.
- [ ] Définition et appel de fonctions
- [ ] Gestion des tableaux
