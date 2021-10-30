# TP2 PDS, version OCaml

Ceci est notre (Hugo Boulier et Florent Dufay) TP2 de PDS, avec quelques remarques sur nos choix d'implémentations.

- [x] Expressions simples
    - Elles ne respectent pas encore les priorités opératoires usuelles, et sont pour le moment évaluées de droite à gauche
- [x] Affectation de variables
    - Nous avons décidé de précéder les identifiants de variables par des `v` dans la représentation intermédiaire LLVM afin qu'ils ne rentrent pas en conflit avec les identifiants générés automatiquement (de la forme `tmpX`)
- [x] Gestion des blocs
    - Pour l'instant, les blocs doivent être entourés d'accolades dans le code VSL+ pour que le parsing s'effectue correctement.
- [x] Déclaration de variables
- [x] Expressions avec variables
- [x] Instructions de contrôle `if` et `while`
- [ ] Fonctions de bibliothèque (`PRINT` & `READ`)
    - Pour le moment, seul le parsing est fait et pas encore la génération de code LLVM pour ces instructions.
- [ ] Définition et appel de fonctions
- [ ] Gestion des tableaux
