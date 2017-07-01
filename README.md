nary-Decision-Diagram
Course project of INF441

Vous pouvez tester nos programmes comme ceci :

1. Déplacez-vous dans le répertoire de notre projet.

2. Créez des fichiers exécutables en utilisant la commande make all 

3. Faites des tests en utilisant les commandes ./bdd.native dump,./bdd.native valid, ./bdd.native satisfiable, ./bdd.native tetravex. Vous pouvez utiliser true, false, ∼,&&,||,=>et<=> pour représenter resp.⊤,⊥,¬,∧,∨,⇒,⇔. La commande ./bdd.native tetravex doit être suivie par un nom de fichier, par exemple a.txt. Alors la solution du puzzle sera stockée dans a_solution.txt qui se trouve dans le répertoire data.

Exemples :
(1) ./bdd.native dump "(a&&b=>∼c)||false" : transforme une formule propositionnelle en un diagramme de décision binaire
(2) ./bdd.native satisfiable "a&&∼a" : vérifier si une formule propositionnelle est satisfaisante
(3) ./bdd.native tetravex "data/tetravex3.txt" : résoudre un puzzle de tetravex

4. On fournit aussi quelques tests dans test.ml. Vous pouvez exercer ces fonctions de test en utilisant la commande ./test.native n, où n est un entier entre 1 et 8.
