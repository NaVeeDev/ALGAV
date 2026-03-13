# Projet d'ALGAV

Ce projet a été réalisé en décembre 2025 dans le cadre de l'UE Algorithmique Avancé du M1 STL de UPMC.
Le sujet du projet est disponible dans cette archive sous le nom de `sujet.pdf`. 
Plus de détails sont disponibles dans `rapport.pdf`.

## Etudiants
Yvan Parent - 21500313 - M1 STL - Yvan.Parent@etu.sorbonne-universite.fr - yvanprt \
Laura Ly - 21500152 - M1 STL - Laura.Ly@etu.sorbonne-universite.fr - NaVeeDev

## Langage de programmation utilisé
OCaml
### Version
5.3.0

## Compilateur
ocamlopt
### Version
5.3.0

## Instruction de compilation
Pour compiler les tests (primitives inclues) et les lancer: 
```
make test
```

Pour lancer une analyse statistique sur l'efficacité de nos algorithmes: 
```
make analyse
```

Pour compresser individuellement un fichier `file.txt` en un fichier `file.huff`: 
```
./compresser.sh file.txt file.huff
```

Il est possible d'ajouter à cette commande l'option `--visual` qui permet d'obtenir dans un fichier `compression_tree.dot` comptabile avec GraphViz:
```
./compresser.sh file.txt file.huff --visual
```

La décompression fonctionne de façon analogue:
```
./decompresser file.huff file.txt
```
et l'option `--visual` est également disponible pour la décompression.
