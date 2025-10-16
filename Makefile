# Nom de l'exécutable principal
MAIN_EXEC = main
# Nom de l'exécutable de tests
TEST_EXEC = test_runner

# Modules sources
SRC_MLI = utils.mli test.mli
SRC_ML  = utils.ml main.ml test.ml

# Options de compilation
OCAMLFLAGS = -g

# Compilation des .mli en .cmi
%.cmi: %.mli
	ocamlc $(OCAMLFLAGS) -c $<

# Compilation des .ml en .cmo
%.cmo: %.ml
	ocamlc $(OCAMLFLAGS) -c $<

# Exécutable principal
$(MAIN_EXEC): utils.cmi utils.cmo main.cmo
	ocamlc $(OCAMLFLAGS) utils.cmo main.cmo -o $@

# Exécutable des tests
$(TEST_EXEC): utils.cmi utils.cmo test.cmi test.cmo
	ocamlc $(OCAMLFLAGS) utils.cmo test.cmo -o $@

# Lancer les tests
test: $(TEST_EXEC)
	./$(TEST_EXEC)

# Nettoyer les fichiers compilés
clean:
	rm -f *.cmo *.cmi $(MAIN_EXEC) $(TEST_EXEC)
