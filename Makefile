# Nom de l'exécutable principal
MAIN_EXEC = main
# Nom de l'exécutable de tests
TEST_EXEC = test_runner

# Dossiers
TEST_DIR = test_files

# Modules sources
SRC_MLI = utils.mli $(TEST_DIR)/test.mli
SRC_ML  = utils.ml main.ml $(TEST_DIR)/test.ml

# Options de compilation
OCAMLFLAGS = -g
OCAMLOPT = ocamlopt

# Fichiers compilés (objets natifs .cmx/.o et interfaces .cmi)
CMX = utils.cmx main.cmx $(TEST_DIR)/test.cmx
CMI = utils.cmi $(TEST_DIR)/test.cmi

.PHONY: all test clean

all: $(MAIN_EXEC)

# Compilation des .mli en .cmi
%.cmi: %.mli
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

# Compilation des .ml en .cmx (natifs)
# Si une interface .cmi existe, s'assurer qu'elle est générée avant le .cmx
# Ajouter -I $(TEST_DIR) pour que le compilateur trouve les .cmi dans le dossier de tests
%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLFLAGS) -I $(TEST_DIR) -c $<

# Exécutable principal (natif)
$(MAIN_EXEC): utils.cmi utils.cmx main.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) utils.cmx main.cmx -o $@

# Exécutable des tests (natif)
$(TEST_EXEC): $(CMI) $(TEST_DIR)/test.cmx utils.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ utils.cmx $(TEST_DIR)/test.cmx

# Lancer les tests
test: $(TEST_EXEC)
	./$(TEST_EXEC)

# Nettoyer les fichiers compilés
clean:
	rm -f *.cmx *.o *.cmi $(MAIN_EXEC) $(TEST_EXEC) $(TEST_DIR)/*.cmx $(TEST_DIR)/*.cmi $(TEST_DIR)/*.o
