# Nom de l'exécutable principal
MAIN_EXEC = main
# Nom de l'exécutable de tests
TEST_EXEC = tests

# Dossiers
TEST_DIR = test_files

# Modules sources
SRC_MLI = primitive.mli utils.mli $(TEST_DIR)/test_utils.mli $(TEST_DIR)/test_primitive.mli $(TEST_DIR)/test.mli
SRC_ML  = primitive.ml utils.ml main.ml $(TEST_DIR)/test_utils.ml $(TEST_DIR)/test_primitive.ml $(TEST_DIR)/test.ml

# Options de compilation
OCAMLFLAGS = -g
OCAMLOPT = ocamlopt

# Fichiers compilés (objets natifs .cmx/.o et interfaces .cmi)
CMX = utils.cmx primitive.cmx main.cmx $(TEST_DIR)/test_utils.cmx $(TEST_DIR)/test_primitive.cmx $(TEST_DIR)/test.cmx
CMI = utils.cmi primitive.cmi $(TEST_DIR)/test_utils.cmi $(TEST_DIR)/test_primitive.cmi $(TEST_DIR)/test.cmi

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

# Dépendances
$(TEST_DIR)/test_primitive.cmx: primitive.cmx
$(TEST_DIR)/test_utils.cmx: utils.cmx
$(TEST_DIR)/test.cmx: utils.cmx $(TEST_DIR)/test_utils.cmx

# Exécutable principal
$(MAIN_EXEC): utils.cmi utils.cmx main.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) utils.cmx main.cmx -o $@

# Exécutable des tests
$(TEST_EXEC): $(CMI) utils.cmx primitive.cmx $(TEST_DIR)/test_utils.cmx $(TEST_DIR)/test_primitive.cmx $(TEST_DIR)/test.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ utils.cmx primitive.cmx $(TEST_DIR)/test_utils.cmx $(TEST_DIR)/test_primitive.cmx $(TEST_DIR)/test.cmx

# Lancer les tests
test: $(TEST_EXEC)
	./$(TEST_EXEC)

# Nettoyer les fichiers compilés
clean:
	rm -f *.cmx *.o *.cmi $(MAIN_EXEC) $(TEST_EXEC) $(TEST_DIR)/*.cmx $(TEST_DIR)/*.cmi $(TEST_DIR)/*.o
