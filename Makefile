# Nom de l'exécutable principal
MAIN_EXEC = decompression compression
# Nom de l'exécutable de tests
TEST_EXEC = tests
# Nom de l'exécutable d'analyse
ANALYSE_EXEC = run_analyse

# Dossiers
TEST_DIR = test_files

# Modules sources
SRC_MLI = primitive.mli utils.mli compression.mli decompression.mli $(TEST_DIR)/test_utils.mli $(TEST_DIR)/test_primitive.mli $(TEST_DIR)/test_compression.mli $(TEST_DIR)/test_decompression.mli $(TEST_DIR)/test.mli $(TEST_DIR)/etude_autre_language.mli $(TEST_DIR)/etude_gutemberg.mli $(TEST_DIR)/etude_random.mli $(TEST_DIR)/analyse.mli
SRC_ML  = primitive.ml utils.ml main.ml compression.ml decompression.ml $(TEST_DIR)/test_utils.ml $(TEST_DIR)/test_primitive.ml $(TEST_DIR)/test_compression.ml $(TEST_DIR)/test_decompression.ml $(TEST_DIR)/test.ml $(TEST_DIR)/etude_autre_language.ml $(TEST_DIR)/etude_gutemberg.ml $(TEST_DIR)/etude_random.ml $(TEST_DIR)/analyse.ml

# Options de compilation
OCAMLFLAGS = -g
OCAMLOPT = ocamlfind ocamlopt
OCAMLOPTFLAGS = $(OCAMLFLAGS) -package uutf -package unix -linkpkg

# Fichiers compilés (objets natifs .cmx/.o et interfaces .cmi)
CMX = utils.cmx primitive.cmx compression.cmx decompression.cmx main.cmx $(TEST_DIR)/test_utils.cmx $(TEST_DIR)/test_primitive.cmx $(TEST_DIR)/test_compression.cmx $(TEST_DIR)/test_decompression.cmx $(TEST_DIR)/test.cmx $(TEST_DIR)/etude_autre_language.cmx $(TEST_DIR)/etude_gutemberg.cmx $(TEST_DIR)/etude_random.cmx $(TEST_DIR)/analyse.cmx
CMI = utils.cmi primitive.cmi compression.cmi decompression.cmi $(TEST_DIR)/test_utils.cmi $(TEST_DIR)/test_primitive.cmi $(TEST_DIR)/test_compression.cmi $(TEST_DIR)/test_decompression.cmi $(TEST_DIR)/test.cmi $(TEST_DIR)/etude_autre_language.cmi $(TEST_DIR)/etude_gutemberg.cmi $(TEST_DIR)/etude_random.cmi $(TEST_DIR)/analyse.cmi

.PHONY: all test clean analyse

all: $(MAIN_EXEC)

# Compilation des .mli en .cmi
%.cmi: %.mli
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Compilation des .ml en .cmx (natifs)
# Si une interface .cmi existe, s'assurer qu'elle est générée avant le .cmx
# Ajouter -I $(TEST_DIR) pour que le compilateur trouve les .cmi dans le dossier de tests
%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -I $(TEST_DIR) -c $<

# Dépendances
primitive.cmx: primitive.ml utils.cmi
primitive.cmi: primitive.mli utils.cmi

$(TEST_DIR)/test_utils.cmx: utils.cmx
$(TEST_DIR)/test_primitive.cmx: primitive.cmx utils.cmx
$(TEST_DIR)/test_compression.cmx: compression.cmx
$(TEST_DIR)/test_decompression.cmx: decompression.cmx
$(TEST_DIR)/etude_random.cmx: compression.cmx decompression.cmx
$(TEST_DIR)/etude_gutemberg.cmx: compression.cmx decompression.cmx
$(TEST_DIR)/etude_autre_language.cmx: compression.cmx decompression.cmx
$(TEST_DIR)/analyse.cmx: utils.cmx primitive.cmx $(TEST_DIR)/etude_autre_language.cmx $(TEST_DIR)/etude_gutemberg.cmx $(TEST_DIR)/etude_random.cmx
$(TEST_DIR)/test.cmx: utils.cmx primitive.cmx $(TEST_DIR)/test_utils.cmx $(TEST_DIR)/test_primitive.cmx $(TEST_DIR)/test_compression.cmx $(TEST_DIR)/test_decompression.cmx

# Exécutable principal
$(MAIN_EXEC): utils.cmi utils.cmx primitive.cmi primitive.cmx compression.cmi compression.cmx decompression.cmi decompression.cmx 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) utils.cmx primitive.cmx compression.cmx decompression.cmx -o $@

# Exécutable des tests
$(TEST_EXEC): $(CMI) utils.cmx primitive.cmx compression.cmx decompression.cmx $(TEST_DIR)/test_utils.cmx $(TEST_DIR)/test_primitive.cmx $(TEST_DIR)/test_compression.cmx $(TEST_DIR)/test_decompression.cmx $(TEST_DIR)/test.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ utils.cmx primitive.cmx compression.cmx decompression.cmx $(TEST_DIR)/test_utils.cmx $(TEST_DIR)/test_primitive.cmx $(TEST_DIR)/test_compression.cmx $(TEST_DIR)/test_decompression.cmx $(TEST_DIR)/test.cmx

# Exécutable de l'analyse
$(ANALYSE_EXEC): $(CMI) utils.cmx primitive.cmx compression.cmx decompression.cmx $(TEST_DIR)/etude_autre_language.cmx $(TEST_DIR)/etude_gutemberg.cmx $(TEST_DIR)/etude_random.cmx $(TEST_DIR)/analyse.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ utils.cmx primitive.cmx compression.cmx decompression.cmx $(TEST_DIR)/etude_autre_language.cmx $(TEST_DIR)/etude_gutemberg.cmx $(TEST_DIR)/etude_random.cmx $(TEST_DIR)/analyse.cmx

# Lancer les tests
test: $(TEST_EXEC)
	./$(TEST_EXEC)

# Lancer l'analyse
analyse: $(ANALYSE_EXEC)
	   ./$(ANALYSE_EXEC)

# Nettoyer les fichiers compilés
clean:
	rm -f *.cmx *.o *.cmi $(MAIN_EXEC) $(TEST_EXEC) $(TEST_DIR)/*.cmx $(TEST_DIR)/*.cmi $(TEST_DIR)/*.o *.dot
