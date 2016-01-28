FILES= morpi.ml
FICHIERS= $(FILES) morpi.ml
LISTE_SOURCES= morpi.ml
EXCLUDE= $(LISTE_SOURCES) Makefile README.md
EXE_CLI=Morpion-cli
LISTE=$(wildcard *)
LISTE_TO_SUPPR=$(filter-out $(EXCLUDE),$(LISTE))

$(EXE_CLI): morpi.ml
	@ocamlc -o $(EXE_CLI) morpi.ml 
	@echo "Compilation cli OK"

clean: 
	@rm -f *.cmo
	@rm -f *.cmi
	
clean-all: 
	@for FIC in $(LISTE_TO_SUPPR); do rm -Rf $$FIC; done

run: $(EXE_CLI)
	@./$(EXE_CLI)

clean-and-run: clean-all run

