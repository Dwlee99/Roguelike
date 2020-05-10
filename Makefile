UNITS=action armor ascii_panel authors battleaxe board inventory main math Messages monster name ranger short_bow short_sword State swordsman weapon
MLS_WITHOUT_MLIS=test
MLS=$(UNITS:=.ml) $(MLS_WITHOUT_MLIS:=.ml)
OBJECTS=$(UNITS:=.cmo) $(MLS_WITHOUT_MLIS:=.cmo)
MLS=$(UNITS:=.ml) $(MLS_WITHOUT_MLIS:=.ml)
MLIS=$(UNITS:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=graphics,yojson,oUnit

default: build
	utop

install: 
	opam install -y conf-pkg-config conf-libX11 dune dune-configurator graphics yojson

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

check:
	bash checkenv.sh
	
finalcheck: check
	bash checkzip.sh
	bash finalcheck.sh

zip:
	zip roguelike.zip *.ml* _tags Makefile INSTALL.txt
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private game.zip