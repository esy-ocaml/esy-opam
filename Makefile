BIN = ./node_modules/.bin

prepare: \
	opam/src/core/opamVersion.ml \
	opam/src/core/opamCompat.ml \
	opam/src/core/opamCompat.mli \
	opam-file-format/src/opamBaseParser.ml

opam-file-format/src/opamBaseParser.ml: opam-file-format/src/opamBaseParser.mly
	ocamlyacc -b $(@:%.ml=%) $(<)

opam/src/core/opamVersion.ml: opam/src/core/opamVersion.ml.in
	cat $(<) | sed -e 's/@PACKAGE_VERSION@/1.2/g' > $(@)

opam/src/core/opamCompat.ml: opam/src/core/opamCompat.ml.4.02
	cp $(<) $(@)

opam/src/core/opamCompat.mli: opam/src/core/opamCompat.mli.4.02
	cp $(<) $(@)

build: prepare
	$(BIN)/bsb -clean-world -make-world

watch: prepare
	$(BIN)/bsb -w -clean-world -make-world
