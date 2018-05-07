SRCFILES = FPASLanLexer.hs FPASLanParser.hs FPASLan.hs AIFOm.hs PrettyPrinter.hs Types.hs Modesti.hs Main.hs

########################

all: aifomega

########################

aifomega:	$(SRCFILES)
	ghc -XNoMonomorphismRestriction -XFlexibleInstances -XScopedTypeVariables -fspec-constr-count=5 -static   -package base --make -O2 Main.hs -o aifomega

########################

FPASLanLexer.hs:	FPASLanLexer.x
	alex FPASLanLexer.x

########################

FPASLanParser.hs:	FPASLanParser.y
	happy -ioutput  FPASLanParser.y

########################

clean:
	rm -f *\.hi *\.o FPASLanLexer.hs FPASLanParser.hs

########################

compiler:
	alex FPASLanLexer.x
	happy -a -g -c -iFPASLan_grammar.txt FPASLanParser.y -oFPASLanParser.hs

#########################
