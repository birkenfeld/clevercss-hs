all:
	ghc --make -W -o clevercss CCMain.hs
all-opt: clean
	ghc --make -O1 -funbox-strict-fields -o clevercss CCMain.hs
all-prof:
	ghc --make -prof -auto-all -o clevercss CCMain.hs
test: all
	./clevercss -D supply_me_in_default_variables="5px + 2px" example.ccs
	cat example.css
clean:
	rm -fr clevercss dist *.o *.hi *.css
	runhaskell Setup.lhs clean
docs: all
	./clevercss docstyle.ccs
	rst2html.py --stylesheet-path=docstyle.css documentation.rst > documentation.html
	rm docstyle.css
