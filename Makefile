all:
	ghc --make -hide-package monads-fd -W -o clevercss CCMain.hs
all-opt: clean
	ghc --make -hide-package monads-fd -O1 -funbox-strict-fields -o clevercss CCMain.hs
all-prof:
	ghc --make -hide-package monads-fd -prof -auto-all -o clevercss CCMain.hs
all-cov:
	ghc --make -hide-package monads-fd -fhpc -auto-all -o clevercss CCMain.hs
	./clevercss -D supply_me_in_default_variables="5px + 2px" example.ccs
	hpc markup --destdir=cov clevercss.tix
test: all
	./clevercss -D supply_me_in_default_variables="5px + 2px" example.ccs
	cat example.css
clean:
	rm -fr clevercss dist *.css
	find -name '*.o' -exec rm {} +
	find -name '*.hi' -exec rm {} +
	runhaskell Setup.lhs clean
docs: all
	./clevercss docstyle.ccs
	rst2html.py --stylesheet-path=docstyle.css documentation.rst > documentation.html
	rm docstyle.css
