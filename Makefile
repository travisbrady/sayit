CFLAGS = \
		 -threaded \
		 -package fastcgi \
		 -XPatternGuards \
		 -hidir build \
		 -odir build \
		 --make \

sayit.fcgi:
	ghc $(CFLAGS) -o build/sayit.fcgi Main.hs

all: sayit.fcgi

clean:
	rm -f build/*.o build/*.hi
