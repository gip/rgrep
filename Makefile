

all: rgrep

rgrep: dirs.hs rgrep.hs
	ghc -O2 --make rgrep

clean:
	-rm -f rgrep *.hi *.o


