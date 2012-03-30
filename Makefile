

all: rgrep

rgrep: dirs.hs rgrep.hs
	ghc -O2 -optl"-Wl,-read_only_relocs,suppress" --make rgrep

clean:
	-rm -f rgrep *.hi *.o

install:
	cp rgrep ~/local/bin

