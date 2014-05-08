GHC = ghc

.PHONY:
default: Main

.PHONY:
test: TestClosestPair

Main:
	$(GHC) Main.hs

TestClosestPair:
	$(GHC) TestClosestPair.hs

.PHONY:
all: clean default test

.PHONY:
clean:
	rm -f *.o *.hi *.*~ TestClosestPair Main
