
all:
	ghc-7.4.2 --make Edentest.hs -parcp -rtsopts -O Main

run:
	./Main +RTS -N2

time:
	time ./Main +RTS -N1
	time ./Main +RTS -N2