all:
	-mkdir build
	cd app && \
	ghc --make Main.hs -odir ../build -hidir ../build -o ../interpreter
	
clean:
	-rm -rf build
	-rm -f interpreter