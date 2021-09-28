lolcat: lolcat.hs
	ghc -dynamic -O3 $< -o $@

.PHONY: clean
clean:
	$(RM) lolcat
