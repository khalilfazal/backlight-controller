all: backlight-server

backlight-server: Main.hs
	ghc -Wall -optc-O3 -o $@ $^

clean:
	$(RM) backlight-server
