LEX = alex
CC = ghc
ALEX_DIR = ./alex

all : lexer.hs
	$(CC) lexer.hs -o lexer

lexer.hs : $(ALEX_DIR)/minisculus.x
	$(LEX) $(ALEX_DIR)/minisculus.x -o lexer.hs

clean:
	rm -f lexer.*
	rm -f lexer