CC = ghc
LEX = alex
LEX_DIR = ./alex
SRC_DIR = ./src
README_NAME = README.FIRST

 mcc : $(SRC_DIR)/*.hs $(SRC_DIR)/lexer.hs
	$(CC) $(SRC_DIR)/*.hs -o mcc

$(SRC_DIR)/lexer.hs : $(LEX_DIR)/minisculus.x
	$(LEX) $(LEX_DIR)/minisculus.x -o $(SRC_DIR)/lexer.hs

clean: 
	rm -f *.o $(SRC_DIR)/*.o
	rm -f mcc
	rm -f *.hi $(SRC_DIR)/*.hi
	rm -f $(SRC_DIR)/lexer.hs
	rm -f ./stack_asm/*.a
