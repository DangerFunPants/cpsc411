BNFC_DIR = ./bnfc
BNFC = bnfc
MAKE = make
CF_FILE = M.cf
CC = ghc
SRC_DIR = ./src
BNFC_SOURCES = $(BNFC_DIR)/AbsM.hs $(BNFC_DIR)/ErrM.hs $(BNFC_DIR)/LexM.hs $(BNFC_DIR)/ParM.hs $(BNFC_DIR)/PrintM.hs $(BNFC_DIR)/SkelM.hs

mcc : $(BNFC_DIR)/$(CF_FILE) $(SRC_DIR)/*.hs TestM
	$(CC) $(SRC_DIR)/*.hs $(BNFC_SOURCES) -o mcc

TestM : $(BNFC_DIR)/$(CF_FILE)
	$(BNFC) -haskell -m $(BNFC_DIR)/$(CF_FILE) -o $(BNFC_DIR)
	$(MAKE) -C $(BNFC_DIR)
	mv $(BNFC_DIR)/TestM ./

clean : 
	rm -f TestM
	rm -f mcc
	rm -f $(SRC_DIR)/*.hi $(SRC_DIR)/*.o
	$(MAKE) distclean -C $(BNFC_DIR)
	

