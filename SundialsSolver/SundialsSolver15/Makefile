#ProjectDir=$(shell pwd)
COMPILER= g++ 

MAINFILE=SundialsSolver
SOURCE_FILE=$(MAINFILE).cpp
TARGET_DIR=./x64/Release
PLATFORM := $(shell uname -s)

INCLUDE= -I. -I./../../Lib/Sundials_v2.6.2_64bit/include

ifeq ($(PLATFORM),Darwin)
	LIBPATH=./../../Lib/Sundials_v2.6.2_64bit/lib_osx_Darwin
	EXT=dylib
	CVODE_EXT=1.$(EXT)
	NVEC_EXT=0.$(EXT)
#ifeq ($(PLATFORM),Linux)
else
	OS=$(shell lsb_release -si)
	VER=$(shell lsb_release -sr)
	LIBPATH=./../../Lib/Sundials_v2.6.2_64bit/lib_linux_$(OS)$(VER)
	EXT=so
	CVODE_EXT=$(EXT).1
	NVEC_EXT=$(EXT).0
endif

DEFINITIONS= -DNIX -DSUNDIALSSOLVER_EXPORTS
OTHEROPTS= -std=c++11 
MAKE_LIB= -fPIC 

LINK_LIB= --shared
LIBFLAG= -L$(LIBPATH)
LIBSsundials= -lsundials_cvode -lsundials_nvecserial

all: clean compile link

clean:
	rm -f $(TARGET_DIR)/$(MAINFILE).o
	rm -f $(TARGET_DIR)/$(MAINFILE)15.$(EXT)


compile:
	mkdir -p $(TARGET_DIR)
	$(COMPILER) $(OTHEROPTS) $(MAKE_LIB) $(INCLUDE) $(DEFINITIONS) -c $(SOURCE_FILE) -o $(TARGET_DIR)/$(MAINFILE).o

link:
	mkdir -p $(TARGET_DIR)
	$(COMPILER) $(TARGET_DIR)/$(MAINFILE).o $(LINK_LIB) $(LIBFLAG) $(LIBSsundials) -o $(TARGET_DIR)/$(MAINFILE)15.$(EXT)


#make install INSTALLPATH=/home/nmurphy/coding/Repo/BiologyGit/MaximiseProbability/CRNSynthesis/bin/Debug
install:
	cp $(TARGET_DIR)/SundialsSolver15.$(EXT) $(INSTALLPATH)/libSundialsSolver15.dll.$(EXT)
	cp $(LIBPATH)/libsundials_cvode.$(EXT) $(INSTALLPATH)/libsundials_cvode.$(CVODE_EXT)
	cp $(LIBPATH)/libsundials_nvecserial.$(EXT) $(INSTALLPATH)/libsundials_nvecserial.$(NVEC_EXT)


