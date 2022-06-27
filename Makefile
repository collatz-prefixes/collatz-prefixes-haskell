# Binary targets
TGTDIR  := bin
TARGET  ?= collatz
TESTTGT  = test

# Code extensions
SRCEXT	:= .hs

# Code directories
OBJDIR  := build
SRCDIR	:= src

# Compiler and entry point
HC			:= ghc
ENTRY   ?= ./$(SRCDIR)/Main$(SRCEXT)

# Code files
SRCS		 = $(shell find $(SRCDIR) -type f -name '*$(SRCEXT)') 
 
# First rule
all: $(TARGET) | $(TGTDIR)
	@mv $(TARGET) $(TGTDIR)/

# Compile and link
$(TARGET): $(SRCS) | $(OBJDIR)
	$(HC) --make $(ENTRY) -odir ./$(OBJDIR) -hidir ./$(OBJDIR) -o $(TARGET) -i$(SRCDIR)

# Objects directory
$(OBJDIR):
	mkdir -p $(OBJDIR)  

# Target directory
$(TGTDIR):
	mkdir -p $(TGTDIR)  

# Run program with arguments
run: 
	@./$(TGTDIR)/$(TARGET) $(ARGUMENTS)

# Run tests
validate: 
	@./$(TGTDIR)/$(TESTTGT)

# Clean objects and binaries
clean: 
	rm -f $(TARGET) $(OBJDIR)/*

# Create a test binary
tests:
	@make clean
	@make TARGET=$(TESTTGT) ENTRY=./$(SRCDIR)/Tests$(SRCEXT)

# Clean and make again
again:
	@make clean
	@make

# Diagnostic to show files
show:
	@echo $(SRCS)

.PHONY: all run validate clean again test show