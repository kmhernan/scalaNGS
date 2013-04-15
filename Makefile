SRC = src
SOURCES = $(shell ls $(SRC)/main/scala/*.scala $(SRC)/com/read/*.scala)
S = scala
SC = scalac
TARGET = bin 
CP = src/com:src/com/read
SPEC = scala.RomanSpec

compile: $(SOURCES:.scala=.class)

%.class: %.scala
	@echo "Compiling $*.scala.."
	@$(SC) -sourcepath $(SRC) -d $(TARGET) $*.scala
clean:
	@$(RM) $(SRC)/main/scala/*.class
