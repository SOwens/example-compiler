COMPILER = ../src/compile.byte
EXEC_FILES = fact.x fact2.x

.PHONY : all clean

all : $(EXEC_FILES)

%.s : %.simp $(COMPILER)
	$(COMPILER) $<

%.o : %.s
	nasm -f macho64 $<

%.x : %.o
	ld $< -o $@

.PRECIOUS : %.s

clean :
	rm -f *.o *.s $(EXEC_FILES)
