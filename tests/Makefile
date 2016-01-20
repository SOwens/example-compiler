COMPILER = ../src/compile.byte
EXEC_FILES = fact.x

.PHONY : all clean

all : $(EXEC_FILES)

%.s : %.simp $(COMPILER)
	$(COMPILER) $<

%.o : %.s
	nasm -f macho64 $<

%.x : %.o ../runtime/io.o
	ld -lc ../runtime/io.o $< -o $@

.PRECIOUS : %.s

clean :
	rm -f *.o *.s $(EXEC_FILES)
