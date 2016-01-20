COMPILER = ../src/compile.byte
EXEC_FILES = fact.x

.PHONY : all clean

all : $(EXEC_FILES)

%.s : %.simp $(COMPILER)
	$(COMPILER) $<

%.o : %.s
	nasm -f macho64 $<

%.x : %.o ../runtime/io.o
	# Use gcc to do the linking since we want to include libc, and our
	# object code starts from  a C-style main function. We do not want to 
	# enter at _start (which is where the OS dumps you on process
	# creation), because there's a bunch of book keeping and alignment
	# stuff to sort out. gcc/libc include code to do that for us.
	gcc ../runtime/io.o $< -o $@

.PRECIOUS : %.s

clean :
	rm -f *.o *.s $(EXEC_FILES)
