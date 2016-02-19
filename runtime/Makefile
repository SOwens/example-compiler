all : array.o io.o

array.o : array.c
	gcc -c array.c -o array.o -std=c99

io.o : io.c
	gcc -c io.c -o io.o -std=c99

clean :
	rm -f io.o array.o
