#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>

char *inp;

int main(int argc, char *argv[])
{
	int i, fd;
	struct stat stat;

	if (argc < 2) {
		fprintf(stderr, "Usage: %s file...\n", argv[0]);
		exit(EXIT_FAILURE);
	}
	for (i = 1; i < argc; i++) {
		fd = open(argv[1], O_RDONLY);
		if (fd == -1) {
			fprintf(stderr, "%s: error: %s: %s\n",
					argv[0], argv[1], strerror(errno));
			exit(EXIT_FAILURE);
		}
		if (fstat(fd, &stat) == -1) {
			// TODO: Make less repetitive
			fprintf(stderr, "%s: error: %s: %s\n",
					argv[0], argv[1], strerror(errno));
			exit(EXIT_FAILURE);
		}
		inp = mmap(NULL, stat.st_size + 1, PROT_READ | PROT_WRITE,
				MAP_PRIVATE, fd, 0);
		inp[stat.st_size] = '\0';
	}
}
