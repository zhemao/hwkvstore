#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "accel-inst.h"
#include "accel.h"

int main(int argc, char *argv[])
{
	int opt, mode = -1;

	while ((opt = getopt(argc, argv, "wr")) != -1) {
		switch (opt) {
		case 'w':
			mode = 1;
			break;
		case 'r':
			mode = 0;
			break;
		default:
			fprintf(stderr, "Unknown option -%c\n", opt);
			return -1;
		}
	}

	if (mode == -1) {
		fprintf(stderr, "Must specify -r or -w\n");
		return -1;
	}
	if (mode == 1)
		write_mode();
	else
		read_mode();

	return 0;
}
