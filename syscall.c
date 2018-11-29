
#include <sys/types.h>
#include <sys/syscall.h>
#include <fcntl.h>
#include <stdio.h>

int main(int argc, char **argv)
{
	if (argc != 2)
		return 1;
	int res = syscall(SYS_open, argv[1], O_RDWR, 0);

	printf("Res: %d\n", res);
}

