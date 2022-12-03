#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

enum { NumTopElves = 3 };

int
main(void)
{
    char buf[BUFSIZ], *s;
    long top[NumTopElves] = {0};
    long sum = 0;

    while ((s = fgets(buf, sizeof buf, stdin))) {
        if (*s != '\n') {
            char *e;
            long n = strtol(s, &e, 10);

            assert(*e == '\n');
            assert(n >= 0);

            sum += n;
        } else {
            long *mintop = &top[0];

            for (int i = 1; i < NumTopElves; i++) {
                if (*mintop > top[i]) {
                    mintop = &top[i];
                }
            }

            if (*mintop < sum) {
                *mintop = sum;
            }

            sum = 0;
        }
    }

    long max = top[0];
    long topsum = top[0];

    for (int i = 1; i < NumTopElves; i++) {
        if (max < top[i]) {
            max = top[i];
        }
        topsum += top[i];
    }

    printf("%ld\n%ld\n", max, topsum);

    return 0;
}
