#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

struct range {
    long lo;
    long hi;
};

static int fully_contains(struct range x, struct range y);
static int partly_contains(struct range x, struct range y);

int
main(void)
{
    char buf[BUFSIZ], *s;
    int full_overlaps = 0, part_overlaps = 0;

    while ((s = fgets(buf, sizeof buf, stdin))) {
        char *e;
        long a = strtol(s, &e, 10);

        assert(*e == '-');
        if (!*e) return 1;

        s = e + 1;
        long b = strtol(s, &e, 10);

        assert(*e == ',');
        if (!*e) return 1;

        s = e + 1;
        long c = strtol(s, &e, 10);

        assert(*e == '-');
        if (!*e) return 1;

        s = e + 1;
        long d = strtol(s, &e, 10);

        assert(*e == '\n');

        struct range x = {a, b};
        struct range y = {c, d};

        full_overlaps +=  fully_contains(x, y) ||  fully_contains(y, x);
        part_overlaps += partly_contains(x, y) || partly_contains(y, x);
    }

    printf("%d\n%d\n", full_overlaps, part_overlaps);

    return 0;
}

int
fully_contains(struct range x, struct range y)
{
    return x.lo <= y.lo && x.hi >= y.hi;
}

int
partly_contains(struct range x, struct range y)
{
    return (x.lo <= y.lo && x.hi >= y.lo) || (x.lo <= y.hi && x.hi >= y.hi);
}
