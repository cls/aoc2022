#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

struct range {
    long lo;
    long hi;
};

static int full_overlap(struct range x, struct range y);
static int ends_overlap(struct range x, struct range y);

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

        full_overlaps += full_overlap(x, y);
        part_overlaps += full_overlap(x, y) || ends_overlap(x, y);
    }

    printf("%d\n%d\n", full_overlaps, part_overlaps);

    return 0;
}

int
full_overlap(struct range x, struct range y)
{
    return (x.lo <= y.lo && x.hi >= y.hi) || (y.lo <= x.lo && y.hi >= x.hi);
}

int
ends_overlap(struct range x, struct range y)
{
    return (x.lo <= y.lo && x.hi >= y.lo) || (x.lo <= y.hi && x.hi >= y.hi);
}
