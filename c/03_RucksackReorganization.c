#define _DEFAULT_SOURCE // for ffsll(3)

#include <assert.h>
#include <stdio.h>
#include <string.h>

enum { ElvesPerGroup = 3 };

static long long item_to_bitset(char c);
static int item_priority(long long set);

int
main(void)
{
    char buf[BUFSIZ], *s;
    long sum = 0;
    long groupsum = 0;
    long long groupset = -1;

    int i;

    for (i = 0; (s = fgets(buf, sizeof buf, stdin)); i++) {
        size_t len = strlen(s);

        assert(len > 0); // otherwise s would be null
        assert(s[len-1] == '\n');

        len--;

        assert(len % 2 == 0);

        long long set1 = 0, set2 = 0;

        const char *p = s;
        while (p != &s[len / 2]) {
            set1 |= item_to_bitset(*p++);
        }
        while (p != &s[len]) {
            set2 |= item_to_bitset(*p++);
        }

        sum += item_priority(set1 & set2);

        groupset &= set1 | set2;

        if ((i + 1) % ElvesPerGroup == 0) {
            groupsum += item_priority(groupset);
            groupset = -1;
        }
    }

    assert(i % ElvesPerGroup == 0);

    printf("%ld\n%ld\n", sum, groupsum);

    return 0;
}

long long
item_to_bitset(char c)
{
    if (c >= 'a' && c <= 'z') {
        return 1LL << (c - 'a');
    }
    if (c >= 'A' && c <= 'Z') {
        return 1LL << (c - 'A' + 26);
    }

    assert(0);
    return 0;
}

int
item_priority(long long set)
{
    assert(set != 0);
    assert((set & (set-1)) == 0); // power of 2

    return ffsll(set);
}
