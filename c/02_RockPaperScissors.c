#include <assert.h>
#include <stdio.h>

static char outcome_of_shapes(char a, char x);
static char shape_for_outcome(char a, char x);

int
main(void)
{
    char buf[5], *s;
    long score1 = 0, score2 = 0;

    while ((s = fgets(buf, sizeof buf, stdin))) {
        assert(s[0] >= 'A' && s[0] <= 'C');
        assert(s[1] == ' ');
        assert(s[2] >= 'X' && s[2] <= 'Z');
        assert(s[3] == '\n');

        char a = s[0] - 'A';
        char x = s[2] - 'X';

        score1 += x+1 + outcome_of_shapes(a, x)*3;
        score2 += shape_for_outcome(a, x)+1 + x*3;
    }

    printf("%ld\n%ld\n", score1, score2);

    return 0;
}

char
outcome_of_shapes(char a, char x)
{
    return (x - a + 4) % 3;
}

char
shape_for_outcome(char a, char x)
{
    return (x + a + 2) % 3;
}
