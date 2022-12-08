#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int
main(void)
{
    size_t len = 0;
    size_t cap = BUFSIZ;
    char *buf = malloc(cap);
    if (!buf) return 1;

    int c;
    int w = 0;
    while ((c = getchar()) != EOF) {
        if (c == '\n') {
            if (w == 0) {
                w = len;
            } else {
                assert(len % w == 0);
            }
        } else {
            if (len == cap) {
                cap *= 2;
                buf = realloc(buf, cap);
                if (!buf) return 1;
            }
            buf[len++] = c;
        }
    }

    assert(len % w == 0);

    int h = len / w;

#define POS(X, Y) ((X) + (Y)*h)

    char *vis = calloc(w * h, sizeof *vis);
    if (!vis) return 1;

#define MARK_VIS(N, X, Y)           \
    do {                            \
        if ((N) < buf[POS(X, Y)]) { \
            (N) = buf[POS(X, Y)];   \
            vis[POS(X, Y)] = 1;     \
        }                           \
    } while (0)

    for (int y = 0; y < h; y++) {
        {
            // Left
            char min = '\0';
            for (int x = 0; x < w; x++) {
                MARK_VIS(min, x, y);
            }
        }
        {
            // Right
            char min = '\0';
            for (int x = w-1; x >= 0; x--) {
                MARK_VIS(min, x, y);
            }
        }
    }

    for (int x = 0; x < w; x++) {
        {
            // Top
            char min = '\0';
            for (int y = 0; y < h; y++) {
                MARK_VIS(min, x, y);
            }
        }
        {
            // Bottom
            char min = '\0';
            for (int y = h-1; y >= 0; y--) {
                MARK_VIS(min, x, y);
            }
        }
    }

#undef MARK_VIS

    int nvis = 0;
    for (int i = 0; i < len; i++) {
        nvis += vis[i];
    }

    free(vis);

    long max = 0;

    for (int y = 0; y < h; y++) {
        for (int x = 0; x < w; x++) {
            long up = 0, left = 0, down = 0, right = 0;

            for (int y2 = y-1; y2 >= 0; y2--) {
                up++;
                if (buf[POS(x, y2)] >= buf[POS(x, y)]) break;
            }

            for (int x2 = x-1; x2 >= 0; x2--) {
                left++;
                if (buf[POS(x2, y)] >= buf[POS(x, y)]) break;
            }

            for (int y2 = y+1; y2 < h; y2++) {
                down++;
                if (buf[POS(x, y2)] >= buf[POS(x, y)]) break;
            }

            for (int x2 = x+1; x2 < w; x2++) {
                right++;
                if (buf[POS(x2, y)] >= buf[POS(x, y)]) break;
            }

            long n = up * left * down * right;

            if (max < n) {
                max = n;
            }
        }
    }

#undef POS

    free(buf);

    printf("%d\n%ld\n", nvis, max);

    return 0;
}
