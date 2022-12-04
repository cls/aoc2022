CFLAGS  = -pedantic -std=c99 -O3 -Wall -Werror
LDFLAGS =

BIN = \
    bin/01_CalorieCounting \
    bin/02_RockPaperScissors \
    bin/03_RucksackReorganization \
    bin/04_CampCleanup \

all: $(BIN)

bin/%: %.c | bin
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

bin:
	mkdir -p $@

clean:
	rm -r bin

.PHONY: all clean
