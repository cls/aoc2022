CFLAGS  = -pedantic -std=c11 -O3 -Wall -Werror
LDFLAGS =

BIN = \
    bin/01_CalorieCounting \
    bin/02_RockPaperScissors \
    bin/03_RucksackReorganization \
    bin/04_CampCleanup \
    bin/08_TreetopTreeHouse \
    bin/20_GrovePositioningSystem \

all: $(BIN)

bin/%: %.c | bin
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

bin:
	mkdir -p $@

clean:
	rm -r bin

.PHONY: all clean
