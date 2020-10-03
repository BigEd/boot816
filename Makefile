

BASE ?= 0x8000
SRECORD_D ?=0

SHELL=/bin/tcsh -f

all: boot816.bin irqhandler.srec

clean:
	rm -f *.o *.lst *.srec *.bin

%.o : %.as
	ca65 -l $< -D BASE=$(BASE) -D SRECORD_D=${SRECORD_D}

%.bin: %.o
	cl65 $< --target none --start-addr $(BASE) -o $@

%.srec: %.bin
	srec_cat $< > $@ -Binary -offset $(BASE) -crlf -data-only
	awk '/PAGE/,/RUN/' srec2telnet
	cat $@
	echo S1x
