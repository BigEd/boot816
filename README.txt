The boot816 project makes a ROM for a BBC micro

The ROM has several purposes
 - set serial redirect at boot time
 - *commands for detecting and testing 65816 hardware

The ROM source is boot816.as and the ROM can be built with
  make boot816.bin
or
  make

The ROM image can be tested in emulation using
  ../lib6502/run6502 -B -l c000 ../rom-images/OS12.ROM ../rom-images/BASIC2.ROM boot816.bin
or
  ../lib65816/run65816 -B -l c000 ../rom-images/OS12.ROM ../rom-images/BASIC2.ROM boot816.bin


There are some auxiliary files here too:
 - Makefile
 - srec2telnet  a utility including a BASIC srecord loader and a slowed-down serial port loader
 - irqhandler.as  a work-in-progress to provide an 816-mode IRQ handler which hands off to 6502-mode
 - memtest.as   a cut-down version of the memory test code from boot816, to load and run at 0x2000
     make -B memtest.srec BASE=0x2000

An srec can be piped slowly to a serial-connected beeb using something like
  perl -e ' \
     $|=1;while (<>) {select undef, undef, undef, 0.4; print $_."\r"}sleep(2)' | \
    telnet localhost 3000


