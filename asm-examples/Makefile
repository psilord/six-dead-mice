all: img

img: img.o
	objcopy img.o img -O binary
	objdump -D -b binary -marm img > img.lst

img.o: img.s
	as img.s -o img.o

cold-boot: all
	sudo /home/psilord/code/six-dead-mice/tools/bbb_ctrl --boot-spio-send-kernel img

boot: all
	sudo /home/psilord/code/six-dead-mice/tools/bbb_ctrl --send-kernel img

test: boot
	kermit -c

clean:
	rm -f *.o *.bin *.lst img
