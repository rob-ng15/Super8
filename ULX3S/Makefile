.DEFAULT: SYSTEM8-ULX3S.ice
	silice-make.py -s SYSTEM8-ULX3S.ice -b $@ -p audio,basic,buttons,hdmi,sdcard,sdram,uart,gpio,i2c -o BUILD_$(subst :,_,$@) -t shell

verilator:
	silice-make.py -s SYSTEM8-ULX3S.ice -b $@ -p basic,vga,sdram -o BUILD_$(subst :,_,$@) -t shell

clean:
	rm -rf BUILD_*
