all:
	haxe -main Main -swf main.swf -debug -D assertions
	debugfp main.swf

.PHONY: haxelib
haxelib:
	rm -f goodies.zip
	zip -r goodies goodies haxelib.json -x \*goodies/*.sw[op]\*
	haxelib local goodies.zip
