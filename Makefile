all:
	haxe -main Main -swf main.swf -debug
	debugfp main.swf

.PHONY: haxelib
haxelib:
	rm -f goodies.zip
	zip -r goodies goodies haxelib.json -x \*goodies/*.swp\*
	haxelib local goodies.zip
