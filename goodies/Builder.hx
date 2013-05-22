package goodies;

#if macro
    import haxe.macro.Expr;
    import haxe.macro.Context;
#end

import goodies.MacroUtils;

@:autoBuild(goodies.MacrosBuilder.run())
interface Builder {}

class MacrosBuilder {
#if macro
    public static function run() {
        var self = MacroUtils.self();
        var iface = MacroUtils.isInterface();

        var fields = Context.getBuildFields();
        var gields = [];
        var inits = [];
        var access = if (iface) [APublic] else [APublic, AInline];
        for (f in fields) {
            var meta = MacroUtils.hasMeta(f, ":builder");
            if (meta == null) continue;
            var react = macro {};
            var meta = meta.extract();
            for (m in meta) {
                switch (m) {
                case macro ret=$i{e}:
                    self = Context.toComplexType(Context.getType(e));
                case macro react=$e:
                    react = macro $e(param);
                default:
                }
            }
            switch (f.kind) {
            case FVar(t,e):
                // Prefix field with _
                var fname = f.name;
                f.name = '_$fname';

                // Move initialisation to constructor
                if (e != null) {
                    if (t == null) t = Context.toComplexType(Context.typeof(e));
                    inits.push(macro $i{f.name} = $e);
                    f.kind = FVar(t, null);
                }

                // Remove on interfaces.
                if (iface) f.kind = null;

                // Add builder getter/setter
                gields.push(MacroUtils.field(macro function (param:$t):$self {
                    $react;
                    $i{f.name} = param;
                    return this;
                }, access, fname, iface));
                var fname2 = "get"+fname.charAt(0).toUpperCase()+fname.substr(1);
                gields.push(MacroUtils.field(macro function ():$t {
                    return $i{f.name};
                }, access, fname2, iface));
            default:
            }
        }
        for (f in fields) {
            if (f.name == "new") {
                switch (f.kind) {
                case FFun(f):
                    inits.push(f.expr);
                    f.expr = macro $b{inits};
                default:
                }
            }
        }
        fields = fields.filter(function (f) return f.kind != null);
        fields = fields.concat(gields);
//        trace("\n"+fields.map((new haxe.macro.Printer()).printField).join("\n"));
        return fields;
    }
#end
}
