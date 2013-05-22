package goodies;

#if macro
    import haxe.macro.Expr;
    import haxe.macro.Context;
#end

import goodies.MacroUtils;

@:autoBuild(goodies.LazyEnvImpl.run()) @:remove extern interface LazyEnv {}
class LazyEnvImpl {
#if macro
    public static function run() {
        var intf = MacroUtils.isInterface();
        var fields = Context.getBuildFields();
        var gields = [];
        for (f in fields) {
            var meta = MacroUtils.hasMeta(f, ":lazyVar");
            if (meta == null) continue;
            switch (f.kind) {
            case FVar(t, e):
                if (t == null) t = Context.toComplexType(Context.typeof(e));
                f.kind = FProp("get", "set", t, null);
                if (intf) continue;

                var isStatic = f.access.filter(Type.enumEq.bind(AStatic)).length != 0;
                var a1 = [APrivate, AInline];
                var a2 = [APrivate];
                if (isStatic) {
                    a1.push(AStatic);
                    a2.push(AStatic);
                }
                var g = macro $i{'___${f.name}'};
                if (e != null) {
                    gields.push(MacroUtils.field(macro function():$t {
                        return if ($g == null) $g = $e else $g;
                    }, a1, "get_"+f.name));
                }
                else {
                    gields.push(MacroUtils.field(macro function():$t {
                        #if debug
                            if ($g == null) throw 'Error: ${$v{Context.currentPos()}}, field ${$v{f.name}} has not been initialised yet';
                        #end
                        return $g;
                    }, a1, "get_"+f.name));
                }
                gields.push(MacroUtils.field(macro function(param:$t):$t {
                    $g = param;
                    return $i{"get_"+f.name}();
                }, a1, "set_"+f.name));
                gields.push(MacroUtils.field((macro var _:$t), a2, '___${f.name}'));
            default:
            }
        }
        fields = fields.concat(gields);
        return fields;
    }
#end
}
