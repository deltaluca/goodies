package goodies;

import haxe.macro.Expr;
import haxe.macro.Context;
import haxe.macro.Type;

import goodies.Maybe;

class MacroUtils {
#if macro
    public static function field(e:Expr, access:Array<Access>, name:String, iface=false):Field {
        return {
            pos: e.pos,
            name: name,
            meta: null,
            doc: null,
            access: access,
            kind: switch (e.expr) {
                case EVars([{type:t, expr:e}]): FVar(t, e);
                case EFunction(_,f): {
                    if (iface) f.expr = null;
                    FFun(f);
                }
                default: null;
            }
        };
    }

    public static function self() {
        var local = Context.getLocalClass();
        return Context.toComplexType(TInst(local, [for (p in local.get().params) p.t]));
    }

    public static function isInterface() {
        var local = Context.getLocalClass();
        return local.get().isInterface;
    }

    public static function hasMeta(f:Field, name:String):Maybe<Array<Expr>> {
        if (f.meta == null) return null;
        for (m in f.meta) if (m.name == name) return m.params;
        return null;
    }
#end
}
