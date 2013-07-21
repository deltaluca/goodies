package goodies;
#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
import goodies.MacroUtils;
class BitFields {
    public static function run() {
        var self = switch (MacroUtils.self()) {
            case TPath({sub:sub}):
                Context.toComplexType(Context.getType(sub.substr(0,-6)));
            default: null;
        };

        var total = 0;
        var fields = Context.getBuildFields();
        for (f in fields) {
            if (MacroUtils.hasMeta(f, ":compound") != null) continue;
            switch (f.kind) {
            case FVar(t,e):
                f.kind = FVar(self, macro __from($e));
                f.access = [AStatic,APublic,AInline];
                total |= switch(e.expr) {
                    case EConst(CInt(n)): Std.parseInt(n);
                    default: 0;
                };
            default:
            }
        }

        return fields.concat((macro class {
            static inline function __from(x:Int):$self return untyped x;
            static inline function __to(x:$self):Int return untyped x;

            public static inline var zero:$self = __from(0);

            @:op(A|B) public static inline function __or (a:$self, b:$self):$self
                return __from(__to(a) | __to(b));
            @:op(A&B) public static inline function __and(a:$self, b:$self):$self
                return __from(__to(a) & __to(b));
            @:op(A^B) public static inline function __xor(a:$self, b:$self):$self
                return __from(__to(a) ^ __to(b));

            @:op(A-B) public static inline function __sub(a:$self, b:$self):$self
                return __from(__to(a) & (~__to(b)));

            @:op(~A) public static inline function __not(a:$self):$self
                return __from($v{total} ^ __to(a));

            @:op(A!=B) public static inline function __neq(a:$self, b:$self):Bool
                return __to(a) != __to(b);
            @:op(A==B) public static inline function __eq (a:$self, b:$self):Bool
                return __to(a) == __to(b);

            @:op(A<=B) public static inline function __leq(a:$self, b:$self):Bool
                return __to(a) & __to(b) == __to(a);
            @:op(A>=B) public static inline function __geq(a:$self, b:$self):Bool
                return __to(a) & __to(b) == __to(b);
            @:op(A<B) public static inline function __lt(a:$self, b:$self):Bool {
                var and = __to(a) & __to(b);
                return and == __to(a) && (__to(a) ^ and) != 0;
            }
            @:op(A>B) public static inline function __gt(a:$self, b:$self):Bool {
                var and = __to(a) & __to(b);
                return and == __to(b) && (__to(b) ^ and) != 0;
            }

            @:impl public static inline function toString(a:Int):String {
                if (a == 0) return "zero";
                else {
                    var ret = "";
                    $b{[for (f in fields)
                        if (MacroUtils.hasMeta(f, ":compound") != null) macro {}
                        else macro if (a & __to($p{[f.name]}) != 0) {
                            if (ret.length != 0) ret += "|";
                            ret += $v{f.name};
                        }
                    ]};
                    return ret;
                }
            }
        }).fields);
    }
}
#end
