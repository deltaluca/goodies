package goodies;

import haxe.macro.Expr;
import haxe.macro.Context;

/**
 * #debug only assertions.
 */
class Assert {
    macro public static function assert(e:Expr) {
        if (Context.defined("assertions")) {
            var pos = Context.currentPos();
            var print = (new haxe.macro.Printer()).printExpr(e);
            return macro {
                if (!($e)) throw 'Assertion Error: ${$v{pos}} : ${$v{print}}';
            };
        }else {
            return macro {};
        }
    }
}
