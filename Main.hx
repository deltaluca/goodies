import goodies.Maybe;
import goodies.Lazy;
import goodies.FFT;
import goodies.Builder;
import goodies.Func;
using goodies.Assert;


class Main implements MaybeEnv implements LazyEnv {
    static var enter:Maybe<Void->Void> = null;
    static function tryit() {
        enter.runOr(Func.call0);
    }
    static function main() {
        tryit();
        enter = function () trace("hi");
        tryit();

        function add(a:Int, b:Int, c:Int, d:Int) return a+b+c+d;

        var maybeAdd = Maybe.liftM4(add);
        trace(maybeAdd(10, 20, 30, 40));
        trace(maybeAdd(10, null, 30, 40));
        trace(maybeAdd(null, 20, 30, 40));
        trace(maybeAdd(null, null, 30, 40));
    }
}
