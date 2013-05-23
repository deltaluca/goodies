import goodies.Maybe;
import goodies.Lazy;
import goodies.FFT;
import goodies.Builder;
import goodies.Func;
using goodies.Assert;

import ogl.GLM;

class Main implements MaybeEnv implements LazyEnv implements Builder {
    @:builder public var name:Maybe<Vec2> = null;
    public function get():Vec2 return [1,2];
    public function new() {
        trace(getName());
        name([10,20]);
        trace(getName());
        name(null);
        trace(getName());
        name(get());
        trace(getName());
    }
    static function main() new Main();
}
