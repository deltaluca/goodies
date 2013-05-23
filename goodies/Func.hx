package goodies;

import goodies.Maybe;

class Func {
    public static function call<T>(f:Void->T) return f();
    public static function call1<S,T>(f:S->T, x:S) return f(x);
    public static function call2<S,T,R>(f:R->S->T, x:R, y:S) return f(x,y);
}
