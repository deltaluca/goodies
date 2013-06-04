package goodies;

import goodies.Maybe;

class Func {
    public static function call<T>(f:Void->T) return f();
    public static function call1<S,T>(f:S->T, x:S) return f(x);
    public static function call2<S,T,R>(f:R->S->T, x:R, y:S) return f(x,y);
    public static function call3<S,T,R,Q>(f:R->S->T->Q, x:R, y:S, z:T) return f(x,y,z);
    public static function call4<S,T,R,Q,P>(f:R->S->T->Q->P, x:R, y:S, z:T, w:Q) return f(x,y,z,w);
}
