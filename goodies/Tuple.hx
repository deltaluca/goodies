package goodies;

typedef T_2<S,T> = { v0 : S, v1 : T };
typedef T_3<S,T,R> = {> T_2<S,T>, v2 : R };
typedef T_4<S,T,R,P> = {> T_3<S,T,R>, v3 : P };
typedef T_5<S,T,R,P,Q> = {> T_4<S,T,R,P>, v4 : Q };

abstract T2<S,T>(T_2<S,T>) from T_2<S,T> to T_2<S,T> {
    inline public function new (s:S,t:T) this = {v0:s,v1:t};
    public inline static function make<S,T>(s:S,t:T) return new T2(s,t);
    public var v0(get,set):S; inline function get_v0() return this.v0; inline function set_v0(v0:S) return this.v0 = v0;
    public var v1(get,set):T; inline function get_v1() return this.v1; inline function set_v1(v1:T) return this.v1 = v1;
    inline public function toString() return '($v0,$v1)';
}
abstract T3<S,T,R>(T_3<S,T,R>) from T_3<S,T,R> to T_3<S,T,R> {
    inline public function new (s:S,t:T,r:R) this = {v0:s,v1:t,v2:r};
    public inline static function make<S,T,R>(s:S,t:T,r:R) return new T3(s,t,r);
    public var v0(get,set):S; inline function get_v0() return this.v0; inline function set_v0(v0:S) return this.v0 = v0;
    public var v1(get,set):T; inline function get_v1() return this.v1; inline function set_v1(v1:T) return this.v1 = v1;
    public var v2(get,set):R; inline function get_v2() return this.v2; inline function set_v2(v2:R) return this.v2 = v2;
    inline public function toString() return '($v0,$v1,$v2)';
}
abstract T4<S,T,R,P>(T_4<S,T,R,P>) from T_4<S,T,R,P> to T_4<S,T,R,P> {
    inline public function new (s:S,t:T,r:R,p:P) this = {v0:s,v1:t,v2:r,v3:p};
    public inline static function make<S,T,R,P>(s:S,t:T,r:R,p:P) return new T4(s,t,r,p);
    public var v0(get,set):S; inline function get_v0() return this.v0; inline function set_v0(v0:S) return this.v0 = v0;
    public var v1(get,set):T; inline function get_v1() return this.v1; inline function set_v1(v1:T) return this.v1 = v1;
    public var v2(get,set):R; inline function get_v2() return this.v2; inline function set_v2(v2:R) return this.v2 = v2;
    public var v3(get,set):P; inline function get_v3() return this.v3; inline function set_v3(v3:P) return this.v3 = v3;
    inline public function toString() return '($v0,$v1,$v2,$v3)';
}
abstract T5<S,T,R,P,Q>(T_5<S,T,R,P,Q>) from T_5<S,T,R,P,Q> to T_5<S,T,R,P,Q> {
    inline public function new (s:S,t:T,r:R,p:P,q:Q) this = {v0:s,v1:t,v2:r,v3:p,v4:q};
    public inline static function make<S,T,R,P,Q>(s:S,t:T,r:R,p:P,q:Q) return new T5(s,t,r,p,q);
    public var v0(get,set):S; inline function get_v0() return this.v0; inline function set_v0(v0:S) return this.v0 = v0;
    public var v1(get,set):T; inline function get_v1() return this.v1; inline function set_v1(v1:T) return this.v1 = v1;
    public var v2(get,set):R; inline function get_v2() return this.v2; inline function set_v2(v2:R) return this.v2 = v2;
    public var v3(get,set):P; inline function get_v3() return this.v3; inline function set_v3(v3:P) return this.v3 = v3;
    public var v4(get,set):Q; inline function get_v4() return this.v4; inline function set_v4(v4:Q) return this.v4 = v4;
    inline public function toString() return '($v0,$v1,$v2,$v3,$v4)';
}
