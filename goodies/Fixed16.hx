package goodies;

abstract Fixed16(Int) {
    inline function new(x:Int) this = x;
    inline function raw() return this;
    inline static function RAW(x:Int) return new Fixed16(x);

    public static inline var MIN_VALUE:Fixed16 = RAW(0x80000000);
    public static inline var MAX_VALUE:Fixed16 = RAW(0x7fffffff);
    public static inline var EPSILON  :Fixed16 = RAW(1);

    inline static function __int(x:Float)
        return #if flash untyped __int__(x)
           #elseif cpp   untyped __global__.__int__(x)
           #elseif js    cast(x) | 0
           #elseif java  cast x
           #else          Std.int(x)
           #end;

    @:from public static inline function fromf(x:Float) {
        #if debug
            if (x >= 32768.0 || x < -32768.0) throw "Conversion to Fixed16 will overflow";
        #end
        return RAW(__int(x*65536.0));
    }
    @:from public static inline function fromi(x:Int) {
        #if debug
            if (x >= 1<<15 || x < -(1<<15)) throw "Conversion to Fixed16 will overflow";
        #end
        return RAW(x<<16);
    }

         public inline function float():Float return this*0.0000152587890625;
         public inline function int  ():Int   return this>>16;
    @:to public inline function tof  ():Float return float();
    @:to public inline function toi  ():Int   return int();

    public inline function toString() return Std.string(float());

    @:op(A+B) public inline static function add(f:Fixed16, g:Fixed16) {
        #if debug
            var fr:Float = f.raw();
            var gr:Float = g.raw();
            if (fr+gr >= 2147483648.0 || fr+gr < -2147483648.0) throw "Addition of Fixed16 values will overflow";
        #end
        return RAW(f.raw()+g.raw());
    }
    @:commutative @:op(A+B) public inline static function addf(f:Fixed16, g:Float) return add(f, fromf(g));
    @:commutative @:op(A+B) public inline static function addi(f:Fixed16, g:Int)   return add(f, fromi(g));

    @:op(A-B) public inline static function sub(f:Fixed16, g:Fixed16) {
        #if debug
            var fr:Float = f.raw();
            var gr:Float = g.raw();
            if (fr-gr >= 2147483648.0 || fr-gr < -2147483648.0) throw "Subtraction of Fixed16 values will overflow";
        #end
        return RAW(f.raw()-g.raw());
    }
    @:op(A-B) public inline static function subf(f:Fixed16, g:Float) return sub(f, fromf(g));
    @:op(A-B) public inline static function subi(f:Fixed16, g:Int)   return sub(f, fromi(g));
    @:op(A-B) public inline static function fsub(f:Float, g:Fixed16) return sub(fromf(f), g);
    @:op(A-B) public inline static function isub(f:Int,   g:Fixed16) return sub(fromi(f), g);

    @:op(-A) public inline static function neg(f:Fixed16) {
        #if debug
            if (f.raw() == 0x80000000) throw "Negation of Fixed16 will overflow";
        #end
        return RAW(-f.raw());
    }

    @:op(A*B) public inline static function mul(f:Fixed16, g:Fixed16) {
        #if debug
            var fr:Float = f.raw();
            var gr:Float = g.raw();
            var res:Float = fr*gr*0.0000152587890625;
            if (res >= 2147483648.0 || res < -2147483648.0) throw "Multiplication of Fixed16 values will overflow";
        #end
        var ff = f&0xffff; var fi = f>>16;
        var gf = g&0xffff; var gi = g>>16;
        return RAW(((fi*gi)<<16) + fi*gf + ff*gi);
    }
    @:commutative @:op(A*B) public inline static function mulf(f:Fixed16, g:Float) return mul(f, fromf(g));
    @:commutative @:op(A*B) public inline static function muli(f:Fixed16, g:Int)   return mul(f, fromi(g));

    @:op(A/B) public inline static function div(f:Fixed16, g:Fixed16) {
        #if debug
            var fr:Float = f.raw();
            var gr:Float = g.raw();
            var res:Float = fr/gr*65536.0;
            if (res >= 2147483648.0 || res < -2147483648.0) throw "Division of Fixed16 values will overflow";
        #end
        return RAW(__int((f.raw()/g.raw())*65536.0));
    }
    @:op(A/B) public inline static function divf(f:Fixed16, g:Float) return div(f, fromf(g));
    @:op(A/B) public inline static function divi(f:Fixed16, g:Int)   return div(f, fromi(g));
    @:op(A/B) public inline static function fdiv(f:Float, g:Fixed16) return div(fromf(f), g);
    @:op(A/B) public inline static function idiv(f:Int,   g:Fixed16) return div(fromi(f), g);

                  @:op(A==B) public inline static function eq  (f:Fixed16, g:Fixed16) return f.raw() == g.raw();
    @:commutative @:op(A==B) public inline static function eqf (f:Fixed16, g:Float)   return eq (f, fromf(g));
    @:commutative @:op(A==B) public inline static function eqi (f:Fixed16, g:Int)     return eq (f, fromi(g));

                  @:op(A!=B) public inline static function neq (f:Fixed16, g:Fixed16) return f.raw() != g.raw();
    @:commutative @:op(A!=B) public inline static function neqf(f:Fixed16, g:Float)   return neq(f, fromf(g));
    @:commutative @:op(A!=B) public inline static function neqi(f:Fixed16, g:Int)     return neq(f, fromi(g));

                  @:op(A>B)  public inline static function ngt (f:Fixed16, g:Fixed16) return f.raw() >  g.raw();
    @:commutative @:op(A>B)  public inline static function ngtf(f:Fixed16, g:Float)   return ngt(f, fromf(g));
    @:commutative @:op(A>B)  public inline static function ngti(f:Fixed16, g:Int)     return ngt(f, fromi(g));

                  @:op(A<B)  public inline static function nlt (f:Fixed16, g:Fixed16) return f.raw() <  g.raw();
    @:commutative @:op(A<B)  public inline static function nltf(f:Fixed16, g:Float)   return nlt(f, fromf(g));
    @:commutative @:op(A<B)  public inline static function nlti(f:Fixed16, g:Int)     return nlt(f, fromi(g));

                  @:op(A>=B) public inline static function nge (f:Fixed16, g:Fixed16) return f.raw() >= g.raw();
    @:commutative @:op(A>=B) public inline static function ngef(f:Fixed16, g:Float)   return nge(f, fromf(g));
    @:commutative @:op(A>=B) public inline static function ngei(f:Fixed16, g:Int)     return nge(f, fromi(g));

                  @:op(A<=B) public inline static function nle (f:Fixed16, g:Fixed16) return f.raw() <= g.raw();
    @:commutative @:op(A<=B) public inline static function nlef(f:Fixed16, g:Float)   return nle(f, fromf(g));
    @:commutative @:op(A<=B) public inline static function nlei(f:Fixed16, g:Int)     return nle(f, fromi(g));
}

