package goodies;

import goodies.Maybe;
import goodies.Tuple;

class Func {
    public static inline function id<S>(x:S):S return x;
    public static inline function flip<S,T,R>(f:S->T->R) return function (t:T, s:S) return f(s,t);
    public static inline function dot<S,T,R>(f:T->R, g:S->T):S->R return function (s:S) return f(g(s));

    public static inline function curry2<S,T,R>(f:S->T->R) return function (s:S) return function (t:T) return f(s,t);
    public static inline function curry3<S,T,R,P>(f:S->T->R->P) return function (s:S) return function (t:T) return function (r:R) return f(s,t,r);
    public static inline function curry4<S,T,R,P,Q>(f:S->T->R->P->Q) return function (s:S) return function (t:T) return function (r:R) return function (p:P) return f(s,t,r,p);

    public static inline function uncurry2<S,T,R>(f:S->(T->R)) return function (s:S, t:T) return f(s)(t);
    public static inline function uncurry3<S,T,R,P>(f:S->(T->(R->P))) return function (s:S, t:T, r:R) return f(s)(t)(r);
    public static inline function uncurry4<S,T,R,P,Q>(f:S->(T->(R->(P->Q)))) return function (s:S, t:T, r:R, p:P) return f(s)(t)(r)(p);

    public static inline function lift<S,T>(f:S->T):Array<S>->Array<T>
        return curry2(map)(f);

    public static inline function map<S,T>(f:S->T, xs:Array<S>):Array<T>
        return [for (x in xs) f(x)];
    public static inline function iter<S,T>(f:S->T, xs:Array<S>):Void
        for (x in xs) f(x);
    public static inline function intersperse<S>(x:S, xs:Array<S>):Array<S> {
        var ys = [];
        var fst = true;
        for (y in xs) {
            if (!fst) ys.push(x); else fst = false;
            ys.push(y);
        }
        return ys;
    }
    public static inline function intercalate<S>(x:Array<S>, xs:Array<Array<S>>):Array<S> {
        var ys = [];
        var fst = true;
        for (y in xs) {
            if (!fst) for (x in x) ys.push(x); else fst = false;
            for (y in y) ys.push(y);
        }
        return ys;
    }
    public static inline function transpose<S>(xs:Array<Array<S>>):Array<Array<S>> {
        var ys = [for (xs in xs) []];
        for (xs in xs) for (i in 0...xs.length) ys[i].push(xs[i]);
        return ys;
    }
    public static inline function subsequences<S>(xs:Array<S>):Array<Array<S>> {
        var sub = [[]];
        for (x in xs) for (i in 0...sub.length) sub.push(sub[i].concat([x]));
        return sub;
    }

    public static inline function foldl<S,T>(f:S->T->S, x:S, ys:Array<T>):S {
        var ret = x;
        for (y in ys) ret = f(ret, y);
        return ret;
    }
    public static inline function foldr<S,T>(f:S->T->T, x:T, ys:Array<S>):T {
        var ret = x;
        for (i in 0...ys.length) ret = f(ys[ys.length-1-i], ret);
        return ret;
    }
    public static inline function foldl1<S>(f:S->S->S, xs:Array<S>):S {
        var ret = xs[0];
        for (i in 1...xs.length) ret = f(ret, xs[i]);
        return ret;
    }
    public static inline function foldr1<S>(f:S->S->S, xs:Array<S>):S {
        var ret = xs[xs.length-1];
        for (i in 1...xs.length) ret = f(xs[xs.length-1-i], ret);
        return ret;
    }

    public static inline function concat<S>(xs:Array<Array<S>>):Array<S> {
        var ys = [];
        for (x in xs) for (y in x) ys.push(y);
        return ys;
    }
    public static inline function concatMap<S,T>(f:S->Array<T>, xs:Array<S>):Array<T> {
        var ys = [];
        for (x in xs) for (y in f(x)) ys.push(y);
        return ys;
    }

    public static inline function and(xs:Array<Bool>):Bool {
        var ret = true;
        for (x in xs) if (!x) { ret = false; break; }
        return ret;
    }
    public static inline function or(xs:Array<Bool>):Bool {
        var ret = false;
        for (x in xs) if (x) { ret = true; break; }
        return ret;
    }
    public static inline function any<S>(f:S->Bool, xs:Array<S>):Bool {
        var ret = false;
        for (x in xs) if (f(x)) { ret = true; break; }
        return ret;
    }
    public static inline function all<S>(f:S->Bool, xs:Array<S>):Bool {
        var ret = true;
        for (x in xs) if (!f(x)) { ret = false; break; }
        return ret;
    }

    public static inline function sum(xs:Array<Int>):Int {
        var ret = 0;
        for (x in xs) ret += x;
        return ret;
    }
    public static inline function product(xs:Array<Int>):Int {
        var ret = 1;
        for (x in xs) ret *= x;
        return ret;
    }
    public static inline function maximum(xs:Array<Int>):Int {
        var max = xs[0];
        for (i in 1...xs.length) if (xs[i] > max) max = xs[i];
        return max;
    }
    public static inline function minimum(xs:Array<Int>):Int {
        var min = xs[0];
        for (i in 1...xs.length) if (xs[i] < min) min = xs[i];
        return min;
    }

    public static inline function scanl<S,T>(f:S->T->S, x:S, ys:Array<T>):Array<S> {
        var xs = [x];
        for (y in ys) xs.push(x = f(x, y));
        return xs;
    }
    public static inline function scanr<S,T>(f:S->T->T, x:T, ys:Array<S>):Array<T> {
        var xs = [x];
        for (i in 0...ys.length) xs.unshift(x = f(ys[ys.length-1-i], x));
        return xs;
    }
    public static inline function scanl1<S>(f:S->S->S, xs:Array<S>):Array<S> {
        var x = xs[0];
        var ys = [x];
        for (i in 1...xs.length) ys.push(x = f(x, xs[i]));
        return ys;
    }
    public static inline function scanr1<S>(f:S->S->S, xs:Array<S>):Array<S> {
        var x = xs[xs.length-1];
        var ys = [x];
        for (i in 1...xs.length) ys.unshift(x = f(xs[xs.length-i-1], x));
        return ys;
    }

    public static inline function mapAccumL<S,T,R>(f:S->T->T2<S,R>, acc:S, xs:Array<T>):T2<S, Array<R>> {
        var ys = [];
        for (x in xs) {
            var nxt = f(acc, x);
            acc = nxt.v0;
            ys.push(nxt.v1);
        }
        return {v0:acc, v1:ys};
    }
    public static inline function mapAccumR<S,T,R>(f:S->T->T2<S,R>, acc:S, xs:Array<T>):T2<S, Array<R>> {
        var ys = [];
        for (i in 0...xs.length) {
            var nxt = f(acc, xs[xs.length-1-i]);
            acc = nxt.v0;
            ys.unshift(nxt.v1);
        }
        return {v0:acc, v1:ys};
    }

    public static inline function replicate<S>(n:Int, x:S):Array<S> {
        var xs = [];
        for (i in 0...n) xs.push(x);
        return xs;
    }

    public static inline function unfoldr<S,T>(f:T->Maybe<T2<S,T>>, y:T):Array<S> {
        var xs = [];
        var val:T2<S,T> = null;
        while ((val = untyped f(y)) != null) {
            xs.push(val.v0);
            y = val.v1;
        }
        return xs;
    }

    public static inline function take<S>(n:Int, xs:Array<S>):Array<S>
        return [for (i in 0...n) xs[i]];
    public static inline function drop<S>(n:Int, xs:Array<S>):Array<S>
        return [for (i in n...xs.length) xs[i]];
    public static inline function splitAt<S>(n:Int, xs:Array<S>):T2<Array<S>,Array<S>>
        return {v0:[for (i in 0...n) xs[i]], v1:[for (i in n...xs.length) xs[i]]};
    public static inline function takeWhile<S>(f:S->Bool, xs:Array<S>):Array<S> {
        var ys = [];
        for (x in xs) {
            if (f(x)) ys.push(x);
            else break;
        }
        return ys;
    }
    public static inline function dropWhile<S>(f:S->Bool, xs:Array<S>):Array<S> {
        var n = 0;
        for (x in xs) {
            if (f(x)) n++;
            else break;
        }
        return drop(n, xs);
    }
    public static inline function dropWhileEnd<S>(f:S->Bool, xs:Array<S>):Array<S> {
        var n = xs.length;
        for (i in 0...xs.length) {
            if (f(xs[xs.length-1-i])) n--;
            else break;
        }
        return take(n, xs);
    }
    public static inline function span<S>(f:S->Bool, xs:Array<S>):T2<Array<S>,Array<S>> {
        var u = [], v = [];
        for (x in xs) if (f(x)) u.push(x) else break;
        for (i in u.length...xs.length) v.push(xs[i]);
        return {v0:u, v1:v};
    }
    public static inline function _break<S>(f:S->Bool, xs:Array<S>):T2<Array<S>,Array<S>> {
        var u = [], v = [];
        for (x in xs) if (!f(x)) u.push(x) else break;
        for (i in u.length...xs.length) v.push(xs[i]);
        return {v0:u, v1:v};
    }
    public static inline function stripPrefix<S>(pre:Array<S>, xs:Array<S>):Maybe<Array<S>> {
        if (pre.length > xs.length) return null;
        else {
            var prefix = true;
            for (i in 0...pre.length) if (pre[i] != xs[i]) { prefix = false; break; }
            return if (prefix) [for (i in pre.length...xs.length) xs[i]] else null;
        }
    }
    public static inline function group<S>(xs:Array<S>):Array<Array<S>> {
        var ys = [];
        var y = null;
        var cur = null;
        for (x in xs) {
            if (cur == null) { cur = x; y = [x]; }
            else if (x != cur) { ys.push(y); cur = x; y = [x]; }
            else y.push(x);
        }
        if (cur != null) ys.push(y);
        return ys;
    }
    public static inline function inits<S>(xs:Array<S>):Array<Array<S>> {
        var init = [], ys = [];
        for (x in xs) { ys.push(init.copy()); init.push(x); }
        return ys;
    }
    public static inline function tails<S>(xs:Array<S>):Array<Array<S>> {
        var init = [], ys = [];
        for (x in xs) { ys.unshift(init.copy()); init.push(x); }
        return ys;
    }

    public static inline function find<S>(f:S->Bool, xs:Array<S>):Maybe<S> {
        var ret = null;
        for (x in xs) if (f(x)) { ret = x; break; }
        return ret;
    }
    public static inline function filter<S>(f:S->Bool, xs:Array<S>):Array<S> {
        var ys = [];
        for (x in xs) if (f(x)) ys.push(x);
        return ys;
    }
    public static inline function partition<S>(f:S->Bool, xs:Array<S>):T2<Array<S>,Array<S>> {
        var u = [], v = [];
        for (x in xs) (if (f(x)) u else v).push(x);
        return {v0:u, v1:v};
    }

    inline static function minLength(xs:Array<Array<Dynamic>>) {
        var min = xs[0].length;
        for (i in 1...xs.length) if (xs[i].length < min) min = xs[i].length;
        return min;
    }

    public static inline function zip<S,T>(xs:Array<S>, ys:Array<T>):Array<T2<S,T>>
        return [for (i in 0...minLength([xs,ys])) {v0:xs[i],v1:ys[i]}];
    public static inline function zip3<S,T,R>(xs:Array<S>, ys:Array<T>, zs:Array<R>):Array<T3<S,T,R>>
        return [for (i in 0...minLength([xs,ys,zs])) {v0:xs[i],v1:ys[i],v2:zs[i]}];
    public static inline function zip4<S,T,R,P>(xs:Array<S>, ys:Array<T>, zs:Array<R>, ws:Array<P>):Array<T4<S,T,R,P>>
        return [for (i in 0...minLength([xs,ys,zs,ws])) {v0:xs[i],v1:ys[i],v2:zs[i],v3:ws[i]}];
    public static inline function zip5<S,T,R,P,Q>(xs:Array<S>, ys:Array<T>, zs:Array<R>, ws:Array<P>, us:Array<Q>):Array<T5<S,T,R,P,Q>>
        return [for (i in 0...minLength([xs,ys,zs,ws,us])) {v0:xs[i],v1:ys[i],v2:zs[i],v3:ws[i],v4:us[i]}];

    public static inline function zipWith<S,T,R>(f:S->T->R, xs:Array<S>, ys:Array<T>):Array<R>
        return [for (i in 0...minLength([xs,ys])) f(xs[i],ys[i])];
    public static inline function zipWith3<S,T,R,P>(f:S->T->R->P, xs:Array<S>, ys:Array<T>, zs:Array<R>):Array<P>
        return [for (i in 0...minLength([xs,ys,zs])) f(xs[i],ys[i],zs[i])];
    public static inline function zipWith4<S,T,R,P,Q>(f:S->T->R->P->Q, xs:Array<S>, ys:Array<T>, zs:Array<R>, ws:Array<P>):Array<Q>
        return [for (i in 0...minLength([xs,ys,zs,ws])) f(xs[i],ys[i],zs[i],ws[i])];
    public static inline function zipWith5<S,T,R,P,Q,U>(f:S->T->R->P->Q->U, xs:Array<S>, ys:Array<T>, zs:Array<R>, ws:Array<P>, us:Array<Q>):Array<U>
        return [for (i in 0...minLength([xs,ys,zs,ws,us])) f(xs[i],ys[i],zs[i],ws[i],us[i])];

    public static inline function unzip<S,T>(xy:Array<T2<S,T>>):T2<Array<S>,Array<T>> {
        var xs = [], ys = [];
        for (xy in xy) {
            xs.push(xy.v0);
            ys.push(xy.v1);
        }
        return {v0:xs, v1:ys};
    }
    public static inline function unzip3<S,T,R>(xyz:Array<T3<S,T,R>>):T3<Array<S>,Array<T>,Array<R>> {
        var xs = [], ys = [], zs = [];
        for (xyz in xyz) {
            xs.push(xyz.v0);
            ys.push(xyz.v1);
            zs.push(xyz.v2);
        }
        return {v0:xs, v1:ys, v2:zs};
    }
    public static inline function unzip4<S,T,R,P>(xyzw:Array<T4<S,T,R,P>>):T4<Array<S>,Array<T>,Array<R>,Array<P>> {
        var xs = [], ys = [], zs = [], ws = [];
        for (xyzw in xyzw) {
            xs.push(xyzw.v0);
            ys.push(xyzw.v1);
            zs.push(xyzw.v2);
            ws.push(xyzw.v3);
        }
        return {v0:xs, v1:ys, v2:zs, v3:ws};
    }
    public static inline function unzip5<S,T,R,P,Q>(xyzwu:Array<T5<S,T,R,P,Q>>):T5<Array<S>,Array<T>,Array<R>,Array<P>,Array<Q>> {
        var xs = [], ys = [], zs = [], ws = [], us = [];
        for (xyzwu in xyzwu) {
            xs.push(xyzwu.v0);
            ys.push(xyzwu.v1);
            zs.push(xyzwu.v2);
            ws.push(xyzwu.v3);
            us.push(xyzwu.v4);
        }
        return {v0:xs, v1:ys, v2:zs, v3:ws, v4:us};
    }

    public static inline function nub<S>(xs:Array<S>):Array<S> {
        var ys = [];
        for (x in xs) {
            var found = false;
            for (y in ys) if (x == y) { found = true; break; }
            if (!found) ys.push(x);
        }
        return ys;
    }
    public static inline function delete<S>(x:S, xs:Array<S>):Array<S> {
        var ys = [];
        for (y in xs) if (x == y) { x = null; continue; } else ys.push(y);
        return ys;
    }
    public static inline function subtract<S>(from:Array<S>, val:Array<S>):Array<S> {
        var ys = [];
        var del = [for (v in val) false];
        for (f in from) {
            var found = false;
            for (i in 0...val.length) if (!del[i] && f == val[i]) { found = true; del[i] = true; break; }
            if (!found) ys.push(f);
        }
        return ys;
    }
    public static inline function union<S>(xs:Array<S>, ys:Array<S>) {
        return xs.concat(subtract(nub(ys),xs));
    }
    public static inline function intersect<S>(xs:Array<S>, ys:Array<S>):Array<S> {
        var zs = [];
        for (x in xs) {
            var found = false;
            for (y in ys) if (y == x) { found = true; break; }
            if (found) zs.push(x);
        }
        return zs;
    }


    public static inline function call<T>(f:Void->T) return f();
    public static inline function call1<S,T>(f:S->T, x:S) return f(x);
    public static inline function call2<S,T,R>(f:R->S->T, x:R, y:S) return f(x,y);
    public static inline function call3<S,T,R,Q>(f:R->S->T->Q, x:R, y:S, z:T) return f(x,y,z);
    public static inline function call4<S,T,R,Q,P>(f:R->S->T->Q->P, x:R, y:S, z:T, w:Q) return f(x,y,z,w);
}
