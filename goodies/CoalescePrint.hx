package goodies;

#if (cpp||neko)

import #if cpp cpp #else neko #end.Lib;
import #if cpp cpp #else neko #end.vm.Mutex;

enum Log {
    EUnit(x:String);
    EMult(es:Array<Log>, n:Int);
}

class CoalescePrint {

    static function deleteN(n:Int) Lib.print('\033[${n}F\033[J');

    static function allEqual(xs:Array<Log>, ys:Array<Log>) return
        xs.length == ys.length &&
        Lambda.foreach([for (i in 0...xs.length) i], function (i) return equal(xs[i], ys[i]));

    static function equal(x:Log, y:Log) {
        return switch([x,y]) {
        case [EUnit(x), EUnit(y)]: x == y;
        case [EMult(xs,n), EMult(ys,m)] if (n == m): allEqual(xs, ys);
        default: false;
        }
    }

    static function match(xs:Array<Log>, ys:Array<Log>):Log {
        if (xs.length == ys.length && allEqual(xs, ys))
            return EMult(xs, 2);

        if (xs.length == 1)
            switch (xs[0]) {
            case EMult(xs, n):
                if (allEqual(xs, ys)) return EMult(xs, n+1);
            default:
            }

        return null;
    }

    static function unprint(x:Log):Int {
        return switch (x) {
        case EUnit(_): 1;
        case EMult(es, _):
            var sum = 0;
            for (e in es) sum += unprint(e);
            sum;
        }
    }
    static function print(x:Log, tab:String=null):String {
        if (tab == null) tab = "";
        return switch (x) {
        case EUnit(x): '$x';
        case EMult(es, n):
            var ret = '\033[31;1m[\033[m   ${print(es[0], tab+"    ")}';
            for (e in es.slice(1))
                ret += '\n$tab    ${print(e, tab+"    ")}';
            ret + '   \033[31;1m]\033[m*\033[32;1m$n\033[m';
        }
    }
    public static inline var limit:Int = 10; // max pattern search depth.
    static var es:Array<Log> = [];
    static var m = new Mutex();
    static public function clear() {
        m.acquire();
        es = [];
        m.release();
    }
    static public function log(n:String) {
        m.acquire();
        var e = EUnit(n);
        Lib.println(print(e));
        es.push(e);
        var pattern = [e];
        var size = 1;
        while (size <= limit) {
            var i = es.length-size-1;
            var m = match(es.slice(i, i+1), pattern);
            if (m != null) {
                for (j in 0...size+1) deleteN(unprint(es.pop()));
                Lib.println(print(m));
                es.push(m);
                pattern = [m];
                size = 1;
                continue;
            }

            var i = es.length-size*2;
            if (i < 0) break;
            var m = match(es.slice(i, i+size), pattern);
            if (m != null) {
                for (j in 0...size*2) deleteN(unprint(es.pop()));
                Lib.println(print(m));
                es.push(m);
                pattern = [m];
                size = 1;
                continue;
            }

            pattern.unshift(es[es.length-(++size)]);
        }
        m.release();
    }
}

#end
