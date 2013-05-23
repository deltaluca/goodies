package goodies;

#if macro
    import haxe.macro.Expr;
    import haxe.macro.Context;
#end

import goodies.Assert;

/**
 * Abstract type wrapping Null<T> into a Maybe environment
 * for null-conciously safe codes with no overheads.
 *
 * Abstract invariance means can integrate with non-safe
 * code implicitly.
 */
abstract Maybe<T>(Null<T>) from Null<T> {
    public inline function new(x:Null<T>) this = x;

    /**
     * (Unsafely) extract underlying value.
     * in #debug this will throw an error at runtime.
     */
    public #if !macro inline #end function extract():T {
        // macro in macro fails with inline ^
        Assert.assert(this != null);
        return untyped this;
    }

    /**
     * Return underyling value is non-null, and default otherwise.
     *
     * x.or(y) == x.run(function (x) return x, function () return y)
     */
    public inline function or(def:T):T return if (this==null) def else untyped this;

    /**
     * Map function to value if non-null, otherwise return default value.
     *
     * x.runOr(f, y) == x.run(f, function () return y);
     */
    public inline function runOr<S>(eval:T->S, def:S=null):S return if (this==null) def   else eval(untyped this);

    /**
     * Map eval function to value if non-null, and run default function otherwise.
     */
    public inline function run  <S>(eval:T->S, def:Void->S):S return if (this==null) def() else eval(untyped this);

    public inline function bool() return this != null;

    /*
     * Extract possibly null/non-existant item from head of array.
     */
    public #if !macro inline #end static function listToMaybe<T>(xs:Array<T>):Maybe<T> {
        // macro in macro fails with inline ^
        Assert.assert(xs != null);
        return if (xs.length == 0) null else new Maybe<T>(xs[0]);
    }

    /*
     * Convert value into singleton list if non-null, and empty list otherwise.
     */
    public inline function maybeToList():Array<T> {
        return if (this==null) [] else [untyped this];
    }

    /*
     * Concatenate list of possibly null values into array of only the non-null elements.
     */
    public #if !macro inline #end static function catMaybes<T>(xs:Array<Maybe<T>>):Array<T> {
        // macro in macro fails with inline ^
        Assert.assert(xs != null);
        var ret = [];
        for (x in xs) if (x!=null) ret.push(untyped x);
        return ret;
    }

    /*
     * Map function over list of values, returning only the non-null results.
     */
    public #if !macro inline #end static function mapMaybe<T,S>(eval:T->Maybe<S>, xs:Array<T>):Array<S> {
        // macro in macro fails with inline ^
        Assert.assert(xs != null);
        var ret = [];
        for (x in xs) {
            var y = eval(x);
            if (y!=null) ret.push(untyped y);
        }
        return ret;
    }

    public static function liftM<T,S>(f:T->S):Maybe<T>->Maybe<S> return
        function (x) return x.runOr(f);
    public static function liftM2<T,S,R>(f:T->S->R):Maybe<T>->Maybe<S>->Maybe<R> return
        function (x, y) return x.runOr(
        function (x) return y.runOr(f.bind(x)));
    public static function liftM3<T,S,R,Q>(f:T->S->R->Q):Maybe<T>->Maybe<S>->Maybe<R>->Maybe<Q> return
        function (x, y, z) return x.runOr(
        function (x) return y.runOr(
        function (y) return z.runOr(f.bind(x,y))));
    public static function liftM4<T,S,R,Q,P>(f:T->S->R->Q->P):Maybe<T>->Maybe<S>->Maybe<R>->Maybe<Q>->Maybe<P> return
        function (x, y, z, w) return x.runOr(
        function (x) return y.runOr(
        function (y) return z.runOr(
        function (z) return w.runOr(f.bind(x,y,z)))));

    public static function call<T>(f:Maybe<Void->T>) return liftM(Func.call)(f);
    public static function call1<T,S>(f:Maybe<T->S>, x:Maybe<T>) return liftM2(Func.call1)(f, x);
    public static function call2<T,S,R>(f:Maybe<T->S->R>, x:Maybe<T>, y:Maybe<S>) return liftM3(Func.call2)(f, x, y);

    public inline function toString() {
        var x:T = untyped this;
        return if (this == null) "Nothing" else 'Just($x)';
    }
}

@:autoBuild(goodies.MaybeEnvImpl.run()) @:remove extern interface MaybeEnv {}
class MaybeEnvImpl {
#if macro
    static function baseType(t:ComplexType) return
    if (Context.defined("flash") || Context.defined("cpp") || Context.defined("java") || Context.defined("cs")) {
        switch(t) {
            case macro: StdTypes.Int: true;
            case macro: StdTypes.Float: true;
            case macro: StdTypes.Bool: true;
            case macro: Int: true;
            case macro: Float: true;
            case macro: Bool: true;
            default: false;
        };
    }else {
        // Dynamic targets can have base types with null values.
        false;
    };
    public static function run() {
        var intf = MacroUtils.isInterface();
        var fields = Context.getBuildFields();
        for (field in fields) {
            switch (field.kind) {
            case FFun(f):
                for (arg in f.args) {
                    var t = arg.type;
                    if (t == null && arg.value != null)
                        t = Context.toComplexType(Context.typeof(arg.value));

                    var isMaybe = t != null && switch(t) {
                        case TPath({name:"Maybe"}): true;
                        default: false;
                    };

                    var nullArg = (arg.value != null && switch (arg.value) {
                        case macro null: true;
                        default: false;
                    });

                    // ?x = 3      is not null optional
                    // ?x          is null optional
                    // [?]x = null is null optional
                    var nullOptional = (arg.value == null && arg.opt) || nullArg;

                    if (nullOptional && !isMaybe)
                        Context.error('Maybe Error - ${field.name}: Argument "${arg.name}" is optional, permitting null, but not typed with Maybe', field.pos);

                    if (isMaybe) continue;
                    if (nullOptional) continue;
                    if (t != null && baseType(t)) continue;

#if debug
                    var err = 'Maybe Error - ${field.name}: Passing null for non-Maybe type argument "${arg.name}"';
                    if (!intf) {
                        if (t == null || !baseType(t)) {
                            var check = macro if ($i{arg.name} == null) throw $v{err};
                            var block = [check, f.expr];
                            f.expr = macro $b{block};
                        }
                    }
#end
                }
            default:
            }
        }
        return fields;
    }
#end
}
