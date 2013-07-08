package goodies;

/**
 * Types:
 *         These can be used to type variables/fields/arguments to aid in Shacks limited
 *         type inference. Variables of a stack-object type are replaced by
 *         element declarations.
 *
 *          V#N    (eg V3)   Stack-Vector
 *          S#N    (eg S3)   Stack-Symmetric-Matrix
 *          M#Nx#M (eg M3x3) Stack-Matrix
 *          Scalar           (Implicit type)
 *
 * Type constructors:
 *          These can be used to construct an object to be used either as an
 *          l-value or an r-value in expressions.
 *
 *          VN()            Zero-Vector
 *          VN(x)           Constant-Vector
 *          VN(...)         (N values)
 *
 *          SN()            Zero-Sym-Matrix
 *          SN([all=]x)     Constant-Sym-Matrix
 *          SN(diag=x)      x-Diagonal-Sym-Matrix
 *          SN(...)         (N(N+1)/2 values)
 *
 *          MNxM()          Zero-Matrix
 *          MNxM([all=]x)   Constant-Matrix
 *          MNxM(diag=x)    x-Diagonal-Matrix
 *          MNxM(...)       (NM values)
 *
 * Slices:
 *         Slices permit selecting a subset of the objects elements to be used
 *         either as an l-value or an r-value in expressions.
 *
 *         A slice range is one of:
 *             i             Single index
 *             a...b         Range of indices a <= i < b
 *             all           All valid indices
 *
 *         vec(range)        - Of type Scalar for single index range
 *                           - Of appropriately sized vector otherwise
 *
 *         sym(range)        Extract a range from the matrix's diagonal
 *                           (rules as per vector range)
 *
 *         sym(row-range, col-range)  - Scalar when both are single index
 *                                    - Appropriately sized sym. matrix if
 *                                      ranges are equal
 *                                    - Appropriately sized vector if col-range
 *                                      is single index
 *                                    - Appropriately sized matrix otherwise
 *
 *         mat(range)        Extract a range from the matrix's diagonal
 *                           (rules as per vector range)
 *
 *         mat(row-range, col-range)  - Scalar when both are single index
 *                                    - Appropriately sized vector if col-range
 *                                      is single index
 *                                    - Appropriately sized matrix otherwise
 *
 * Operators:
 *        Any floating point operator can be used with stack objects.
 *
 *        These operators work on a component-component basis only and any
 *        mixture of stack object types can be used as long as they're implicit matrix-dimensions
 *        are compatible.
 *
 *        Restrictions obviously hold on assignments, for instance we can multiply component-wise
 *        an S3 matrix and an M3x3 matrix, but we cannot assign an M3x3 matrix to an S3 matrix
 *        (Other direction is of course permitted).
 *
 *        Any stack object can also be operated on with scalar values, with the appropriately typed
 *        default type constructor called, so that S3(diag=2) * 10 is equivalent to S3(diag=2) * S3(10)
 *        which is equivalent to S3(diag=2) * S3(all=10) so that every element of the matrix is
 *        multiplied by 10.
 *
 *        In all operator cases (and toplevel function cases) extra variables may be declared and used
 *        to cache any non-basic expression values that are used more than once.
 *
 * Top-Level Functions:
 *        These are functions in the global scope (Think trace) operating on stack objects:
 *
 *        As with operators, different stack types can be used whenever the same is expected
 *        as long as they are compatibile. Eg: lerp(V2(1,2), V2(3, 5), 10); will lift 10 to
 *        V2(10,10) for the computation.
 *
 *        string(A, tab=""):String              tab is used to prefix multiple line strings
 *        lerp(A, A, A):A                       component wise lerp
 *
 *        dot(vec, vec):scalar                  vector dot-product
 *        lsq(vec):scalar                       vector squared length
 *        length(vec):scalar                    vector length
 *        unit(vec):vec                         vector unit
 *        perp(V2):V2           (strict)        vector perp
 *        cross(scalar, V2):V2  (strict)
 *        cross(V2, scalar):V2  (strict)        various cross products
 *        cross(V2, V2):scalar  (strict)
 *        cross(V3, V3):V3      (strict)
 *        mul(Mnxm, Mmxr):Mnxr  (no scalar args) matrix multiplication
 *                                          return value specialised
 *                                          whenever possible
 *        transpose(A):tr(A)                    transposition
 *        outer(Mnx,):Sn                        outer-self-product, produces
 *        determinant(S2|S3|M2x2|M3x3):scalar   determinant
 *        invert(S2|S3|M2x2):Int                safe-invert matrix. Flag denotes null ranks (bitflag)
 *        solve(S2,3,V2,3):void                 safe-solve  (also scalar,scalar accepted for 1dim)
 *
 * Cross-Object Shack math
 *        Shack operates on a single object via build macro.
 *        To interface with other 'Shack' enabled objects you need to provide type annotations
 *        for the shack properties you use via shack casts. These just look like the type constructors
 *        but as top-level functions with lower case identifiers: v2 instead of V2 and m2x2 instead of M2x2
 *
 *    eg: class V implements Shack { public var p:V2; public function new() { p = 0; } }
 *        class Main implements Shack {
 *            static function main() {
 *                var v = new V();
 *                var p = v2(v.p);  // type of p is now V2 equal to the V2 of v.p
 *            }
 *        }
 *
 *        or tag it in the 'Shack type system'
 *        class Main implements Shack {
 *            static function main() {
 *                var v = new V();
 *                @tag(V2) v.p; // type of v.p is now V2 in this scope
 *            }
 *        }
 */

import haxe.macro.Context;
import haxe.macro.Expr;

using goodies.Func;
import goodies.TExpr;

@:autoBuild(goodies.Shacks.run())
@:remove extern interface Shack {}

#if macro

typedef MDim = {
    rows:Int,
    cols:Int
}

typedef Range = {
    from:Int,
    cnt:Int
}

#end

class Shacks {

    public static function colsPrint(cols:Array<Array<String>>, pre:String, white:String) {
        var cs = cols.length;
        var rs = cols[0].length;
        for (j in 0...cs) {
            var col = cols[j];
            var maxw = col[0].length;
            for (i in 1...rs) {
                var l = col[i].length;
                if (l > maxw) maxw = l;
            }
            for (i in 0...rs) {
                var c = col[i];
                while (c.length < maxw) {
                    c = " "+c;
                    if (c.length < maxw) c += " ";
                }
                col[i] = c;
            }
        }
        var ret = pre;
        for (i in 0...rs) {
            if (i != 0) ret += white;
            ret += "(";
            for (j in 0...cs) {
                if (j != 0) ret += " ";
                ret += cols[j][i];
            }
            ret += ")";
        }
        return ret;
    }

#if macro
    static var printer = new haxe.macro.Printer("    ");

    static inline function vec_type(dimension:Int)  return 'V$dimension';
    static inline function mat_type(dimension:MDim) return 'M${dimension.rows}x${dimension.cols}';
    static inline function sym_type(dimension:Int)  return 'S$dimension';

    static var vec_regex = ~/V[0-9]+/;
    static var mat_regex = ~/M[0-9]+x[0-9]+/;
    static var sym_regex = ~/S[0-9]+/;

    static inline function vec_component(name:String, index:Int) return '${name}v$index';
    static inline function mat_component(name:String, row:Int, col:Int) return '${name}m${row}m$col';
    static inline function sym_component(name:String, row:Int, col:Int) {
        if (row > col) { var tmp = row; row = col; col = tmp; }
        return '${name}s${row}s$col';
    }

    static inline function vec_t(dimension:Int)  return TTVec(dimension);
    static inline function sym_t(dimension:Int)  return TTSym(dimension);
    static inline function mat_t(dimension:MDim) return TTMat(dimension.rows, dimension.cols);

    static inline function make_vec(dim:Int, vals:Array<TExpr>)
        return texpr(TECall(null, vals), TTVec(dim), vals[0].pos);
    static inline function make_sym(dim:Int, vals:Array<TExpr>)
        return texpr(TECall(null, vals), TTSym(dim), vals[0].pos);
    static inline function make_mat(dim:MDim, vals:Array<TExpr>)
        return texpr(TECall(null, vals), TTMat(dim.rows, dim.cols), vals[0].pos);

    static function mat_from_sym(cache:TExpr->TExpr, sym_dim:Int, rows:Range, cols:Range, li:Array<TExpr>) {
        var counts = sym_total(sym_dim).replicate(0);
        var indices = (function (i, j) {
            var ind = sym_index(sym_dim, i+rows.from, j+cols.from);
            if (++counts[ind] == 2) li[ind] = cache(li[ind]);
            return ind;
        }).imap2(rows.cnt, function (_) return cols.cnt);
        return (function (i) return li[i]).map(indices);
    }

    static inline function dim_vec(dim:Int)  return function (t:TType) return vec_dim(t) == dim;
    static inline function dim_sym(dim:Int)  return function (t:TType) return sym_dim(t) == dim;
    static inline function dim_mat(dim:MDim) return function (t:TType) {
        var d = mat_dim(t);
        return d.cols == dim.cols && d.rows == dim.rows;
    }

    static inline function vec_dim(t:TType):Null<Int> return t == null ? 1 : switch (t) {
        case TTVec(n): n;
        case TTSym(1): 1;
        case TTMat(n,1): n;
        default: null;
    }
    static inline function sym_dim(t:TType):Null<Int> return t == null ? 1 : switch (t) {
        case TTSym(n): n;
        default: null;
    }
    static inline function mat_dim(t:TType):Null<MDim> return t == null ? {rows:1,cols:1} : switch (t) {
        case TTMat(n,m): {rows:n, cols:m};
        case TTSym(m):   {rows:m, cols:m};
        case TTVec(m):   {rows:m, cols:1};
        default: null;
    }

    static inline function any_dim(t:TType):Null<MDim> return t == null ? {rows:1,cols:1} : switch (t) {
        case TTVec(n):   {rows:n, cols:1};
        case TTSym(n):   {rows:n, cols:n};
        case TTMat(n,m): {rows:n, cols:m};
        case TTScalar:   {rows:1, cols:1};
    }

    static inline function float_binop(op:Binop) return (switch (op) {
        case OpSub | OpNotEq | OpMult | OpMod | OpLte | OpLt | OpGte | OpGt | OpEq | OpDiv | OpAdd: true;
        default: false;
        });
    static inline function float_assignop(op:Binop) return switch (op) {
        case OpAssign: true;
        case OpAssignOp(op): float_binop(op);
        default: false;
    }
    static inline function float_unop(op:Unop) return switch (op) {
        case OpNeg | OpIncrement | OpDecrement: true;
        default: false;
    }

    static function slice(e:TExpr) return switch(e.expr) {
        case TEConst(CIdent("all")): {from:0, cnt:-1};
        case TEConst(CInt(x)): {from:Std.parseInt(x), cnt:null};
        case TEParenthesis(x): slice(x);
        case TEBinop(OpInterval, {expr:TEConst(CInt(i))}, {expr:TEConst(CInt(j))}):
            {from:Std.parseInt(i),cnt:Std.parseInt(j)-Std.parseInt(i)};
        default: null;
    }

    static inline function maybeType(e:TExpr) return (e == null ? null : e.type);

    static inline function any_def_type(t:TType) return !Func.call1.zipWith([real_vec, real_mat, real_sym], [t,t,t]).or();
    static inline function any_num(t:TType) return t == null ? true : switch (t) {
        case TTScalar: true;
        default: false;
    }
    static inline function any_vec(t:TType) return t == null ? true : switch (t) {
        case TTVec(_): true;
        case TTScalar: true;
        case TTMat(_,1): true;
        default: false;
    }
    static inline function any_sym(t:TType) return t == null ? true : switch (t) {
        case TTSym(_): true;
        case TTScalar: true;
        default: false;
    }
    static inline function any_mat(t:TType) return t == null ? true : switch (t) {
        case TTVec(_): true;
        case TTSym(_): true;
        case TTMat(_,_): true;
        case TTScalar: true;
    }
    static inline function any_sqr(t:TType) return t == null ? true : switch (t) {
        case TTSym(_): true;
        case TTMat(n,m): n==m;
        case TTScalar: true;
        default: false;
    }
    static inline function real_vec(t:TType) return t == null ? false : switch (t) {
        case TTVec(_): true;
        default: false;
    }
    static inline function real_sym(t:TType) return t == null ? false : switch (t) {
        case TTSym(_): true;
        default: false;
    }
    static inline function real_mat(t:TType) return t == null ? false : switch (t) {
        case TTMat(_,_): true;
        default: false;
    }

    static function sym_index(size:Int, i:Int, j:Int) {
        if (j < i) { var tmp = i; i = j; j = tmp; }
        var ind = 0;
        for (a in 0...size) {
            for (b in a...size) {
                if (a==i && b==j) return ind;
                ind++;
            }
        }
        return null;
    }
    static inline function mat_index(dim:MDim, i:Int, j:Int) {
        return i*dim.cols + j;
    }

    static inline function sym_total(size:Int) return ((size*(size+1))>>1);
    static inline function mat_total(dim:MDim) return dim.rows*dim.cols;

    static function sym_diag(cache:TExpr->TExpr, size:Int, e:TExpr) {
        if (size > 1) e = cache(e);
        var zero = texpr(TEConst(CInt("0")), TTScalar, e==null ? Context.currentPos() : e.pos);
        return (function (i, j) return j==0 ? e : zero).imap2(size, function (i) return size-i);
    }
    static function mat_diag(cache:TExpr->TExpr, dim:MDim, e:TExpr) {
        if (Math.min(dim.rows, dim.cols) > 1) e = cache(e);
        var zero = texpr(TEConst(CInt("0")), TTScalar, e==null ? Context.currentPos() : e.pos);
        return (function (i, j) return i==j ? e : zero).imap2(dim.rows, function (_) return dim.cols);
    }

    static function testAssignCompatibility(p:Position, t0:TType, t1:TType) {
        if (t0 == null) return true;
        if (t1 == null) return true;
        var res = switch (t0) {
            case TTScalar: Type.enumEq(t1, TTScalar);
            case TTVec(x): var dim = any_dim(t1); any_num(t1) || (dim.rows == x && dim.cols == 1);
            case TTSym(x): any_num(t1) || sym_dim(t1) == x;
            case TTMat(x,y): var dim = any_dim(t1); any_num(t1) || (dim.rows == x && dim.cols == y);
        }
        if (!res) Context.error('Type Error: Cannot assign ${TTransform.printTType(t1)} to ${TTransform.printTType(t0)}', p);
        return res;
    }
    static function testCompatibility(p:Position, t0:TType, t1:TType) {
        if (t0 == null) t0 = TTScalar;
        if (t1 == null) t1 = TTScalar;
        var res = switch ([t0,t1]) {
            case [TTScalar, TTScalar]:   TTScalar;
            case [TTScalar, TTVec(x)]:   TTVec(x);
            case [TTScalar, TTSym(x)]:   TTSym(x);
            case [TTScalar, TTMat(x,y)]: TTMat(x,y);

            case [TTVec(x), TTScalar]:   TTVec(x);
            case [TTVec(x), TTVec(y)]:   if (x==y) TTVec(x) else null;
            case [TTVec(_), TTSym(_)]:   null;
            case [TTVec(x), TTMat(n,m)]: if (n==x && m==1) TTMat(n, m) else null;

            case [TTSym(x), TTScalar]:   TTSym(x);
            case [TTSym(_), TTVec(_)]:   null;
            case [TTSym(x), TTSym(y)]:   if (x==y) TTSym(x) else null;
            case [TTSym(x), TTMat(n,m)]: if (n==m && n==x) TTMat(n,m) else null;

            case [TTMat(x,y), TTScalar]:   TTMat(x,y);
            case [TTMat(x,y), TTVec(z)]:   if (y==1 && x==z) TTMat(x,y) else null;
            case [TTMat(x,y), TTSym(z)]:   if (x==y && x==z) TTMat(x,y) else null;
            case [TTMat(x,y), TTMat(z,w)]: if (x == z && y == w) TTMat(x,y) else null;
        }
        if (res==null) Context.error('Type Error: Attempt to unify ${TTransform.printTType(t0)} and ${TTransform.printTType(t1)}', p);
        return res;
    }

    static function vec_value(cache:TExpr->TExpr, e:TExpr, t:TType=null):Array<TExpr> {
        if (e != null && e.type != null && t != null && !Type.enumEq(e.type, t)) {
            var dim = vec_dim(t);
            return switch (e.type) {
                case TTScalar:   if (dim == 1) [e] else dim.replicate(cache(e));
                case TTMat(_,_): mat_value(cache, e);
                default: null;
            }
        }

        if (t == null) t = e.type;
        var dim = vec_dim(t);
        if (dim == null) dim = 1;

        if (e == null) return dim.replicate(null);
        return switch (e.expr) {
        case TEParenthesis(x): vec_value(cache, x, t);
        case TEConst(CIdent(v)) if (real_mat(e.type)):
            (function (i) return tident(mat_component(v, i, 0), TTScalar, e.pos))
                .imap(dim);
        case TEConst(CIdent(v)) if (real_vec(e.type)):
            (function (i) return tident(vec_component(v, i), TTScalar, e.pos))
                .imap(dim);
        case TECall(x, args) if (x==null):
            if      (args.length == 0) dim.replicate(tzero);
            else if (args.length == 1) (dim == 1) ? args : dim.replicate(cache(args[0]));
            else                       args;
        default: if (dim == 1) [e] else dim.replicate(cache(e));
        };
    }

    static function sym_value(cache:TExpr->TExpr, e:TExpr, t:TType=null):Array<TExpr> {
        if (t == null) t = e.type;
        var dim = sym_dim(t);

        if (e == null) return sym_total(dim).replicate(null);
        return switch (e.expr) {
        case TEParenthesis(x): sym_value(cache, x, t);
        case TEConst(CIdent(v)) if (real_sym(e.type)):
            (function (i, j) return tident(sym_component(v, i, i+j), TTScalar, e.pos))
                .imap2(dim, function (i) return dim-i);
        case TECall(x, args) if (x==null):
            if      (args.length == 0) sym_total(dim).replicate(tzero);
            else if (args.length == 1) {
                var x = args[0];
                var ret = null;
                while (true) {
                    switch (x.expr) {
                    case TEParenthesis(y): x = y;
                    case TEBinop(OpAssign, {expr:TEConst(CIdent("diag"))}, x):
                        ret = sym_diag(cache, dim, x);
                        break;
                    case TEBinop(OpAssign, {expr:TEConst(CIdent("all"))}, x):
                        ret = if (dim == 1) [x] else sym_total(dim).replicate(cache(x));
                        break;
                    default:
                        ret = if (dim == 1) [x] else sym_total(dim).replicate(cache(x));
                        break;
                    }
                }
                ret;
            }
            else args;
        default: if (dim == 1) [e] else sym_total(dim).replicate(cache(e));
        };
    }

    static function mat_value(cache:TExpr->TExpr, e:TExpr, t:TType=null):Array<TExpr> {
        if (e != null && e.type != null && t != null && !Type.enumEq(e.type, t)) {
            var dim = mat_dim(t);
            return switch (e.type) {
                case TTScalar: if (dim.cols == 1 && dim.rows == 1) [e] else mat_total(dim).replicate(cache(e));
                case TTVec(_): vec_value(cache, e);
                case TTSym(n):
                    mat_from_sym(cache, n, {from:0,cnt:dim.rows}, {from:0,cnt:dim.cols}, sym_value(cache, e));
                default: null;
            }
        }

        if (t == null) t = e.type;
        var dim = any_dim(t);

        if (e == null) return mat_total(dim).replicate(null);
        return switch (e.expr) {
        case TEParenthesis(x): mat_value(cache, x, t);
        case TEConst(CIdent(v)) if (real_vec(e.type)):
            (function (i) return tident(vec_component(v, i), TTScalar, e.pos))
                .imap(dim.rows);
        case TEConst(CIdent(v)) if (real_sym(e.type)):
            (function (i, j) return tident(sym_component(v, i, j), TTScalar, e.pos))
                .imap2(dim.rows, function (_) return dim.cols);
        case TEConst(CIdent(v)) if (real_mat(e.type)):
            (function (i, j) return tident(mat_component(v, i, j), TTScalar, e.pos))
                .imap2(dim.rows, function (_) return dim.cols);
        case TECall(x, args) if (x==null):
            if      (args.length == 0) mat_total(dim).replicate(tzero);
            else if (args.length == 1) {
                var x = args[0];
                var ret = null;
                while (true) {
                    switch (x.expr) {
                    case TEParenthesis(y): x = y;
                    case TEBinop(OpAssign, {expr:TEConst(CIdent("diag"))}, x):
                        ret = mat_diag(cache, dim, x);
                        break;
                    case TEBinop(OpAssign, {expr:TEConst(CIdent("all"))}, x):
                        ret = if (dim.rows==1 && dim.cols==1) [x] else mat_total(dim).replicate(cache(x));
                        break;
                    default:
                        ret = if (dim.rows==1 && dim.cols==1) [x] else mat_total(dim).replicate(cache(x));
                        break;
                    }
                }
                ret;
            }
            else args;
        default: if (dim.rows == 1 && dim.cols == 1) [e] else mat_total(dim).replicate(cache(e));
        };
    }

    static function withCache(ctx:Ctx, f:(TExpr->TExpr)->TExpr):TExpr {
        function cache(e:TExpr) {
            if (e == null) return false;
            return switch(e.expr) {
            case TEConst(_): false;
            case TEParenthesis(e): cache(e);
            default: true;
            };
        }

        var ret = f(function (e:TExpr) {
            if (cache(e)) {
                var name = 'cache${ctx.id++}';
                ctx.block.push(tvar(name, e));
                ctx.define(name, TTScalar);
                return tident(name, TTScalar, e.pos);
            }
            else return e;
        });
        return ret == null ? null : tpar(ret);
    }

    static function ARGS(p:Position, ctx:Ctx, args:Array<FunctionArg>):Array<TFunctionArg> {
        var qargs = [];
        for (a in args) {
            for (v in VARS(p, ctx, [{name:a.name, expr:a.value, type:a.type}])) {
                qargs.push({
                    name: v.name,
                    type: v.type,
                    value: v.expr,
                    opt: a.opt
                });
            }
        }
        return qargs;
    }
    static function VARS(p:Position, ctx:Ctx, vars:Array<Var>):Array<TVar> {
        var qars = [];
        withCache(ctx, function (cache:TExpr->TExpr) {
            for (v in vars) {
                var expr = toTExpr(ctx, v.expr);
                var etype = maybeType(expr);
                var vtype = tComplexType(v.type);
                testAssignCompatibility(p, vtype, etype);

                var type = vtype == null ? etype : vtype;
                ctx.define(v.name, type);

                var modded = false;
                if (type != null) {
                    switch (type) {
                    case TTVec(_):
                        var values = vec_value(cache, expr, type);
                        for (i in 0...values.length) {
                            var name = vec_component(v.name, i);
                            qars.push({name:name, type:macro :Float, expr:values[i]});
                            ctx.define(name, TTScalar);
                        }
                        modded = true;
                    case TTSym(dim):
                        var values = sym_value(cache, expr, type);
                        for (i in 0...dim) {
                        for (j in i...dim) {
                            var name = sym_component(v.name, i, j);
                            qars.push({name:name, type:macro :Float, expr:values[sym_index(dim, i, j)]});
                            ctx.define(name, TTScalar);
                        }}
                        modded = true;
                    case TTMat(n, m):
                        var dim = {rows:n, cols:m};
                        var values = mat_value(cache, expr, type);
                        for (i in 0...n) {
                        for (j in 0...m) {
                            var name = mat_component(v.name, i, j);
                            qars.push({name:name, type:macro :Float, expr:values[mat_index(dim, i, j)]});
                            ctx.define(name, TTScalar);
                        }}
                        modded = true;
                    default:
                    }
                }

                if (!modded) {
                    qars.push({name:v.name, type:v.type, expr:expr});
                }
            }
            return null;
        });
        return qars;
    }

    static inline function texpr(ed, t, p):TExpr return {expr:ed, type:t, pos:p}
    static function tfield(field:String, t:TType=null, p:Position=null):TExpr {
        if (p == null) p = Context.currentPos();

        var fs = field.split(".");
        var ret = texpr(TEConst(CIdent(fs.shift())), t, p);
        while (fs.length > 0) {
            ret = texpr(TEField(ret, fs.shift()), t, p);
        }

        return ret;
    }
    static inline function tident(n:String, t:TType=null, p:Position=null)
        return tfield(n, t, p);
    static inline function tcall(e:TExpr, es:Array<TExpr>=null)
        return texpr(TECall(e, es==null?[]:es), null, e.pos);
    static inline function tlambda(e:TExpr)
        return texpr(TEFunction(null, {
            ret: macro :Int,
            params: [],
            expr: e,
            args: []
        }), null, e.pos);
    static inline function tblock(es:Array<TExpr>)
        return texpr(TEBlock(es), null, es.length == 0 ? Context.currentPos() : es[0].pos);
    static inline function tif(c:TExpr, eif:TExpr, eelse:TExpr=null)
        return texpr(TEIf(c, eif, eelse), eif.type, eif.pos);
    static inline function treturn(e:TExpr=null)
        return texpr(TEReturn(e), null, e==null ? Context.currentPos() : e.pos);
    static inline function tvar(n:String, e:TExpr) {
        return texpr(TEVars([{
            name: n,
            type: null,
            expr: e
        }]), null, e.pos);
    }
    static inline function tint(i:Int) return texpr(TEConst(CInt(Std.string(i))), TTScalar, Context.currentPos());
    static inline function tpar(e:TExpr) return texpr(TEParenthesis(e), e.type, e.pos);

    static var tzero = tint(0);
    static var tone  = tint(1);

    static inline function uop(op:Unop, post=false)
        return inline function (a) return texpr(TEUnop(op, post, a), TTScalar, a.pos);
    static inline function mop(op:Binop)
        return inline function (a, b) return texpr(TEBinop(op, a, b), TTScalar, a.pos);
    static var madd = mop(OpAdd);
    static var msub = mop(OpSub);
    static var mmul = mop(OpMult);
    static var mdiv = mop(OpDiv);
    static var mass = mop(OpAssign);
    static var mneq = mop(OpNotEq);
    static var meq  = mop(OpEq);
    static inline function msqr (a) return mmul(a,a);
    static var mneg = uop(OpNeg);

    static var unop:Array<{op:Unop->Bool, arg:TType->Bool, ret:(TExpr->TExpr)->Unop->Bool->TExpr->TExpr}>;
    static var binop:Array<{op:Binop->Bool, args:Array<TType->Bool>, ret:Ctx->(TExpr->TExpr)->Bool->Binop->TExpr->TExpr->TExpr}>;
    static var slices:Array<{type:TType->Bool, validRange:Int->Bool, ret:(TExpr->TExpr)->TExpr->Array<TExpr>->TExpr}>;
    static var toplevel:Array<{name:String->Bool, types:Array<TType->Bool>, ret:(TExpr->TExpr)->Array<TExpr>->String->TExpr}>;

    static function inits() {
        unop = [
            {op: null, arg: any_def_type,
             ret: function (_,op,post,e) return uop(op, post)(e)
            }
        ];
        binop = [
            {op: null, args: 2.replicate(any_def_type),
             ret: function (_,_,_,op,e1,e2) return mop(op)(e1, e2)
            }
        ];

        var types = [{t1:real_vec, t2:any_vec, v:vec_value, m:function (t) return make_vec.bind(vec_dim(t))},
                     {t1:real_sym, t2:any_sym, v:sym_value, m:function (t) return make_sym.bind(sym_dim(t))},
                     {t1:real_mat, t2:any_mat, v:mat_value, m:function (t) return make_mat.bind(mat_dim(t))}];
        for (fs in types) {
            unop.push({
                op: float_unop, arg: fs.t1,
                ret: function (cache, op, post, e) {
                    return fs.m(e.type)(uop(op, post).map(fs.v(cache, e)));
                }
            });
            binop.push({
                op: float_assignop, args: [fs.t1, fs.t2],
                ret: function (ctx, cache, statement, op, e1, e2) {
                    testAssignCompatibility(e1.pos, e1.type, e2.type);
                    var vals = mop(op).zipWith(fs.v(cache, e1), fs.v(cache, e2, e1.type));
                    if (statement) {
                        ctx.block.push.map(vals);
                        return tblock([]);
                    }
                    else return fs.m(e1.type)(vals);
                }
            });
            binop.push({
                op: float_binop, args: 2.replicate(fs.t2),
                ret: function (_, cache, _, op, e1, e2) {
                    var t = testCompatibility(e1.pos, e1.type, e2.type);
                    return fs.m(t)(mop(op).zipWith(fs.v(cache, e1, t), fs.v(cache, e2, t)));
                }
            });
        }

        slices = [
            { type: real_vec,
              validRange: function (n) return n==1,
              ret: function (cache, x, args) {
                  var dim = vec_dim(x.type);
                  var range = slice(args[0]);
                  if (range == null) return null;
                  if (range.cnt == -1) range.cnt = dim;

                  var li = vec_value(cache, x);
                  if (range.cnt == null) {
                      if (range.from >= li.length) Context.error("Taking element beyond vector length", li[0].pos);
                      if (range.from < 0) Context.error("Element with negative index", li[0].pos);
                      return li[range.from];
                  }
                  else {
                      if (range.from >= li.length) Context.error("Taking slice beyond vector length", li[0].pos);
                      if (range.from + range.cnt > li.length) Context.error("Slice extends beyond vector length", li[0].pos);
                      if (range.from < 0) Context.error("Slice with negative start", li[0].pos);
                      var slice = li.slice(range.from, range.from+range.cnt);
                      if (slice.length == 0) Context.error("Taking empty slice of vector", li[0].pos);
                      return make_vec(range.cnt, slice);
                  }
              }
            },
            { type: real_sym,
              validRange: function (n) return n==1,
              ret: function (cache, x, args) {
                  var dim = sym_dim(x.type);
                  var range = slice(args[0]);
                  if (range == null) return null;
                  if (range.cnt == -1) range.cnt = dim;

                  var li = sym_value(cache, x);
                  if (range.cnt == null) return li[sym_index(dim, range.from, range.from)];
                  else {
                      var f = range.from;
                      return make_vec(range.cnt,
                        (function (i) return li[sym_index(dim, i+f, i+f)]).imap(range.cnt)
                      );
                  }
              }
            },
            { type: real_sym,
              validRange: function (n) return n==2,
              ret: function (cache, x, args) {
                  var dim = sym_dim(x.type);
                  var ranges = slice.map(args);

                  if (ranges[0] == null || ranges[1] == null) return null;
                  if (ranges[0].cnt == -1) ranges[0].cnt = dim;
                  if (ranges[1].cnt == -1) ranges[1].cnt = dim;

                  var li = sym_value(cache, x);

                  // (a,b) -> (a,b) = Float
                  // (a,b) -> (a,c) = M(1)x(c-b)
                  // (a,b) -> (c,b) = V(c-a)
                  // (a,a) -> (b,b) = S(b-a)
                  // (a,b) -> (c,d) = M(c-a)x(d-b)  // requires cacheing.

                  if (ranges[0].cnt == null && ranges[1].cnt == null)
                      return li[sym_index(dim, ranges[0].from, ranges[1].from)];
                  if (ranges[0].cnt == null)
                        return make_mat({rows:1, cols:ranges[1].cnt},
                          (function (i) return li[sym_index(dim, ranges[0].from, i+ranges[1].from)])
                          .imap(ranges[1].cnt)
                        );
                  if (ranges[1].cnt == null)
                        return make_vec(ranges[0].cnt,
                          (function (i) return li[sym_index(dim, i+ranges[0].from, ranges[1].from)])
                          .imap(ranges[0].cnt)
                        );
                  if (ranges[0].from == ranges[1].from && ranges[0].cnt == ranges[1].cnt) {
                      return make_sym(ranges[0].cnt,
                          (function (i, j) return li[sym_index(dim, i+ranges[0].from, j+i+ranges[0].from)])
                          .imap2(ranges[0].cnt, function (i) return ranges[0].cnt-i)
                      );
                  }
                  return make_mat({rows:ranges[0].cnt, cols:ranges[1].cnt},
                                  mat_from_sym(cache, dim, ranges[0], ranges[1], li));
              }
            },
            { type: real_mat,
              validRange: function (n) return n==1,
              ret: function (cache, x, args) {
                  var dim = mat_dim(x.type);
                  var range = slice(args[0]);
                  if (range == null) return null;
                  if (range.cnt == -1) range.cnt = Std.int(Math.min(dim.cols, dim.rows));

                  var li = mat_value(cache, x);
                  if (range.cnt == null) return li[mat_index(dim, range.from, range.from)];
                  else {
                      var f = range.from;
                      return make_vec(range.cnt,
                        (function (i) return li[mat_index(dim, i+f, i+f)]).imap(range.cnt)
                      );
                  }
              }
            },
            { type: real_mat,
              validRange: function (n) return n==2,
              ret: function (cache, x, args) {
                  var dim = mat_dim(x.type);
                  var ranges = slice.map(args);

                  if (ranges[0] == null || ranges[1] == null) return null;
                  if (ranges[0].cnt == -1) ranges[0].cnt = dim.rows;
                  if (ranges[1].cnt == -1) ranges[1].cnt = dim.cols;

                  var li = mat_value(cache, x);

                  // (a,b) -> (a,b) = Float
                  // (a,b) -> (a,c) = M(1)x(c-b)
                  // (a,b) -> (c,b) = V(c-a)
                  // (a,b) -> (c,d) = M(c-a)x(d-b)  // requires cacheing.

                  if (ranges[0].cnt == null && ranges[1].cnt == null)
                      return li[mat_index(dim, ranges[0].from, ranges[1].from)];
                  if (ranges[0].cnt == null)
                        return make_mat({rows:1, cols:ranges[1].cnt},
                          (function (i) return li[mat_index(dim, ranges[0].from, i+ranges[1].from)])
                          .imap(ranges[1].cnt)
                        );
                  if (ranges[1].cnt == null)
                        return make_vec(ranges[0].cnt,
                          (function (i) return li[mat_index(dim, i+ranges[0].from, ranges[1].from)])
                          .imap(ranges[0].cnt)
                        );
                  return make_mat({rows:ranges[0].cnt, cols:ranges[1].cnt},
                    (function (i, j) return li[mat_index(dim, ranges[0].from+i, ranges[1].from+j)])
                    .imap2(ranges[0].cnt, function (_) return ranges[1].cnt)
                  );
              }
            }
        ];

        function nn(x:String) return function (n) return n==x;
        function name(x:TExpr) return switch (x.expr) {
            case TEConst(CIdent(x)): x;
            case TEParenthesis(x): name(x);
            case TEField(x, y): '${name(x)}.$y';
            default: "";
        };

        toplevel = [
            {name: function (v) return (~/v[0-9]+/).match(v), types: [function (_) return true],
             ret: function (cache, args, v) {
                 var x = name(args[0]);
                 var dim = Std.parseInt(v.substr(1));
                 var elts = (function (i) return tident(vec_component(x, i), TTScalar, args[0].pos)).imap(dim);
                 return make_vec(dim, elts);
             }
            },
            {name: function (v) return (~/s[0-9]+/).match(v), types: [function (_) return true],
             ret: function (cache, args, v) {
                 var x = name(args[0]);
                 var dim = Std.parseInt(v.substr(1));
                 var elts = (function (i,j) return tident(sym_component(x, i, i+j), TTScalar, args[0].pos))
                            .imap2(dim, function (i) return dim-i);
                 return make_sym(dim, elts);
             }
            },
            {name: function (v) return (~/m[0-9]+x[0-9]+/).match(v), types: [function (_) return true],
             ret: function (cache, args, v) {
                 var x = name(args[0]);
                 var v = v.substr(1).split("x");
                 var rows = Std.parseInt(v[0]);
                 var cols = Std.parseInt(v[1]);
                 var elts = (function (i,j) return tident(mat_component(x, i, j), TTScalar, args[0].pos))
                            .imap2(rows, function (_) return cols);
                 return make_mat({rows:rows, cols:cols}, elts);
             }
            },

            {name: nn("dot"), types: 2.replicate(any_vec),
             ret: function (cache, args, _) {
                var t = testCompatibility(args[0].pos, args[0].type, args[1].type);
                var values = vec_value.bind(cache,_,t).map(args);
                return madd.foldl1(mmul.zipWith(values[0], values[1]));
             }
            },
            {name: nn("outer"), types: [any_mat],
             ret: function (cache, args, _) {
                 var values = cache.map(mat_value(cache, args[0]));
                 var dim = any_dim(args[0].type);

                 var elts = (function (i, j) return madd.foldl1((function (k) return
                        mmul(values[mat_index(dim, i, k)], values[mat_index(dim, i+j, k)])
                     ).imap(dim.cols))
                 ).imap2(dim.rows, function (i) return dim.rows-i);

                 if (dim.rows == 1) return elts[0];
                 else               return make_sym(dim.rows, elts);
             }
            },
            {name: nn("lsq"), types: [any_vec],
             ret: function (cache, args, _) {
                var values = vec_value(cache, args[0]);
                return madd.foldl1(cache.dot(msqr).map(values));
             }
            },
            {name: nn("length"), types: [any_vec],
             ret: function (cache, args, _) {
                var values = vec_value(cache, args[0]);
                return tcall(tfield("Math.sqrt"), [madd.foldl1(cache.dot(msqr).map(values))]);
             }
            },
            {name: nn("unit"), types: [any_vec],
             ret: function (cache, args, _) {
                 var values = vec_value(cache, args[0]);
                 var length = tcall(tfield("Math.sqrt"), [madd.foldl1(cache.dot(msqr).map(values))]);
                 var den = cache(mdiv(tone, length));
                 var elts = mmul.bind(den).map(values);
                 return any_num(args[0].type) ? elts[0] : make_vec(vec_dim(args[0].type), elts);
             }
            },
            {name: nn("perp"), types: [dim_vec(2)],
             ret: function (cache, args, _) {
                 var values = vec_value(cache, args[0]);
                 return make_vec(2, [mneg(values[1]), values[0]]);
             }
            },
            {name: nn("cross"), types: [any_num, dim_vec(2)],
             ret: function (cache, args, _) {
                 var mult = cache(args[0]);
                 var values = vec_value(cache, args[1]);
                 return make_vec(2, mmul.bind(mult).map([mneg(values[1]), values[0]]));
             }
            },
            {name: nn("cross"), types: [dim_vec(2), any_num],
             ret: function (cache, args, _) {
                 var mult = cache(args[1]);
                 var values = vec_value(cache, args[0]);
                 return make_vec(2, mmul.bind(mult).map([values[1], mneg(values[0])]));
             }
            },
            {name: nn("cross"), types: 2.replicate(dim_vec(2)),
             ret: function (cache, args, _) {
                 var values = vec_value.bind(cache).map(args);
                 return msub(mmul(values[0][0], values[1][1]), mmul(values[0][1], values[1][0]));
             }
            },
            {name: nn("cross"), types: 2.replicate(dim_vec(3)),
             ret: function (cache, args, _) {
                 var values = cache.map.map(vec_value.bind(cache).map(args));
                 return make_vec(3, [
                    msub(mmul(values[0][1], values[1][2]), mmul(values[0][2], values[1][1])),
                    msub(mmul(values[0][2], values[1][0]), mmul(values[0][0], values[1][2])),
                    msub(mmul(values[0][0], values[1][1]), mmul(values[0][1], values[1][0]))
                 ]);
             }
            },
            {name: nn("transpose"), types: [any_sym], ret: function (_, args, _) return args[0]},
            {name: nn("transpose"), types: [any_mat],
             ret: function (cache, args, _) {
                 var d = any_dim(args[0].type);
                 var rows = d.cols;
                 var cols = d.rows;

                 var values = mat_value(cache, args[0]);
                 var elts = (function (j, i) return values[mat_index(d, i, j)])
                            .imap2(rows, function (_) return cols);

                 if (cols == 1) return make_vec(rows, elts);
                 else           return make_mat({rows:rows,cols:cols}, elts);
             }
            },
            {name: nn("mul"), types: 2.replicate(any_mat),
             ret: function (cache, args, _) {
                 var d1 = any_dim(args[0].type);
                 var d2 = any_dim(args[1].type);
                 if (d1.cols != d2.rows)
                    Context.error("Type error for matrix multiplication", args[0].pos);

                 var rows = d1.rows; var cols = d2.cols; var ins = d1.cols;
                 var t = mat_t({rows:rows,cols:cols});

                 var values = mat_value.bind(cache).map(args);
                 var counts1 = mat_total(d1).replicate(0);
                 var counts2 = mat_total(d2).replicate(0);
                 (function (i, j) (function (k) {
                         var ind1 = mat_index(d1, i, k);
                         var ind2 = mat_index(d2, k, j);
                         if (++counts1[ind1] == 2) values[0][ind1] = cache(values[0][ind1]);
                         if (++counts2[ind2] == 2) values[1][ind2] = cache(values[1][ind2]);
                     }).iiter(ins)
                 ).iiter2(rows, function (_) return cols);

                 var elts = (function (i, j) return madd.foldl1((function (k) return
                        mmul(values[0][mat_index(d1, i, k)], values[1][mat_index(d2, k, j)])
                     ).imap(ins))
                 ).imap2(rows, function (_) return cols);

                 if (rows == 1 && cols == 1) return elts[0];
                 if (cols == 1) return make_vec(rows, elts);
                 return make_mat({rows:rows,cols:cols}, elts);
             }
            },
            {name: nn("determinant"), types: [dim_sym(2)],
             ret: function (cache, args, _) {
                 var values = sym_value(cache, args[0]);
                 return msub(mmul(values[0], values[2]), msqr(cache(values[1])));
             }
            },
            {name: nn("determinant"), types: [dim_mat({rows:2,cols:2})],
             ret: function (cache, args, _) {
                 var values = mat_value(cache, args[0]);
                 return msub(mmul(values[0], values[3]), mmul(values[1], values[2]));
             }
            },
            {name: nn("determinant"), types: [dim_sym(3)],
             ret: function (cache, args, _) {
                 var values = sym_value(cache, args[0]);
                 var a = values[0]; var b = cache(values[1]); var c = cache(values[2]);
                   /*b*/            var d = cache(values[3]); var e = cache(values[4]);
                   /*c*/              /*e*/                   var f = cache(values[5]);
                 // a(df-ee) + b(2ec-bf) - ccd
                 return madd(mmul(a, msub(mmul(d,f), msqr(e))),
                        msub(mmul(b, msub(mmul(tint(2), mmul(c, e)), mmul(b, f))),
                             mmul(msqr(c), d)));
             }
            },
            {name: nn("determinant"), types: [dim_mat({rows:3,cols:3})],
             ret: function (cache, args, _) {
                 var values = sym_value(cache, args[0]);
                 var a = values[0];        var b = values[1];        var c = values[2];
                 var d = cache(values[3]); var e = cache(values[4]); var f = cache(values[5]);
                 var g = cache(values[6]); var h = cache(values[7]); var i = cache(values[8]);
                 // a(ei-fh) + b(gf-di) + c(dh-eg)
                 return madd(mmul(a,msub(mmul(e,i),mmul(f,h))),
                        madd(mmul(b,msub(mmul(g,f),mmul(d,i))),
                             mmul(c,msub(mmul(d,h),mmul(e,g)))));
             }
            },
            {name: nn("invert"), types: [dim_sym(2)],
             ret: function (cache, args, _) {
                 // no cacheing, expression should be an l-value.
                 var values = sym_value(cache, args[0]);
                 var a = values[0]; var b = values[1];
                                    var c = values[2];

                 var det  = tident("det");
                 var flag = tident("flag");
                 return tcall(tlambda(tblock([
                     tvar("det", msub(mmul(a,c), msqr(b))),
                     tif(mneq(det,det), tblock([
                        mass(a, mass(b, mass(c, tzero))),
                        treturn(tint(3))
                     ]), tif(meq(det,tzero), tblock([
                        tvar("flag", tzero),
                        tif(mneq(a,tzero), mass(a, mdiv(tone,a)),
                          tblock([
                            mass(a, tzero),
                            mop(OpAssignOp(OpOr))(flag, tone)
                          ])),
                        tif(mneq(c,tzero), mass(c, mdiv(tone,c)),
                          tblock([
                            mass(c, tzero),
                            mop(OpAssignOp(OpOr))(flag, tint(2))
                          ])),
                        mass(b, tzero),
                        treturn(flag)
                     ]), tblock([
                        mass(det, mdiv(tone, det)),
                        tvar("t", mmul(c, det)),
                        mass(c, mmul(a, det)),
                        mass(a, tident("t")),
                        mop(OpAssignOp(OpMult))(b, mneg(det)),
                        treturn(tzero)
                     ])))
                 ])));
             }
            },
            {name: nn("invert"), types: [dim_mat({rows:2,cols:2})],
             ret: function (cache, args, _) {
                 // no cacheing, expression should be an l-value.
                 var values = mat_value(cache, args[0]);
                 var a = values[0]; var b = values[1];
                 var c = values[2]; var d = values[3];

                 var det  = tident("det");
                 var flag = tident("flag");
                 return tcall(tlambda(tblock([
                     tvar("det", msub(mmul(a,d), mmul(b,c))),
                     tif(mneq(det,det), tblock([
                        mass(a, mass(b, mass(c, mass(d, tzero)))),
                        treturn(tint(3))
                     ]), tif(meq(det,tzero), tblock([
                        tvar("flag", tzero),
                        tif(mneq(a,tzero), mass(a, mdiv(tone,a)),
                          tblock([
                            mass(a, tzero),
                            mop(OpAssignOp(OpOr))(flag, tone)
                          ])),
                        tif(mneq(d,tzero), mass(c, mdiv(tone,d)),
                          tblock([
                            mass(d, tzero),
                            mop(OpAssignOp(OpOr))(flag, tint(2))
                          ])),
                        mass(b, mass(c, tzero)),
                        treturn(flag)
                     ]), tblock([
                        mass(det, mdiv(tone, det)),
                        mass(b, mneg(mmul(b, det))),
                        mass(c, mneg(mmul(c, det))),
                        tvar("t", mmul(a, det)),
                        mass(a, mmul(d, det)),
                        mass(d, tident("t")),
                        treturn(tzero)
                     ])))
                 ])));
             }
            },
            {name: nn("invert"), types: [dim_sym(3)],
             ret: function (cache, args, _) {
                 // no cacheing, expression should be an l-value.
                 var values = sym_value(cache, args[0]);
                 var a = values[0]; var b = values[1]; var c = values[2];
                                    var d = values[3]; var e = values[4];
                                                       var f = values[5];
                 var A = tident("A"); var B = tident("B"); var C = tident("C");
                                      var D = tident("D"); var E = tident("E");
                                                           var F = tident("F");
                 var det  = tident("det");
                 var flag = tident("flag");
                 return tcall(tlambda(tblock([
                     tvar("A", msub(mmul(d,f), msqr(e))),
                     tvar("B", msub(mmul(e,c), mmul(b,f))),
                     tvar("C", msub(mmul(b,e), mmul(c,d))),
                     tvar("det", madd(madd(mmul(a,A), mmul(b,B)), mmul(c,C))),
                     tif(mneq(det,det), tblock([
                        mass(a, mass(b, mass(c, mass(d, mass(e, mass(f, tzero)))))),
                        treturn(tint(7))
                     ]), tif(meq(det,tzero), tblock([
                        tvar("flag", tzero),
                        tif(mneq(a,tzero), mass(a, mdiv(tone,a)),
                          tblock([
                            mass(a, tzero),
                            mop(OpAssignOp(OpOr))(flag, tone)
                          ])),
                        tif(mneq(d,tzero), mass(d, mdiv(tone,d)),
                          tblock([
                            mass(d, tzero),
                            mop(OpAssignOp(OpOr))(flag, tint(2))
                          ])),
                        tif(mneq(f,tzero), mass(f, mdiv(tone,f)),
                          tblock([
                            mass(f, tzero),
                            mop(OpAssignOp(OpOr))(flag, tint(4))
                          ])),
                        mass(b, mass(c, mass(e, tzero))),
                        treturn(flag)
                     ]), tblock([
                        mass(det, mdiv(tone, det)),
                        tvar("D", msub(mmul(a,f), msqr(c))),
                        tvar("E", msub(mmul(b,c), mmul(a,e))),
                        tvar("F", msub(mmul(a,d), msqr(b))),
                        mass(a, mmul(det,A)),
                        mass(b, mmul(det,B)), mass(d, mmul(det,D)),
                        mass(c, mmul(det,C)), mass(e, mmul(det,E)), mass(f, mmul(det,F)),
                        treturn(tzero)
                     ])))
                 ])));
             }
            },
            {name: nn("solve"), types: 2.replicate(any_num),
             ret: function (cache, args, _) {
                 var x = cache(args[0]);
                 var y = args[1];
                 return
                    tif(mop(OpBoolOr)(mneq(x,x), mneq(x,tzero)), mass(y, tzero), mop(OpAssignOp(OpDiv))(y, x));
             }
            },
            {name: nn("solve"), types: [dim_sym(2), dim_vec(2)],
             ret: function (cache, args, _) {
                 var m = cache.map(sym_value(cache, args[0]));
                 var a = m[0]; var b = m[1]; var c = m[2];
                 var v = vec_value(cache, args[1]);
                 var x = v[0]; var y = v[1];

                 var det = tident("det");
                 return tblock([
                    tvar("det", msub(mmul(a,c), msqr(b))),
                    tif(mneq(det,det), mass(x, mass(y, tzero)),
                    tif(meq(det,tzero), tblock([
                        tif(mneq(a,tzero), mop(OpAssignOp(OpDiv))(x,a), mass(x,tzero)),
                        tif(mneq(c,tzero), mop(OpAssignOp(OpDiv))(y,c), mass(y,tzero))
                    ]),tblock([
                        mass(det, mdiv(tone,det)),
                        tvar("t", mmul(det, msub(mmul(c,x), mmul(b,y)))),
                        mass(y, mmul(det, msub(mmul(a,y), mmul(b,x)))),
                        mass(x, tident("t"))
                    ])))
                 ]);
             }
            },
            {name: nn("solve"), types: [dim_sym(3), dim_vec(3)],
             ret: function (cache, args, _) {
                 var m = cache.map(sym_value(cache, args[0]));
                 var a = m[0]; var b = m[1]; var c = m[2];
                 var d = m[3]; var e = m[4]; var f = m[5];
                 var v = vec_value(cache, args[1]);
                 var x = v[0]; var y = v[1]; var z = v[2];

                 var det = tident("det");
                 var A = tident("A"); var B = tident("B"); var C = tident("C");
                 var E = tident("E");
                 return tblock([
                    tvar("A", msub(mmul(d,f), msqr(e))),
                    tvar("B", msub(mmul(e,c), mmul(b,f))),
                    tvar("C", msub(mmul(b,e), mmul(c,d))),
                    tvar("det", madd(madd(mmul(a,A), mmul(b,B)), mmul(c,C))),
                    tif(mneq(det,det), mass(x, mass(y, mass(z, tzero))),
                    tif(meq(det,tzero), tblock([
                        tif(mneq(a,tzero), mop(OpAssignOp(OpDiv))(x,a), mass(x,tzero)),
                        tif(mneq(d,tzero), mop(OpAssignOp(OpDiv))(y,d), mass(y,tzero)),
                        tif(mneq(f,tzero), mop(OpAssignOp(OpDiv))(z,f), mass(z,tzero))
                    ]),tblock([
                        mass(det, mdiv(tone,det)),
                        tvar("E", msub(mmul(b,c), mmul(a,e))),
                        tvar("X", mmul(det, madd(madd(mmul(x,A), mmul(y,B)), mmul(z,C)))),
                        tvar("Y", mmul(det, madd(madd(mmul(x,B), mmul(y,msub(mmul(a,f), msqr(c)))), mmul(z,E)))),
                        mass(z, mmul(det, madd(madd(mmul(x,C), mmul(y,E)), mmul(z,msub(mmul(a,d), msqr(b)))))),
                        mass(x, tident("X")),
                        mass(y, tident("Y"))
                    ])))
                 ]);
             }
            },
            {name: nn("string"), types: [any_mat],
             ret: function (cache, args, _) {
                 var x = args[0];
                 if (x.type == null) return null;

                 var texpr = texpr.bind(_, null, x.pos);

                 function string(a:String) return texpr(TEConst(CString(a)));
                 function stdString(a:TExpr) return tcall(tfield("Std.string"), [a]);

                 if (any_num(x.type))
                     return stdString(x);
                 else if (any_vec(x.type)) {
                    var ret = string(vec_type(vec_dim(x.type))+"(");
                    var fst = true;
                    for (v in vec_value(cache, x)) {
                        if (!fst) ret = madd(ret, string(","));
                        fst = false;
                        ret = madd(ret, stdString(v));
                    }
                    return madd(ret, string(")"));
                 }
                 else {
                     var rs = 0, cs = 0;
                     var cols =
                     if (any_sym(x.type)) {
                         var dim = sym_dim(x.type);
                         rs = cs = dim;

                         var values = sym_value(cache, x);
                         (function (j) return (function (i) return
                             if (i==dim-1 && j==0) string("#")
                             else if (i>j) string("")
                             else stdString(values[sym_index(dim, i, j)])
                         ).imap(dim)).imap(dim);
                     }
                     else {
                         var dim = mat_dim(x.type);
                         rs = dim.rows;
                         cs = dim.cols;

                         var values = mat_value(cache, x);
                         (function (j) return (function (i) return
                             stdString(values[mat_index(dim, i, j)])
                         ).imap(rs)).imap(cs);
                     }

                     var cols2 = (function (xs) return texpr(TEArrayDecl(xs))).map(cols);
                     var cols3 = texpr(TEArrayDecl(cols2));

                     var prefix = (any_sym(x.type) ? sym_type(sym_dim(x.type))
                                                   : mat_type(mat_dim(x.type)));
                     var whitespace = string(("\n")+(prefix.length.replicate(" ").join("")));
                     if (args.length > 1) whitespace = madd(whitespace, args[1]);

                     return texpr(TEParenthesis(
                        tcall(tfield("goodies.Shack.Shacks.colsPrint"), [cols3, string(prefix), whitespace]))
                     );
                 }

                 return null;
             }
            }
        ];

        for (fs in [
            {t:any_num, v:function (_,x,?_) return [x], ret: function (dim, elts) return elts[0]},
            {t:any_vec, v:vec_value, ret: function (dim, elts) return make_vec(dim.rows, elts)},
            {t:any_sym, v:sym_value, ret: function (dim, elts) return make_sym(dim.rows, elts)},
            {t:any_mat, v:mat_value, ret: function (dim, elts) return make_mat(dim, elts)}
        ]) {
            toplevel.push({name: nn("lerp"), types: 3.replicate(fs.t),
             ret: function (cache, args, _) {
                 var t0 = testCompatibility(args[0].pos, args[0].type, args[1].type);
                 var t1 = testCompatibility(args[1].pos, args[1].type, args[2].type);
                 var t = testCompatibility(args[2].pos, t0, t1);
                 var dim = any_dim(t);
                 if (any_num(args[2].type)) {
                     if (dim.cols > 1 || dim.rows > 1) {
                         args[2] = cache(args[2]);
                     }
                     args.push(msub(tone, args[2]));
                 }
                 var values = fs.v.bind(cache,_,t).map(args);
                 if (!any_num(args[2].type)) {
                     values.push(msub.bind(tone).map(values[2]));
                 }
                 var elts = madd.zipWith(mmul.zipWith(values[0], values[3]), mmul.zipWith(values[1], values[2]));
                 return fs.ret(dim, elts);
             }
            });
        }
    }

    static function toTExpr(ctx:Ctx, e:Null<Expr>, statement:Bool=false):TExpr {
        if (e == null) return null;

        var toTExpr_ = toTExpr;
        var toTExpr = toTExpr.bind(ctx);

        if (unop == null) inits();

        var texpr = texpr.bind(_,_,e.pos);
        var ret:Null<TExpr> = null;
        try {
            ret = switch (e.expr) {
            case EMeta(m, e):
                if (m.name == "tag") {
                    var t = namedType(switch (m.params[0].expr) { default: ""; case EConst(CIdent(n)): n; });
                    if (t != null) switch (e.expr) {
                        case EConst(CIdent(n)):
                            ctx.define(n, t);
                        case EField(_,_):
                            ctx.define(printer.printExpr(e), t);
                        default:
                    }
                    texpr(TEBlock([]), null);
                }
                else texpr(TEMeta(m, toTExpr(e)), null);
            case EConst(CIdent(v)):
                texpr(TEConst(CIdent(v)), ctx.lookup(v));
            case EConst(c=CInt(_)|CFloat(_)):
                texpr(TEConst(c), TTScalar);
            case EConst(c):
                texpr(TEConst(c), null);
            case EArray(e1, e2):
                texpr(TEArray(toTExpr(e1), toTExpr(e2)), null);
            case EBinop(op, e1, e2):
                withCache(ctx, function (cache) {
                    var e1 = toTExpr(e1);
                    var e2 = toTExpr(e2);
                    var ret = texpr(TEBinop(op, e1, e2), null);
                    for (b in binop) {
                        if (b.op != null && !b.op(op)) continue;
                        if (Func.call1.zipWith(b.args, [e1.type, e2.type]).and()) {
                            var ex = b.ret(ctx, cache, statement, op, e1, e2);
                            if (ex != null) {
                                ret = ex;
                                break;
                            }
                        }
                    }
                    return ret;
                });
            case EField(e, field):
                var name = printer.printExpr({expr:EField(e, field),pos:null});
                if (ctx.lookup(name) != null)
                    texpr(TEConst(CIdent(name)), ctx.lookup(name));
                else
                    texpr(TEField(toTExpr(e), field), null);
            case EParenthesis(e):
                var f = toTExpr(e);
                texpr(TEParenthesis(f), f.type);
            case EObjectDecl(fields):
                texpr(TEObjectDecl(
                    (function (f) return {field:f.field, expr:toTExpr(f.expr)}).map(fields)
                ), null);
            case EArrayDecl(values):
                texpr(TEArrayDecl(toTExpr.map(values)), null);
            case ECall(e, args):
                withCache(ctx, function (cache) {
                    var e = toTExpr(e);
                    var args = toTExpr.map(args);

                    switch (e.expr) {
                    case TEConst(CIdent(v)):
                        var t = namedType(v);
                        if (t != null) {
                            return texpr(TECall(null, args), t);
                        }

                        for (t in toplevel) {
                            if (!t.name(v)) continue;
                            if (!Func.call1.zipWith(t.types, maybeType.map(args)).and()) continue;
                            var r = t.ret(cache, args, v);
                            if (r != null) return r;
                        }
                    default:
                    }

                    if (e.type != null) {
                        for (s in slices) {
                            if (!s.type(e.type)) continue;
                            if (!s.validRange(args.length)) continue;
                            var e = s.ret(cache, e, args);
                            if (e != null) {
                                return e;
                            }
                        }
                    }

                    var oargs = [];
                    for (a in args) {
                        if (a.type == null) oargs.push(a);
                        else switch (a.type) {
                        case TTVec(_):
                            oargs = oargs.concat(vec_value(cache, a));
                        case TTSym(_):
                            oargs = oargs.concat(sym_value(cache, a));
                        case TTMat(_):
                            oargs = oargs.concat(mat_value(cache, a));
                        default: oargs.push(a);
                        }
                    }
                    return texpr(TECall(e, oargs), null);
                });
            case ENew(t, params):
                texpr(TENew(t, toTExpr.map(params)), null);
            case EUnop(op, postFix, e):
                withCache(ctx, function (cache) {
                    var e = toTExpr(e);
                    var ret = texpr(TEUnop(op, postFix, e), e.type);
                    for (u in unop) {
                        if (u.op != null && !u.op(op)) continue;
                        if (u.arg(e.type)) {
                            var e2 = u.ret(cache, op, postFix, e);
                            if (e2 != null) {
                                ret = e2;
                                break;
                            }
                        }
                    }
                    return ret;
                });
            case EVars(vars):
                texpr(TEVars(VARS(e.pos, ctx, vars)), null);
            case EFunction(name, f):
                var ctx2 = ctx.advance();
                var qargs = (function (a) {
                    var vtype = tComplexType(a.type);
                    var expr = toTExpr_(ctx2, a.value);
                    var etype = maybeType(expr);
                    if (vtype != null && etype != null && !Type.enumEq(vtype, etype))
                        Context.error("Inferred type for function argument does not match", e.pos);
                    ctx2.define(a.name, vtype == null ? etype : vtype);
                    return {name:a.name, opt:a.opt, type:a.type, value:expr};
                }).map(f.args);
                var g = {args:qargs, ret:f.ret, expr:toTExpr_(ctx2, f.expr), params:f.params};
                ctx2.reset();
                texpr(TEFunction(name, g), null);
            case EBlock(xs):
                var ctx2 = ctx.advance();
                var ys = ctx2.block = [];
                for (x in xs) {
                    var y = toTExpr_(ctx2, x, true);
                    if (y != null) ys.push(y);
                }
                ctx2.reset();
                texpr(TEBlock(ys), ys.length == 0 ? null : maybeType(ys[ys.length-1]));
            case EFor(it, expr):
                texpr(TEFor(toTExpr(it), toTExpr(expr)), null);
            case EIn(e1, e2):
                texpr(TEIn(toTExpr(e1), toTExpr(e2)), null);
            case EIf(econd, eif, eelse):
                var tif = toTExpr(eif); var ift = maybeType(tif);
                var telse = toTExpr(eelse); var elset = (telse == null ? null : maybeType(telse));
                if (ift != null && elset != null && !Type.enumEq(ift, elset))
                    Context.error("Inferred types of if/else branches do not match", eif.pos);
                texpr(TEIf(toTExpr(econd), tif, telse), ift);
            case EWhile(econd, e, normalWhile):
                texpr(TEWhile(toTExpr(econd), toTExpr(e), normalWhile), null);
            case ESwitch(e, cases, edef):
                var qases = (function (c:Case):TCase return {values:toTExpr.map(c.values), guard:toTExpr(c.guard), expr:toTExpr(c.expr)}).map(cases);
                texpr(TESwitch(toTExpr(e), qases, toTExpr(edef)), null);
            case ETry(e, catches):
                var qatches = (function (c) return {name:c.name, type:c.type, expr:toTExpr(c.expr)}).map(catches);
                texpr(TETry(toTExpr(e), qatches), null);
            case EReturn(e):
                texpr(TEReturn(toTExpr(e)), null);
            case EBreak:
                texpr(TEBreak, null);
            case EContinue:
                texpr(TEContinue, null);
            case EUntyped(e):
                var t = toTExpr(e);
                texpr(TEUntyped(t), t == null ? null : maybeType(t));
            case EThrow(e):
                texpr(TEThrow(toTExpr(e)), null);
            case ECast(e, t):
                texpr(TECast(toTExpr(e), t), null);
            case EDisplay(e, isCall):
                texpr(TEDisplay(toTExpr(e), isCall), null);
            case EDisplayNew(t):
                texpr(TEDisplayNew(t), null);
            case ETernary(econd, eif, eelse):
                var tif = toTExpr(eif); var ift = maybeType(tif);
                var telse = toTExpr(eelse); var elset = (telse == null ? null : maybeType(telse));
                if (ift != null && elset != null && !Type.enumEq(ift, elset))
                    Context.error("Inferred types of if/else branches in ?: do not match", eif.pos);
                texpr(TETernary(toTExpr(econd), tif, telse), ift);
            case ECheckType(e, t):
                texpr(TECheckType(toTExpr(e), t), null);
            };
        }
        catch (err:Array<Dynamic>) {
        }
        return if (ret == null) null else ret;
    }

    static function namedType(v:String):TType {
        if (vec_regex.match(v)) {
            return TTVec(Std.parseInt(v.substr(1)));
        }
        if (sym_regex.match(v)) {
            return TTSym(Std.parseInt(v.substr(1)));
        }
        if (mat_regex.match(v)) {
            var vs = v.substr(1).split("x");
            return TTMat(Std.parseInt(vs[0]), Std.parseInt(vs[1]));
        }
        if (v == "Int" || v == "Float") {
            return TTScalar;
        }
        return null;
    }

    static function tComplexType(t:ComplexType):TType {
        if (t == null) return null;
        return switch (t) {
        case TPath({name: v}):
            namedType(v);
        default: null;
        }
    }

    static function run() {
        var fields = Context.getBuildFields();

        var gields = [];
        var ctx = new Ctx();
        for (f in fields) {
            var kind = switch (f.kind) {
            case FVar(t, eo):
                var vars = VARS(f.pos, ctx, [{name:f.name, expr:eo, type:t}]);
                for (v in vars) {
                    gields.push({
                        name: v.name,
                        doc: f.doc,
                        access: f.access,
                        kind: TFVar(v.type, v.expr),
                        pos: f.pos,
                        meta: f.meta
                    });
                }
                null;
            case FProp(get, set, t, e):
                var es = toTExpr(ctx, e);
                var ftype = tComplexType(t);
                if (ftype != null && es != null && !Type.enumEq(es.type, ftype))
                    Context.error("Field's declared type does not match its default expression type", f.pos);
                ctx.define(f.name, ftype == null ? maybeType(es) : ftype);
                TFProp(get, set, t, es);
            case FFun(fu):
                var ctx2 = ctx.advance();
                var qargs = ARGS(f.pos, ctx2, fu.args);
                var expr = toTExpr(ctx2, fu.expr);
                ctx2.reset();
                TFFun({
                    args: qargs,
                    ret: fu.ret,
                    expr: expr,
                    params: fu.params
                });
            }

            if (kind == null) continue;
            gields.push({
                name: f.name,
                doc: f.doc,
                access: f.access,
                kind: kind,
                pos: f.pos,
                meta: f.meta
            });
        }

        return TTransform.toField.map(gields);
    }
#end
}
