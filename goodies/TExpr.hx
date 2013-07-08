package goodies;

import haxe.macro.Context;
import haxe.macro.Expr;

using goodies.Func;

#if macro

// Context for Shack inferred typing
class Ctx {
    // Types inferred for variables/fields in this scope
    var types:Map<String, TType>;

    // Set of contexts for each block scope introduced (in order)
    // within this scope
    var scopes:Array<Ctx>;

    // Current block scope for traversal
    var scope:Int;

    // Parent context for type lookups
    var parent:Ctx;

    // Set of expressions for current EBlock being processed
    // This is used to inject cached variable stores, with id
    // for unique generation.
    public var block:Array<TExpr>;
    public var id:Int;

    public function new() {
        types = new Map();
        scopes = [];
        scope = 0;
        parent = null;
        block = null;
        id = 0;
    }

    // Advance traversal to next (returned) block scope
    // A new scope is lazily constructed if required.
    public function advance():Ctx {
        if (scope >= scopes.length) {
            var ctx = new Ctx();
            scopes.push(ctx);
            ctx.parent = this;
        }
        return scopes[scope++];
    }

    // Insert a new scope before currently processed scope
    // used when injecting new EBlock expressions to scope
    // in transformations
    public function inject():Ctx {
        var ctx = new Ctx();
        scopes.insert(scope++, ctx);
        ctx.parent = this;
        return ctx;
    }

    // Reset for next traversal
    public function reset():Void {
        scope = 0;
    }

    // Lookup type for given name in context.
    public function lookup(name:String):Null<TType> {
        var ctx = this;
        while (ctx != null) {
            if (ctx.types.exists(name)) return ctx.types.get(name);
            ctx = ctx.parent;
        }
        return null;
    }

    // Define type for given name in context.
    public function define(name:String, type:TType) {
        types.set(name, type);
    }

    static var printer = new haxe.macro.Printer();
    public function toString(tab="") {
        var tabString = "    ";
        var ret = '$tab{\n';
        var tab2 = tab+tabString;
        for (v in types.keys()) {
            ret += '$tab2$v:${TTransform.printTType(types.get(v))}\n';
        }
        for (s in scopes) {
            ret += s.toString(tab2)+"\n";
        }
        return '$ret$tab}';
    }
}

typedef TFunctionArg = {
    value:Null<TExpr>,
    type:Null<ComplexType>,
    opt:Bool,
    name:String
};

typedef TFunction = {
    ret:Null<ComplexType>,
    params:Array<TypeParamDecl>,
    expr:Null<TExpr>,
    args:Array<TFunctionArg>
};

typedef TCase = {
    var values:Array<TExpr>;
    @:optional var guard:Null<TExpr>;
    var expr:Null<TExpr>;
}

typedef TVar = {
    type : Null<ComplexType>,
    name : String,
    expr : Null<TExpr>
}

enum TExprDef {
    TEWhile(econd:TExpr, e:TExpr, normalWhile:Bool);
    TEVars(vars:Array<TVar>);
    TEUntyped(e:TExpr);
    TEUnop(op:Unop, postFix:Bool, e:TExpr);
    TETry(e:TExpr, catches:Array<{type:ComplexType, name:String, expr:TExpr}>);
    TEThrow(e:TExpr);
    TETernary(econd:TExpr,eif:TExpr,eelse:TExpr);
    TESwitch(e:TExpr,cases:Array<TCase>,edef:Null<TExpr>);
    TEReturn(?e:TExpr);
    TEParenthesis(e:TExpr);
    TEObjectDecl(fields:Array<{field:String,expr:TExpr}>);
    TENew(t:TypePath, params:Array<TExpr>);
    TEIn(e1:TExpr, e2:TExpr);
    TEIf(econd:TExpr, eif:TExpr, eelse:Null<TExpr>);
    TEFunction(name:Null<String>, f:TFunction);
    TEFor(it:TExpr, expr:TExpr);
    TEField(e:TExpr, field:String);
    TEDisplayNew(t:TypePath);
    TEDisplay(e:TExpr, isCall:Bool);
    TEContinue;
    TEConst(c:Constant);
    TECheckType(e:TExpr, t:ComplexType);
    TECast(e:TExpr, t:Null<ComplexType>);
    TECall(e:TExpr, params:Array<TExpr>);
    TEMeta(s:MetadataEntry, e:TExpr);
    TEBreak;
    TEBlock(exprs:Array<TExpr>);
    TEBinop(op:Binop, e1:TExpr, e2:TExpr);
    TEArrayDecl(values:Array<TExpr>);
    TEArray(e1:TExpr, e2:TExpr);
}

typedef TExpr = {
    expr: TExprDef,
    pos: Position,
    type: TType
}

typedef TField = {
    var name:String;
    var doc:String;
    var access:Array<Access>;
    var kind: TFieldType;
    var pos: Position;
    var meta: Metadata;
}

enum TFieldType {
    TFVar(t:Null<ComplexType>, ?e:Null<TExpr>);
    TFFun(f:TFunction);
    TFProp(get:String,set:String,?t:Null<ComplexType>,?e:Null<TExpr>);
}

class TTransform {
    public static function toField(f:TField) return {
        name: f.name,
        doc: f.doc,
        access: f.access,
        kind: toFieldType(f.kind),
        pos: f.pos,
        meta: f.meta
    }
    public static function toFieldType(f:TFieldType) return switch (f) {
        case TFVar(t, e): FVar(t, toExpr(e));
        case TFFun(f): FFun(toFunction(f));
        case TFProp(get, set, t, e): FProp(get, set, t, toExpr(e));
    }
    public static function toCase(c:TCase):Case {
        return {values:toExpr.map(c.values), guard:toExpr(c.guard), expr:toExpr(c.expr)};
    }
    public static function toFunctionArg(a:TFunctionArg):FunctionArg {
        return {value:toExpr(a.value), type:a.type, opt:a.opt, name:a.name};
    }
    public static function toFunction(f:TFunction):Function {
        return {ret:f.ret, params:f.params, expr:toExpr(f.expr), args:toFunctionArg.map(f.args)};
    }
    public static function toExpr(ex:Null<TExpr>):Null<Expr> {
        if (ex == null) return null;
        var expr = switch(ex.expr) {
        case TEWhile(econd, e, normalWhile):
            EWhile(toExpr(econd), toExpr(e), normalWhile);
        case TEVars(vars):
            EVars((function (v) return {type:v.type, name:v.name, expr:toExpr(v.expr)}).map(vars));
        case TEUntyped(e):
            EUntyped(toExpr(e));
        case TEUnop(op, postFix, e):
            EUnop(op, postFix, toExpr(e));
        case TETry(e, catches):
            ETry(toExpr(e), (function (c) return {type:c.type, name:c.name, expr:toExpr(c.expr)}).map(catches));
        case TEThrow(e):
            EThrow(toExpr(e));
        case TETernary(econd, eif, eelse):
            ETernary(toExpr(econd), toExpr(eif), toExpr(eelse));
        case TESwitch(e, cases, edef):
            ESwitch(toExpr(e), toCase.map(cases), toExpr(edef));
        case TEReturn(e):
            EReturn(toExpr(e));
        case TEParenthesis(e):
            EParenthesis(toExpr(e));
        case TEObjectDecl(fields):
            EObjectDecl((function (f) return {field:f.field, expr:toExpr(f.expr)}).map(fields));
        case TENew(t, params):
            ENew(t, toExpr.map(params));
        case TEIn(e1, e2):
            EIn(toExpr(e1), toExpr(e2));
        case TEIf(econd, eif, eelse):
            EIf(toExpr(econd), toExpr(eif), toExpr(eelse));
        case TEFunction(name, f):
            EFunction(name, toFunction(f));
        case TEFor(it, expr):
            EFor(toExpr(it), toExpr(expr));
        case TEField(e, field):
            EField(toExpr(e), field);
        case TEDisplayNew(t):
            EDisplayNew(t);
        case TEDisplay(e, isCall):
            EDisplay(toExpr(e), isCall);
        case TEContinue:
            EContinue;
        case TEConst(c):
            EConst(c);
        case TECheckType(e, t):
            ECheckType(toExpr(e), t);
        case TECast(e, t):
            ECast(toExpr(e), t);
        case TECall(e, params):
            if (e == null) Context.error("Stack literal persisted to output! -- " + Std.string(TECall(e, params)), ex.pos);
            ECall(toExpr(e), toExpr.map(params));
        case TEMeta(s, e):
            EMeta(s, toExpr(e));
        case TEBreak:
            EBreak;
        case TEBlock(exprs):
            EBlock(toExpr.map(exprs));
        case TEBinop(op, e1, e2):
            EBinop(op, toExpr(e1), toExpr(e2));
        case TEArrayDecl(values):
            EArrayDecl(toExpr.map(values));
        case TEArray(e1, e2):
            EArray(toExpr(e1), toExpr(e2));
        };
        return {expr:expr, pos:ex.pos};
    }
    static var printer = new haxe.macro.Printer("    ");
    public static function printTType(t:TType) {
        if (t == null) return "null";
        return switch (t) {
            case TTScalar: "Scalar";
            case TTVec(n): 'V$n';
            case TTSym(n): 'S$n';
            case TTMat(n,m): 'M${n}x$m';
        };
    }
    static function opt<T>(v:T, f:T->String, prefix="") return v == null ? "" : (prefix + f(v));
    static function printVar(v:TVar) {
        return v.name + opt(v.type, printer.printComplexType, ":")
                      + opt(v.expr, printExpr, "=");
    }
    static function printFunctionArg(arg:TFunctionArg) {
        return (arg.opt ? "?" : "")
             + arg.name
             + opt(arg.type, printer.printComplexType, ":")
             + opt(arg.value, printExpr, "=");
    }
    static function printFunction(func:TFunction) {
        return (func.params.length > 0 ? "<" + printer.printTypeParamDecl.map(func.params).join(",") + ">" : "")
            + "(" + printFunctionArg.map(func.args).join(",") + ")"
            + opt(func.ret, printer.printComplexType, ":")
            + opt(func.expr, printExpr, " ");
    }
    public static function printField(field:TField):String {
        return (field.meta != null && field.meta.length > 0 ? printer.printMetadata.map(field.meta).join(" ") + " " : "")
        + (field.access.length > 0 ? printer.printAccess.map(field.access).join(" ") + " " : "")
        + switch (field.kind) {
            case TFVar(t, eo): 'var ${field.name}' + opt(t, printer.printComplexType, ":") + opt(eo, printExpr, "=");
            case TFFun(f): 'function ${field.name}' + printFunction(f);
            case TFProp(get, set, t, eo): 'var ${field.name}($get,$set)' + opt(t, printer.printComplexType, ":") + opt(eo, printExpr, "=");
        };
    }
    public static var tab = "";
    public static function printExpr(e:Null<TExpr>):String {
        if (e == null) return "#NULL";
        return (e.type==null?"":"<[") + (e.expr==null ? "@null" : switch(e.expr) {
        case TEConst(c): printer.printConstant(c);
        case TEWhile(econd, e, true): 'while(${printExpr(econd)}) ${printExpr(e)}';
        case TEWhile(econd, e, false): 'do ${printExpr(e)} while(${printExpr(econd)})';
        case TEVars(vars): 'var ${printVar.map(vars).join(", ")}';
        case TEBlock([]): '{\n$tab}';
        case TEBlock(el):
            var old = tab;
            tab += "    ";
            var s = '{\n$tab' + printExpr.map(el).join(';\n$tab');
            tab = old;
            s + ';\n$tab}';
        case TEField(el, n): '${printExpr(el)}.$n';
        case TEBinop(op, e1, e2): '${printExpr(e1)}${printer.printBinop(op)}${printExpr(e2)}';
        case TEUnop(op, true, el): printExpr(el) + printer.printUnop(op);
        case TEUnop(op, false, el): printer.printUnop(op) + printExpr(el);
        case TEFunction(no, func) if (no != null): 'function $no' + printFunction(func);
        case TEFunction(_, func): 'function ' + printFunction(func);
        case TECall(e, es): '${printExpr(e)}(${printExpr.map(es).join(", ")})';
        case TEContinue: "continue";
        case TEBreak: "break";
        case TEReturn(e) if (e != null): 'return ${printExpr(e)}';
        case TEReturn(_): 'return';
        case TEParenthesis(e): '(${printExpr(e)})';
        case TEUntyped(e): 'untyped ${printExpr(e)}';
        case TEThrow(e): 'throw ${printExpr(e)}';
        case TENew(tp, el): 'new ${printer.printTypePath(tp)}(${printExpr.map(el).join(", ")})';
        case TETernary(econd, eif, eelse): '${printExpr(econd)} ? ${printExpr(eif)} : ${printExpr(eelse)}';
        case TEIn(x, y): '${printExpr(x)} in ${printExpr(y)}';
        case TEArray(x, y): '${printExpr(x)}[${printExpr(y)}]';
        case TEArrayDecl(vals): '[${printExpr.map(vals).join(", ")}]';
        case TEIf(econd, eif, eelse) if (eelse == null): 'if (${printExpr(econd)}) ${printExpr(eif)}';
        case TEIf(econd, eif, eelse): 'if (${printExpr(econd)}) ${printExpr(eif)} else ${printExpr(eelse)}';
        case TEFor(it, expr): 'for (${printExpr(it)}) ${printExpr(expr)}';
        case TETry(_, _): "#TRY";
        case TESwitch(_, _, _): "#SWITCH";
        case TEObjectDecl(_): "#OBJECT";
        case TEDisplayNew(_): "#DISPLAYNEW";
        case TEDisplay(_, _): "#DISPLAY";
        case TECheckType(_, _): "#CHECK";
        case TECast(_, _): "#CAST";
        case TEMeta(_, _): "#META";
        }) + (e.type==null?"":"::" + printTType(e.type) + "]>");
    }
}

enum TType {
    TTVec(dim:Int);
    TTSym(dim:Int);
    TTMat(rows:Int, cols:Int);
    TTScalar;
}

#end
