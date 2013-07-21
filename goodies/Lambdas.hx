package goodies;

import haxe.macro.Expr;
import haxe.macro.ExprTools;
import haxe.macro.Context;

@:autoBuild(goodies.LambdasImpl.run()) @:remove extern interface Lambdas {}

class LambdasImpl {
    static function replace(e:Expr) {
        return switch (e.expr) {
            case EArrayDecl(es):
                {pos:e.pos, expr:EArrayDecl([for (e in es) {
                    switch (e.expr) {
                    case EBinop(OpArrow, x, y):
                        {pos:e.pos, expr:EBinop(OpArrow, replace(x), replace(y))};
                    default: ExprTools.map(e, replace);
                }}])};
            case EBinop(OpArrow, {expr:EConst(CIdent(n))}, expr):
                return {pos:e.pos, expr:EFunction(null, {
                    ret: null,
                    params: [],
                    expr: macro return $expr,
                    args: [{
                        name: n,
                        opt: false,
                        type: null,
                        value: null
                    }]
                })};
            case EBinop(OpArrow, {expr:EArrayDecl(ns)}, expr):
                return {pos:e.pos, expr:EFunction(null, {
                    ret: null,
                    params: [],
                    expr: macro return $expr,
                    args: ns.map(function (n) return switch (n) {
                        case {expr:EConst(CIdent(n))}:
                            {name:n, opt:false, type:null, value:null};
                        default: null;
                    })
                })};
            case EBinop(OpArrow, {expr:EObjectDecl(ns)}, expr):
                return {pos:e.pos, expr:EFunction(null, {
                    ret: null,
                    params: [],
                    expr: macro return $expr,
                    args: ns.map(function (n) return {name:n.field, opt:false, value:null, type: switch (n.expr.expr) {
                        case EConst(CIdent("_")): null;
                        case EConst(CIdent(t)): Context.toComplexType(Context.getType(t));
                        default: null;
                    }})
                })};
            case _: ExprTools.map(e, replace);
        }
    }
    static function run() {
        var fields = Context.getBuildFields();
        for (f in fields) {
            switch (f.kind) {
            case FFun(f):
                f.expr = replace(f.expr);
            default:
            }
        }
        return fields;
    }
}
