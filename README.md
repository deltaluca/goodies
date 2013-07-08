goodies
=======

Collection of little Haxe goodies I don't yet have an official place for.

#### Table of Contents  
- [FFT](#FFT)  Fast-Fourier Transforms for complex Vector data.
- [Assert](#Assert)  Runtime Assertions via macros.
- [MacroUtils](#MacroUtils)  Utilities to complement writing Macros.
- [Maybe](#Maybe)  Maybe abstract type for null safety + Build macro for runtime assertion checking of usage.
- [Lazy](#Lazy)  Lazily allocated member variables, with safety checks via build macros.
- [Builder](#Builder)  Builder pattern for member variables via build macros.
- [Tuple](#Tuple)  Tuple abstract types.
- [Func](#Func)    Functional programming module (Mostly revolving around extending Array type)
- [CoalescePrint](#CoalescePrint) Neko/C++ Printing utility for coalesced logs.
- [Shack](#Shack)  Stack allocated vectors/matrices via build macros.
- [Fixed16](#Fixed16) Abstract type for 16.16 fixed-point numerical values and ops.

<a name="FFT"/>
## FFT

(Complex) Fast Fourier Transforms.

```cs
FFT.DFT  (x:Vector<Float>, y:Vector<Float>, skip:Int=2, oX:Int=0, oy:Int=0);
FFT.IDFT (x:Vector<Float>, y:Vector<Float>, skip:Int=2, ox:Int=0, oy:Int=0);
```

In both cases ```x``` and ```y``` vectors should be of equal size.

+ ```DFT``` performs a Discrete Fourier Transform ```y = Cn*x```
+ ```IDFT``` performs the inverse transform ```x = (1/N)Cn^-1*y```

the contents of the ```x```,```y``` vectors may be specified as starting from a certain offset ```oX``` and ```oY```, whilst ```skip``` specifies the offset between each consecutive complex numbers in the vectors, suitable for performing multidimensional transforms.

The number of complex elements in ```x```,```y``` taking into account offsets and ```skip``` must be a power of ```2```.


<a name="Assert"/>
## Assert

Simple assertions for Haxe.

```cs
Assert.assert(expression); // [is true]

// or with using
expression.assert();
```

When compiling with `-D assertions` then these assertions will be enabled, and at runtime if any expression is false an exception will be thrown with the location of the assertion failure and the expression that failed.


<a name="MacroUtils"/>
## MacroUtils

Basic utilities to complement reification and ```Context```

```cs
// Get actual type of local class/interface in build macro including type parameters
// eg:
// @:build(MyMacro.run()) interface A<S,T> {}
// then MacroUtils.self() would return the ComplexType associated with A<S,T>
MacroUtils.self() : ComplexType;

// Determine if the type being built in @:build macro is an interface.
MacroUtils.isInterface() : Bool;

// Determine if given Field has metadata with the given name, and if so return its parameters.
MacroUtils.hasMeta(f:Field, name:String):Maybe<Array<Expr>>;

// Until Haxe has proper reification for Fields, this helps avoid some boilerplate.
// eg:
// MacroUtils.field((macro var _:String), [APublic], "x");
// MacroUtils.field(macro function():Int return 10, [APublic], "ten");
// passing true for iface argument in the function example would cause the field to 
// be generated with an empty expression (for use in building interfaces).
MacroUtils.field(e:Expr, access:Array<Access>, name:String, iface=false):Field;
```


<a name="Maybe"/>
## Maybe

Abstract type implementing a Maybe/Option type modifier based on Null<>. Allows (selective) use
of Maybe types without overhead and working seamlessly with non-Maybe'd code via implict casts.

```cs
abstract Maybe<T>(Null<T>) from Null<T> {

    // Construt new Maybe type (if cannot use implicit casting)
    function new(x:Null<T>):Maybe<T>;

    // (Unsafely) extract underlying value.
    // If #debug is set, then this will give an exception when Maybe type is Nothing/null
    function extract():T;
    
    // (Safely) extract underlying value, with a suitable default in case of Nothing/null
    function or(defaultValue:T):T;
    
    // (Safely) apply function to underlying value, with a suitable default return in case of Nothing/null
    function runOr<S>(eval:T->S, defaultValue:S):S;
    
    // (Safely) apply function to underyling value, with a suitable alternative call in case of Nothing/null
    function run<S>(eval:T->S, defaultEval:Void->S):S;
    
    // Convert Maybe<T> to Array<T>
    // Just x  -> [x]
    // Nothing -> []
    function maybeToList():Array<T>;
    
    // Convert singular Array<T> to Maybe<T>
    // [] -> Nothing
    // [x:xs] -> x
    static function listToMaybe(xs:Array<T>):Maybe<T>;
    
    // Convert list of possibly null values to list of non-null elements.
    // eg with using: [null,10,20,null].catMaybes() = [10,20]
    // Can see implict casting coming in handy here :)
    static function catMaybes<T>(xs:Array<Maybe<T>>):Array<T>;
    
    // Map function over list of values, collecting non-null results.
    // Maybe.mapMaybe(Maybe.listToMaybe, [[10],[],[],[20]]) = [10,20]
    static function mapMaybe<T,S>(eval:T->Maybe<S>, xs:Array<T>):Array<S>;
    
    // Lift a standard N arg function into the Maybe monad
    static function liftM<T,S>(f:T->S):Maybe<T>->Maybe<S>;
    static function liftM2<T,S,R>(f:T->S->R):Maybe<T>->Maybe<S>->Maybe<R>;
    static function liftM3<T,S,R,Q>(f:T->S->R->Q):Maybe<T>->Maybe<S>->Maybe<R>->Maybe<Q>
    static function liftM4<T,S,R,Q,P>(f:T->S->R->Q->P):Maybe<T>->Maybe<S>->Maybe<R>->Maybe<Q>->Maybe<P>;
    
    // Call a maybe functino with given arity.
    static function call<T>(f:Maybe<Void->T>):Maybe<T>;
    static function call2<T,S>(f:Maybe<T->S>, x:Maybe<T>):Maybe<S>;
    static function call2<T,S,R>(f:Maybe<T->S->R>, x:Maybe<T>, y:Maybe<S>):Maybe<R>;
}
```

Additionaly there is a MaybeEnv interface which adds strict non-null checks to a type's methods ensuring that
any arguments not typed with Maybe<> are non-null on entry at runtime. Along with some compile time checks of optional argumennts.

```cs
class Main implements MaybeEnv {
   // Compile time error, String is nullable but not typed as Maybe<String>
   static function fail1(?x:String) {}
   
   // Compile time error, argument's default value is null, but not typed with Maybe
   static function fail2(z:String=null) {}
   
   static function testdyn(x:Int) {}
   static function teststat(x:String) {}
   
   static function main() {
      // on dynamic platforms!! otherwise already a compiler error, this will give a runtime
      // error for passing null to an argument not typed with Maybe (in #debug)
      testdyn(null);
      
      // always gives runtime error (in #debug)
      teststat(null);
   }
}
```

<a name="Lazy"/>
## Lazy

Simple build macro for lazy instantiation of nullable fields (Doesn't work on Int/Float/Bool for static platforms without Null<> or Maybe<>)

```cs
class Main implements LazyEnv {
   @:lazyVar var x:Array<Int> = [0,1]; // not just constants :)
   @:lazyVar var y:String;
   static function main() {
      trace(x); // traces [0,1], noting that until x was accessed, the field was actually null.
      trace(x = []); // traces []
      trace(x = null); // traces [0,1]. We set x to null, and the lazy instantiation kicked back!
      
      trace(y); // runtime error (in #debug), y has not been instantiated yet!
      trace(y = "hi"); // traces hi
      y = null;
      trace(y); // runtime error again, y has not been instantiated yet!
   }
}
```

The real fun happens when you combine this with ```Maybe``` :)

```cs
class Main implements MaybeEnv implements LazyEnv {
   @:lazyVar var x:Array<Int> = [0,1];
   @:lazyVar var y:String;
   @:lazyVar var z:Maybe<String> = "hi";
   static function main() {
      trace(x); // traces [0,1] as before
      trace(x = []); // traces [] as before
      trace(x = null); // runtime error, can't assign null to non-Maybe type! so not allowed to reset value.
      
      trace(y); // runtime error as before
      trace(y = "hello"); // same as before
      y = null; // runtime error, can't assign null to non-Maybe type! y is 'never' allowed
                // to be used uninstantiated by user.
                
      trace(z); // Just("hi")
      trace(z = "lol"); // Just("lol")
      trace(z = null); // Just("hi")  // behaviour we had before introducing MaybeEnv to LazyEnv, can
                       //never be null, but allowed to be reset.
   }
}
```

<a name="Builder"/>
## Builder

Build macro for reducing boilerplate in the builder pattern of object state mutation.

```cs
class Pizza implements Builder {
   @:builder var size:Float = 10;
   @:builder var sauce:SauceType = TomatoSauce; // can have any expression
   @:builder(react = function (topping) {
      trace('topping just changed to $topping');
   }) var topping:Topping = Peppers;
   
   // Default values above (optional) are moved into top of constructor.
   public function new() {}
}

// Creates a size 20 pizza with tomato sauce and jalapenos.
// traces toping just changed to Jalapenos
var pizza = new Pizza().size(20).topping(Jalapenos);
trace(pizza.getSize()); // 20
```

This can be combined with ```Maybe``` and ```Lazy```

```cs
class N implements MaybeEnv implements LazyEnv implements Builder {
    @:lazyVar @:builder(react=function (metric) {
        Assert.assert(metric[0] != 1);
    }) var metric:Array<Int> = [0,1,2];

    public function new() {}
}

class Main {
    static function main() {
        var n = new N();
        trace(n.getMetric());
        n.metric([1,3,4]);  // runtime error, metric[0] != 1 assertion
        trace(n.getMetric());
        n.metric(null);     // runtime error, assigning null to non Maybe type
        trace(n.getMetric());
    }
}
```

Currently there's one other mutator for ```@:builder``` which is:
```cs
@:builder(ret=T) var ...;
```

which can be combined with ```react``` as two arguments. The ```ret``` mutator changes the Type of the property getter from the parent Type to the one given as argument (Can be useful sometimes).

<a name="Tuple"/>
## Tuple

Tuple abstract types.

```cs
// Tuple.T2,T3,T4,T5
var v = new T2(10, "hello");
trace(v); // (10, "hello")
v = T2.make(10, "hello");
trace(v.v0); // 10
trace(v.v1); // "hello"
```


<a name="Func"/>
## Func

```cs
// General operations
Func.id : S -> S
Func.flip : (S->T->R) -> (T->S->R)
Func.dot : (T->R) -> (S->T) -> (S->R) // Haskell (.)

Func.curry2 : (S->T->R) -> (S->(T->R))
Func.curry3 : (S->T->R->P) -> (S->(T->(R->P)))
Func.curry4 : (S->T->R->P->Q) -> (S->(T->(R->(P->Q))))

Func.tuple2 : (S->T->R) -> (T2<S,T>->R)
Func.tuple3 : (S->T->R->P) -> (T3<S,T,R>->P)
Func.tuple4 : (S->T->R->P->Q) -> (T4<S,T,R,P>->Q)

Func.uncurry2 : (S->(T->R)) -> (S->T->R)
Func.uncurry3 : (S->(T->(R->P))) -> (S->T->R->P)
Func.uncurry4 : (S->(T->(R->(P->Q)))) -> (S->T->R->P->Q)

// Array operations
Func.lift : (S->T) -> (Array<S>->Array<T>) // Func.map.bind(f) :: Func.curry2(Func.map)(f)
Func.count : Int -> Array<Int> // [for (i in 0...n) i], eg: 4 -> [0,1,2,3]
Func.count2 : Int -> Int -> Array<T2<Int,Int>> // eg: 2 3 -> [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]

Func.map  : (S->T) -> Array<S> -> Array<T>
Func.iter : (S->T) -> Array<S> -> Void // map with no return

Func.imap : (Int->S) -> Int -> Array<S> // [for (i in 0...n) f(i)]
Func.iiter : (Int->S) -> Int -> Void // same, but no return
Func.imap2 : (Int->Int->S) -> Int -> (Int->Int) -> Array<S> // 2d-integer map, Int->Int used for sub-iteration count
Func.iiter2 : (Int->Int->S) -> Int -> (Int->Int) -> Void // same, but no return

Func.intersperse : S -> Array<S> -> Array<S>
Func.intercalate : Array<S> -> Array<Array<S>> -> Array<S>
Func.transpose : Array<Array<S>> -> Array<Array<S>>
Func.subsequences : Array<S> -> Array<Array<S>>

Func.foldl  : (S->T->S) -> S -> Array<T> -> S
Func.foldr  : (S->T->T) -> T -> Array<S> -> T
Func.foldl1 : (S->S->S) -> Array<S> -> S
Func.foldr1 : (S->S->S) -> Array<S> -> S

Func.concat : Array<Array<S>> -> Array<S>
Func.concatMap : (S->Array<T>) -> Array<S> -> Array<T>

Func.and : Array<Bool> -> Bool
Func.or  : Array<Bool> -> Bool
Func.all : (S->Bool) -> Array<S> -> Bool
Func.any : (S->Bool) -> Array<S> -> Bool

Func.sum     : Array<Int> -> Int
Func.product : Array<Int> -> Int
Func.maximum : Array<Int> -> Int
Func.minimum : Array<Int> -> Int

Func.scanl  : (S->T->S) -> S -> Array<T> -> Array<S>
Func.scanr  : (S->T->T) -> T -> Array<S> -> Array<T>
Func.scanl1 : (S->S->S) -> Array<S> -> Array<S>
Func.scanr1 : (S->S->S) -> Array<S> -> Array<S>

Func.mapAccumL : (S->T->T2<S,R>) -> S -> Array<T> -> T2<S, Array<R>>
Func.mapAccumR : (S->T->T2<S,R>) -> S -> Array<T> -> T2<S, Array<R>>

Func.replicate : Int -> S -> Array<S>

Func.unfoldr : (T->Maybe<T2<S,T>>) -> T -> Array<S>

Func.take : Int -> Array<S> -> Array<S>
Func.drop : Int -> Array<S> -> Array<S>
Func.splitAt : Int -> Array<S> -> T2<Array<S>,Array<S>>
Func.takeWhile : (S->Bool) -> Array<S> -> Array<S>
Func.dropWhile : (S->Bool) -> Array<S> -> Array<S>
Func.dropWhileEnd : (S->Bool) -> Array<S> -> Array<S>
Func.span : (S->Bool) -> Array<S> -> T2<Array<S>,Array<S>>
Func._break : (S->Bool) -> Array<S> -> T2<Array<S>,Array<S>>
Func.stripPrefix : Array<S> -> Array<S> -> Maybe<Array<S>>
Func.group : Array<S> -> Array<Array<S>>
Func.inits : Array<S> -> Array<Array<S>>
Func.tails : Array<S> -> Array<Array<S>>

Func.find : (S->Bool) -> Array<S> -> Maybe<S>
Func.filter : (S->Bool) -> Array<S> -> Array<S>
Func.partition : (S->Bool) -> Array<S> -> T2<Array<S>, Array<S>>

Func.zip : Array<S> -> Array<T> -> Array<T2<S,T>
Func.zip3 : Array<S> -> Array<T> -> Array<R> -> Array<T3<S,T,R>>
Func.zip4 : Array<S> -> Array<T> -> Array<R> -> Array<P> -> Array<T4<S,T,R,P>>
Func.zip5 : Array<S> -> Array<T> -> Array<R> -> Array<P> -> Array<Q> -> Array<T5<S,T,R,P,Q>>

Func.zipWith : (S->T->O) -> Array<S> -> Array<T> -> Array<O>
Func.zipWith3 : (S->T->R->O) -> Array<S> -> Array<T> -> Array<R> -> Array<O>
Func.zipWith4 : (S->T->R->P->O) -> Array<S> -> Array<T> -> Array<R> -> Array<P> -> Array<O>
Func.zipWith5 : (S->T->R->P->Q->O) -> Array<S> -> Array<T> -> Array<R> -> Array<P> -> Array<Q> -> Array<O>

Func.unzip : Array<T2<S,T>> -> T2<Array<S>,Array<T>>
Func.unzip3 : Array<T3<S,T,R>> -> T3<Array<S>,Array<T>,Array<R>>
Func.unzip4 : Array<T4<S,T,R,P>> -> T4<Array<S>,Array<T>,Array<R>,Array<P>>
Func.unzip5 : Array<T5<S,T,R,P,Q>> -> T5<Array<S>,Array<T>,Array<R>,Array<P>,Array<Q>>

Func.nub : Array<S> -> Array<S>
Func.delete : S -> Array<S> -> Array<S>
Func.subtract : Array<S> -> Array<S> -> Array<S> // Haskell (\\)
Func.union : Array<S> -> Array<S> -> Array<S>
Func.intersect : Array<S> -> Array<S> -> Array<S>


// Function call wrappers
Func.call  : (Void->S) -> S
Func.call1 : (S->T) -> S -> T
Func.call2 : (S->T->R) -> S -> T -> R
Func.call3 : (S->T->R->Q) -> S -> T -> R -> Q
Func.call4 : (S->T->R->Q->P) -> S -> T -> R -> Q -> P

Func.callT2 : (S->T->R) -> T2<S,T> -> R
Func.callT3 : (S->T->R->P) -> T3<S,T,R> -> P
Func.callT4 : (S->T->R->P->Q) -> T4<S,T,R,P> -> Q
```

<a name="CoalescePrint"/>
## CoalescePrint

Replacement for trace for neko/cpp targets. Uses ANSI escape codes to 'delete' lines in stdout and collect repeat traces into groups (up to a limited look-back)

```cs
haxe.Log.trace = CoalescePrint.log;
trace("Hello");
trace("Hi there!");
trace("Hi there!");
trace("Hello");
trace("Hi there!");
trace("Hi there!");
trace("lol");
```

results in (when console supports ANSI codes):
```cs
[   Hello
    [   Hi there!   ]*2  ]*2
lol    
```

with braces in red, and repeat counts in green.

<a name="Shack"/>
## Shack

Uber build macro magic for stack (local-var) allocated vector, matrix and symmetric matrix types and operations.

To use this, you should add ```implements goodies.Shack``` to your class.

These types exist only at compile time, and are replaced with suitable (compilable) Haxe code using only local variables/parameters. As implied, these types can be assigned for member variables, local variables and function arguments (but not returns!)

#### Types
```cs
    VN (eg V2, V3) Vector (column vector), uses N variables
                  [ v0 ]
       storage =  [ v1 ]
                  [ v2 ]
                          
    SN (eg S2, S3) Symmetric Matrix (row=col=N), uses N(N+1)/2 variables
                 [ s0, s1, s2 ]
       storage = [ s1, s3, s4 ] (noticing duplicates)
                 [ s2, s4, s5 ]
       
    MNxM (eg M2x3) Matrix (row=N, col=M) uses NM variables
                 [ m0 m1 m2 ]
       storage = [ m3 m4 m5 ]
                 [ m6 m7 m8 ]
                 
    Scalar, used by Shack internals only, may appear in error messages.    
```

#### Constructors
```cs
    VN()       : VN; Zero-Vector
    VN(x)      : VN; Constant-Vector
    VN(x,y...) : VN; Vector with given values
    
    SN()        : SN; Identitity-Matrix
    SN([all=]x) : SN; Constant-Matrix
    SN(diag=x)  : SN; Matrix with constant diagonal
    SN(x,y...)  : SN; Matrix with given values
    
    MNxM() : MNxM; .. etc (as for Symmetric Matrix)
```

#### Slices

Slices are defined by numerical ranges of the type:
```cs
    i;      single index
    i...j;  range of indices
    all;    all valid indices
```

Ranges are used to select slices from vectors/matrices as follows:
```cs
   v(range) : Scalar (for single index), or Vector or suitable dimension otherwise.
   s(range) : Select a range from matrix diagonal, type as per Vector slice
   m(range) : ""
   
   s(range,range) : Select sub-matrix, Scalar for single element,
                                       Symmetric Matrix of suitable dimension if appropriate
                                       Matrix otherwise of suitable dimensions.

   m(range,range) : Select sub-matrix, Scalar for single element, otherwise suitably sized Matrix.
```

#### Operators

Shack supports arithmetic Float operators for the stack-allocated types, these operate on a component-wise basis and dimensions of operands must be compatible*. eg: an S3 may be multiplied component-wise with an M3x3 happily. Furthermore for assignments the RHS must be of the same dimension, and no greater in density. Eg, we can assign an S3 to an M3x3, but not an M3x3 to an S3.

(*) In the case of component wise operations with a Scalar type, the Scalar will be promoted to the larger type using the default constructor, with the effect that component-wise addition of a Vector with a Scalar, will have that Scalar added to all entries of the Vector.

In all cases, extra variables may be assigned and used to hold results of intermediate values to ensure any side-effects are not compounded, and that unecessary re-computation is avoided.

#### Top-Level functions

Shack defines top-level (unqualified) functions that may be used when types unify for Shack objects.

As with operators, Scalars may be promoted so that ```lerp(V2(1,2), 3, 5) == lerp(V2(1,2), V2(3), V2(5))``` etc.

```cs
    string(A, tab="") : String; Convert Shack object to a (possibly multiline) string format
    
    lerp(x:A, y:A, t:A) : A; Linearly interpolate, component wise returning x*(1-t) + y*t
    
    dot(VN, VN) : Scalar; Scalar dot-product of two vectors
    lsq(VN) : Scalar; Squared magnitude of a vector
    length(VN) : Scalar; Magnitude of vector
    unit(VN) : VN; Normalisation of vector, input is not modified.
    perp(V2) : V2; 2D right-perpendicular vector, input is not modified.
    
    cross(Scalar, V2) : V2; scalar multiple of right-perpendicular
    cross(V2, Scalar) : V2; scalar multiple of left-perpendicular
    cross(V2, V2) : Scalar; 2D perp-dot product
    cross(V3, V3) : V3;     3D cross product
    
    mul(A, B) : C; generalised true multiplication, valid as long as dimensions are compatible
                     will return a lesser-type (eg: mul(M2x2,V2):V2) when possible.
                     special case: Scalars are not promoted (Will be treat as 1x1 matrix only)
    transpose(A) : A; generalised transpose, will return a lesser-type (eg: transpose(M1x2):V2) when possible.
    determinant(S2|S3|M2x2|M3x3) : Scalar; matrix determinant (Restricted usage)
    invert(S2|S3|M2x2|M3x3) : Int; in-place inversion, return Int denotes matrix ranks there were degenerate
                                   and ignored. eg: if a 3x3 matrix has a zero middle row+col, it will be ignored
                                   and matrix inverted as though middle row+col did not exist. Return integer would
                                   in this case have the 1th bit set (return value = 2)
    solve(Scalar|S2|S3, Scalar|V2|V3) : Void;  in-place linear-equation solving, as with inversion null-ranks will
                                               be ignored.
```

#### Type-Inference shortcomings.

Shack will largely infer types at the basic level, but it is restricted to the current Type being built in the macro system.

If you have multiple types all using Shack that need to interopt, then you will have to tell Shack what the 'Type' of the object is (V2/S3 etc) as the default assigned type in Shack is Scalar.

This can be done in two different ways, the first is that each Type has a corresponding top-level function (in lower-case) which acts similar to Haxe 3 ECheckType syntax for explict type casting, this may also be used to force another type in a Shack expression.
```cs
    var vel = V3(1,2,3);
    trace(string(vel)); // 1 2 3 
    trace(string(v2(vel))); // 1 2, type of vel in expression was coerced to V2
```

In this way, can indicate to Shack what the type of an unreachable definition is, eg: from another object.

The other way, is to add a scoped type-declaration for a foreign identifier/field access
```cs
    @tag(V2) v.p; // declare in local-scope, that 'v.p' is a V2 object
```


<a name="Fixed16"/>
### Fixed16

Fixed16 provides 16.16 fixed point numbers which can largely be used as a (CAREFUL!) drop-in replacement for Float.
All operators are defined, and work cross-platform.

As with all abstract types, the only true limitation, is that if using Fixed16 as a type-perameter, the relevant code will not be able to correctly use the defined operator overloads.
