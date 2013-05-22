goodies
=======

Collection of little Haxe goodies I don't yet have an official place for.

##### Table of Contents  
[FFT](#FFT)  
[Assert](#Assert)  
[MacroUtils](#MacroUtils)  
[Maybe](#Maybe)  
[Lazy](#Lazy)  
[Builder](#Builder)

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

This can be combined with ```Maybe```, though I've no idea how it'd work together with ```@:lazyVar```!

Currently there's one other mutator for ```@:builder``` which is:
```cs
@:builder(ret=T) var ...;
```

which can be combined with ```react``` as two arguments. The ```ret``` mutator changes the Type of the property getter from the parent Type to the one given as argument (Can be useful sometimes).
