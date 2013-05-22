import goodies.Maybe;
import goodies.Lazy;
import goodies.FFT;

class Main implements MaybeEnv implements LazyEnv {
    @:lazyVar static var x:Maybe<Array<Int>> = [0,1,2];
    @:lazyVar static var y:String;

    static function main() {
        trace(x);
        trace(x = [1,2,3]);
        trace(x = null);
/*
        var N = 512;
        var x = new haxe.ds.Vector<Float>((N)*2);
        var y = new haxe.ds.Vector<Float>((N)*2);
        for (i in 0...(N)) y[i*2] = -(y[i*2+1] = i);
        trce(y);
        var t0 = flash.Lib.getTimer();
        for (i in 0...10) {
            FFT.DFT(x, y);
            FFT.IDFT(x, y);
        }
        var t1 = flash.Lib.getTimer();
        trce(x);
        trce(y);
        trace((t1-t0)/20);
        trace(N);*/
    }
}
