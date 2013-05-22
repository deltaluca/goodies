package goodies;

class FFT {
    static function fft(
        N:Int,
        x:haxe.ds.Vector<Float>, w:haxe.ds.Vector<Float>, oX:Int, dX:Int,
        y:haxe.ds.Vector<Float>, oY:Int, dY:Int,
        Wp:haxe.ds.Vector<Float>, dWp:Int
    ) {
        switch (N) {
        case 1:
            x[oX]   = y[oY];
            x[oX+1] = y[oY+1];
        case 2:
            var odY = oY+dY;
            x[oX]      = y[oY]   + y[odY];
            x[oX+1]    = y[oY+1] + y[odY+1];
            x[oX+dX]   = y[oY]   - y[odY];
            x[oX+dX+1] = y[oY+1] - y[odY+1];
        default:
            var N2 = N>>1;
            var NX = N2*dX;
            var oNX = oX+NX;
            fft(N2, w, x, oX,  dX, y, oY,    dY<<1, Wp, dWp<<1);
            fft(N2, w, x, oNX, dX, y, oY+dY, dY<<1, Wp, dWp<<1);
            x[oX]    = w[oX]   + w[oNX];
            x[oX+1]  = w[oX+1] + w[oNX+1];
            x[oNX]   = w[oX]   - w[oNX];
            x[oNX+1] = w[oX+1] - w[oNX+1];
            var i = oX+dX;
            var iw = dWp;
            while (i < oNX) {
                var iNX = i+NX;
                var re = Wp[iw]*w[iNX]   - Wp[iw+1]*w[iNX+1];
                var im = Wp[iw]*w[iNX+1] + Wp[iw+1]*w[iNX];
                x[i]     = w[i]   + re;
                x[i+1]   = w[i+1] + im;
                x[iNX]   = w[i]   - re;
                x[iNX+1] = w[i+1] - im;
                iw += dWp;
                i += dX;
            }
        }
    }

    public static function DFT(x:haxe.ds.Vector<Float>, y:haxe.ds.Vector<Float>, skip:Int=2, oX:Int=0, oY:Int=0) {
        var N = Std.int((y.length - oX)/skip);
        var Wp = new haxe.ds.Vector<Float>(N*2);
        var re = Math.cos(Math.PI*2/N);
        var im = Math.sin(Math.PI*2/N);
        var cx = 1.0;
        var cy = 0.0;
        for (i in 0...N) {
            var i2 = i<<1;
            Wp[i2]   = cx;
            Wp[i2+1] = cy;
            var x2 = cx*re - cy*im;
            cy = cx*im + cy*re;
            cx = x2;
        }
        fft(
            N,
            x, new haxe.ds.Vector<Float>(x.length), oX, skip,
            y, oY, skip,
            Wp, 2
        );
    }

    public static function IDFT(x:haxe.ds.Vector<Float>, y:haxe.ds.Vector<Float>, skip:Int=2, oX:Int=0, oY:Int=0) {
        var N = Std.int((x.length - oX)/skip);
        var Wp = new haxe.ds.Vector<Float>(N*2);
        var re = Math.cos(Math.PI*2/N);
        var im = -Math.sin(Math.PI*2/N);
        var cx = 1.0;
        var cy = 0.0;
        for (i in 0...N) {
            var i2 = i<<1;
            Wp[i2]   = cx;
            Wp[i2+1] = cy;
            var x2 = cx*re - cy*im;
            cy = cx*im + cy*re;
            cx = x2;
        }
        fft(
            N,
            y, new haxe.ds.Vector<Float>(x.length), oY, skip,
            x, oX, skip,
            Wp, 2
        );
        var s = 1/N;
        for (i in 0...(N*2)) y[i] *= s;
    }
}
