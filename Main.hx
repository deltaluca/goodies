import goodies.Maybe;
import goodies.Lazy;
import goodies.FFT;
import goodies.Builder;
using goodies.Assert;

class Nuke implements MaybeEnv implements LazyEnv implements Builder {
    @:lazyVar @:builder(react=function (metric) {
        Assert.assert(metric[0] != 1);
    }) var metric:Array<Int> = [0,1,2];

    public function new() {}
}

class Main implements MaybeEnv implements LazyEnv {
    static function main() {
        var n = new Nuke();
        trace(n.getMetric());
        n.metric([1,3,4]);
        trace(n.getMetric());
        n.metric(null);
        trace(n.getMetric());
    }
}
