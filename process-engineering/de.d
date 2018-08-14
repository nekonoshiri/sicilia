import std.algorithm : reduce;
import std.math : isFinite, sin;
import std.random : uniform, uniform01, randomSample;
import std.typecons : Tuple;

unittest {
  immutable f1 = function (in real[1] x) { return sin(x[0] ^^ 2) / x[0]; };
  immutable Domain!real[1] dom1 = [Domain!real(-100, 100)];
  immutable result1 = differentialEvolution!(f1, 1, MinMax.max)(0.5, 1.0, 300, dom1, 100);
  assert(1.0 < result1.x[0] && result1.x[0] < 1.1);
  assert(0.8 < result1.f && result1.f < 0.9);
  
  immutable f2 = function (in real[2] x) { return (x[0] - 1) ^^ 2 + 100 * (x[0]^^3 - x[1]) ^^ 2; };
  immutable Domain!real[2] dom2 = [Domain!real(-5, 5), Domain!real(-5, 5)];
  immutable result2 = differentialEvolution!(f2, 2, MinMax.min)(0.5, 1.0, 300, dom2, 100);
  
  assert(0.9 < result2.x[0] && result2.x[0] < 1.1);
  assert(0.9 < result2.x[1] && result2.x[1] < 1.1);
  assert(result2.f < 0.1);
}

enum MinMax { Min, Max }

alias Domain(T) = Tuple!(T, "min", T, "max");
alias XF(size_t dim, T) = Tuple!(T[dim], "x", T, "f");

auto differentialEvolution(alias func, size_t dim, MinMax minmax)
  (in real CR, in real F, in size_t trial, in Domain!real[dim] dom, in size_t numofInitPoints){
  real[dim][] initPoints;
  makeInitPoints!dim(numofInitPoints, dom, initPoints);
  return differentialEvolution!(func, dim, minmax)(CR, F, trial, dom, initPoints);
}

auto differentialEvolution(alias func, size_t dim, MinMax minmax)
  (in real CR, in real F, in size_t trial, in Domain!(real)[dim] dom, in real[dim][] initPoints){
  // 0 <= p <= 1
  // 0 <= s <= 2
  // 0 <= initPoints.length <= ?
  immutable fit = function (real a, real b) { return (minmax == MinMax.Min) ? a < b : a > b; };

  auto ps = new XF!(dim, real)[initPoints.length];
  foreach(i, ref p; ps){
    p.x = initPoints[i];
    p.f = func(p.x);
  }
  
  real[dim][3] x_s;
  XF!(dim, real) mut;
  int R;
  size_t k;
  
  foreach(_; 0..trial){
    foreach(j, ref pj; ps){
      k = 0;
      foreach(e; randomSample(ps[0..j] ~ ps[j + 1..$], 3)){
        x_s[k++] = e.x;
      }
      R = uniform(0, dim);
      foreach(int i, ref x; mut.x){
        if(uniform01() < CR || i == R){
          x = x_s[2][i] + F * (x_s[0][i] - x_s[1][i]);
          if(x < dom[i].min || dom[i].max < x)
            x = pj.x[i];
        }else{
          x = pj.x[i];
        }
      }
      mut.f = func(mut.x);
      if(fit(mut.f, pj.f) && isFinite(mut.f)){
        pj = mut;
      }
    }
  }
  return reduce!((a, b) => a.f < b.f ? a : b)(ps[0], ps);
}

void makeInitPoints(size_t dim)(in size_t numofInitPoints, in Domain!real[dim] dom, out real[dim][] initPoints){
  initPoints.length = numofInitPoints;
  foreach(ref p; initPoints)
    foreach(i, ref x; p)
      x = uniform!"()"(dom[i].min, dom[i].max);
  return;
}
