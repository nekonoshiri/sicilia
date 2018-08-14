import std.math : abs;
import std.stdio : writefln;
import std.typecons : Tuple;

import unit;
import de;

alias Mol_s(T) = typeof (Mol!(T)(1) / Second!(T)(1));
alias X(T) = Mol_s!T[4];

void main(){
  enum feed_NH3 = 5.0 * mol / second;
  enum feed_CO2 = 1.0 * mol / second;
  enum feed_H2O = 0.6 * mol / second;
  enum feed_NH2CONH2 = 0.0 * mol / second;
  enum t_s = celsius!real(200);
  enum t_f = celsius!real(120);
  immutable X1 = reactor(feed_NH3, feed_CO2, feed_H2O, feed_NH2CONH2, t_s);

  { // (1)
    enum η_carbamate = 0.9;
    immutable η = calc_η(X1, 0.9, t_f);
    X!real X2, X3;
    foreach(i, η_i; η) X2[i] = X1[i] * η_i;
    X2[3] = 0 * mol / second;
    foreach(i, _; X1) X3[i] = X1[i] - X2[i];

    writefln("(1)");
    writefln("X1 = (%s, %s, %s, %s)", X1[0], X1[1], X1[2], X1[3]);
    writefln("X2 = (%s, %s, %s, %s)", X2[0], X2[1], X2[2], X2[3]);
    writefln("X3 = (%s, %s, %s, %s)", X3[0], X3[1], X3[2], X3[3]);
  }
  { // (2)
    enum P_f = 16.12 * atm;
    enum CR = 0.5;
    enum F = 1.0;
    enum size_t trial = 100;
    enum size_t numofInitPoints = 100;
    enum Domain!real[1] dom = [Domain!real(0, 1)];
    immutable objfunc = delegate(in real[1] x){
      return (calc_P(X1, x[0], t_f) - P_f).getValue.abs;
    };
    immutable res = differentialEvolution!(objfunc, 1, MinMax.min)(CR, F, trial, dom, numofInitPoints);
    immutable η_carbamate = res.x[0];
    immutable η = calc_η(X1, η_carbamate, t_f);
    X!real X2, X3;
    foreach(i, η_i; η) X2[i] = X1[i] * η_i;
    X2[3] = 0 * mol / second;
    foreach(i, _; X1) X3[i] = X1[i] - X2[i];

    writefln("(2)");
    writefln("X1 = (%s, %s, %s, %s)", X1[0], X1[1], X1[2], X1[3]);
    writefln("X2 = (%s, %s, %s, %s)", X2[0], X2[1], X2[2], X2[3]);
    writefln("X3 = (%s, %s, %s, %s)", X3[0], X3[1], X3[2], X3[3]);
  }
}

auto calc_P(in X!real X1, in real η_carbamate, in Kelvin!real t_f){
  immutable η = calc_η(X1, η_carbamate, t_f);
  immutable Pascal!real[3] p0 = calc_p0(t_f);
  X!real X2, X3;

  foreach(i, η_i; η) X2[i] = X1[i] * η_i;
  X2[3] = 0 * mol / second;
  foreach(i, _; X1) X3[i] = X1[i] - X2[i];
  immutable real[3] x = [
    X3[0] / (X3[0] + X3[1] + X3[2]),
    X3[1] / (X3[0] + X3[1] + X3[2]),
    X3[2] / (X3[0] + X3[1] + X3[2])
  ];
  immutable real[3] γ = calc_γ(x);
  immutable P_f = γ[0] * x[0] * p0[0] + γ[1] * x[1] * p0[1] + γ[2] * x[2] * p0[2];

  return P_f;
}

auto real[3] calc_η(in X!real X1, in real η_carbamate, in Kelvin!real t_f){
  immutable real[3] η_init = [0.5, η_carbamate, 0.5];
  immutable calcfunc = delegate(in real[3] η_assume){
    return calc_next_η(X1, η_assume, t_f);
  };
  immutable termpred = function(in real[3] η_assume, in real[3] η_calc){
    return abs(η_assume[0] - η_calc[0]) + abs(η_assume[2] - η_calc[2]) < 1e-6;
  };
  immutable res = stairing!(calcfunc, η => η, termpred, real[3], real[3])(η_init, false);
  return res.y;
}

pure real[3] calc_next_η(in X!real X1, in real[3] η_assume, in Kelvin!real t_f){
  immutable Pascal!real[3] p0 = calc_p0(t_f);
  X!real X2, X3;
  real[3] η_calc;

  foreach(i, η_i; η_assume) X2[i] = X1[i] * η_i;
  X2[3] = 0 * mol / second;
  foreach(i, _; X1) X3[i] = X1[i] - X2[i];

  immutable real[3] x = [
    X3[0] / (X3[0] + X3[1] + X3[2]),
    X3[1] / (X3[0] + X3[1] + X3[2]),
    X3[2] / (X3[0] + X3[1] + X3[2])
  ];
  immutable real[3] γ = calc_γ(x);
  immutable Pascal!real[3] γp = [
    γ[0] * p0[0],
    γ[1] * p0[1],
    γ[2] * p0[2]
  ];
  η_calc[0] = 1 / ((γp[1] / γp[0]) * (1 - η_assume[1]) / η_assume[1] + 1);
  η_calc[1] = η_assume[1];
  η_calc[2] = 1 / ((γp[1] / γp[2]) * (1 - η_assume[1]) / η_assume[1] + 1);
  return η_calc;
}

pure Pascal!real[3] calc_p0(in Kelvin!real t_f){
  immutable t_f_celsius = t_f.getValue - 273.15;
  immutable a = t_f_celsius > 122.0 ? 8.642 : 6.579;
  immutable b = t_f_celsius > 122.0 ? 2640.0 : 1914.3;
  return [
    10 ^^ (5.824316 - 1930.07 / (t_f_celsius + 378.6)) * atm,
    10 ^^ (a - b / (t_f_celsius + 230.0)) * kilogram_weight / centimeter!2,
    10 ^^ (5.08599 - 1668.21 / (t_f_celsius + 228.0)) * atm
  ];
}

pure real[3] calc_γ(in real[3] x){
  return [
    10 ^^ ((-0.796 * x[2] ^^ 2 - 2.155 * x[1] * x[2]) / (x[0] + 1.220 * x[1] + 1.038 * x[2]) ^^ 2),
    10 ^^ ((0.820 * x[2] ^^ 2 + 1.237 * x[0] * x[2]) / (0.819 * x[0] + x[1] + 0.835 * x[2]) ^^ 2),
    10 ^^ ((-0.755 * x[0] ^^ 2 + 1.411 * x[1] ^^ 2 - 0.236 * x[0] * x[1]) / (0.938 * x[0] + 1.198 * x[1] + x[2]) ^^ 2)
  ];
}

pure X!real reactor(in X!real X0, in Kelvin!real t_s){
  immutable a = (X0[0] + 2 * X0[1]) / X0[1];
  immutable b = X0[2] / X0[1];
  immutable t_s_celsius = t_s.getValue - 273.15;
  immutable ζ = 0.2616 * a - 0.0194 * a ^^ 2 + 0.0382 * a * b - 0.1160 * b
    - (0.02732 * a + 0.1030 * b - 1.640) * (t_s_celsius / 100) - 0.1394 * (t_s_celsius / 100) ^^ 3 - 1.869;
  return [X0[0], X0[1] * (1 - ζ), X0[1] * ζ + X0[2], X0[1] * ζ + X0[3]];
}

pure reactor(in Mol_s!real NH3, in Mol_s!real CO2, in Mol_s!real H2O, in Mol_s!real NH2CONH2, in Kelvin!real t_s){
  return reactor([NH3 - 2 * CO2, CO2, H2O, NH2CONH2], t_s);
}

unittest {
  enum a = 3.8;
  enum R = 2.0;
  enum x_D = 0.2;
  enum Max_trial = 100;
  auto resultsX = new real[5];
  auto resultsY = new real[5];
  enum eq_line = function (real x) { return a * x / (1 + (a - 1) * x); };
  enum op_line = function (real y) { return (1 + 1 / R) * y - 1 / R; };
  enum termpred = function (real x, real y) { return abs (x - 1.0) < 0.01 ; };
  immutable result = stairing!(eq_line, op_line, termpred, real, real)(x_D, false, Max_trial, resultsX, resultsY);

  assert(abs (result.x - 1.0) < 0.01);
  foreach(i; 1..resultsX.length - 1){
    assert(resultsY[i] == eq_line(resultsX[i]));
    assert(resultsX[i] == op_line(resultsY[i - 1]));
  }
}

Tuple!(Tx, "x", Ty, "y")
stairing(alias calcfunc, alias updatefunc, alias termpred, Tx, Ty)
(in Tx initval, in bool print = false, in int trial = -1, Tx[] resultsX = null, Ty[] resultsY = null){
  Tx x = initval;
  Ty y = calcfunc(x);
  int i = 0;
  while(true){
    if(print) writefln("%-5s: %s -> %s", i, x, y);
    if(resultsX != null && resultsX.length > i) resultsX[i] = x;
    if(resultsY != null && resultsY.length > i) resultsY[i] = y;
    if(termpred(x, y) || (++i >= trial && trial >= 0)) break;
    x = updatefunc(y);
    y = calcfunc(x);
  }
  return typeof(return)(x, y);
}
