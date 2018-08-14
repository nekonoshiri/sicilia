import std.algorithm : reduce;
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
  enum η_carbamate = 0.9; // for (1)
  enum P_f = 16.12 * atm; // for (2)
  enum t_f = celsius!real(120);
  enum CR = 0.5;
  enum F = 1.0;
  enum size_t trial = 1000;
  enum size_t numofInitPoints = 500;

  immutable X1 = reactor(feed_NH3, feed_CO2, feed_H2O, feed_NH2CONH2, t_s);

  immutable Domain!real[2] dom1 = [Domain!real(0, X1[0].getValue), Domain!real(0, X1[2].getValue)];
  immutable calc_X3_for_de_1 = delegate (in real[2] x) {  //x[0]: X3_NH3_assume, x[1]: X3_H2O_assume
    immutable X!real X3_assume = [Mol_s!real(x[0]), X1[1] * (1 - η_carbamate), Mol_s!real(x[1]), X1[3]];
    immutable X3_calc = calc_X3(X1, X3_assume, η_carbamate, t_f);
    immutable ΔX3_NH3 = (X3_calc[0] - X3_assume[0]).getValue.abs;
    immutable ΔX3_H2O = (X3_calc[2] - X3_assume[2]).getValue.abs;
    return ΔX3_NH3 + ΔX3_H2O;
  };
  immutable res1 = differentialEvolution!(calc_X3_for_de_1, 2, MinMax.min)(CR, F, trial, dom1, numofInitPoints);
  immutable X3_1 = [Mol_s!real(res1.x[0]), X1[1] * (1 - η_carbamate), Mol_s!real(res1.x[1]), X1[3]];
  X!real X2_1;
  foreach(i, ref e; X2_1) e = X1[i] - X3_1[i];

  immutable Domain!real[3] dom2 = [Domain!real(0, X1[0].getValue), Domain!real(0, X1[1].getValue), Domain!real(0, X1[2].getValue)];
  immutable calc_X3_for_de_2 = delegate (in real[3] x) {  //x[0]: X3_NH3_assume, x[1]: X3_carbamate_assume, x[2] : X3_H2O_assume
    immutable X!real X3_assume = [Mol_s!real(x[0]), Mol_s!real(x[1]), Mol_s!real(x[2]), X1[3]];
    immutable X3_calc = calc_X3(X1, X3_assume, P_f, t_f);
    real[2] ΔX3;
    foreach(i, ref e; ΔX3) e = (X3_calc[i] - X3_assume[i]).getValue.abs;
    return reduce!((a, b) => a + b)(0., ΔX3);
  };
  immutable res2 = differentialEvolution!(calc_X3_for_de_2, 3, MinMax.min)(CR, F, trial, dom2, numofInitPoints);
  immutable X3_2 = [Mol_s!real(res2.x[0]), Mol_s!real(res2.x[1]), Mol_s!real(res2.x[2]), X1[3]];
  X!real X2_2;
  foreach(i, ref e; X2_2) e = X1[i] - X3_2[i];

  writefln("(1)");
  writefln("X1 = (%s, %s, %s, %s)", X1[0], X1[1], X1[2], X1[3]);
  writefln("X2 = (%s, %s, %s, %s)", X2_1[0], X2_1[1], X2_1[2], X2_1[3]);
  writefln("X3 = (%s, %s, %s, %s)", X3_1[0], X3_1[1], X3_1[2], X3_1[3]);
  writefln("error of (X3_NH3 + X3_H2O) : %s", res1.f);

  writefln("\n(2)");
  writefln("X1 = (%s, %s, %s, %s)", X1[0], X1[1], X1[2], X1[3]);
  writefln("X2 = (%s, %s, %s, %s)", X2_2[0], X2_2[1], X2_2[2], X2_2[3]);
  writefln("X3 = (%s, %s, %s, %s)", X3_2[0], X3_2[1], X3_2[2], X3_2[3]);
  writefln("error of (X3_NH3 + X3_carbamate + X3_H2O) : %s", res2.f);
  return;
}

pure calc_X3(in X!real X1, in X!real X3_assume, in real η_carbamate, in Kelvin!real t_f){
  X!real X2, X3_calc;
  immutable Pascal!real[3] p0 = calc_p0(t_f);
  X2[1] = X1[1] * η_carbamate;
  X2[3] = 0 * mol / second;
  X3_calc[1] = X1[1] - X2[1];
  X3_calc[3] = X1[3];

  immutable real[3] x = [
    X3_assume[0] / (X3_assume[0] + X3_assume[1] + X3_assume[2]),
    X3_calc[1] / (X3_assume[0] + X3_assume[1] + X3_assume[2]),
    X3_assume[2] / (X3_assume[0] + X3_assume[1] + X3_assume[2])
  ];
  immutable real[3] γ = calc_γ(x);
  immutable P_f = γ[0] * x[0] * p0[0] + γ[1] * x[1] * p0[1] + γ[2] * x[2] * p0[2];
  immutable real[3] y = calc_y(x, γ, p0, P_f);

  X2[0] = X2[1] * y[0] / y[1];
  X3_calc[0] = X1[0] - X2[0];

  X2[2] = X2[1] * y[2] / y[1];
  X3_calc[2] = X1[2] - X2[2];
  return X3_calc;
}

pure calc_X3(in X!real X1, in X!real X3_assume, in Pascal!real P_f, in Kelvin!real t_f){
  X!real X2, X3_calc;
  immutable Pascal!real[3] p0 = calc_p0(t_f);
  X2[3] = 0 * mol / second;
  X3_calc[3] = X1[3];

  immutable real[3] x = [
    X3_assume[0] / (X3_assume[0] + X3_assume[1] + X3_assume[2]),
    X3_calc[1] / (X3_assume[0] + X3_assume[1] + X3_assume[2]),
    X3_assume[2] / (X3_assume[0] + X3_assume[1] + X3_assume[2])
  ];
  immutable real[3] γ = calc_γ(x);
  immutable real[3] y = calc_y(x, γ, p0, P_f);

  X2[0] = X1[0] - X3_assume[0];
  X3_calc[0] = X1[0] - X2[0];
  X2[1] = X2[0] * y[1] / y[0];
  X3_calc[1] = X1[1] - X2[1];
  X2[2] = X2[0] * y[2] / y[0];
  X3_calc[2] = X1[2] - X2[2];
  return X3_calc;
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

pure real[3] calc_y(in real[3] x, in real[3] γ, in Pascal!real[3] p0, in Pascal!real P_f){
  return [
    γ[0] * p0[0] * x[0] / P_f,
    γ[1] * p0[1] * x[1] / P_f,
    γ[2] * p0[2] * x[2] / P_f
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
