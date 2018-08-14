import std.math : abs, exp, isFinite, sqrt, PI;
import std.stdio : writefln;
import std.typecons : tuple, Tuple;

import unit;
import de;

enum Gp = 63 * ton / day;
enum ζ_total = 0.60;
enum ρ = 850 * kilogram / meter!3;
enum α = 1.3L;
enum ΔH = 72.8 * kilojoule / mole;
enum T1 = celsius!real(50);
enum λ = 0.128 * watt / (meter * kelvin);
enum Cp = 1.68 * joule / (gram * kelvin);
enum M_B = 54 * gram / mole;
enum k1 = 0.3 / hour;

enum ΔT2 = 10 * kelvin;
enum Cp_w = 4.18 * joule / (gram * kelvin);
enum Depreciaiton_Interval = 330 * 5 * day;
enum L_T = 363 * joule / gram;
enum L_W = 2.23 * kilojoule / gram;

enum TRIAL_MAX = 100;

class InputException : Exception{
  this(string s) { super(s); }
}

class TimeOutException : Exception{
  this(string s) { super(s); }
}

class DivergenceException : Exception{
  this(string s) { super(s); }
}


void p(int N)(){
  immutable Domain!real[2] dom = [Domain!real(0, 1), Domain!real(-50, 0)];
  immutable result = differentialEvolution!(calc_cost!N, 2, MinMax.min)(0.5, 1.0, 300, dom, 100);
  writefln("%12s%12.4e%12.4e%12.4e", N, result.x[0], result.x[1], result.f);
}

void main(){
  writefln("%12s%12s%12s%12s", "N", "gamma0", "T2", "cost");
  p!1; p!2; p!3; p!4; p!5; p!6; p!7; p!8; p!9; p!10; p!11; p!12; p!13; p!14; p!15;
}

auto calc_cost(int N)(in real[2] x){
  immutable γ0 = x[0];
  immutable T2 = x[1].celsius!real;
  immutable ε = 1e-6;

  immutable V = volume(N, γ0);
  immutable D = diameter(V);

  Watt!real P_i, Qp_i_reac, P_sum = 0 * watt;
  Second!(-1, real) n_i;
  Second!(-1, real) cost_Water_i, cost_Water_sum = 0 / second;
  try{
    foreach(i; 1..N + 1){
      immutable res = calcP(i, γ0, T2, ε, D, TRIAL_MAX);
      P_i = res.P;
      n_i = res.n;
      Qp_i_reac = res.Qp;
      P_sum = P_sum + P_i;
      cost_Water_i = cost_coolingwater(P_i, Qp_i_reac, T2);
      cost_Water_sum = cost_Water_sum + cost_Water_i;
    }
  }catch(Exception e){
    return real.infinity;
  }
  immutable cost_Electricity = cost_electricity(P_sum);
  immutable cost_Steam = cost_steam(γ0);
  immutable cost_Vessel = cost_vessel(N, V, Depreciaiton_Interval);
  immutable cost_Total = cost_Water_sum + cost_Electricity + cost_Steam + cost_Vessel;
  return cost_Total.getValue;
}

Tuple!(Watt!real, "P", Watt!real, "Qp", Second!(-1, real), "n")
calcP(in int i, in real γ0, in Kelvin!real T2, in real ε, in Meter!real D, in int trial){
  auto P_assume = 0 * watt;
  Watt!real P_calc;
  Second!(-1, real) n_calc;
  Watt!real Qp_reac;
  int j = 0;
  while(++j){
    immutable res = power(i, γ0, T2, D, P_assume);
    P_calc = res.P;
    n_calc = res.n;
    Qp_reac = res.Qp;
    if((P_calc - P_assume).getValue.abs < (ε * P_assume).getValue) break;
    if(j >= trial) throw new TimeOutException("");
    if(!P_calc.getValue.isFinite) throw new DivergenceException("");
    P_assume = P_calc;
  }
  return typeof(return)(P_calc, Qp_reac, n_calc);
}

pure volume(in int N, in real γ0){
  immutable v = Gp / (ρ * γ0 * ζ_total);
  immutable τ = ((1 - ζ_total)^^(-1./N) - 1) / k1;
  immutable V = v * τ;
  return V;
}

pure diameter(in Meter!(3, real) V){
  return (4 * V / (α * PI)).cbrt;
}

pure Tuple!(Watt!real, "P", Watt!real, "Qp", Second!(-1, real), "n")
power(in int i, in real γ0, in Kelvin!real T2, in Meter!real D, in Watt!real P_i_assume){
  immutable F_B_0 = Gp / (M_B * ζ_total);
  immutable v = Gp / (ρ * γ0 * ζ_total);
  immutable d = D / 2;
  immutable H = D * α;
  immutable A = H * D * PI; 
  immutable τ = PI * d * d * H / v;

  immutable ζ_i = conversion(k1, τ, i);
  immutable ζ_i_minus_1 = conversion(k1, τ, i - 1);
  immutable Qp_i_reac = ΔH * F_B_0 * (ζ_i - ζ_i_minus_1);
  immutable h_i = (Qp_i_reac + P_i_assume) / (A * (T1 - T2));
  immutable µ_i = viscosity(50.0, ζ_i, γ0);

  immutable Nu_i = h_i * D / λ;
  immutable Pr_i = µ_i * Cp / λ;
  immutable Re_i = (Nu_i / (0.5 * Pr_i.cbrt)) ^^ (1.5);
  immutable n_i = Re_i * µ_i / (d * d * ρ);
  immutable Np_i = 14.6 * Re_i ^^ (-0.28);
  immutable P_i_calc = ρ * n_i.pow!3 * d.pow!5 * Np_i;
  return typeof(return)(P_i_calc, Qp_i_reac, n_i);
}

pure conversion(in Second!(-1, real) k1, in Second!real τ, in int i){
  return 1 - (1 + (k1 * τ)) ^^ (-i);
}

pure viscosity(in real ML, in real ζi, in real γ0){
  return 1.0 * ML ^^ 1.7 * ζi ^^ 2.5 * exp(21. * γ0) * 1e-2 * poise;
}

pure cost_electricity(in Watt!real P){
  return P * 10 / kilowatt_hour;
}

pure cost_steam(in real γ0){
  immutable G_B_0 = Gp / ζ_total;
  immutable G_T = G_B_0 * (1.0 / γ0 - 1.0);
  immutable G_W = L_T * G_T / (L_W * 0.30);
  return G_W * 3000 / ton;
}

pure cost_vessel(in int N, in Meter!(3, real) V, in Second!real depIntval){
  immutable cost_V = 40_000_000 + 5.1e6 * V.getValue.sqrt;
  return N * cost_V / depIntval;
}

pure cost_coolingwater(in Watt!real P, in Watt!real Qp_reac, in Kelvin!real T2){
  immutable G_W = (Qp_reac + P) / (Cp_w * ΔT2);
  return G_W * cost_water(T2 - ΔT2 / 2);
}

pure cost_water(in Kelvin!real T){
  immutable cost_W = 0.039359351988218 * T.getValue ^^ 2 - 24.2991421207659 * T.getValue + 3765.45956010678;
  return cost_W / ton; // tmp
}