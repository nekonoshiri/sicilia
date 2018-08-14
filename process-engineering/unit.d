import std.conv : to;
import std.string : capitalize, format, toLower;

alias exn = int;

struct Unit(T, exn s = 0, exn m = 0, exn kg = 0, exn A = 0, exn K = 0, exn mol = 0, exn cd = 0){
  private T value = 1;

  this(in T value){
    this.value = value;
  }

  private static pure exnCheck(alias pred)() {
    return (pred(s) && pred(m) && pred(kg) && pred(A) && pred(K) && pred(mol) && pred(cd));
  }

  auto opUnary(string op, this U)() if(op == "+" || op == "-" || op == "++" || op == "--"){
    return U(mixin(op ~ "value"));
  }

  pure opBinary(string op, this U)(in U rhs) const if(op == "+" || op == "-"){
    return U(mixin("value" ~ op ~ "rhs.value"));
  }

  pure opBinary(string op, exn s1, exn m1, exn kg1, exn A1, exn K1, exn mol1, exn cd1)(in Unit!(T, s1, m1, kg1, A1, K1, mol1, cd1) rhs) const if(op == "*" || op == "/"){
    pure nothrow @nogc @safe add(in exn x, in exn y){ return x + y; }
    pure nothrow @nogc @safe sub(in exn x, in exn y){ return x - y; }
    static if(op == "*") alias f = add;
    else alias f = sub;
    return Unit!(T, f(s, s1), f(m, m1), f(kg, kg1), f(A, A1), f(K, K1), f(mol, mol1), f(cd, cd1))(mixin("value" ~ op ~ "rhs.value"));
  }

  pure opBinary(string op, this U)(in T rhs) const if(op == "*" || op == "/"){
    return U(mixin("value" ~ op ~ "rhs"));
  }

  pure opBinaryRight(string op)(in T lhs) const if(op == "*" || op == "/"){
    return Unit!(T, 0)(lhs).opBinary!op(this);
  }
  
  pure opCast(U1: Unit!(T1, s, m, kg, A, K, mol, cd), T1)() const{
    return U1(cast(T1) value);
  }
  
  auto opCast(T: string)() const{
    return to!string(this);
  }
  
  @property pure pow(int n)() const{
    return Unit!(T, s * n, m * n, kg * n, A * n, K * n, mol * n, cd * n)(value ^^ n);
  }

  @property pure nthroot(int n)() const if(n != 0 && exnCheck!(x => x % n == 0)){
    return Unit!(T, s / n, m / n, kg / n, A / n, K / n, mol / n, cd / n)(cast (T) (cast(real) value ^^ (1. / n)));
  }
  
  @property pure sqrt()() const if(exnCheck!(x => x % 2 == 0)){
    return this.nthroot!2;
  }
 
  @property pure cbrt()() const if(exnCheck!(x => x % 3 == 0)){
    return this.nthroot!3;
  }

  @property pure getValue() const{
    return this.value;
  }

  @property auto toString() const{
    pure f = function(in exn x, in string str){
      switch(x){
        case 0:  return ""; break;
        case 1:  return " " ~ str; break;
        default: return " " ~ str ~ "^" ~ to!string(x); break;
      }
    };
    return to!string(this.value) ~ " [" ~ f(s, "s") ~ f(m, "m") ~ f(kg, "kg")
      ~ f(A, "A") ~ f(K, "K") ~ f(mol, "mol") ~ f(cd, "cd") ~ " ]";
  }

  static if(exnCheck!((exn x) => x == 0)){
    alias getValue this;
  }
}


mixin template MKFunc(string FuncName, string FuncCont){
  mixin(format(
    "pure %s(T = real, exn n = 1)(in T x = 1) { return %s; }",
    FuncName, FuncCont));
  mixin(format(
    "pure %s(exn n, T = real)(in T x = 1) { return %s!(T, n)(x); }",
    FuncName, FuncName));
}


mixin template SIUnit(string UnitName, exn s = 0, exn m = 0, exn kg = 0, exn A = 0, exn K = 0, exn mol = 0, exn cd = 0){
  mixin(format(
    "alias %s(T, exn n = 1) = Unit!(T, %s*n, %s*n, %s*n, %s*n, %s*n, %s*n, %s*n);",
    capitalize(UnitName), s, m, kg, A, K, mol, cd));
  mixin(format(
    "alias %s(exn n, T) = Unit!(T, %s*n, %s*n, %s*n, %s*n, %s*n, %s*n, %s*n);",
    capitalize(UnitName), s, m, kg, A, K, mol, cd));
  mixin MKFunc!(toLower(UnitName), format("%s!(T, n)(x)", capitalize(UnitName)));
}


// SI base units
mixin SIUnit!("second",     1);
mixin SIUnit!("meter",      0,  1);
mixin SIUnit!("metre",      0,  1);
mixin SIUnit!("kilogram",   0,  0,  1);
mixin SIUnit!("ampere",     0,  0,  0,  1);
mixin SIUnit!("kelvin",     0,  0,  0,  0,  1);
mixin SIUnit!("mole",       0,  0,  0,  0,  0,  1);
mixin SIUnit!("mol",        0,  0,  0,  0,  0,  1);
mixin SIUnit!("candela",    0,  0,  0,  0,  0,  0,  1);

// SI derived units
mixin SIUnit!("hertz",     -1);
mixin SIUnit!("newton",    -2,  1,  1);
mixin SIUnit!("pascal",    -2, -1,  1);
mixin SIUnit!("joule",     -2,  2,  1);
mixin SIUnit!("watt",      -3,  2,  1);
mixin SIUnit!("coulomb",    1,  0,  0,  1);
mixin SIUnit!("volt",      -3,  2,  1, -1);
mixin SIUnit!("farad",      4, -2, -1,  2);
mixin SIUnit!("ohm",       -3,  2,  1, -2);
mixin SIUnit!("siemens",    3, -2, -1,  2);
mixin SIUnit!("weber",     -2,  2,  1, -1);
mixin SIUnit!("tesla",     -2,  0,  1, -1);
mixin SIUnit!("henry",     -2,  2,  1, -2);
pure celsius(T, exn n = 1)(in T x) { return kelvin!(T, n)(to!T(x + 273.15)); }
pure celsius(exn n, T)(in T x)     { return celsius!(T, n)(x); }
mixin SIUnit!("lumen",      0,  0,  0,  0,  0,  0,  1);
mixin SIUnit!("lux", 0,    -2,  0,  0,  0,  0,  1);
mixin SIUnit!("becquerel", -1);
mixin SIUnit!("gray",      -2,  2);
mixin SIUnit!("sievert",   -2,  2);
mixin SIUnit!("katal",     -1,  0,  0,  0,  0,  1);

// Non-SI units mentioned in the SI
mixin MKFunc!("minute",     "second!(T, n)(x * 60. ^^ n)");
mixin MKFunc!("hour",       "minute!(T, n)(x * 60. ^^ n)");
mixin MKFunc!("day",        "hour!(T, n)(x * 24. ^^ n)");
mixin MKFunc!("hectare",    "meter!(T, 2 * n)(x * 1e4 ^^ n)");
mixin MKFunc!("liter",      "meter!(T, 3 * n)(x / 1e3 ^^ n)");
mixin MKFunc!("litre",      "meter!(T, 3 * n)(x / 1e3 ^^ n)");
mixin MKFunc!("ton",        "kilogram!(T, n)(x * 1e3 ^^ n)");
mixin MKFunc!("tonne",      "kilogram!(T, n)(x * 1e3 ^^ n)");
mixin MKFunc!("bar",        "pascal!(T, n)(x * 1e5 ^^ n)");
mixin MKFunc!("mmHg",       "pascal!(T, n)(x * (101325. / 760.) ^^ n)");
mixin MKFunc!("torr",       "mmHg!(T, n)(x)");
mixin MKFunc!("angstrom",   "meter!(T, n)(x / 1e10 ^^ n)");
mixin MKFunc!("barn",       "meter!(T, n)(to!T(x * 1e-28 ^^ n))");
mixin MKFunc!("erg",        "joule!(T, n)(x / 1e7 ^^ n)");
mixin MKFunc!("dyne",       "newton!(T, n)(x / 1e5 ^^ n)");
mixin MKFunc!("poise",      "pascal!(T, n)(x / 1e1 ^^ n) * second!(T, n)");

// prefixed units
mixin MKFunc!("centimeter", "meter!(T, n)(x / 1e2 ^^ n)");
mixin MKFunc!("centimetre", "metre!(T, n)(x / 1e2 ^^ n)");
mixin MKFunc!("kilometer",  "meter!(T, n)(x * 1e3 ^^ n)");
mixin MKFunc!("kilometre",  "metre!(T, n)(x * 1e3 ^^ n)");
mixin MKFunc!("gram",       "kilogram!(T, n)(x / 1e3 ^^ n)");
mixin MKFunc!("kilojoule",  "joule!(T, n)(x * 1e3 ^^ n)");
mixin MKFunc!("centipoise", "poise!(T, n)(x / 1e2 ^^ n)");

// other
mixin MKFunc!("watt_hour",  "joule!(T, n)(x * 3600. ^^ n)");
mixin MKFunc!("kilowatt_hour",  "joule!(T, n)(x * 3600000. ^^ n)");
mixin MKFunc!("atm", "pascal!(T, n)(x * 101325. ^^ n)");
mixin MKFunc!("kilogram_force", "newton!(T, n)(x * 9.80665 ^^ n)");
mixin MKFunc!("kilogram_weight", "kilogram_force!(T, n)(x)");