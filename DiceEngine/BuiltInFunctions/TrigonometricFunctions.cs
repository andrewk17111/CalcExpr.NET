﻿using DiceEngine.Attributes;
using DiceEngine.Expressions;
using DiceEngine.FunctionAttributes.ConditionalAttributes;

namespace DiceEngine.BuiltInFunctions;

public static class TrigonometricFunctions
{
    [BuiltInFunction("sin")]
    [Elementwise]
    public static double Sin(double x)
        => Math.Sin(x);

    [BuiltInFunction("cos")]
    [Elementwise]
    public static double Cos(double x)
        => Math.Cos(x);

    [BuiltInFunction("tan")]
    [Elementwise]
    public static double Tan(double x)
    {        
        return (x / Math.PI - 0.5) % 1 == 0
            ? Double.NaN
            : Math.Tan(x);
    }

    [BuiltInFunction("asin", "arcsin", "arsin")]
    [Elementwise]
    public static double Asin(double x)
        => Math.Asin(x);

    [BuiltInFunction("acos", "arccos", "arcos")]
    [Elementwise]
    public static double Acos(double x)
        => Math.Acos(x);

    [BuiltInFunction("atan", "arctan", "artan")]
    [Elementwise]
    public static double Atan(double x)
        => Double.IsFinite(x)
            ? Math.Atan(x)
            : Double.IsPositiveInfinity(x)
                ? Math.PI / 2
                : Double.IsNegativeInfinity(x)
                    ? -Math.PI / 2
                    : Double.NaN;

    [BuiltInFunction("csc")]
    [Elementwise]
    public static double Csc(double x)
    {
        return x / Math.PI % 1 == 0
            ? Double.NaN
            : 1 / Math.Sin(x);
    }

    [BuiltInFunction("sec")]
    [Elementwise]
    public static double Sec(double x)
    {
        return (x / Math.PI - 0.5) % 1 == 0
            ? Double.NaN
            : 1 / Math.Cos(x);
    }

    [BuiltInFunction("cot")]
    [Elementwise]
    public static double Cot(double x)
    {
        return x / Math.PI % 1 == 0
            ? Double.NaN
            : 1 / Math.Tan(x);
    }

    [BuiltInFunction("acsc", "arccsc", "arcsc")]
    [Elementwise]
    public static double Acsc([Gap(-1, 1)] double x)
        => Double.IsFinite(x)
            ? Math.Asin(1 / x)
            : Double.IsInfinity(x)
                ? 0
                : Double.NaN;

    [BuiltInFunction("asec", "arcsec", "arsec")]
    [Elementwise]
    public static double Asec([Gap(-1, 1)] double x)
        => Double.IsFinite(x)
            ? Math.Acos(1 / x)
            : Double.IsInfinity(x)
                ? Math.PI / 2
                : Double.NaN;

    [BuiltInFunction("acot", "arccot", "arcot")]
    [Elementwise]
    public static double Acot(double x)
        => Double.IsFinite(x)
            ? Math.PI / 2 - Math.Atan(x)
            : Double.IsPositiveInfinity(x)
                ? 0
                : Double.IsNegativeInfinity(x)
                    ? Math.PI
                    : Double.NaN;

    [BuiltInFunction("sinh")]
    [Elementwise]
    public static double Sinh(double x)
        => Double.IsFinite(x)
            ? Math.Sinh(x)
            : Double.IsInfinity(x)
                ? x
                : Double.NaN;

    [BuiltInFunction("cosh")]
    [Elementwise]
    public static double Cosh(double x)
        => Double.IsFinite(x)
            ? Math.Cosh(x)
            : Double.IsInfinity(x)
                ? Double.PositiveInfinity
                : Double.NaN;

    [BuiltInFunction("tanh")]
    [Elementwise]
    public static double Tanh(double x)
        => Double.IsFinite(x)
            ? Math.Tanh(x)
            : Double.IsPositiveInfinity(x)
                ? 1
                : Double.IsNegativeInfinity(x)
                    ? -1
                    : Double.NaN;

    [BuiltInFunction("asinh", "arcsinh", "arsinh")]
    [Elementwise]
    public static double Asinh(double x)
        => Double.IsFinite(x)
            ? Math.Asinh(x)
            : Double.IsPositiveInfinity(x)
                ? Double.PositiveInfinity
                : Double.IsNegativeInfinity(x)
                    ? Double.NegativeInfinity
                    : Double.NaN;

    [BuiltInFunction("acosh", "arccosh", "arcosh")]
    [Elementwise]
    public static double Acosh([Minimum(1)] double x)
        => Double.IsFinite(x)
            ? Math.Acosh(x)
            : Double.IsPositiveInfinity(x)
                ? Double.PositiveInfinity
                : Double.NaN;

    [BuiltInFunction("atanh", "arctanh", "artanh")]
    [Elementwise]
    public static double Atanh(double x)
        => Math.Atanh(x);

    [BuiltInFunction("csch")]
    [Elementwise]
    public static double Csch([Gap(0, 0, inclusive: true)] double x)
        => Double.IsFinite(x)
            ? 1 / Math.Sinh(x)
            : Double.IsInfinity(x)
                ? 0
                : Double.NaN;

    [BuiltInFunction("sech")]
    [Elementwise]
    public static double Sech(double x)
        => Double.IsFinite(x)
            ? 1 / Math.Cosh(x)
            : Double.IsInfinity(x)
                ? 0
                : Double.NaN;

    [BuiltInFunction("coth")]
    [Elementwise]
    public static double Coth([Gap(0, 0, inclusive: true)] double x)
        => Double.IsFinite(x)
            ? Math.Cosh(x) / Math.Sinh(x)
            : Double.IsPositiveInfinity(x)
                ? 1
                : Double.IsNegativeInfinity(x)
                    ? -1
                    : Double.NaN;

    [BuiltInFunction("acsch", "arccsch", "arcsch")]
    [Elementwise]
    public static double Acsch([Gap(0, 0, inclusive: true)] double x)
        => Double.IsFinite(x)
            ? Asinh(1 / x)
            : Double.IsInfinity(x)
                ? 0
                : Double.NaN;

    [BuiltInFunction("asech", "arcsech", "arsech")]
    [Elementwise]
    public static double Asech([Range(0, 1)] double x)
        => Acosh(1 / x);

    [BuiltInFunction("acoth", "arccoth", "arcoth")]
    [Elementwise]
    public static double Acoth([Gap(-1, 1)] double x)
        => Double.IsFinite(x)
            ? x switch
            {
                1 => Double.PositiveInfinity,
                -1 => Double.NegativeInfinity,
                _ => Atanh(1 / x)
            }
            : Double.IsInfinity(x)
                ? 0
                : Double.NaN;
}
