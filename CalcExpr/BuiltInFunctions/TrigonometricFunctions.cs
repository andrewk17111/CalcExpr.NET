using CalcExpr.Attributes;
using CalcExpr.Expressions;

namespace CalcExpr.BuiltInFunctions;

public static class TrigonometricFunctions
{
    [BuiltInFunction("sin")]
    public static IExpression Sin(IExpression x)
        => x is Number num ? new Number(Math.Sin(num.Value)).Evaluate() : Constant.UNDEFINED;

    [BuiltInFunction("cos")]
    public static IExpression Cos(IExpression x)
        => x is Number num ? new Number(Math.Cos(num.Value)).Evaluate() : Constant.UNDEFINED;

    [BuiltInFunction("tan")]
    public static IExpression Tan(IExpression x)
        => x is not Number num
            ? Constant.UNDEFINED
            : (num.Value / Math.PI - 0.5) % 1 == 0
                ? Constant.UNDEFINED
                : new Number(Math.Tan(num.Value)).Evaluate();

    [BuiltInFunction("asin", "arcsin", "arsin")]
    public static IExpression Asin(IExpression x)
        => x is Number num ? new Number(Math.Asin(num.Value)).Evaluate() : Constant.UNDEFINED;

    [BuiltInFunction("acos", "arccos", "arcos")]
    public static IExpression Acos(IExpression x)
        => x is Number num ? new Number(Math.Acos(num.Value)).Evaluate() : Constant.UNDEFINED;

    [BuiltInFunction("atan", "arctan", "artan")]
    public static IExpression Atan(IExpression x)
        => x is Number num
            ? new Number(Math.Atan(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x)
                ? new Number(Math.PI / 2)
                : Constant.NEGATIVE_INFINITY.Equals(x)
                    ? new Number(-Math.PI / 2)
                    : Constant.UNDEFINED;

    [BuiltInFunction("csc")]
    public static IExpression Csc(IExpression x)
        => x is not Number num
            ? Constant.UNDEFINED
            : num.Value / Math.PI % 1 == 0
                ? Constant.UNDEFINED
                : new Number(1 / Math.Sin(num.Value)).Evaluate();

    [BuiltInFunction("sec")]
    public static IExpression Sec(IExpression x)
        => x is not Number num
            ? Constant.UNDEFINED
            : (num.Value / Math.PI - 0.5) % 1 == 0
                ? Constant.UNDEFINED
                : new Number(1 / Math.Cos(num.Value)).Evaluate();

    [BuiltInFunction("cot")]
    public static IExpression Cot(IExpression x)
        => x is not Number num
            ? Constant.UNDEFINED
            : num.Value / Math.PI % 1 == 0
                ? Constant.UNDEFINED
                : new Number(1 / Math.Tan(num.Value)).Evaluate();

    [BuiltInFunction("acsc", "arccsc", "arcsc")]
    public static IExpression Acsc(IExpression x)
        => x is Number num
            ? new Number(Math.Asin(1 / num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x) || Constant.NEGATIVE_INFINITY.Equals(x)
                ? new Number(0)
                : Constant.UNDEFINED;

    [BuiltInFunction("asec", "arcsec", "arsec")]
    public static IExpression Asec(IExpression x)
        => x is Number num
            ? new Number(Math.Acos(1 / num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x) || Constant.NEGATIVE_INFINITY.Equals(x)
                ? new Number(Math.PI / 2)
                : Constant.UNDEFINED;

    [BuiltInFunction("acot", "arccot", "arcot")]
    public static IExpression Acot(IExpression x)
        => x is Number num
            ? new Number(Math.PI / 2 - Math.Atan(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x)
                ? new Number(0)
                : Constant.NEGATIVE_INFINITY.Equals(x)
                    ? new Number(Math.PI)
                    : Constant.UNDEFINED;

    [BuiltInFunction("sinh")]
    public static IExpression Sinh(IExpression x)
        => x is Number num
            ? new Number(Math.Sinh(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x) || Constant.NEGATIVE_INFINITY.Equals(x)
                ? x.Clone()
                : Constant.UNDEFINED;

    [BuiltInFunction("cosh")]
    public static IExpression Cosh(IExpression x)
        => x is Number num
            ? new Number(Math.Cosh(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x) || Constant.NEGATIVE_INFINITY.Equals(x)
                ? Constant.INFINITY.Clone()
                : Constant.UNDEFINED;

    [BuiltInFunction("tanh")]
    public static IExpression Tanh(IExpression x)
        => x is Number num
            ? new Number(Math.Tanh(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x)
                ? new Number(1)
                : Constant.NEGATIVE_INFINITY.Equals(x)
                    ? new Number(-1)
                    : Constant.UNDEFINED;

    [BuiltInFunction("asinh", "arcsinh", "arsinh")]
    public static IExpression Asinh(IExpression x)
        => x is Number num
            ? new Number(Math.Asinh(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x)
                ? Constant.INFINITY.Clone()
                : Constant.NEGATIVE_INFINITY.Equals(x)
                    ? Constant.NEGATIVE_INFINITY.Clone()
                    : Constant.UNDEFINED;

    [BuiltInFunction("acosh", "arccosh", "arcosh")]
    public static IExpression Acosh(IExpression x)
        => x is Number num
            ? new Number(Math.Acosh(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x)
                ? Constant.INFINITY.Clone()
                : Constant.UNDEFINED;

    [BuiltInFunction("atanh", "arctanh", "artanh")]
    public static IExpression Atanh(IExpression x)
        => x is Number num ? new Number(Math.Atanh(num.Value)).Evaluate() : Constant.UNDEFINED;

    [BuiltInFunction("csch")]
    public static IExpression Csch(IExpression x)
        => x is Number num
            ? num.Value == 0
                ? Constant.UNDEFINED
                : new Number(1 / Math.Sinh(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x) || Constant.NEGATIVE_INFINITY.Equals(x)
                ? new Number(0)
                : Constant.UNDEFINED;

    [BuiltInFunction("sech")]
    public static IExpression Sech(IExpression x)
        => x is Number num
            ? new Number(1 / Math.Cosh(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x) || Constant.NEGATIVE_INFINITY.Equals(x)
                ? new Number(0)
                : Constant.UNDEFINED;

    [BuiltInFunction("coth")]
    public static IExpression Coth(IExpression x)
        => x is Number num
            ? num.Value == 0
                ? Constant.UNDEFINED
                : new Number(Math.Cosh(num.Value) / Math.Sinh(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x)
                ? new Number(1)
                : Constant.NEGATIVE_INFINITY.Equals(x)
                    ? new Number(-1)
                    : Constant.UNDEFINED;

    [BuiltInFunction("acsch", "arccsch", "arcsch")]
    public static IExpression Acsch(IExpression x)
        => x is Number num
            ? num.Value == 0
                ? Constant.UNDEFINED
                : Asinh(new Number(1 / num.Value))
            : Constant.INFINITY.Equals(x) || Constant.NEGATIVE_INFINITY.Equals(x)
                ? new Number(0)
                : Constant.UNDEFINED;

    [BuiltInFunction("asech", "arcsech", "arsech")]
    public static IExpression Asech(IExpression x)
        => x is Number num && num.Value >= 0 && num.Value <= 1
            ? Acosh(new Number(1 / num.Value))
            : Constant.UNDEFINED;

    [BuiltInFunction("acoth", "arccoth", "arcoth")]
    public static IExpression Acoth(IExpression x)
        => x is Number num
            ? num.Value switch
            {
                1 => Constant.INFINITY,
                -1 => Constant.NEGATIVE_INFINITY,
                _ => Atanh(new Number(1 / num.Value))
            }
            : Constant.INFINITY.Equals(x) || Constant.NEGATIVE_INFINITY.Equals(x)
                ? new Number(0)
                : Constant.UNDEFINED;
}
