using CalcExpr.Attributes;
using CalcExpr.Expressions;
using CalcExpr.FunctionAttributes.ConditionalAttributes;

namespace CalcExpr.BuiltInFunctions;

public static class TrigonometricFunctions
{
    [BuiltInFunction("sin")]
    [Elementwise]
    public static IExpression Sin(Number x)
        => new Number(Math.Sin(((Number)x).Value));

    [BuiltInFunction("cos")]
    [Elementwise]
    public static IExpression Cos(Number x)
        => new Number(Math.Cos(((Number)x).Value));

    [BuiltInFunction("tan")]
    [Elementwise]
    public static IExpression Tan(Number x)
    {
        Number num = (Number)x;
        
        return (num.Value / Math.PI - 0.5) % 1 == 0
            ? Constant.UNDEFINED
            : new Number(Math.Tan(num.Value)).Evaluate();
    }

    [BuiltInFunction("asin", "arcsin", "arsin")]
    [Elementwise]
    public static IExpression Asin(Number x)
        => new Number(Math.Asin(((Number)x).Value)).Evaluate();

    [BuiltInFunction("acos", "arccos", "arcos")]
    [Elementwise]
    public static IExpression Acos(Number x)
        => new Number(Math.Acos(((Number)x).Value)).Evaluate();

    [BuiltInFunction("atan", "arctan", "artan")]
    [Elementwise]
    public static IExpression Atan([NotUndefined] IExpression x)
        => x is Number num
            ? new Number(Math.Atan(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x)
                ? new Number(Math.PI / 2)
                : Constant.NEGATIVE_INFINITY.Equals(x)
                    ? new Number(-Math.PI / 2)
                    : Constant.UNDEFINED;

    [BuiltInFunction("csc")]
    [Elementwise]
    public static IExpression Csc(Number x)
    {
        Number num = (Number)x;

        return num.Value / Math.PI % 1 == 0
            ? Constant.UNDEFINED
            : new Number(1 / Math.Sin(num.Value)).Evaluate();
    }

    [BuiltInFunction("sec")]
    [Elementwise]
    public static IExpression Sec(Number x)
    {
        Number num = (Number)x;
        
        return (num.Value / Math.PI - 0.5) % 1 == 0
            ? Constant.UNDEFINED
            : new Number(1 / Math.Cos(num.Value)).Evaluate();
    }

    [BuiltInFunction("cot")]
    [Elementwise]
    public static IExpression Cot(Number x)
    {
        Number num = (Number)x;

        return num.Value / Math.PI % 1 == 0
            ? Constant.UNDEFINED
            : new Number(1 / Math.Tan(num.Value)).Evaluate();
    }

    [BuiltInFunction("acsc", "arccsc", "arcsc")]
    [Elementwise]
    public static IExpression Acsc([NotUndefined][Gap(-1, 1)] IExpression x)
        => x is Number num
            ? new Number(Math.Asin(1 / num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x) || Constant.NEGATIVE_INFINITY.Equals(x)
                ? new Number(0)
                : Constant.UNDEFINED;

    [BuiltInFunction("asec", "arcsec", "arsec")]
    [Elementwise]
    public static IExpression Asec([NotUndefined][Gap(-1, 1)] IExpression x)
        => x is Number num
            ? new Number(Math.Acos(1 / num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x) || Constant.NEGATIVE_INFINITY.Equals(x)
                ? new Number(Math.PI / 2)
                : Constant.UNDEFINED;

    [BuiltInFunction("acot", "arccot", "arcot")]
    [Elementwise]
    public static IExpression Acot([NotUndefined] IExpression x)
        => x is Number num
            ? new Number(Math.PI / 2 - Math.Atan(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x)
                ? new Number(0)
                : Constant.NEGATIVE_INFINITY.Equals(x)
                    ? new Number(Math.PI)
                    : Constant.UNDEFINED;

    [BuiltInFunction("sinh")]
    [Elementwise]
    public static IExpression Sinh([NotUndefined] IExpression x)
        => x is Number num
            ? new Number(Math.Sinh(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x) || Constant.NEGATIVE_INFINITY.Equals(x)
                ? x
                : Constant.UNDEFINED;

    [BuiltInFunction("cosh")]
    [Elementwise]
    public static IExpression Cosh([NotUndefined] IExpression x)
        => x is Number num
            ? new Number(Math.Cosh(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x) || Constant.NEGATIVE_INFINITY.Equals(x)
                ? Constant.INFINITY
                : Constant.UNDEFINED;

    [BuiltInFunction("tanh")]
    [Elementwise]
    public static IExpression Tanh([NotUndefined] IExpression x)
        => x is Number num
            ? new Number(Math.Tanh(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x)
                ? new Number(1)
                : Constant.NEGATIVE_INFINITY.Equals(x)
                    ? new Number(-1)
                    : Constant.UNDEFINED;

    [BuiltInFunction("asinh", "arcsinh", "arsinh")]
    [Elementwise]
    public static IExpression Asinh([NotUndefined] IExpression x)
        => x is Number num
            ? new Number(Math.Asinh(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x)
                ? Constant.INFINITY
                : Constant.NEGATIVE_INFINITY.Equals(x)
                    ? Constant.NEGATIVE_INFINITY
                    : Constant.UNDEFINED;

    [BuiltInFunction("acosh", "arccosh", "arcosh")]
    [Elementwise]
    public static IExpression Acosh([NotUndefined][Minimum(1)] IExpression x)
        => x is Number num
            ? new Number(Math.Acosh(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x)
                ? Constant.INFINITY
                : Constant.UNDEFINED;

    [BuiltInFunction("atanh", "arctanh", "artanh")]
    [Elementwise]
    public static IExpression Atanh(Number x)
        => new Number(Math.Atanh(((Number)x).Value)).Evaluate();

    [BuiltInFunction("csch")]
    [Elementwise]
    public static IExpression Csch([NotUndefined][Gap(0, 0, inclusive: true)] IExpression x)
        => x is Number num
            ? new Number(1 / Math.Sinh(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x) || Constant.NEGATIVE_INFINITY.Equals(x)
                ? new Number(0)
                : Constant.UNDEFINED;

    [BuiltInFunction("sech")]
    [Elementwise]
    public static IExpression Sech([NotUndefined] IExpression x)
        => x is Number num
            ? new Number(1 / Math.Cosh(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x) || Constant.NEGATIVE_INFINITY.Equals(x)
                ? new Number(0)
                : Constant.UNDEFINED;

    [BuiltInFunction("coth")]
    [Elementwise]
    public static IExpression Coth([NotUndefined][Gap(0, 0, inclusive: true)] IExpression x)
        => x is Number num
            ? new Number(Math.Cosh(num.Value) / Math.Sinh(num.Value)).Evaluate()
            : Constant.INFINITY.Equals(x)
                ? new Number(1)
                : Constant.NEGATIVE_INFINITY.Equals(x)
                    ? new Number(-1)
                    : Constant.UNDEFINED;

    [BuiltInFunction("acsch", "arccsch", "arcsch")]
    [Elementwise]
    public static IExpression Acsch([NotUndefined][Gap(0, 0, inclusive: true)] IExpression x)
        => x is Number num
            ? Asinh(new Number(1 / num.Value))
            : Constant.INFINITY.Equals(x) || Constant.NEGATIVE_INFINITY.Equals(x)
                ? new Number(0)
                : Constant.UNDEFINED;

    [BuiltInFunction("asech", "arcsech", "arsech")]
    [Elementwise]
    public static IExpression Asech([IsNumber][Range(0, 1)] IExpression x)
        => Acosh(new Number(1 / ((Number)x).Value));

    [BuiltInFunction("acoth", "arccoth", "arcoth")]
    [Elementwise]
    public static IExpression Acoth([NotUndefined][Gap(-1, 1)] IExpression x)
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
