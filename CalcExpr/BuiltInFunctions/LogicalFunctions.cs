using CalcExpr.Attributes;
using CalcExpr.Context;
using CalcExpr.Expressions;
using CalcExpr.FunctionAttributes.ConditionalAttributes;
using CalcExpr.FunctionAttributes.PreprocessAttributes;

namespace CalcExpr.BuiltInFunctions;

public static class LogicalFunctions
{
    [BuiltInFunction("and")]
    [Elementwise]
    public static IExpression And([AsBoolean][NotUndefined] IExpression a, [AsBoolean][NotUndefined] IExpression b)
        => Constant.FALSE.Equals(a) || Constant.FALSE.Equals(b)
            ? Constant.FALSE
            : Constant.TRUE;

    [BuiltInFunction("or")]
    [Elementwise]
    public static IExpression Or([AsBoolean][NotUndefined] IExpression a, [AsBoolean][NotUndefined] IExpression b)
        => Constant.TRUE.Equals(a) || Constant.TRUE.Equals(b)
            ? Constant.TRUE
            : Constant.FALSE;

    [BuiltInFunction("xor")]
    [Elementwise]
    public static IExpression Xor([AsBoolean][NotUndefined] IExpression a, [AsBoolean][NotUndefined] IExpression b)
    {
        bool a_bool = Constant.TRUE.Equals(a);
        bool b_bool = Constant.TRUE.Equals(b);

        return (a_bool || b_bool) && !(a_bool && b_bool)
            ? Constant.TRUE
            : Constant.FALSE;
    }

    [BuiltInFunction("not")]
    [Elementwise]
    public static IExpression Not([AsBoolean][NotUndefined] IExpression x)
        => Constant.TRUE.Equals(x) ? Constant.FALSE : Constant.TRUE;

    [BuiltInFunction("bool")]
    [Elementwise]
    public static IExpression Bool([AsBoolean][NotUndefined] IExpression x)
        => x;

    [BuiltInFunction("if")]
    [Elementwise]
    public static IExpression If([AsBoolean][NotUndefined] IExpression condition, IExpression is_true,
        IExpression is_false)
        => Constant.TRUE.Equals(condition) ? is_true : is_false;

    [BuiltInFunction("is_na")]
    public static IExpression IsNa(IExpression x, ExpressionContext _)
        => Constant.UNDEFINED.Equals(x) ? Constant.TRUE : Constant.FALSE;

    [BuiltInFunction("is_num", "is_number")]
    public static IExpression IsNum(IExpression x, ExpressionContext _)
        => x is Number ? Constant.TRUE : Constant.FALSE;

    [BuiltInFunction("is_int", "is_integer")]
    public static IExpression IsInt(IExpression x, ExpressionContext _)
        => x is Number num && num.Value % 1 == 0
            ? Constant.TRUE
            : Constant.FALSE;

    [BuiltInFunction("is_logical")]
    public static IExpression IsLogical(IExpression x, ExpressionContext _)
        => Constant.TRUE.Equals(x) || Constant.FALSE.Equals(x)
            ? Constant.TRUE
            : Constant.FALSE;

    [BuiltInFunction("is_even")]
    public static IExpression IsEven(IExpression x)
        => x is Number num
            ? num.Value % 2 == 0
                ? Constant.TRUE
                : Constant.FALSE
            : Constant.UNDEFINED;

    [BuiltInFunction("is_odd")]
    public static IExpression IsOdd(IExpression x)
        => x is Number num
            ? Math.Abs(num.Value % 2) == 1
                ? Constant.TRUE
                : Constant.FALSE
            : Constant.UNDEFINED;

    [BuiltInFunction("is_positive")]
    public static IExpression IsPositive(IExpression x)
        => (x is Number num && num.Value > 0) || Constant.INFINITY.Equals(x)
            ? Constant.TRUE
            : !Constant.UNDEFINED.Equals(x)
                ? Constant.FALSE
                : Constant.UNDEFINED;

    [BuiltInFunction("is_negative")]
    public static IExpression IsNegative(IExpression x, ExpressionContext _)
        => (x is Number num && num.Value < 0) || Constant.NEGATIVE_INFINITY.Equals(x)
            ? Constant.TRUE
            : !Constant.UNDEFINED.Equals(x)
                ? Constant.FALSE
                : Constant.UNDEFINED;
}
