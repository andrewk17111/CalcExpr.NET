using CalcExpr.Attributes;
using CalcExpr.Context;
using CalcExpr.Expressions;

namespace CalcExpr.BuiltInFunctions;

public static class LogicalFunctions
{
    [BuiltInFunction("and")]
    public static IExpression And(IExpression a, IExpression b)
        => Constant.UNDEFINED.Equals(a) || Constant.UNDEFINED.Equals(b)
            ? Constant.UNDEFINED
            : Constant.FALSE.Equals(Bool(a)) || Constant.FALSE.Equals(Bool(b))
                ? Constant.FALSE
                : Constant.TRUE;

    [BuiltInFunction("or")]
    public static IExpression Or(IExpression a, IExpression b)
        => Constant.UNDEFINED.Equals(a) || Constant.UNDEFINED.Equals(b)
            ? Constant.UNDEFINED
            : Constant.TRUE.Equals(Bool(a)) || Constant.TRUE.Equals(Bool(b))
                ? Constant.TRUE
                : Constant.FALSE;

    [BuiltInFunction("xor")]
    public static IExpression Xor(IExpression a, IExpression b)
    {
        IExpression a_eval = Bool(a);
        IExpression b_eval = Bool(b);

        if (Constant.UNDEFINED.Equals(a_eval) || Constant.UNDEFINED.Equals(b_eval))
            return Constant.UNDEFINED;

        bool a_bool = Constant.TRUE.Equals(a_eval);
        bool b_bool = Constant.TRUE.Equals(b_eval);

        return (a_bool || b_bool) && !(a_bool && b_bool)
            ? Constant.TRUE
            : Constant.FALSE;
    }

    [BuiltInFunction("not")]
    public static IExpression Not(IExpression x)
        => Constant.UNDEFINED.Equals(x)
            ? Constant.UNDEFINED
            : Constant.TRUE.Equals(Bool(x))
                ? Constant.FALSE
                : Constant.TRUE;

    [BuiltInFunction("bool")]
    public static IExpression Bool(IExpression x)
        => Constant.UNDEFINED.Equals(x)
            ? Constant.UNDEFINED
            : Constant.TRUE.Equals(x) || Constant.FALSE.Equals(x)
                ? x.Clone()
                : x is Number n && n.Value == 0
                    ? Constant.FALSE
                    // Any value that is not 0 or undefined should result in 1.
                    : Constant.TRUE;

    [BuiltInFunction("if")]
    public static IExpression If(IExpression condition, IExpression is_true, IExpression is_false)
        => Constant.UNDEFINED.Equals(condition)
            ? Constant.UNDEFINED
            : Constant.TRUE.Equals(Bool(condition))
                ? is_true.Clone()
                : is_false.Clone();

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
