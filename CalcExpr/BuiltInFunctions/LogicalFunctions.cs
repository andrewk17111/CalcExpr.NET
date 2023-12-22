using CalcExpr.Attributes;
using CalcExpr.Context;
using CalcExpr.Expressions;

namespace CalcExpr.BuiltInFunctions;

public static class LogicalFunctions
{
    [BuiltInFunction("and")]
    public static IExpression And(IExpression a, IExpression b)
        => ((Number)Bool(a)).Value == 0 || ((Number)Bool(b)).Value == 0
            ? new Number(0)
            : new Number(1);

    [BuiltInFunction("or")]
    public static IExpression Or(IExpression a, IExpression b)
        => ((Number)Bool(a)).Value == 0 && ((Number)Bool(b)).Value == 0
            ? new Number(0)
            : new Number(1);

    [BuiltInFunction("xor")]
    public static IExpression Xor(IExpression a, IExpression b)
    {
        bool a_bool = ((Number)Bool(a)).Value != 0;
        bool b_bool = ((Number)Bool(b)).Value != 0;

        return new Number((a_bool || b_bool) && !(a_bool && b_bool) ? 1 : 0);
    }

    [BuiltInFunction("not")]
    public static IExpression Not(IExpression x)
        => Constant.UNDEFINED.Equals(x)
            ? Constant.UNDEFINED
            : x is Number n && n.Value == 0
                ? new Number(1)
                // Any value that is not 0 or undefined should result in 0.
                : new Number(0);

    [BuiltInFunction("bool")]
    public static IExpression Bool(IExpression x)
        => Constant.UNDEFINED.Equals(x)
            ? Constant.UNDEFINED
            : x is Number n && n.Value == 0
                ? new Number(0)
                // Any value that is not 0 or undefined should result in 1.
                : new Number(1);

    [BuiltInFunction("if")]
    public static IExpression If(IExpression condition, IExpression is_true, IExpression is_false)
        => new Number(1).Equals(Bool(condition)) ? is_true.Clone() : is_false.Clone();

    [BuiltInFunction("is_na")]
    public static IExpression IsNa(IExpression x, ExpressionContext _)
        => Constant.UNDEFINED.Equals(x) ? Constant.TRUE : Constant.FALSE;

    [BuiltInFunction("is_num")]
    public static IExpression IsNum(IExpression x, ExpressionContext _)
        => x is Number ? Constant.TRUE : Constant.FALSE;
}
