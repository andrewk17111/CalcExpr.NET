using CalcExpr.Expressions.Terminals;

namespace CalcExpr.Expressions.Interfaces;

public interface ILogicalConvertible : IExpression
{
    Logical ToLogical();

    public static Logical? ConvertToLogical(IExpression expr)
    {
        if (expr is Logical logical)
            return logical;
        else if (expr is ILogicalConvertible boolConvertible)
            return boolConvertible.ToLogical();

        return null;
    }
}
