namespace CalcExpr.Expressions.Interfaces;

public interface IBoolConvertible : IExpression
{
    Logical ToBool();
}
