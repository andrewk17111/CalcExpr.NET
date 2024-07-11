namespace CalcExpr.Expressions.Interfaces;

public interface IBoolConvertible : IExpression
{
    Constant ToBool();
}
