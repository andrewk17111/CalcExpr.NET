using CalcExpr.Context;

namespace CalcExpr.Expressions.Interfaces;

public interface IPrefixOperable
{
    IExpression PrefixOperate(string identifier, ExpressionContext context);
}
