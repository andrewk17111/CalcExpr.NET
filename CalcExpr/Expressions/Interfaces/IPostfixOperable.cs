using CalcExpr.Context;

namespace CalcExpr.Expressions.Interfaces;

public interface IPostfixOperable
{
    IExpression PostfixOperate(string identifier, ExpressionContext context);
}
