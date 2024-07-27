using CalcExpr.Context;

namespace CalcExpr.Expressions.Interfaces;

public interface IBinaryOperable
{
    IExpression? BinaryLeftOperate(string identifier, IExpression right, ExpressionContext context);

    IExpression? BinaryRightOperate(string identifier, IExpression left, ExpressionContext context);
}
