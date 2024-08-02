using CalcExpr.Context;
using CalcExpr.Expressions.Collections;

namespace CalcExpr.Expressions.Interfaces;

public interface IPostfixOperable
{
    IExpression PostfixOperate(string identifier, ExpressionContext context);

    public static IExpression Operate(string identifier, IExpression inside, ExpressionContext? context = null)
    {
        context ??= new ExpressionContext();

        if (inside is IPostfixOperable operable)
        {
            IExpression result = operable.PostfixOperate(identifier, context);

            return result;
        }
        else if (inside is IEnumerableExpression enumExpr)
        {
            return enumExpr.Map(e => Operate(identifier, e, context));
        }

        return Undefined.UNDEFINED;
    }
}
