using CalcExpr.Context;
using CalcExpr.Expressions.Collections;

namespace CalcExpr.Expressions.Interfaces;

public interface IPrefixOperable
{
    IExpression PrefixOperate(string identifier, ExpressionContext context);

    public static IExpression Operate(string identifier, IExpression inside, ExpressionContext? context = null)
    {
        context ??= new ExpressionContext();

        if (inside is IPrefixOperable operable)
        {
            IExpression result = operable.PrefixOperate(identifier, context);

            return result;
        }
        else if (inside is IEnumerableExpression enumExpr)
        {
            return enumExpr.Map(e => Operate(identifier, e, context));
        }

        return Undefined.UNDEFINED;
    }
}
