using CalcExpr.Context;

namespace CalcExpr.Expressions.Interfaces;

public interface IPostfixOperable
{
    IExpression PostfixOperate(string identifier, ExpressionContext context);

    public static IExpression Operate(string identifier, IExpression inside, ExpressionContext? context = null)
    {
        context ??= new ExpressionContext();

        if (inside is IPostfixOperable postfixOperable)
            return postfixOperable.PostfixOperate(identifier, context);

        return Undefined.UNDEFINED;
    }
}
