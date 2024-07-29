using CalcExpr.Context;

namespace CalcExpr.Expressions.Interfaces;

public interface IPrefixOperable
{
    IExpression PrefixOperate(string identifier, ExpressionContext context);

    public static IExpression Operate(string identifier, IExpression inside, ExpressionContext? context = null)
    {
        context ??= new ExpressionContext();

        if (inside is IPrefixOperable prefixOperable)
            return prefixOperable.PrefixOperate(identifier, context);

        return Undefined.UNDEFINED;
    }
}
