using CalcExpr.Attributes;
using CalcExpr.Context;
using CalcExpr.Expressions;
using CalcExpr.Expressions.Collections;

namespace CalcExpr.BuiltInFunctions;

public static class CollectionFunctions
{
    [BuiltInFunction("map")]
    public static IExpression Map(IEnumerableExpression collection, IFunction operation, ExpressionContext context)
    {
        IEnumerable<IExpression> expressions = collection.Select(x => operation.Invoke([x.Evaluate(context)], context));
        IExpression? result = (IExpression?)Activator.CreateInstance(collection.GetType(), expressions);

        return result ?? Constant.UNDEFINED;
    }
}
