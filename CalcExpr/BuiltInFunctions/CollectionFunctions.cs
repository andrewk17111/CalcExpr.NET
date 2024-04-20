﻿using CalcExpr.Attributes;
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

    [BuiltInFunction("filter", "where")]
    public static IExpression Filter(IEnumerableExpression collection, IFunction selector, ExpressionContext context)
    {
        IEnumerable<IExpression> expressions = collection
            .Select(x => x.Evaluate(context))
            .Where(x => !Constant.FALSE.Equals(LogicalFunctions.Bool(selector.Invoke([x], context))));
        IExpression? result = (IExpression?)Activator.CreateInstance(collection.GetType(), expressions);

        return result ?? Constant.UNDEFINED;
    }

    [BuiltInFunction("aggregate")]
    public static IExpression Aggregate(IEnumerableExpression collection, IFunction aggregator,
        ExpressionContext context)
    {
        if (!collection.Any())
            return Constant.UNDEFINED;

        IExpression? result = collection
            .Select(x => x.Evaluate(context))
            .Aggregate((a, b) => aggregator.Invoke([a, b], context));

        return result ?? Constant.UNDEFINED;
    }

    [BuiltInFunction("range")]
    public static IExpression Range(Number start, Number count, Number step)
    {
        if (start.Value % 1 != 0 || count.Value % 1 != 0 || step.Value % 1 != 0 || count.Value < 0)
            return Constant.UNDEFINED;

        int start_value = (int)start.Value;
        int count_value = (int)count.Value;
        int step_value = (int)step.Value;

        return Vector.ConvertIEnumerable(Enumerable.Range(0, count_value)
            .Select(i => (Number)(start_value + i * step_value)));
    }
}
