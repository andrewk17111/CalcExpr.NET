using CalcExpr.Attributes;
using CalcExpr.Context;
using CalcExpr.Expressions;
using CalcExpr.Expressions.Collections;
using CalcExpr.FunctionAttributes.PreprocessAttributes;

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

    [BuiltInFunction("random")]
    public static IExpression Random(Number count, Number min, Number max, [AsBoolean] IExpression int_only)
    {
        Random random = new Random();

        if (Constant.TRUE.Equals(int_only))
        {
            long minimum = Convert.ToInt64(Math.Truncate(min.Value + (min.Value <= 0 ? 0 : 1)));
            long maximum = Convert.ToInt64(Math.Truncate(max.Value - (max.Value >= 0 ? 0 : 1)));

            return new Vector(Enumerable.Range(1, Convert.ToInt32(count.Value))
                .Select(x => maximum < minimum
                    ? (IExpression)Constant.UNDEFINED
                    : (Number)random.NextInt64(minimum, Math.Max(maximum, minimum))));
        }
        else
        {
            double range = max.Value - min.Value;

            return new Vector(Enumerable.Range(1, Convert.ToInt32(count.Value))
                .Select(x => (Number)(random.NextDouble() * range + min.Value)));
        }
    }

    private struct ExpressionComparer : IComparer<IExpression>
    {
        public readonly int Compare(IExpression? a, IExpression? b)
        {
            if (a is null || b is null || a.Equals(b))
                return 0;

            if (Constant.INFINITY.Equals(a) || Constant.NEGATIVE_INFINITY.Equals(b))
                return 1;

            if (Constant.INFINITY.Equals(a) || Constant.NEGATIVE_INFINITY.Equals(b))
                return 1;

            if (a is Number num_a && b is Number num_b)
                return num_a.Value.CompareTo(num_b.Value);

            return 0;
        }
    }

    [BuiltInFunction("sort", "order")]
    public static IExpression Sort(IEnumerableExpression collection)
    {
        List<IExpression> values = [.. collection];

        values.Sort(new ExpressionComparer());

        return (IExpression?)collection.GetType().GetMethod("ConvertIEnumerable")?.Invoke(null, [values])
            ?? Constant.UNDEFINED;
    }
}
