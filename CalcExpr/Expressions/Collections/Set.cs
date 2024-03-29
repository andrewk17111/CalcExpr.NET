using CalcExpr.Context;
using System.Collections;

namespace CalcExpr.Expressions.Collections;

public class Set(IEnumerable<IExpression> elements) : IEnumerableExpression
{
    private readonly SortedSet<IExpression> _elements = [.. elements];

    public int Count
        => _elements.Count;

    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext context)
        => throw new NotImplementedException();

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext context)
        => throw new NotImplementedException();

    public IEnumerableExpression Map(Func<IExpression, IExpression> map)
        => new Set(_elements.Select(x => map(x)));

    public IEnumerableExpression Combine(IEnumerable<IExpression> expressions,
        Func<IExpression, IExpression, IExpression> combine)
    {
        expressions = expressions.OrderBy(x => x.GetHashCode());

        IEnumerable<IExpression> elements = _elements.OrderBy(x => x.GetHashCode());
        List<IExpression> result = [];
        int count = expressions.Count();

        for (int i = 0; i < Count && i < count; i++)
            result.Add(combine(elements.ElementAt(i), expressions.ElementAt(i)));

        return new Set(result);
    }

    public static IEnumerableExpression ConvertIEnumerable(IEnumerable<IExpression> elements)
        => new Set(elements);

    public override bool Equals(object? obj)
    {
        if (obj is not null && obj is Set set)
        {
            bool elements_equal = set.Count == Count &&
                (Count == 0 || !set.Select((arg, i) => arg.Equals(this.ElementAt(i))).Any(x => !x));

            return elements_equal;
        }

        return false;
    }

    public override int GetHashCode()
        => _elements.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"{{{String.Join(", ", elements.Select(e => e.ToString(format)))}}}";

    public IEnumerator<IExpression> GetEnumerator()
        => ((IEnumerable<IExpression>)_elements).GetEnumerator();

    IEnumerator IEnumerable.GetEnumerator()
        => _elements.GetEnumerator();
}
