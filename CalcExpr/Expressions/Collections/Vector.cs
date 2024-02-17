using CalcExpr.Context;
using System.Collections;

namespace CalcExpr.Expressions.Collections;

public class Vector(IEnumerable<IExpression> elements) : IEnumerableExpression
{
    private readonly IExpression[] _elements = elements.ToArray();

    public IExpression this[int index]
        => _elements[index];

    public IExpression[] this[Range range]
        => _elements[range];

    public int Length
        => _elements.Length;

    public IExpression Clone()
        => new Vector(_elements.Select(e => e.Clone()));

    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext context)
        => throw new NotImplementedException();

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext context)
        => throw new NotImplementedException();

    public static IEnumerableExpression ConvertIEnumerable(IEnumerable<IExpression> elements)
        => new Vector(elements);

    public override bool Equals(object? obj)
    {
        if (obj is not null && obj is Vector vect)
        {
            bool elements_equal = vect.Length == Length &&
                (Length == 0 || !vect.Select((arg, i) => arg.Equals(this[i])).Any(x => !x));

            return elements_equal;
        }

        return false;
    }

    public override int GetHashCode()
        => _elements.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"[{String.Join(", ", elements.Select(e => e.ToString(format)))}]";

    public IEnumerator<IExpression> GetEnumerator()
        => ((IEnumerable<IExpression>)_elements).GetEnumerator();

    IEnumerator IEnumerable.GetEnumerator()
        => _elements.GetEnumerator();
}
