using DiceEngine.Context;
using DiceEngine.Expressions;
using System.Collections;
using System.Text;

namespace TestDiceEngine.TestModels;

internal class RandomCollection(int? minCount, int? maxCount, double? min, double? max,
    Func<IExpression, bool>? validate = null)
    : IEnumerable<IExpression>, IExpression
{
    public readonly int? MinCount = minCount;
    public readonly int? MaxCount = maxCount;
    public readonly double? Min = min;
    public readonly double? Max = max;
    public readonly Func<IExpression, bool>? Validate = validate;

    public IExpression Evaluate()
        => throw new NotImplementedException();

    public IExpression Evaluate(ExpressionContext context)
        => throw new NotImplementedException();

    public IExpression StepEvaluate()
        => throw new NotImplementedException();

    public IExpression StepEvaluate(ExpressionContext context)
        => throw new NotImplementedException();

    public IEnumerator<IExpression> GetEnumerator()
        => throw new NotImplementedException();

    IEnumerator IEnumerable.GetEnumerator()
        => throw new NotImplementedException();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
    {
        StringBuilder builder = new StringBuilder();

        if (MinCount.HasValue)
            builder.Append($"MinCount: {MinCount}. ");

        if (MaxCount.HasValue)
            builder.Append($"MaxCount: {MaxCount}. ");

        if (Min.HasValue)
            builder.Append($"Min: {Min}. ");

        if (Max.HasValue)
            builder.Append($"Max: {Max}. ");

        if (Validate is not null)
            builder.Append($"Has Validation. ");

        if (builder.Length > 0)
            return $"RandomCollection: ({builder})";
        else
            return "RandomCollection";
    }

    public override int GetHashCode()
        => HashCode.Combine(MinCount, MaxCount, Min, Max, Validate);

    public override bool Equals(object? obj)
        => obj is not null &&
        ((obj is RandomCollection collection && collection.MinCount == MinCount && collection.MaxCount == MaxCount &&
            collection.Min == Min && collection.Max == Max && collection.Validate == Validate) ||
        (obj is IEnumerable<IExpression> enumExpr && obj is IExpression expr &&
            (!MinCount.HasValue || enumExpr.Count() >= MinCount) &&
            (!MaxCount.HasValue || enumExpr.Count() <= MaxCount) &&
            (!Min.HasValue ||
                enumExpr.All(x => !Constant.FALSE.Equals(new BinaryOperator(">=", x, (Number)Min.Value).Evaluate()))) &&
            (!Max.HasValue ||
                enumExpr.All(x => !Constant.FALSE.Equals(new BinaryOperator("<=", x, (Number)Max.Value).Evaluate()))) &&
            (Validate is null || enumExpr.All(Validate))));
}
