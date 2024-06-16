using DiceEngine.Context;
using DiceEngine.Expressions;
using System.Collections;
using System.Numerics;
using System.Text;

namespace TestDiceEngine.TestModels;

internal class RandomEnumerable<T>(int? minCount, int? maxCount, T? min, T? max,
    Func<T, bool>? elementValidator = null, Func<IEnumerable<T>, bool>? enumerableValidator = null)
    : IEnumerable<T>, IExpression
    where T : struct, IComparisonOperators<T, T, bool>, IEquatable<T>
{
    public readonly int? MinCount = minCount;
    public readonly int? MaxCount = maxCount;
    public readonly T? Min = min;
    public readonly T? Max = max;
    public readonly Func<T, bool>? ElementValidator = elementValidator;
    public readonly Func<IEnumerable<T>, bool>? EnumerableValidator = enumerableValidator;

    public IExpression Evaluate()
        => throw new NotImplementedException();

    public IExpression Evaluate(ExpressionContext context)
        => throw new NotImplementedException();

    public IExpression StepEvaluate()
        => throw new NotImplementedException();

    public IExpression StepEvaluate(ExpressionContext context)
        => throw new NotImplementedException();

    public IEnumerator<T> GetEnumerator()
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

        if (ElementValidator is not null)
            builder.Append($"Has Element Validation. ");

        if (EnumerableValidator is not null)
            builder.Append($"Has Enumerable Validation. ");

        if (builder.Length > 0)
            return $"RandomEnumerable: ({builder})";
        else
            return "RandomEnumerable";
    }

    public override int GetHashCode()
        => HashCode.Combine(MinCount, MaxCount, Min, Max, ElementValidator, EnumerableValidator);

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (obj is RandomEnumerable<T> rEnumerable && rEnumerable.MinCount == MinCount && rEnumerable.MaxCount == MaxCount &&
            rEnumerable.Min.Equals(Min) && rEnumerable.Max.Equals(Max) && rEnumerable.ElementValidator == ElementValidator &&
            rEnumerable.EnumerableValidator == EnumerableValidator)
            return true;

        if (obj is IEnumerable<T> enumerable)
            if ((!MinCount.HasValue || enumerable.Count() >= MinCount) &&
                (!MaxCount.HasValue || enumerable.Count() <= MaxCount) &&
                (!Min.HasValue || enumerable.All(x => x >= Min.Value)) &&
                (!Max.HasValue || enumerable.All(x => x <= Max.Value)))
                if (ElementValidator is null || enumerable.All(ElementValidator))
                    if (EnumerableValidator is null || EnumerableValidator(enumerable))
                        return true;

        return false;
    }
}
