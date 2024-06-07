using CalcExpr.FunctionAttributes;
using System.Diagnostics.CodeAnalysis;

namespace CalcExpr.Expressions.Components;

public struct ContextParameter(IEnumerable<FunctionAttribute> attributes) : IParameter
{
    public FunctionAttribute[] Attributes { get; } = attributes.ToArray();

    public bool AllowNull { get; } = false;

    public object? ProcessArgument(IExpression argument)
    {
        throw new NotImplementedException();
    }

    public override int GetHashCode()
        => HashCode.Combine(Attributes);

    public override bool Equals([NotNullWhen(true)] object? obj)
        => obj is not null && obj is ContextParameter;
}
