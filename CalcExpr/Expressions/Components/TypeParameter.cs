using CalcExpr.FunctionAttributes;

namespace CalcExpr.Expressions.Components;

public struct TypeParameter(Type type, IEnumerable<FunctionAttribute> attributes, bool allow_null) : IParameter
{
    public readonly Type ParameterType = type;

    public FunctionAttribute[] Attributes { get; } = attributes.ToArray();

    public bool AllowNull { get; } = allow_null;

    public object? ProcessArgument(IExpression argument)
    {
        throw new NotImplementedException();
    }

    public override int GetHashCode()
        => HashCode.Combine(ParameterType, AllowNull);

    public override bool Equals(object? obj)
        => obj is TypeParameter parameter && ParameterType == parameter.ParameterType &&
            AllowNull == parameter.AllowNull;
}
