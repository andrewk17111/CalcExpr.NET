using CalcExpr.Context;
using CalcExpr.FunctionAttributes;
using System.Reflection;

namespace CalcExpr.Expressions.Components;

public readonly struct Parameter(string name, IEnumerable<FunctionAttribute> attributes, bool is_context)
{
    public readonly string Name = name;
    public readonly FunctionAttribute[] Attributes = attributes.ToArray();
    public readonly bool IsContext = is_context;

    public Parameter(string name) : this(name, [], false)
    { }

    public Parameter(string name, IEnumerable<FunctionAttribute> attributes) : this(name, attributes, false)
    { }

    public Parameter(string name, bool is_context) : this(name, [], is_context)
    { }

    public override int GetHashCode()
        => Name.GetHashCode();

    public override bool Equals(object? obj)
        => obj is not null && obj is Parameter parameter && parameter.Name == Name;

    public override string ToString()
        => (Attributes.Length == 0
            ? ""
            : $"[{String.Join(", ", Attributes.Select(a => a.GetType().Name.Replace("Attribute", "")))}] ") + Name;

    public static bool operator ==(Parameter left, Parameter right)
        => left.Equals(right);

    public static bool operator !=(Parameter left, Parameter right)
        => !left.Equals(right);

    public static implicit operator Parameter(ParameterInfo parameter)
        => new Parameter(parameter.Name!,
            parameter.GetCustomAttributes(typeof(FunctionAttribute)).Cast<FunctionAttribute>(),
            parameter.ParameterType == typeof(ExpressionContext));

    public static implicit operator Parameter(string parameter)
        => new Parameter(parameter);
}
