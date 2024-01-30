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

    public Parameter(string name, IEnumerable<string> attributes)
        : this(name, attributes.Select(a => GetAttribute(a)), false)
    { }

    public Parameter(string name, bool is_context) : this(name, [], is_context)
    { }

    private static FunctionAttribute GetAttribute(string name)
    {
        Type? attribute_type = AppDomain.CurrentDomain.GetAssemblies()
            .SelectMany(a => a.GetTypes())
            .FirstOrDefault(t => t.Name == name + "Attribute");
        
        if (attribute_type is not null && attribute_type.BaseType != typeof(FunctionAttribute))
        {
            FunctionAttribute? result = (FunctionAttribute?)Activator.CreateInstance(attribute_type);

            if (result is not null)
                return result;
        }

        throw new Exception($"Cannot find FunctionAttribute type '{name}' with a constructor that takes no arguments");
    }

    public override int GetHashCode()
        => Name.GetHashCode();

    public override bool Equals(object? obj)
    {
        if (obj is not null && obj is Parameter parameter)
            return parameter.Name == Name && (parameter.IsContext == IsContext);

        return false;
    }

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
