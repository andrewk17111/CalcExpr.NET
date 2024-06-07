using CalcExpr.FunctionAttributes;
using System.Text.RegularExpressions;
using CalcExpr.FunctionAttributes.ConditionalAttributes;
using CalcExpr.FunctionAttributes.PreprocessAttributes;

namespace CalcExpr.Expressions.Components;

public readonly struct Parameter(string name, IEnumerable<FunctionAttribute> attributes) : IParameter
{
    public readonly string Name = name;

    public FunctionAttribute[] Attributes { get; } = attributes.ToArray();

    public bool AllowNull { get; } = false;

    public Parameter(string name) : this(name, (IEnumerable<FunctionAttribute>)[])
    { }

    public Parameter(string name, IEnumerable<string> attributes) : this(name, attributes.Select(GetAttribute))
    { }

    public object? ProcessArgument(IExpression argument)
    {
        return IParameter.ApplyAttributes(argument, Attributes);
    }

    private static FunctionAttribute GetAttribute(string attribute)
    {
        string attribute_name = Regex.Match(attribute, @"(?<=^\s*)[A-Za-z][A-Za-z_0-9]*").Value;
        Type? attribute_type = AppDomain.CurrentDomain.GetAssemblies()
            .SelectMany(a => a.GetTypes())
            .FirstOrDefault(t => t.Name == attribute_name + "Attribute");
        
        if (attribute_type is not null && attribute_type.BaseType != typeof(FunctionAttribute))
        {
            double[] parameters = attribute.Contains('(')
                ? Regex.Match(attribute, @"(?<=\().*?(?=\))").Value.Split(',')
                    .Select(p => Convert.ToDouble(p.Trim().TrimStart('+'))).ToArray()
                : [];
            var constructors = attribute_type.GetConstructors()
                .Select(c => new
                {
                    Constructor = c,
                    Parameters = c.GetParameters(),
                    MinParameters = c.GetParameters().Where(p => !p.HasDefaultValue).Count()
                })
                .Where(c => parameters.Length >= c.MinParameters && parameters.Length <= c.Parameters.Length);
            FunctionAttribute? result = null; // = (FunctionAttribute?)Activator.CreateInstance(attribute_type);

            foreach (var constructor in constructors)
            {
                try
                {
                    object[] constructor_parameters = [.. constructor.Parameters
                        .Select((p, i) => i < parameters.Length
                            ? (object?)Convert.ChangeType(parameters[i], p.ParameterType)
                            : p.DefaultValue)];

                    result = (FunctionAttribute?)constructor.Constructor.Invoke(constructor_parameters);
                    break;
                }
                catch
                { }
            }

            if (result is not null)
                return result;
        }

        throw new Exception($"Cannot find FunctionAttribute type '{attribute}'.");
    }

    public override int GetHashCode()
        => Name.GetHashCode();

    public override bool Equals(object? obj)
    {
        if (obj is not null && obj is Parameter parameter)
            return parameter.Name == Name;

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

    public static implicit operator Parameter(string parameter)
        => new Parameter(parameter);
}

public interface IParameter
{
    public FunctionAttribute[] Attributes { get; }

    public bool AllowNull { get; }

    object? ProcessArgument(IExpression argument);

    public static IExpression? ApplyAttributes(IExpression argument, IEnumerable<FunctionAttribute> attributes)
    {
        foreach (FunctionAttribute attribute in attributes)
        {
            if (attribute is ConditionAttribute condition)
            {
                if (!condition.CheckCondition(argument))
                    return null;
            }
            else if (attribute is PreprocessAttribute preprocess)
            {
                argument = preprocess.Preprocess(argument);
            }
        }

        return argument;
    }
}
