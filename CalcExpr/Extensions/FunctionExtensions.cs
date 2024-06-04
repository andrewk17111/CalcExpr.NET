using CalcExpr.Attributes;
using CalcExpr.Context;
using CalcExpr.Expressions;
using CalcExpr.Expressions.Components;
using CalcExpr.FunctionAttributes;
using CalcExpr.FunctionAttributes.ConditionalAttributes;
using CalcExpr.FunctionAttributes.PreprocessAttributes;
using System.Linq.Expressions;
using System.Numerics;
using System.Reflection;

namespace CalcExpr.Extensions;

internal static class FunctionExtensions
{
    public static Delegate ToDelegate(this MethodInfo method)
    {
        return method.CreateDelegate(Expression.GetDelegateType(method
            .GetParameters()
            .Select(p => p.ParameterType)
            .Append(method.ReturnType)
            .ToArray()));
    }

    public static List<Parameter>? ToParameters(this IEnumerable<ParameterInfo> parameters)
    {
        List<Parameter> results = [];

        try
        {
            foreach (ParameterInfo parameter in parameters)
            {
                IEnumerable<FunctionAttribute> attributes = parameter.GetCustomAttributes(typeof(FunctionAttribute))
                    .Cast<FunctionAttribute>();

                if (parameter.ParameterType.GetInterface(nameof(IExpression)) is not null)
                {
                    attributes = attributes.Append(new IsExpressionTypeAttribute(parameter.ParameterType));
                }
                else if (parameter.ParameterType.IsAssignableFrom(typeof(IMinMaxValue<>)))
                {
                    int type_code = (int)Type.GetTypeCode(parameter.ParameterType.GetType());
                    double min = Convert.ToDouble(parameter.ParameterType.GetProperty("MinValue")?
                        .GetValue(parameter.ParameterType));
                    double max = Convert.ToDouble(parameter.ParameterType.GetProperty("MaxValue")?
                        .GetValue(parameter.ParameterType));

                    attributes = attributes.Append(new RangeAttribute(min, max));

                    if (type_code > 4 && type_code < 13)
                        attributes = attributes.Append(new IsIntegerAttribute());
                }
                else if (parameter.ParameterType == typeof(bool))
                {
                    attributes = attributes.Append(new AsBooleanAttribute());
                }

                results.Add(new Parameter(parameter.Name!, attributes,
                    parameter.ParameterType == typeof(ExpressionContext)));
            }
        }
        catch
        {
            return null;
        }

        return results;
    }

    public static bool IsCompatibleType(this Type type)
    {
        int type_code = (int)Type.GetTypeCode(type);

        return typeof(IExpression).IsAssignableFrom(type) || (type_code > 4 && type_code < 16) || type == typeof(bool);
    }

    public static Dictionary<string[], Function> GetFunctions(this Type type)
    {
        Dictionary<string[], Function> candidates = [];

        foreach (MethodInfo method in type.GetMethods())
        {
            BuiltInFunctionAttribute? bif = method.GetCustomAttribute<BuiltInFunctionAttribute>();

            if (bif is not null && method.ReturnType.IsCompatibleType())
            {
                List<Parameter>? parameters = method.GetParameters().ToParameters();

                if (parameters is not null)
                {
                    bool is_elementwise = method.GetCustomAttribute<ElementwiseAttribute>() is not null;
                    Function function = new Function(parameters, method.ToDelegate(), is_elementwise);

                    candidates[bif.Aliases] = function;
                }
            }
        }

        return candidates;
    }
}
