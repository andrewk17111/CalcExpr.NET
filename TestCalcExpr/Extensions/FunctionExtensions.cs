using CalcExpr.Attributes;
using CalcExpr.Context;
using CalcExpr.Expressions;
using CalcExpr.Expressions.Components;
using CalcExpr.FunctionAttributes;
using CalcExpr.FunctionAttributes.ConditionalAttributes;
using System.Linq.Expressions;
using System.Reflection;

namespace TestCalcExpr.Extensions;

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

    public static List<IParameter>? ToParameters(this IEnumerable<ParameterInfo> parameters,
        IEnumerable<Type> compatible_types)
    {
        List<IParameter> results = [];

        try
        {
            foreach (ParameterInfo parameter in parameters)
            {
                IEnumerable<FunctionAttribute> attributes = parameter.GetCustomAttributes(typeof(FunctionAttribute))
                    .Cast<FunctionAttribute>();

                if (parameter.ParameterType == typeof(ExpressionContext))
                {
                    results.Add(new ContextParameter());
                    continue;
                }
                else if (parameter.ParameterType.IsAssignableFrom(typeof(IExpression)))
                {
                    if (parameter.ParameterType.GetInterface(nameof(IExpression)) is not null)
                        attributes = attributes.Append(new IsExpressionTypeAttribute(parameter.ParameterType));

                    results.Add(new Parameter(parameter.Name!, attributes));
                }
                else if (compatible_types.Contains(parameter.ParameterType))
                {
                    results.Add(new TypeParameter(parameter.ParameterType, attributes,
                        parameter.ParameterType.IsClass));
                }
                else if (parameter.ParameterType == typeof(Nullable<>))
                {
                    Type type = parameter.ParameterType.GetGenericArguments()[0];

                    if (compatible_types.Contains(type))
                        results.Add(new TypeParameter(type, attributes, true));
                }
            }
        }
        catch
        {
            return null;
        }

        return results;
    }
}
