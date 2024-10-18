using CalcExpr.Attributes;
using CalcExpr.Context;
using CalcExpr.Expressions;
using CalcExpr.Expressions.Components;
using CalcExpr.Expressions.Functions;
using CalcExpr.FunctionAttributes;
using CalcExpr.FunctionAttributes.ConditionalAttributes;
using System.Linq.Expressions;
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
                else if (parameter.ParameterType.IsAssignableTo(typeof(IExpression)))
                {
                    if (parameter.ParameterType.GetInterface(nameof(IExpression)) is not null)
                        attributes = attributes.Append(new IsExpressionTypeAttribute(parameter.ParameterType));

                    results.Add(new Parameter(parameter.Name!, attributes));
                }
                else if (compatible_types.Contains(parameter.ParameterType))
                {
                    IParameter? p = (IParameter?)TypeParameter.InitializeTypeParameter(parameter.ParameterType,
                        attributes, parameter.ParameterType.IsClass);

                    if (p is not null)
                        results.Add(p);
                }
                else if (parameter.ParameterType.IsGenericType &&
                    parameter.ParameterType.GetGenericTypeDefinition() == typeof(Nullable<>))
                {
                    Type type = parameter.ParameterType.GetGenericArguments()[0];
                    IParameter? p = (IParameter?)TypeParameter.InitializeTypeParameter(type, attributes, true);

                    if (p is not null && compatible_types.Contains(type))
                        results.Add(p);
                }
            }
        }
        catch
        {
            return null;
        }

        return results;
    }

    public static Dictionary<string[], Function> GetFunctions(this Type type, IEnumerable<Type> compatible_types)
    {
        Dictionary<string[], Function> candidates = [];

        foreach (MethodInfo method in type.GetMethods().Where(x => x.IsStatic))
        {
            string name = method.Name;
            BuiltInFunctionAttribute? bif = method.GetCustomAttribute<BuiltInFunctionAttribute>();
            Type return_type = method.ReturnType.IsGenericType &&
                method.ReturnType.GetGenericTypeDefinition() == typeof(Nullable<>)
                    ? method.ReturnType.GetGenericArguments().Single()
                    : method.ReturnType;

            if (bif is not null && (method.ReturnType.IsAssignableFrom(typeof(IExpression)) ||
                compatible_types.Contains(return_type)))
            {
                List<IParameter>? parameters = method.GetParameters().ToParameters(compatible_types);

                if (parameters is not null)
                {
                    bool is_elementwise = method.GetCustomAttribute<ElementwiseAttribute>() is not null;
                    NativeFunction function = new NativeFunction(parameters, method.ToDelegate(), is_elementwise);

                    candidates[bif.Aliases] = function;
                }
            }
        }

        return candidates;
    }
}
