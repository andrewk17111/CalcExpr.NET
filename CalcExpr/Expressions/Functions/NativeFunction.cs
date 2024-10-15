using CalcExpr.Attributes;
using CalcExpr.Context;
using CalcExpr.Expressions.Components;
using CalcExpr.Extensions;
using CalcExpr.TypeConverters;
using System.Reflection;

namespace CalcExpr.Expressions.Functions;

public class NativeFunction(IEnumerable<IParameter> parameters, Delegate body, bool is_elementwise = false) : IFunction
{
    private readonly IReadOnlyList<IParameter> _parameters = parameters.ToArray() ?? [];

    public readonly Delegate Body = body;
    public readonly bool RequiresContext = parameters
        .Any(p => p is ContextParameter);

    public IParameter[] Parameters
        => _parameters.ToArray();

    public bool IsElementwise
        => is_elementwise;

    public NativeFunction(MethodInfo method)
        : this(method.ToDelegate(), method.GetCustomAttribute(typeof(ElementwiseAttribute)) is not null)
    { }

    public NativeFunction(Delegate body, bool is_elementwise = false)
        : this(body.Method.GetParameters().ToParameters(ExpressionContext.DEFAULT_TYPES)!, body, is_elementwise)
    { }

    public IExpression Invoke(IExpression[] arguments, ExpressionContext context)
    {
        object?[] args;

        if (RequiresContext)
        {
            object?[]? processed_args = ((IFunction)this).ProcessArguments(arguments, context);

            if (processed_args is null)
                return Undefined.UNDEFINED;

            int i = 0;

            args = [.. Parameters.Select(p => p is ContextParameter ? context : processed_args[i++])];
        }
        else
        {
            object?[]? processed_args = ((IFunction)this)
                .ProcessArguments(arguments.Select(arg => arg.Evaluate(context)), context);

            if (processed_args is null)
                return Undefined.UNDEFINED;

            args = [.. processed_args];
        }

        object? result = Body.Method.Invoke(this, args);

        if (result is null)
        {
            return Undefined.UNDEFINED;
        }
        else if (result is IExpression expr)
        {
            return expr;
        }
        else
        {
            Type return_type = Body.Method.ReturnType.IsGenericType &&
                Body.Method.ReturnType.GetGenericTypeDefinition() == typeof(Nullable<>)
                    ? Body.Method.ReturnType.GetGenericArguments().Single()
                    : Body.Method.ReturnType;
            ITypeConverter[] converter = context.GetTypeConverters(return_type);

            return converter.ConvertToExpression(result) ?? Undefined.UNDEFINED;
        }
    }

    public IExpression Evaluate()
        => this;

    public IExpression Evaluate(ExpressionContext context)
        => this;

    public IExpression StepEvaluate()
        => this;

    public IExpression StepEvaluate(ExpressionContext context)
        => this;

    public override bool Equals(object? obj)
        => obj is not null && obj is NativeFunction func && func.Body.Equals(Body) &&
            func.Parameters.Select((p, i) => p.Equals(Parameters[i])).Aggregate((a, b) => a && b);

    public override int GetHashCode()
        => HashCode.Combine(Parameters, Body);

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"";
}
