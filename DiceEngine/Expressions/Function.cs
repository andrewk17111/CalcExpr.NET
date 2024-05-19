using DiceEngine.Attributes;
using DiceEngine.Context;
using DiceEngine.Expressions.Collections;
using DiceEngine.Expressions.Components;
using DiceEngine.FunctionAttributes;
using DiceEngine.FunctionAttributes.ConditionalAttributes;
using DiceEngine.FunctionAttributes.PreprocessAttributes;
using System.Linq.Expressions;
using System.Reflection;

namespace DiceEngine.Expressions;

public class Function(IEnumerable<Parameter> parameters, Delegate body, bool is_elementwise = false) : IFunction
{
    private readonly IReadOnlyList<Parameter> _parameters = parameters.ToArray() ?? [];

    public readonly Delegate Body = body;
    public readonly bool RequiresContext = body.Method.GetParameters()
        .Select(p => p.ParameterType == typeof(ExpressionContext))
        .Contains(true);

    public Parameter[] Parameters
        => _parameters.ToArray();

    public bool IsElementwise
        => is_elementwise;

    public Function(MethodInfo method)
        : this (method.CreateDelegate(Expression.GetDelegateType(method
            .GetParameters()
            .Select(p => p.ParameterType)
            .Append(method.ReturnType)
            .ToArray())),
        method.GetCustomAttribute(typeof(ElementwiseAttribute)) is not null)
    { }

    public Function(Delegate body, bool is_elementwise = false)
        : this(body.Method.GetParameters().Select(p => (Parameter)p), body, is_elementwise)
    { }

    public static bool IsValidFunction(MethodInfo method, out string[]? aliases)
    {
        BuiltInFunctionAttribute? bif = method.GetCustomAttribute<BuiltInFunctionAttribute>();

        aliases = bif?.Aliases;

        if (bif is null)
            return false;

        if (!typeof(IExpression).IsAssignableFrom(method.ReturnType))
            return false;

        return method.GetParameters()
            .All(p => typeof(IExpression).IsAssignableFrom(p.ParameterType) ||
                p.ParameterType == typeof(ExpressionContext));
    }

    public IExpression Invoke(IExpression[] arguments, ExpressionContext context)
    {
        object?[] args;

        if (RequiresContext)
        {
            IExpression[]? processed_args = ((IFunction)this).ProcessArguments(arguments);

            if (processed_args is null)
                return Constant.UNDEFINED;

            int i = 0;

            args = [.. Parameters.Select(p => (object?)(p.IsContext ? context : processed_args[i++]))];
        }
        else
        {            
            IExpression[]? processed_args = ((IFunction)this)
                .ProcessArguments(arguments.Select(arg => arg.Evaluate(context)));

            if (processed_args is null)
                return Constant.UNDEFINED;

            args = [.. processed_args];
        }
        
        return (IExpression?)Body.Method.Invoke(this, args) ?? Constant.UNDEFINED;
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
        => obj is not null && obj is Function func && func.Body.Equals(Body) &&
            func.Parameters.Select((p, i) => p.Equals(Parameters[i])).Aggregate((a, b) => a && b);

    public override int GetHashCode()
        => HashCode.Combine(Parameters, Body);

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"";
}

public interface IFunction : IExpression
{
    public Parameter[] Parameters { get; }

    public bool IsElementwise { get; }

    public static ExpressionContext ContextReconciliation(ExpressionContext outer_context,
        ExpressionContext inner_context, IEnumerable<Parameter> parameters)
    {
        foreach (string variable in inner_context.Variables.Except(parameters.Select(p => p.Name)))
            outer_context[variable] = inner_context[variable];

        return outer_context;
    }

    public IExpression[]? ProcessArguments(IEnumerable<IExpression> arguments)
    {
        IExpression[] args = arguments.ToArray();
        List<IExpression> results = [];

        for (int i = 0; i < Parameters.Where(p => !p.IsContext).Count(); i++)
        {
            Parameter parameter = Parameters[i];
            IExpression argument = args[i];

            foreach (FunctionAttribute attribute in parameter.Attributes)
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

            results.Add(argument);
        }

        return [.. results];
    }

    public IExpression Invoke(IExpression[] arguments, ExpressionContext context);

    public static IExpression ForEach(MethodInfo function, IEnumerable<IExpression> arguments, ExpressionContext context)
        => ForEach(new Function(function), arguments, context);

    public static IExpression ForEach(IFunction function, IEnumerable<IExpression> arguments, ExpressionContext context)
    {
        if (arguments.Count() != function.Parameters.Where(p => !p.IsContext).Count())
            return Constant.UNDEFINED;

        if (function.IsElementwise && arguments.Any(arg => arg is IEnumerableExpression))
        {
            int length = arguments.Where(arg => arg is IEnumerableExpression)
                .Select(arg => ((IEnumerableExpression)arg).Count())
                .Min();
            List<IExpression> results = [];

            for (int i = 0; i < length; i++)
                results.Add(function.Invoke(arguments.Select(arg => arg is IEnumerableExpression enum_expr
                    ? enum_expr.ElementAt(i)
                    : arg).ToArray(), context));

            Type enum_type = arguments.First(arg => arg is IEnumerableExpression).GetType();
            MethodInfo? create_method = enum_type.GetMethod("ConvertIEnumerable", [typeof(IEnumerable<IExpression>)]);

            MethodInfo[] methods = enum_type.GetMethods(BindingFlags.Static | BindingFlags.Public);
            return (IEnumerableExpression?)create_method!.Invoke(null, [results])!;
        }
        else
        {
            return function.Invoke([.. arguments], context);
        }
    }
}
