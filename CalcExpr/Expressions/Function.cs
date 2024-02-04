using CalcExpr.Context;
using CalcExpr.Expressions.Components;
using CalcExpr.FunctionAttributes;
using CalcExpr.FunctionAttributes.ConditionalAttributes;
using CalcExpr.FunctionAttributes.PreprocessAttributes;
using System.Linq;

namespace CalcExpr.Expressions;

public class Function(IEnumerable<Parameter> parameters, Delegate body) : IFunction
{
    private readonly IReadOnlyList<Parameter> _parameters = parameters.ToArray() ?? [];

    public readonly Delegate Body = body;
    public readonly bool RequiresContext = body.Method.GetParameters()
        .Select(p => p.ParameterType == typeof(ExpressionContext))
        .Contains(true);

    public Parameter[] Parameters
        => _parameters.ToArray();

    public Function(Delegate body) : this(body.Method.GetParameters().Select(p => (Parameter)p), body)
    { }

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
        => Clone();

    public IExpression Evaluate(ExpressionContext context)
        => Clone();

    public IExpression StepEvaluate()
        => Clone();

    public IExpression StepEvaluate(ExpressionContext context)
        => Clone();

    public IExpression Clone()
        => new Function(Parameters, Body);

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
}
