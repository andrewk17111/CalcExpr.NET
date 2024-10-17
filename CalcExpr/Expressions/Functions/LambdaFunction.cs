using CalcExpr.Context;
using CalcExpr.Expressions.Components;
using CalcExpr.Expressions.Terminals;

namespace CalcExpr.Expressions.Functions;

public class LambdaFunction(IEnumerable<Parameter> parameters, IExpression body) : Terminal, IFunction
{
    private readonly IReadOnlyList<Parameter> _parameters = parameters.ToArray();

    public readonly IExpression Body = body;

    public IParameter[] Parameters
        => _parameters.Cast<IParameter>().ToArray();

    public bool IsElementwise
        => false;

    public IExpression Invoke(IExpression[] arguments, ExpressionContext context)
    {
        ExpressionContext inner_context = context.Clone();
        IExpression[]? args = ((IFunction)this).ProcessArguments(arguments, context)?.Cast<IExpression>().ToArray();

        if (args is null)
            return Undefined.UNDEFINED;

        foreach ((IParameter parameter, IExpression? argument) in _parameters.Zip(args))
            if (parameter is Parameter param)
                inner_context[param.Name] = argument;

        IExpression result = Body.Evaluate(inner_context);
        string[] parameters = _parameters.Select(p => p.Name).ToArray();

        foreach (string variable in inner_context.Variables)
            if (!parameters.Contains(variable))
                context[variable] = inner_context[variable];

        return result;
    }

    public override bool Equals(object? obj)
    {
        if (obj is not null && obj is LambdaFunction lambda)
        {
            bool parameters_equal = lambda.Parameters.Length == Parameters.Length &&
                (Parameters.Length == 0 || !lambda.Parameters.Select((arg, i) => arg.Equals(Parameters[i])).Any(x => !x));
            bool bodies_equal = lambda.Body.Equals(Body);

            return parameters_equal && bodies_equal;
        }

        return false;
    }

    public override int GetHashCode()
        => Parameters.GetHashCode();

    public override string ToString(string? format)
        => $"({string.Join(",", _parameters)})=>{Body.ToString(format)}";
}
