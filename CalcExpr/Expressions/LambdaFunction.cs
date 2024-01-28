using CalcExpr.Context;
using CalcExpr.Expressions.Components;

namespace CalcExpr.Expressions;

public class LambdaFunction(IEnumerable<Parameter> parameters, IExpression body) : IFunction
{
    private readonly IReadOnlyList<Parameter> _parameters = parameters.ToArray();

    public readonly IExpression Body = body;

    public Parameter[] Parameters
        => _parameters.ToArray();

    public IExpression Invoke(IExpression[] arguments, ExpressionContext context)
    {
        ExpressionContext inner_context = context.Clone();

        foreach ((Parameter parameter, IExpression argument) in Parameters.Zip(arguments))
            inner_context[parameter.Name] = argument;

        IExpression result = Body.Evaluate(inner_context);
        string[] parameters = Parameters.Select(p => p.Name).ToArray();

        foreach (string variable in inner_context.Variables)
            if (!parameters.Contains(variable))
                context[variable] = inner_context[variable];

        return result;
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
        => new LambdaFunction(Parameters, Body.Clone());

    public override bool Equals(object? obj)
        => obj is not null && obj is LambdaFunction lambda && lambda.Parameters.Length == Parameters.Length &&
            (Parameters.Length == 0 || lambda.Parameters.Select((arg, i) => arg == Parameters[i])
                .Aggregate((a, b) => a && b)) && lambda.Body.Equals(Body);

    public override int GetHashCode()
        => Parameters.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"({String.Join(",", Parameters)})=>{Body.ToString(format)}";
}
