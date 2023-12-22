using CalcExpr.Context;

namespace CalcExpr.Expressions;

public class LambdaFunction : IFunction
{
    private readonly IReadOnlyList<string> _parameters;

    public readonly IExpression Body;

    public string[] Parameters
        => _parameters.ToArray();

    public LambdaFunction(IEnumerable<string> parameters, IExpression body)
    {
        _parameters = parameters.ToArray();
        Body = body;
    }

    public IExpression Invoke(IExpression[] arguments, ExpressionContext context)
    {
        if (Parameters.Length != arguments.Length)
            return Constant.UNDEFINED;

        ExpressionContext inner_context = context.Clone();

        foreach ((string parameter, IExpression argument) in Parameters.Zip(arguments))
            inner_context[parameter] = argument;

        IExpression result = Body.Evaluate(inner_context);

        foreach (string variable in inner_context.Variables)
            if (!Parameters.Contains(variable))
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
            (!Parameters.Any() || lambda.Parameters.Select((arg, i) => arg == Parameters[i])
                .Aggregate((a, b) => a && b)) && lambda.Body.Equals(Body);

    public override int GetHashCode()
        => Parameters.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"({String.Join(",", Parameters)})=>{Body.ToString(format)}";
}
