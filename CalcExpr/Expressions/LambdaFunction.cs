﻿using CalcExpr.Context;
using CalcExpr.Expressions.Components;

namespace CalcExpr.Expressions;

public class LambdaFunction(IEnumerable<Parameter> parameters, IExpression body) : IFunction
{
    private readonly IReadOnlyList<Parameter> _parameters = parameters.ToArray();

    public readonly IExpression Body = body;

    public Parameter[] Parameters
        => _parameters.ToArray();

    public bool IsElementwise
        => false;

    public IExpression Invoke(IExpression[] arguments, ExpressionContext context)
    {
        ExpressionContext inner_context = context.Clone();
        IExpression[]? args = ((IFunction)this).ProcessArguments(arguments);

        if (args is null)
            return Constant.UNDEFINED;

        foreach ((Parameter parameter, IExpression argument) in Parameters.Zip(args))
            inner_context[parameter.Name] = argument;

        IExpression result = Body.Evaluate(inner_context);
        string[] parameters = Parameters.Select(p => p.Name).ToArray();

        foreach (string variable in inner_context.Variables)
            if (!parameters.Contains(variable))
                context[variable] = inner_context[variable];

        return result;
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
    {
        if (obj is not null && obj is LambdaFunction lambda)
        {
            bool parameters_equal = lambda.Parameters.Length == Parameters.Length &&
                (Parameters.Length == 0 || !lambda.Parameters.Select((arg, i) => arg == Parameters[i]).Any(x => !x));
            bool bodies_equal = lambda.Body.Equals(Body);

            return parameters_equal && bodies_equal;
        }

        return false;
    }

    public override int GetHashCode()
        => Parameters.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"({String.Join(",", Parameters)})=>{Body.ToString(format)}";
}
