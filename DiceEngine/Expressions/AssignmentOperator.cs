using DiceEngine.Context;
using DiceEngine.Exceptions;

namespace DiceEngine.Expressions;

public class AssignmentOperator : IExpression
{
    public readonly Variable AssignedVariable;
    public readonly IExpression Value;

    /// <summary>
    /// Initializes a new instance of the <see cref="AssignmentOperator"/> class.
    /// </summary>
    /// <param name="assigned_variable">
    /// The <see cref="IExpression"/> variable that's getting assigned to. If <paramref name="assigned_variable"/> is
    /// not a <see cref="Variable"/>, an exception is thrown.
    /// </param>
    /// <param name="value">
    /// The <see cref="IExpression"/> value being assigned to <paramref name="assigned_variable"/>.
    /// </param>
    public AssignmentOperator(IExpression assigned_variable, IExpression value)
    {
        AssignedVariable = assigned_variable is Variable v
            ? v
            : throw new InvalidAssignmentException(assigned_variable);
        Value = value;
    }

    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext context)
    {
        context ??= new ExpressionContext();

        IExpression eval = Value.Evaluate(context);

        context[AssignedVariable.Name] = eval;
        return eval;
    }

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext context)
    {
        context ??= new ExpressionContext();

        IExpression eval = Value.StepEvaluate(context);

        if (eval.Equals(Value))
        {
            context[AssignedVariable.Name] = eval;
            return eval;
        }

        return new AssignmentOperator(AssignedVariable, eval);
    }

    public IExpression EvaluateDice()
        => EvaluateDice(new ExpressionContext());

    public IExpression EvaluateDice(ExpressionContext context)
        => new AssignmentOperator(AssignedVariable, Value.EvaluateDice(context));

    public override bool Equals(object? obj)
        => obj is not null && obj is AssignmentOperator assn_op &&
            assn_op.AssignedVariable.Equals(AssignedVariable) && assn_op.Value.Equals(Value);

    public override int GetHashCode()
        => HashCode.Combine(AssignedVariable, Value);

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"{AssignedVariable.ToString(format)}={Value.ToString(format)}";
}
