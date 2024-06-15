using DiceEngine.Context;

namespace DiceEngine.Expressions.Dice;

public class PercentileDie : IDie
{
    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext context)
        => new RollResult(Roll(context.Random), this);

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext context)
        => Evaluate(context);

    public int Roll(Random? random = null)
        => (random ?? new Random()).Next(10) * 10;

    public override int GetHashCode()
        => 0;

    public override bool Equals(object? obj)
        => obj is PercentileDie;

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => "d%";
}
