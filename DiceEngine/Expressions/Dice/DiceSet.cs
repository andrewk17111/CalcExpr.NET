using DiceEngine.Context;

namespace DiceEngine.Expressions.Dice;

public class DiceSet(int size, IDie die) : IDie
{
    public readonly int Size = size;
    public readonly IDie Die = die;

    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext context)
        => new RollResult(Enumerable.Range(1, Size).Select(x => Roll(context.Random)), this);

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext context)
        => Evaluate(context);

    public int Roll(Random? random = null)
        => Die.Roll();

    public override int GetHashCode()
        => Size;

    public override bool Equals(object? obj)
        => obj is DiceSet diceSet && diceSet.Size == Size && diceSet.Die.Equals(Die);

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"{Size}{Die.ToString(format)}";
}
