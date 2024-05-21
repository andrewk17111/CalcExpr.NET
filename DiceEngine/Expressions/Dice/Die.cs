using DiceEngine.Context;

namespace DiceEngine.Expressions.Dice;

public class Die(int size) : IDie
{
    public readonly int Size = size;

    public Die(uint size) : this((int)size)
    {
    }

    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext context)
        => (Number)(context.Random.Next(Size) + 1);

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext context)
        => (Number)(context.Random.Next(Size) + 1);

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"d{Size}";
}

public interface IDie : IExpression
{
}
