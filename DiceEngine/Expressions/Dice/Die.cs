using DiceEngine.Context;

namespace DiceEngine.Expressions.Dice;

public class Die(uint size) : IDie
{
    public readonly uint Size = size;

    public Die(int size) : this((uint)size)
    {
    }

    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext context)
        => throw new NotImplementedException();

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext context)
        => throw new NotImplementedException();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => throw new NotImplementedException();
}

public interface IDie : IExpression
{
}
