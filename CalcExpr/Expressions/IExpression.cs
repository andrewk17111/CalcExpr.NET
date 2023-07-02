namespace CalcExpr.Expressions;

public interface IExpression
{
    public IExpression StepSimplify();

    public IExpression Simplify();

    public IExpression StepEvaluate();

    public IExpression Evaluate();
}
