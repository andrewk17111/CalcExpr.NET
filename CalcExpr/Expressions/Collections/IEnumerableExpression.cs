namespace CalcExpr.Expressions.Collections;

public interface IEnumerableExpression : IExpression, IEnumerable<IExpression>
{
    static abstract IEnumerableExpression ConvertIEnumerable(IEnumerable<IExpression> expressions);
}
