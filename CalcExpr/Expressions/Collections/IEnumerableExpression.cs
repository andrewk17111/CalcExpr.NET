namespace CalcExpr.Expressions.Collections;

public interface IEnumerableExpression : IExpression, IEnumerable<IExpression>
{
    /// <summary>
    /// Converts an <see cref="IEnumerable{IExpression}"/> to an <see cref="IEnumerableExpression"/>.
    /// </summary>
    /// <param name="expressions">The <see cref="IEnumerable{IExpression}"/> to convert.</param>
    /// <returns>The new <see cref="IEnumerableExpression"/>.</returns>
    static abstract IEnumerableExpression ConvertIEnumerable(IEnumerable<IExpression> expressions);

    /// <summary>
    /// Maps the <see cref="IEnumerableExpression"/> to a new <see cref="IEnumerableExpression"/> using the specified
    /// map function.
    /// </summary>
    /// <param name="map_func">
    /// The function to transform each element of the <see cref="IEnumerableExpression"/>.
    /// </param>
    /// <returns>A new <see cref="IEnumerableExpression"/> with the transformed elements.</returns>
    IEnumerableExpression Map(Func<IExpression, IExpression> map_func);
}
