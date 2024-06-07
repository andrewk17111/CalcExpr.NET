using CalcExpr.Expressions;

namespace CalcExpr.TypeConverters;

public interface ITypeConverter<T>
{
    /// <summary>
    /// Converts from a value to an expression.
    /// </summary>
    /// <param name="value">The value to convert.</param>
    /// <returns>An <see cref="IExpression"/> representing the value.</returns>
    IExpression ConvertToExpression(T value);

    /// <summary>
    /// Converts from an expression to a value.
    /// </summary>
    /// <param name="expression">The expression to convert.</param>
    /// <returns>A value representing the expression.</returns>
    T? ConvertFromExpression(IExpression expression);
}

public interface ITypeConverter
{
}
