namespace CalcExpr.Exceptions;

/// <summary>
/// Initializes a new instance of the <see cref="UnbalancedParenthesesException"/> class using the specified 
/// unbalanced expression.
/// </summary>
/// <param name="expression">The expression with unbalanced parentheses.</param>
public class UnbalancedParenthesesException(string expression)
    : Exception($"The expression '{expression}' contains unbalanced parentheses.")
{ }
