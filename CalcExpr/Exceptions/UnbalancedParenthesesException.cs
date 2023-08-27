namespace CalcExpr.Exceptions;

public class UnbalancedParenthesesException : Exception
{
    /// <summary>
    /// Initializes a new instance of the <see cref="UnbalancedParenthesesException"/> class using the specified 
    /// unbalanced expression.
    /// </summary>
    /// <param name="expression">The expression with unbalanced parentheses.</param>
    public UnbalancedParenthesesException(string expression)
        : base($"The expression '{expression}' contains unbalanced parentheses.")
    { }
}
