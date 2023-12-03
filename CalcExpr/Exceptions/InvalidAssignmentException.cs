using CalcExpr.Expressions;

namespace CalcExpr.Exceptions;

public class InvalidAssignmentException : Exception
{
    /// <summary>
    /// Initializes a new instance of the <see cref="InvalidAssignmentException"/> class using the specified invalid
    /// expression.
    /// </summary>
    /// <param name="expression">The invalide <see cref="Variable"/> expression.</param>
    public InvalidAssignmentException(IExpression expression)
        : base($"'{expression}' is not a variable and therefore cannot use the assignment operator.")
    { }
}