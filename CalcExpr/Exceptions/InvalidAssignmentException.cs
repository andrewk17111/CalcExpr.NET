using CalcExpr.Expressions;

namespace CalcExpr.Exceptions;

public class InvalidAssignmentException : Exception
{
    /// <summary>
    /// Initializes a new instance of the <see cref="InvalidAssignmentException"/> class using the specified invalid
    /// expression.
    /// </summary>
    /// <param name="expression">The invalide expression.</param>
    public InvalidAssignmentException(IExpression expression)
        : base($"The expression '{expression}' is not a valid assignment expression.")
    { }
}