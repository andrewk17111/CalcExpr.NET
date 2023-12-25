using CalcExpr.Expressions;

namespace CalcExpr.Exceptions;

/// <summary>
/// Initializes a new instance of the <see cref="InvalidAssignmentException"/> class using the specified invalid
/// expression.
/// </summary>
/// <param name="expression">The invalide expression.</param>
public class InvalidAssignmentException(IExpression expression)
    : Exception($"The expression '{expression}' is not a valid assignment expression.")
{ }